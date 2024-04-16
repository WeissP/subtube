module Api.Utils where

import Control.Monad.Except (ExceptT (..), MonadError)
import Control.Monad.Trans.Except (runExceptT)
import Data.ByteString.Builder (toLazyByteString)
import Data.Kind (Type)
import Db.Command (pgRunTa)
import Db.Command qualified as C
import Invidious.Client qualified as I
import MyPrelude hiding (Handler)
import RIO.ByteString.Lazy qualified as BL
import RIO.NonEmpty qualified as NE
import RIO.Vector qualified as V
import RIO.Vector.Partial qualified as V'
import Rel8 qualified as Rel8
import Servant
import Types

type CacheRefreshThreshold = Maybe Unix

noCrt :: CacheRefreshThreshold
noCrt = Just maxBound

ytbChannelWithCache :: YtbChannelID -> CacheRefreshThreshold -> ApiM (UUID, ChannelWithMeta)
ytbChannelWithCache cid maxCrt =
  pgRunTa (C.ytbChannelByID cid) >>= \v -> withCache cid v maxCrt

ytbVideoWithCache :: YtbVideoID -> CacheRefreshThreshold -> ApiM YoutubeVideoWithMeta
ytbVideoWithCache vid crt = pgRunTa (C.ytbVideoByID vid) >>= \v -> withCache vid v crt

tagInfoByName :: MediaTag -> ApiM TagInfo
tagInfoByName tag =
  pgRunTa (C.tagByName tag)
    >>= ifNotFound ("Could not find tag: " <> displayShow tag)

type family param --^ description where
  Capture' mods sym a --^ description =
    Capture' (Description description ': mods) sym a
  QueryParam' mods sym a --^ description =
    QueryParam' (Description description ': mods) sym a
  Header' mods sym a --^ description =
    Header' (Description description ': mods) sym a
  ReqBody' mods sym a --^ description =
    ReqBody' (Description description ': mods) sym a

type CacheRefreshThresholdParam = QueryParam "cache_refresh_threshold" Unix --^ "A UNIX timestamp that sets the oldest allowed cache. If the cached information is (strictly) older than this threshold, it will be updated and then returned; otherwise, it can be directly returned."

type SortOrderParam = QueryParam "sort_order" MediaSortOrder
type OffsetParam = QueryParam "offset" Offset
type LimitParam = QueryParam "limit" Limit

type ApiM = RIO Env

handleApiM :: Env -> ApiM a -> Handler a
handleApiM env m = Handler (ExceptT (runRIO env (try m)))

orNotFound :: ApiM a -> ApiM a
orNotFound run =
  run `catchAny` \e ->
    logException e >> (throwIO $ err404 {errBody = fromString $ displayException e})

ifNotFound :: (MonadIO m) => Utf8Builder -> Maybe a -> m a
ifNotFound (Utf8Builder msg) Nothing =
  throwIO $ err404 {errBody = toLazyByteString msg}
ifNotFound _ (Just v) = return v

class Cacheable a where
  type RetrieveParams a :: Type
  type Retrieved a :: Type
  cachedAtL :: Getter a Unix
  retrieveNew ::
    RetrieveParams a
    -> Maybe a
    -- ^ cache
    -> ApiM (Retrieved a)
  upsertAndReturn :: Retrieved a -> ApiM a
  withCache :: RetrieveParams a -> Maybe a -> Maybe Unix -> ApiM a
  withCache key (Just c) maxCrt = do
    logDebug "Found cache"
    now <- currentUnix
    let cachedSec = now - c ^. cachedAtL
    if cachedSec <= getOrDft maxCrt
      then logDebug "using cached value" >> return c
      else do
        logDebug
          $ "value was cached "
          <> displayShow cachedSec
          <> " seconds ago, which exceeds maximal allowed cached threshold "
          <> displayShow (getOrDft maxCrt)
          <> " , retrieving new"
        new <- retrieveNew key (Just c)
        upsertAndReturn new
  withCache key c@Nothing _ = do
    logDebug "No cache was found, retrieving new"
    n <- retrieveNew key c
    upsertAndReturn n

instance Cacheable YoutubeVideoWithMeta where
  type RetrieveParams YoutubeVideoWithMeta = YtbVideoID
  type Retrieved YoutubeVideoWithMeta = (CachedAt, YoutubeVideo)
  cachedAtL = meta . mediaCachedAt
  retrieveNew vid _ = (,) <$> currentUnix <*> I.videoByID vid
  upsertAndReturn (ca, vid) = pgRunTa $ C.insertYtbVideo ca vid

instance Cacheable (UUID, ChannelWithMeta) where
  type RetrieveParams (UUID, ChannelWithMeta) = YtbChannelID
  type Retrieved (UUID, ChannelWithMeta) = ChannelWithMeta
  cachedAtL = _2 . meta . cmInfoCachedAt
  retrieveNew cid cache = orNotFound do
    ch <- I.channelByID cid
    now <- currentUnix
    return $ WithMeta ch case cache of
      Just c -> c ^. _2 . meta & cmVideosCachedAt .~ now
      Nothing -> ChannelMeta now 0 Nothing
  upsertAndReturn new = do
    uuid <-
      C.pgRunTa $ C.asTa $ Rel8.run1 $ C.insertOne new C.upsert (Rel8.Returning yceID)
    return (uuid, new)

instance Cacheable ChannelCacheInfo where
  type RetrieveParams ChannelCacheInfo = (YtbChannelID, Limit)
  type Retrieved ChannelCacheInfo = (ChannelCacheInfo, Maybe (NonEmpty YoutubeVideo))
  cachedAtL = cciCachedAt
  retrieveNew (cid, limit) cci = do
    let stop vs = length vs >= fromIntegral limit || noNewVideos
          where
            retrievedDate = lastOf (traverse . mediaInfo . mediaPublishedAt) vs
            cacheDate = cci ^? _Just . cciNewestPublishedAt
            noNewVideos = case (retrievedDate, cacheDate) of
              (Just rd, Just c) -> rd <= c
              _ -> False
    now <- currentUnix
    videos <- I.channelVideos stop Newest cid
    return $ case (cci, NE.nonEmpty (toList videos)) of
      (_, Just vs) ->
        ( ChannelCacheInfo cid now (NE.head vs ^. mediaInfo . mediaPublishedAt)
        , Just vs
        )
      (Just c, Nothing) -> (c & cciCachedAt .~ now, Nothing)
      (Nothing, Nothing) -> (ChannelCacheInfo cid now 0, Nothing)

  upsertAndReturn (cci, videos) = C.pgRunTa insert $> cci
    where
      insert = traverse_ (C.insertChannelVideos (cci ^. cciCachedAt)) videos
