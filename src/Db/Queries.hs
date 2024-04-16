module Db.Queries where

import Data.Functor.Apply qualified as A
import Data.Functor.Contravariant ((>$<))
import Data.Functor.Contravariant.Divisible
import Hasql.Session qualified as Session
import MyPrelude hiding (each)
import Rel8
import Rel8 qualified as R
import Types

pagination :: Offset -> Limit -> Query a -> Query a
pagination (Offset os) (Limit lmt) = limit lmt . offset os

byUUID :: (UUIDEntry entry) => Expr UUID -> Query (entry Expr)
byUUID uuid = limit 1 do
  res <- each (entrySchema (Proxy :: Proxy entry))
  where_ $ uuidKey res ==. uuid
  return res

inUUIDs :: (UUIDEntry entry) => [UUID] -> Query (entry Expr)
inUUIDs ids = do
  res <- each (entrySchema (Proxy :: Proxy entry))
  where_ $ in_ (uuidKey res) (lit <$> ids)
  return res

joinedInUUIDs ::
  forall a b.
  (UUIDEntry a, UUIDEntry b) =>
  [UUID]
  -> Query (a Expr, b Expr)
joinedInUUIDs ids = do
  as <- each $ entrySchema (Proxy @a)
  bs <- each $ entrySchema (Proxy @b)
  where_ $ uuidKey as ==. uuidKey bs &&. in_ (uuidKey as) (lit <$> ids)
  pure (as, bs)

ytbVideoAndMediaInUUIDs :: [UUID] -> Query (YtbVideoAndMediaEntry Expr)
ytbVideoAndMediaInUUIDs uuids = do
  (media, ytb) <- joinedInUUIDs uuids
  pure $ YtbVideoAndMediaEntry media ytb

tagByName :: Expr MediaTag -> Query (TagEntry Expr)
tagByName tName = limit 1 $ do
  res <- each $ entrySchema Proxy
  where_ $ teName res ==. tName
  return res

channelsVideos ::
  [UUID]
  -> Query
      ( Expr YtbChannelID
      , Expr Unix
      , ListTable Expr (YtbVideoAndMediaEntry Expr)
      )
channelsVideos ids =
  aggregate1
    do
      cid <- groupByOn (yceChanID . fst)
      ca <- maxOn (yceVideosCachedAt . fst)
      vids <- listAggOn snd
      pure (cid, ca, vids)
    do
      chans <- inUUIDs @YtbChannelEntry ids
      ytbs <- each $ entrySchema (Proxy @YtbVideoEntry)
      medias <- each $ entrySchema (Proxy @MediaEntry)
      where_ $ yceChanID chans ==. yveChanID ytbs &&. yveID ytbs ==. meID medias
      return (chans, YtbVideoAndMediaEntry medias ytbs)

channelCacheInfo :: Expr UUID -> Query (ChannelCacheInfoView Expr)
channelCacheInfo gid = do
  chan <- byUUID @YtbChannelEntry gid
  pa <- aggregate
    do toAggregator (lit (Unix 0)) $ maxOn mePublishedAt
    do
      ytbs <- each $ entrySchema (Proxy @YtbVideoEntry)
      medias <- each $ entrySchema (Proxy @MediaEntry)
      where_ $ meID medias ==. yveID ytbs &&. yveChanID ytbs ==. yceChanID chan
      return medias
  pure $ ChannelCacheInfoView (yceChanID chan) (yceVideosCachedAt chan) pa

ytbVideos :: Expr MediaTag -> Query (YtbVideoEntry Expr)
ytbVideos tName = tagByName tName <&> teID >>= byUUID

tagMembers :: MediaTag -> [TaggingMethod] -> Query (TagMemberEntry Expr)
tagMembers tag methods = do
  tid <- teID <$> tagByName (lit tag)
  res <- each $ entrySchema Proxy
  where_ $ (tmeID res ==. tid) &&. in_ (tmeTaggedBy res) (lit <$> methods)
  return res

groupedTagMembers ::
  MediaTag
  -> [TaggingMethod]
  -> Query (Expr TaggingMethod, ListTable Expr (Expr UUID))
groupedTagMembers tag methods =
  aggregate1
    do
      tm <- groupByOn tmeTaggedBy
      members <- listAggOn tmeMemberID
      pure (tm, members)
    do
      tagMembers tag methods

ytbChannelByID :: Expr YtbChannelID -> Query (YtbChannelEntry Expr)
ytbChannelByID cid = do
  res <- each $ entrySchema Proxy
  where_ $ yceChanID res ==. cid
  return res

ytbVideoByID :: Expr YtbVideoID -> Query (YtbVideoAndMediaEntry Expr)
ytbVideoByID vid = do
  ytbs <- each $ entrySchema (Proxy @YtbVideoEntry)
  medias <- each $ entrySchema (Proxy @MediaEntry)
  where_ $ uuidKey medias ==. uuidKey ytbs &&. yveVideoID ytbs ==. vid
  return $ YtbVideoAndMediaEntry medias ytbs

aggVideosView :: Query AggMediaView
aggVideosView = do
  medias <- each $ entrySchema (Proxy @MediaEntry)
  maybeYtbs <- Rel8.optional $ do
    ytbs <- each $ entrySchema (Proxy @YtbVideoEntry)
    where_ $ meID medias ==. yveID ytbs
    return ytbs
  return (medias, maybeYtbs)

aggVideosByTaggingMethod :: TaggingMethod -> [UUID] -> Query AggMediaView
aggVideosByTaggingMethod ByYoutubeVideo ids = do
  (medias, ytbs) <- aggVideosView
  where_ $ in_ (meID medias) (lit <$> ids)
  return (medias, ytbs)
aggVideosByTaggingMethod ByYoutubeChannel ids = do
  (medias, ytbs) <- aggVideosView
  chans <- inUUIDs @YtbChannelEntry ids
  where_ $ (yveChanID $? ytbs) ==. (yceChanID $? justTable chans)
  return (medias, ytbs)

aggVideoOrder :: Query AggMediaView -> Query AggMediaView
aggVideoOrder = orderBy $ divide id (mePublishedAt >$< desc) mempty

aggVideos :: [(TaggingMethod, [UUID])] -> Query AggMediaView
aggVideos = aggVideoOrder . mconcat . fmap (uncurry aggVideosByTaggingMethod)
