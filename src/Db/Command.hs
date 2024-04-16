module Db.Command where

import MyPrelude hiding (each)

import Data.Functor.Contravariant ((>$<))
import Data.UUID qualified as UUID
import Db.Queries qualified as Q
import GHC.Exts (IsList)
import Hasql.Session qualified as H
import Hasql.Statement qualified as H
import Hasql.Transaction (Transaction)
import Hasql.Transaction qualified as HT
import Hasql.Transaction.Sessions as HT
import RIO.Map qualified as Map
import RIO.NonEmpty qualified as NE
import RIO.Partial qualified as RIO'
import Rel8
import System.Environment (getEnv)
import System.IO (print)
import Types

asTa :: H.Statement () a -> Transaction a
asTa = HT.statement ()

pgExecSession :: (MonadReader env m, HasPgConn env, MonadIO m) => H.Session a -> m a
pgExecSession session = do
  conn <- view pgConn
  res <- liftIO $ H.run session conn
  case res of
    Left msg -> throwIO msg
    Right v -> return v

class (Monad m) => PgM m where
  pgRunTa :: Transaction a -> m a

instance PgM (RIO Env) where
  pgRunTa ta = pgExecSession $ HT.transaction HT.Serializable HT.Write ta

groupedTagMembers ::
  MediaTag
  -> [TaggingMethod]
  -> Transaction [(TaggingMethod, [UUID])]
groupedTagMembers tag methods = asTa $ run $ select $ Q.groupedTagMembers tag methods

aggVideos ::
  Offset
  -> Limit
  -> [(TaggingMethod, [UUID])]
  -> Transaction [AggMediaWithMeta]
aggVideos os lmt tms = do
  res <- asTa $ run $ select $ Q.pagination os lmt $ Q.aggVideos tms
  return $ fromAggVideosResult <$> res

channelCacheInfo :: UUID -> Transaction (Maybe ChannelCacheInfo)
channelCacheInfo gid = do
  res <- asTa $ runMaybe $ select $ Q.channelCacheInfo (lit gid)
  return $ fromChannelCacheInfoView <$> res

tagByName :: MediaTag -> Transaction (Maybe TagInfo)
tagByName tag = do
  res <- asTa $ runMaybe $ select $ Q.tagByName (lit tag)
  return $ fromTagEntry <$> res

tagMemberDetail :: MediaTag -> Transaction (Maybe TagMemberDetail)
tagMemberDetail tag = do
  ti <- tagByName tag
  grouped <- groupedTagMembers tag allTaggingMethods
  let byMethod m = concat $ lookup m grouped
  ytbVids <- asTa $ run $ select $ Q.ytbVideoAndMediaInUUIDs (byMethod ByYoutubeVideo)
  ytbChans <- asTa $ run $ select $ Q.inUUIDs (byMethod ByYoutubeChannel)
  return $ ti <&> \t ->
    TagMemberDetail
      t
      (fromYtbVideoAndMediaEntry <$> ytbVids)
      (snd . fromYtbChannelEntry <$> ytbChans)

updateYtbChannelVideosCachedAt :: YtbChannelID -> CachedAt -> Transaction ()
updateYtbChannelVideosCachedAt cid ca =
  asTa
    $ run_
    $ update
    $ Update
      { target = entrySchema (Proxy @YtbChannelEntry)
      , from = pure ()
      , set = \_ row -> row {yceVideosCachedAt = lit ca}
      , updateWhere = \_ row -> yceChanID row ==. lit cid
      , returning = NoReturning
      }

ytbChannelByID :: YtbChannelID -> Transaction (Maybe (UUID, ChannelWithMeta))
ytbChannelByID cid =
  fmap (fmap fromYtbChannelEntry)
    <$> asTa
    $ runMaybe
    $ select
    $ Q.ytbChannelByID (lit cid)

ytbVideoByID :: YtbVideoID -> Transaction (Maybe YoutubeVideoWithMeta)
ytbVideoByID vid =
  fmap (fmap fromYtbVideoAndMediaEntry)
    <$> asTa
    $ runMaybe
    $ select
    $ Q.ytbVideoByID (lit vid)

class (IsEntry entry) => Insertable entry ins | ins -> entry where
  insertValues :: ins -> entry Expr
  insertOne ::
    ins
    -> OnConflict (entry Name)
    -> Returning (entry Name) a
    -> Statement a
  insertOne ins c r =
    insert
      $ Insert
        { into = entrySchema (Proxy :: Proxy entry)
        , rows = values [insertValues ins]
        , onConflict = c
        , returning = r
        }
  insertList ::
    [ins]
    -> OnConflict (entry Name)
    -> Returning (entry Name) a
    -> Statement a
  insertList inses c r =
    insert
      $ Insert
        { into = entrySchema (Proxy :: Proxy entry)
        , rows = values $ insertValues <$> inses
        , onConflict = c
        , returning = r
        }

class Upsertable entry where
  upsertInfo :: Upsert (entry Name)
  upsert :: OnConflict (entry Name)
  upsert = DoUpdate upsertInfo

instance Insertable YtbChannelEntry ChannelWithMeta where
  insertValues (WithMeta (Channel {..}) (ChannelMeta {..})) =
    YtbChannelEntry
      { yceID = unsafeDefault
      , yceChanID = lit _chID
      , yceName = lit _chName
      , yceDesc = lit _chDesc
      , yceSubCount = lit _chSubCount
      , yceIntroduction = lit _cmIntroduction
      , yceThumbnail = lit _chThumbnail
      , yceInfoCachedAt = lit _cmInfoCachedAt
      , yceVideosCachedAt = lit _cmVideosCachedAt
      , yceUpdatedAt = unsafeDefault
      }

instance Upsertable YtbChannelEntry where
  upsertInfo =
    Upsert
      { index = yceChanID
      , predicate = Nothing
      , set = \new old -> new {yceID = yceID old, yceUpdatedAt = unsafeDefault}
      , updateWhere = \_ _ -> lit True
      }

instance Upsertable MediaEntry where
  upsertInfo =
    Upsert
      { index = meID
      , predicate = Nothing
      , set = \new old -> new {meID = meID old, meUpdatedAt = unsafeDefault}
      , updateWhere = \_ _ -> lit True
      }

instance Insertable MediaEntry MediaWithMeta where
  insertValues (WithMeta (MediaInfo {..}) (MediaMeta {..})) =
    MediaEntry
      { meID = unsafeDefault
      , meTitle = lit _mediaTitle
      , meDuration = lit _mediaDuration
      , meCachedAt = lit (Just _mediaCachedAt)
      , meIntroduction = lit _mediaIntroduction
      , mePublishedAt = lit _mediaPublishedAt
      , meThumbnail = lit _mediaThumbnail
      , meUpdatedAt = unsafeDefault
      }

instance Upsertable YtbVideoEntry where
  upsertInfo =
    Upsert
      { index = yveVideoID
      , predicate = Nothing
      , set = \new old -> new {yveID = yveID old, yveUpdatedAt = unsafeDefault}
      , updateWhere = \_ _ -> lit True
      }

instance Insertable YtbVideoEntry (UUID, YoutubeInfo) where
  insertValues (uuid, YoutubeInfo {..}) =
    YtbVideoEntry
      { yveID = lit uuid
      , yveVideoID = lit _yiVideoID
      , yveChanID = lit _yiChannelID
      , yveDesc = lit _yiDesc
      , yveUpdatedAt = unsafeDefault
      }

instance Insertable MediaEntry (CachedAt, MediaInfo) where
  insertValues (ca, MediaInfo {..}) =
    MediaEntry
      { meID = unsafeDefault
      , meTitle = lit _mediaTitle
      , meDuration = lit _mediaDuration
      , meCachedAt = lit (Just ca)
      , meIntroduction = lit Nothing
      , meThumbnail = lit _mediaThumbnail
      , mePublishedAt = lit _mediaPublishedAt
      , meUpdatedAt = unsafeDefault
      }

instance Insertable TagEntry TagPost where
  insertValues (TagPost {..}) =
    TagEntry
      { teID = unsafeDefault
      , teName = lit _tpTag
      , teIntroduction = lit _tpIntro
      , teUpdatedAt = unsafeDefault
      }
instance Upsertable TagEntry where
  upsertInfo =
    Upsert
      { index = teID
      , predicate = Nothing
      , set = \new old -> new {teID = teID old}
      , updateWhere = \_ _ -> lit True
      }

instance Insertable TagMemberEntry TagMember where
  insertValues (TagMember {..}) =
    TagMemberEntry
      { tmeID = lit _tmID
      , tmeMemberID = lit _tmMemberID
      , tmeTaggedBy = lit _tmTaggedBy
      , tmeUpdatedAt = unsafeDefault
      }

insertChannelVideos :: CachedAt -> NonEmpty YoutubeVideo -> Transaction ()
insertChannelVideos ca videos = do
  let cid = first1Of (traverse1 . mediaExtraInfo . yiChannelID) videos
      mediaInfos = videos ^.. traverse . mediaInfo
      ytbInfos = videos ^.. traverse . mediaExtraInfo
  uuids <- asTa $ run $ insertList ((ca,) <$> mediaInfos) DoNothing (Returning meID)
  asTa $ run_ $ insertList (zip uuids ytbInfos) DoNothing NoReturning
  updateYtbChannelVideosCachedAt cid ca

insertYtbVideo :: CachedAt -> YoutubeVideo -> Transaction YoutubeVideoWithMeta
insertYtbVideo ca vid = do
  media <- asTa $ run1 $ insertOne (ca, vid ^. mediaInfo) upsert (Returning id)
  ytb <-
    asTa
      $ run1
      $ insertOne (meID media, vid ^. mediaExtraInfo) upsert (Returning id)
  return $ fromYtbVideoAndMediaEntry $ YtbVideoAndMediaEntry media ytb
