module Api.Tag where

import Api.Utils
import Api.Youtube (getYtbChannel)
import Data.Aeson
import Data.StaticText (st)
import Data.UnixTime
import Db.Command (pgRunTa)
import Db.Command qualified as C
import MyPrelude hiding (Handler)
import RIO.List qualified as L
import RIO.Map qualified as Map
import RIO.Time (Day (..))
import Rel8 qualified
import Servant
import Servant.Docs (ToSample (..), singleSample)
import Types

type TagAPI =
  ReqBody '[JSON] TagPost :> Post '[JSON] TagInfo
    :<|> Capture "tag" MediaTag :> TagNameAPI

type TagNameAPI =
  Get '[JSON] TagMemberDetail
    :<|> CacheRefreshThresholdParam
      :> OffsetParam
      :> LimitParam
      :> QueryParams "allowed_tagging_methods" TaggingMethod
      :> "videos"
      :> Get '[JSON] [AggMediaWithMeta]
    :<|> ReqBody '[JSON] TagMemberSummary :> Post '[JSON] TagMemberDetail

tagAPI :: Proxy TagAPI
tagAPI = Proxy

tagServer :: ServerT TagAPI ApiM
tagServer = tagPost :<|> tagNameServer

tagNameServer :: MediaTag -> ServerT TagNameAPI ApiM
tagNameServer tag = tagByName tag :<|> tagAggMedias tag :<|> tagMemberPost tag

tagByName :: MediaTag -> ApiM TagMemberDetail
tagByName tag =
  pgRunTa (C.tagMemberDetail tag) >>= ifNotFound "No tag found after inserted"

tagAggMedias :: MediaTag -> Maybe Unix -> Maybe Offset -> Maybe Limit -> [TaggingMethod] -> ApiM [AggMediaWithMeta]
tagAggMedias tag maxCrt offsetMaybe limitMaybe methods = do
  let offset = getOrDft offsetMaybe
      limit = getOrDft limitMaybe
  grouped <- pgRunTa $ C.groupedTagMembers tag methods
  let chans = concat $ L.lookup ByYoutubeChannel grouped
  ccis <- catMaybes <$> traverse (pgRunTa . C.channelCacheInfo) chans
  logDebug $ "retriving youtube channels " <> displayShow ccis
  traverse_
    (\cci -> withCache (cci ^. cciChannelID, limit) (Just cci) maxCrt)
    ccis
  pgRunTa $ C.aggVideos offset limit grouped

tagPost :: TagPost -> ApiM TagInfo
tagPost tp = do
  got <- pgRunTa $ C.asTa $ Rel8.run1 $ C.insertOne tp C.upsert (Rel8.Returning id)
  return $ fromTagEntry got

tagMemberPost :: MediaTag -> TagMemberSummary -> ApiM TagMemberDetail
tagMemberPost tag tp = do
  ti <- tagInfoByName tag
  let tid = ti ^. tagID
  ytbChans <- mapConcurrently (flip ytbChannelWithCache noCrt) (tp ^. tmpByYoutubeChannel)
  ytbVids <- mapConcurrently (flip ytbVideoWithCache noCrt) (tp ^. tmpByYoutubeVideo)
  let chanTm =
        ytbChans <&> \(chUUID, _) -> TagMember tid chUUID ByYoutubeChannel
      vidTm = ytbVids <&> \v -> TagMember tid (v ^. meta . mediaID) ByYoutubeVideo
  pgRunTa
    $ C.asTa
    $ Rel8.run_
    $ C.insertList (chanTm <> vidTm) Rel8.DoNothing Rel8.NoReturning
  tagByName tag

mockTag :: RIO Env ()
mockTag = do
  let tag = MediaTag "test"
  tagPost $ TagPost tag (Just "just for test")
  tagMemberPost tag
    $ TagMemberSummary
      [YtbVideoID (FixedLenText $(st "lOwjw1Ja83Y"))]
      [ YtbChannelID (FixedLenText $(st "UCP1AejCL4DA7jYkZAELRhHQ"))
      , YtbChannelID (FixedLenText $(st "UC_iD0xppBwwsrM9DegC5cQQ"))
      ]
  return ()
