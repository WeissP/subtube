module Api.Youtube where

import Api.Utils
import Data.Aeson
import Data.UnixTime
import Db.Command (pgRunTa)
import Db.Command qualified as C
import Invidious.Client qualified as I
import MyPrelude
import RIO.Time (Day (..))
import Rel8
import Servant
import Servant.Docs (ToSample (..), singleSample)
import Types

type YoutubeAPI = ChannelAPI

type ChannelAPI =
  Capture "channel_id" YtbChannelID :> CacheRefreshThresholdParam :> Get '[JSON] ChannelWithMeta
    :<|> "search" :> QueryParam "query" Text :> Get '[JSON] [ChannelWithMeta]
    :<|> Capture "channel_id" YtbChannelID
      :> "videos"
      :> SortOrderParam
      :> CacheRefreshThresholdParam
      :> OffsetParam
      :> LimitParam
      :> Get '[JSON] [YoutubeVideoWithMeta]

youtubeAPI :: Proxy YoutubeAPI
youtubeAPI = Proxy

youtubeServer :: ServerT YoutubeAPI ApiM
youtubeServer = getYtbChannel :<|> undefined :<|> undefined

getYtbChannel :: YtbChannelID -> Maybe Unix -> ApiM ChannelWithMeta
getYtbChannel = fmap (fmap snd) . ytbChannelWithCache
