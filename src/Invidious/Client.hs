module Invidious.Client where

import Control.Monad.Catch (MonadCatch)
import Data.Aeson qualified as J
import Invidious.Parser
import MyPrelude
import Network.Wreq qualified as W
import Network.Wreq.Lens (responseBody)
import Network.Wreq.Session qualified as WS
import Types

type InvidiousM env m = (MonadReader env m, HasLogFunc env, HasInvidiousSession env, MonadUnliftIO m, MonadThrow m, HasCallStack)

getJSON ::
  (InvidiousM env m, FromJSON a, Show a) =>
  W.Options
  -> String
  -> m a
getJSON opts url = do
  logDebug $ "getting JSON from " <> displayShow url <> " with params: " <> displayShow opts
  sp <- view invidiousSession
  res <- withSessionPool sp (\s root -> WS.getWith opts s (root <> url)) >>= W.asJSON
  case res ^? responseBody of
    (Just v) -> return v
    Nothing -> throwString $ "no responseBody found of response: " <> show res

channelByID ::
  (InvidiousM env m) =>
  YtbChannelID
  -> m Channel
channelByID chId = getJSON opts ("v1/channels/" <> show chId)
  where
    opts = W.defaults & W.param "fields" .~ ["authorId,author,descriptionHtml,subCount,authorThumbnails"]

channelVideos' ::
  (InvidiousM env m, Semigroup (m (Vector YoutubeVideo))) =>
  Maybe Text
  -> Vector YoutubeVideo
  -> (Vector YoutubeVideo -> Bool)
  -> MediaSortOrder
  -> YtbChannelID
  -> m (Vector YoutubeVideo)
channelVideos' cont finished stop sort chId = do
  let opts = W.defaults & contParam cont
  WithCont @"videos" newVideos newCont <- getJSON opts ("v1/channels/" <> show chId <> "/videos")
  let videos = finished <> newVideos
  if isNothing newCont || stop videos
    then return videos
    else channelVideos' newCont videos stop sort chId

channelVideos ::
  (InvidiousM env m, Semigroup (m (Vector YoutubeVideo))) =>
  (Vector YoutubeVideo -> Bool)
  -> MediaSortOrder
  -> YtbChannelID
  -> m (Vector YoutubeVideo)
channelVideos = channelVideos' Nothing mempty

videoByID ::
  (InvidiousM env m) =>
  YtbVideoID
  -> m YoutubeVideo
videoByID vid = getJSON opts ("v1/videos/" <> show vid)
  where
    opts = W.defaults & W.param "fields" .~ ["authorId,videoId,descriptionHtml,published,lengthSeconds,title,videoThumbnails"]
