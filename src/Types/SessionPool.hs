module Types.SessionPool (withSessionPool, ApiSessionPool, initApiSessionPool, HasInvidiousSession (..), establishSession) where

import MyPrelude
import Network.HTTP.Client
import Network.Wreq.Session
import RIO.NonEmpty qualified as NE
import RIO.NonEmpty.Partial qualified as NE'
import System.Random

type Url = String

data ApiSessionPool = ApiSessionPool
  { apiRoots :: NonEmpty Url
  , apiSession :: IORef (Maybe (Url, Session))
  }

checkConnection ::
  (MonadReader env m, HasLogFunc env, MonadUnliftIO m) =>
  Url
  -> m (Maybe (Url, Session))
checkConnection url = catchConnectionError (getSess <&> fmap (url,)) (return Nothing)
  where
    getSess = do
      s <- liftIO newAPISession
      liftIO $ get s url
      logDebug $ "Found working URL for API:" <> display url
      return (Just s)

catchConnectionError :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m) => m a -> m a -> m a
catchConnectionError run onErr = run `catch` handler
  where
    handler e@(HttpExceptionRequest _ (StatusCodeException _ _)) = throwIO e
    handler e = logException e >> onErr

establishSession ::
  (MonadReader env m, HasLogFunc env, MonadUnliftIO m) =>
  ApiSessionPool
  -> m ()
establishSession sp@(ApiSessionPool {..}) = do
  logDebug $ "establishing Session among candidates " <> listDisplay apiRoots
  s <- firstJustsM (apiRoots <&> checkConnection)
  atomicWriteIORef apiSession s
  when (isNothing s) $ do
    logDebug "no available sessions right now, will try to establish Session 20 minutes later"
    withAsync (threadDelay (20 * 60 * 1000 * 1000) >> establishSession sp) (const (return ()))

initApiSessionPool :: (MonadIO m) => NonEmpty Url -> m ApiSessionPool
initApiSessionPool urls = newIORef Nothing <&> ApiSessionPool urls

noAvailableSessions :: (MonadIO m) => m a
noAvailableSessions = throwString "Currently there are no available sessions"

currentSession :: (MonadUnliftIO m) => ApiSessionPool -> m (Url, Session)
currentSession (ApiSessionPool {..}) =
  readIORef apiSession >>= \case
    Just v -> return v
    Nothing -> noAvailableSessions

withSessionPool' ::
  (MonadReader env m, HasLogFunc env, MonadUnliftIO m) =>
  Int
  -> ApiSessionPool
  -> (Session -> Url -> IO a)
  -> m a
withSessionPool' 0 _ _ = noAvailableSessions
withSessionPool' chances sp run = do
  (url, sess) <- currentSession sp
  catchConnectionError (liftIO (run sess url)) (establishSession sp >> withSessionPool' (chances - 1) sp run)

withSessionPool ::
  (MonadReader env m, HasLogFunc env, MonadUnliftIO m) =>
  ApiSessionPool
  -> (Session -> Url -> IO a)
  -> m a
withSessionPool = withSessionPool' 1

class HasInvidiousSession a where
  invidiousSession :: Lens' a ApiSessionPool
