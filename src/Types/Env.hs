module Types.Env where

import Hasql.Connection qualified as H
import MyPrelude
import Servant
import Types.SessionPool

data Env = Env
  { _envPgConn :: !H.Connection
  , _envInvidiousSession :: !ApiSessionPool
  , _envLogFunc :: !LogFunc
  }
makeLenses ''Env

type App a = RIO Env a

instance HasLogFunc Env where
  logFuncL = envLogFunc

class HasPgConn a where
  pgConn :: Lens' a H.Connection
instance HasPgConn H.Connection where
  pgConn = id
instance HasPgConn Env where
  pgConn = envPgConn

instance HasInvidiousSession Env where
  invidiousSession = envInvidiousSession

appEnv :: (MonadUnliftIO m) => LogFunc -> m Env
appEnv lf = do
  conn <- connectPsqlViaEnv "PSQL_URL"
  ivSess <- apiSessionPoolViaEnvs $ "INVIDIOUS_ROOT" :| ["INVIDIOUS_ROOT_FALLBACK"]
  return $ Env conn ivSess lf

connectPsqlViaEnv :: (MonadIO m) => String -> m H.Connection
connectPsqlViaEnv envVar =
  getEnv envVar
    >>= (liftIO . H.acquire . fromString)
    >>= \case
      Left (Just msg) -> error $ "could not get PSQL connection: " <> show msg
      Left Nothing -> error "could not get PSQL connection: unknown"
      Right v -> return v

apiSessionPoolViaEnvs :: (MonadIO m) => NonEmpty String -> m ApiSessionPool
apiSessionPoolViaEnvs envs = traverse getEnv envs >>= initApiSessionPool

initEnv :: RIO Env ()
initEnv = view invidiousSession >>= establishSession

runWithEnv :: LogOptions -> RIO Env a -> IO a
runWithEnv opts rio = withLogFunc opts $ appEnv >=> flip runRIO (initEnv >> rio)
