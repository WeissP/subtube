module Main where

import Api
import Api.Tag (mockTag)
import MyPrelude
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Types

main :: IO ()
main = do
  logOptions <- logOptionsHandle stderr True
  portStr <- getEnv "SERVER_PORT"
  let port =
        fromMaybe
          (error $ "Could not parse port: " <> portStr)
          (readMaybe portStr)
  withLogFunc logOptions $ \lf -> do
    env <- appEnv lf
    runRIO env initEnv
    runRIO env mockTag
    run port (serve appAPI $ hoistServer appAPI (handleApiM env) appServer)
