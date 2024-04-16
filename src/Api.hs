module Api (appServer, appAPI, handleApiM) where

import Api.Docs
import Api.Tag
import Api.Utils
import Api.Youtube
import Data.Swagger (Swagger)
import GHC.TypeLits (Symbol)
import MyPrelude
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Swagger (toSwagger)
import Servant.Swagger.UI
import Types

type BasicAPI (version :: Symbol) =
  version :> ("tag" :> TagAPI :<|> "youtube" :> YoutubeAPI)
type BasicAPI1 = BasicAPI "v1"

basicAPI1 :: Proxy BasicAPI1
basicAPI1 = Proxy

type AppAPI = SwaggerUIAPI :<|> BasicAPI1

appAPI :: Proxy AppAPI
appAPI = Proxy

appServer :: ServerT AppAPI ApiM
appServer = serveSwagger basicAPI1 :<|> tagServer :<|> youtubeServer
