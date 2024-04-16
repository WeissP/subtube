module Api.Docs where

import Api.Utils
import Api.Youtube
import Data.Swagger
import MyPrelude
import Network.Wai
import Servant
import Servant.Docs qualified as D
import Servant.Server
import Servant.Swagger
import Servant.Swagger.UI
import Types

type SwaggerUIAPI = SwaggerSchemaUI "docs" "swagger.json"

basicSwagger :: Swagger
basicSwagger =
  mempty
    & info
    . title
    .~ "Subtube API"
    & info
    . version
    .~ "1.0"
    & info
    . description
    ?~ "API Specification for Subtube"
    & info
    . license
    ?~ ("MIT" & url ?~ URL "http://mit.com")

serveSwagger ::
  forall {k} (api :: k).
  (HasSwagger api) =>
  Proxy api
  -> ServerT SwaggerUIAPI ApiM
serveSwagger api = swaggerSchemaUIServerT $ basicSwagger <> toSwagger api
