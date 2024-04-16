module Javascript where

import ApiServer (userAPI)
import Control.Monad.IO.Class
import Data.Aeson
import Language.Javascript.JQuery
import MyPrelude
import Network.Wai
import Network.Wai.Handler.Warp
import qualified RIO.List as L
import Servant
import Servant.JS
import System.Environment as SE

apiJS1 :: Text
apiJS1 = jsForAPI userAPI jquery

generateJS :: FilePath -> IO ()
generateJS = writeJSForAPI userAPI jquery
