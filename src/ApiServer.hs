module ApiServer where

import ApiType
import MyPrelude
import Network.Wai
import Network.Wai.Handler.Warp
import RIO.Time (Day (..))
import Servant

users1 :: [User]
users1 =
  [ User "Isaac Newton" 372 "isaac@newton.co.uk" (ModifiedJulianDay 3)
  , User "Albert Einstein" 136 "ae@mc2.org" (ModifiedJulianDay 12)
  ]

server1 :: Server UserAPI1
server1 = return users1

userAPI :: Proxy UserAPI1
userAPI = Proxy
