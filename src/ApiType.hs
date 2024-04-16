module ApiType where

import Data.Aeson
import Data.Swagger (ToSchema)
import MyPrelude
import RIO.Time (Day (..))
import Servant
import Servant.Docs (ToSample (..), singleSample)

type UserAPI1 = "users" :> Get '[JSON] [User]

data SortBy = Age | Name

data User = User
  { name :: String
  , age :: Int
  , email :: String
  , registration_date :: Day
  }
  deriving (Eq, Show, Generic)

instance ToJSON User
instance ToSchema User
instance ToSample User where
  toSamples _ = singleSample $ User "Isaac Newton" 372 "isaac@newton.co.uk" (ModifiedJulianDay 3)
