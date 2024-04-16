module Types.TagMember where

import Data.Aeson
import Data.Swagger.Internal.Schema qualified as SW
import Data.Swagger.ParamSchema qualified as SW
import Data.Validity (Validity)
import MyPrelude
import Types.Media
import Types.Utils
import Types.Youtube

data TagMemberSummary = TagMemberSummary
  { _tmpByYoutubeVideo :: [YtbVideoID]
  , _tmpByYoutubeChannel :: [YtbChannelID]
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (Validity)
makeLenses ''TagMemberSummary
instance FromJSON TagMemberSummary where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON TagMemberSummary where
  toJSON = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions
instance SW.ToSchema TagMemberSummary where
  declareNamedSchema =
    fmap (swaggerDesc "the summary of Tag Member")
      . SW.genericDeclareNamedSchema swaggerSchemaOptions

data TagMemberDetail = TagMemberDetail
  { _tmTagInfo :: TagInfo
  , _tmprByYoutubeVideo :: [YoutubeVideoWithMeta]
  , _tmprByYoutubeChannel :: [ChannelWithMeta]
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (Validity)
instance ToJSON TagMemberDetail where
  toJSON = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions
instance SW.ToSchema TagMemberDetail where
  declareNamedSchema =
    fmap (swaggerDesc "the detail of Tag Member")
      . SW.genericDeclareNamedSchema swaggerSchemaOptions

data TagMember = TagMember
  { _tmID :: UUID
  , _tmMemberID :: UUID
  , _tmTaggedBy :: TaggingMethod
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (Validity)
makeLenses ''TagMember
