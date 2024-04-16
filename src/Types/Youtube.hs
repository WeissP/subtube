module Types.Youtube where

import Data.StaticText (st)
import Data.StaticText qualified as S
import Data.Swagger
import Data.Swagger.ParamSchema
import Data.UUID (UUID)
import Data.UnixTime
import Data.Validity (Validation, Validity, validate)
import Data.Validity qualified as VA

import MyPrelude
import RIO.Set qualified as Set
import Servant
import Servant.Docs
import Types.General
import Types.Media
import Types.Utils

idCharSet :: Set Char
idCharSet = Set.fromList $ ['a' .. 'z'] <> ['A' .. 'Z'] <> ['-', '_'] <> ['0' .. '9']
validateIdCharSet :: Char -> Validation
validateIdCharSet c = VA.declare "" $ Set.member c idCharSet

newtype YtbChannelID = YtbChannelID (FixedLenText 24)
  deriving stock (Generic)
  deriving newtype (DBEq, DBType, Eq, Ord, Show, ToParamSchema, FromJSON, ToJSON, FromHttpApiData)
instance ToSchema YtbChannelID where
  declareNamedSchema =
    pure
      . swaggerExp (YtbChannelID (FixedLenText $(st "UCP1AejCL4DA7jYkZAELRhHQ")))
      . swaggerDesc "The youtube channel ID (must be 24 ASCII characters)"
      . paramSchemaToNamedSchema swaggerSchemaOptions
instance Validity YtbChannelID where
  validate (YtbChannelID t) =
    VA.decorate "YtbChannelID"
      $ validate t
      <> decorateFixedLenText t validateIdCharSet

newtype YtbVideoID = YtbVideoID (FixedLenText 11)
  deriving (Generic)
  deriving newtype (DBEq, DBType, Eq, Ord, Show, ToParamSchema, FromJSON, ToJSON, FromHttpApiData)
instance ToSample YtbVideoID where
  toSamples _ = singleSample (YtbVideoID (FixedLenText $(st "lOwjw1Ja83Y")))
instance ToSchema YtbVideoID where
  declareNamedSchema =
    pure
      . swaggerExp (YtbVideoID (FixedLenText $(st "lOwjw1Ja83Y")))
      . swaggerDesc "The youtube video ID (must be 11 ASCII characters)"
      . paramSchemaToNamedSchema swaggerSchemaOptions
instance Validity YtbVideoID where
  validate (YtbVideoID t) =
    VA.decorate "YtbVideoID"
      $ validate t
      <> decorateFixedLenText t validateIdCharSet

data Channel = Channel
  { _chID :: YtbChannelID
  , _chName :: Text
  , _chDesc :: DescHTML
  , _chSubCount :: Int32
  , _chThumbnail :: Thumbnail
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (Validity)
makeClassy ''Channel
instance ToSchema Channel where
  declareNamedSchema =
    fmap (swaggerDesc "Youtube Channel information")
      . genericDeclareNamedSchema swaggerSchemaOptions
instance ToJSON Channel where
  toJSON = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions

data ChannelMeta = ChannelMeta
  { _cmInfoCachedAt :: Unix
  , _cmVideosCachedAt :: Unix
  , _cmIntroduction :: Maybe UTF8Text
  }
  deriving (Generic, Eq, Show)
  deriving anyclass (Validity)
makeClassy ''ChannelMeta
instance IsMeta ChannelMeta
instance ToSchema ChannelMeta where
  declareNamedSchema =
    fmap (swaggerDesc "Youtube Channel meta information, such information can not be found from Youtube but only from internal database")
      . genericDeclareNamedSchema swaggerSchemaOptions
instance ToJSON ChannelMeta where
  toJSON = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions

type ChannelWithMeta = WithMeta "ChannelWithMeta" Channel ChannelMeta

data YoutubeInfo = YoutubeInfo
  { _yiChannelID :: YtbChannelID
  , _yiVideoID :: YtbVideoID
  , _yiDesc :: DescHTML
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (Validity)
makeLenses ''YoutubeInfo
instance ToJSON YoutubeInfo where
  toJSON = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions
instance ToSchema YoutubeInfo where
  declareNamedSchema =
    fmap (swaggerDesc "Youtube video information")
      . genericDeclareNamedSchema swaggerSchemaOptions

type YoutubeVideo = Media "YoutubeVideo" YoutubeInfo
type YoutubeVideoWithMeta = WithMeta "YoutubeVideoWithMeta" YoutubeVideo MediaMeta

data FromChannel a = FromChannel
  { _fcChannelID :: YtbChannelID
  , _fcCachedAt :: Unix
  , _fcVideos :: [a]
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (Validity)
makeLenses ''FromChannel

data ChannelCacheInfo = ChannelCacheInfo
  {_cciChannelID :: YtbChannelID, _cciCachedAt :: Unix, _cciNewestPublishedAt :: Unix}
  deriving (Generic, Show, Eq)
  deriving anyclass (Validity)
makeLenses ''ChannelCacheInfo
