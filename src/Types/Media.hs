module Types.Media where

import Data.Aeson
import Data.Swagger
import Data.Swagger.Internal.Schema (named)
import Data.Swagger.Internal.Schema qualified as SW
import Data.Swagger.ParamSchema qualified as SW
import Data.Validity
import Data.Validity.Text (decorateText)
import Data.Validity.UUID ()
import GHC.TypeLits (KnownNat, KnownSymbol, Symbol, symbolVal)
import MyPrelude
import RIO.ByteString qualified as B
import RIO.Text qualified as T
import Servant
import Types.General
import Types.Utils
import Web.HttpApiData (parseBoundedTextData)

data MediaSource = Youtube | Local
  deriving stock (Generic, Eq, Read, Show, Enum, Bounded)
  deriving anyclass (DBEq, ToJSON, Validity)
  deriving (DBType) via ReadShow MediaSource
makePrisms ''MediaSource
instance ToParamSchema MediaSource
instance ToSchema MediaSource where
  declareNamedSchema =
    pure
      . swaggerDesc "The source of the media"
      . named "MediaSource"
      . paramSchemaToSchema
instance FromHttpApiData MediaSource where
  parseQueryParam = parseBoundedTextData

data MediaSortOrder = Newest | Popular
  deriving stock (Generic, Eq, Read, Show, Enum, Bounded)
  deriving anyclass (DBEq, ToJSON, Validity)
  deriving (DBType) via ReadShow MediaSortOrder
instance Default MediaSortOrder where def = Newest
instance ToParamSchema MediaSortOrder where
  toParamSchema ty = withDefault ty $ genericToParamSchema swaggerSchemaOptions ty
instance ToSchema MediaSortOrder where
  declareNamedSchema =
    pure
      . swaggerDesc "The sort order of a list of medias"
      . named "MediaSortOrder"
      . paramSchemaToSchema
instance FromHttpApiData MediaSortOrder where
  parseQueryParam = parseBoundedTextData

newtype MediaTag = MediaTag UTF8Text
  deriving stock (Generic)
  deriving newtype (DBEq, DBType, Eq, Read, Show, ToJSON, FromJSON)
instance BoundedLength MediaTag where
  bLength (MediaTag tag) = T.length $ tag ^. from isoUTF8Text
  bMinLength _ = 1
  bMaxLength _ = 50
instance Validity MediaTag where
  validate mt@(MediaTag tag) = decorate "MediaTag" $ validateLength mt <> validate tag

data TagPost = TagPost
  { _tpTag :: MediaTag
  , _tpIntro :: Maybe UTF8Text
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (Validity)
makeLenses ''TagPost
instance FromJSON TagPost where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON TagPost where
  toJSON = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions
instance SW.ToSchema TagPost where
  declareNamedSchema =
    fmap (swaggerDesc "Tag Post")
      . SW.genericDeclareNamedSchema swaggerSchemaOptions

instance ToParamSchema MediaTag where
  toParamSchema _ =
    SW.toParamSchema (Proxy @Text)
      & validLengthSchema (Proxy @MediaTag)
instance ToSchema MediaTag where
  declareNamedSchema =
    pure
      . swaggerDesc "The tag of media"
      . named "MediaTag"
      . paramSchemaToSchema
instance FromHttpApiData MediaTag where
  parseUrlPiece t = parseUrlPiece t <&> MediaTag >>= prettyValidateText

data TagInfo = TagInfo
  { _tagID :: UUID
  , _tagName :: MediaTag
  , _tagIntro :: Maybe UTF8Text
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (Validity)
makeClassy ''TagInfo
instance ToJSON TagInfo where
  toJSON = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions
instance SW.ToSchema TagInfo where
  declareNamedSchema =
    fmap (swaggerDesc "Tag information")
      . genericDeclareNamedSchema swaggerSchemaOptions

data TaggingMethod = ByYoutubeVideo | ByYoutubeChannel
  deriving stock (Generic, Eq, Read, Show, Enum, Bounded, Ord)
  deriving anyclass (DBEq, ToJSON, Validity)
  deriving (DBType) via ReadShow TaggingMethod
makePrisms ''TaggingMethod
instance FromHttpApiData TaggingMethod where
  parseQueryParam = parseBoundedTextData
instance ToParamSchema TaggingMethod
instance ToSchema TaggingMethod where
  declareNamedSchema =
    pure
      . swaggerDesc "The method to tag a media"
      . named "TaggingMethod"
      . paramSchemaToSchema
allTaggingMethods :: [TaggingMethod]
allTaggingMethods = [minBound .. maxBound]

data MediaTagMeta = MediaTagMeta
  {_mtmTag :: MediaTag, _mtmMethods :: [TaggingMethod]}
  deriving (Generic, Show, Eq)
  deriving anyclass (Validity)
makeLenses ''MediaTagMeta
instance IsMeta MediaTagMeta
instance ToSchema MediaTagMeta where
  declareNamedSchema =
    fmap (swaggerDesc "media meta information regarding tag")
      . genericDeclareNamedSchema swaggerSchemaOptions

newtype Thumbnail = Thumbnail Text
  deriving stock (Generic)
  deriving newtype (DBEq, DBType, Eq, Show, ToParamSchema, ToJSON, FromHttpApiData)
  deriving anyclass (Validity)
instance ToSchema Thumbnail where
  declareNamedSchema =
    pure
      . swaggerDesc "The url to the thumbnail"
      . paramSchemaToNamedSchema swaggerSchemaOptions

type MediaTitle = UTF8Text
data MediaInfo = MediaInfo
  { _mediaDuration :: Duration
  , _mediaTitle :: MediaTitle
  , _mediaThumbnail :: Thumbnail
  , _mediaPublishedAt :: Unix
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (Validity)
makeLenses ''MediaInfo
instance ToJSON MediaInfo where
  toJSON = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions
instance ToSchema MediaInfo where
  declareNamedSchema =
    fmap (swaggerDesc "General media information")
      . genericDeclareNamedSchema swaggerSchemaOptions

data Media (name :: Symbol) extra = Media
  { _mediaExtraInfo :: extra
  , _mediaInfo :: MediaInfo
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (Validity)
makeLenses ''Media
instance (ToJSON extra) => ToJSON (Media name extra) where
  toJSON = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions
instance (ToSchema extra, KnownSymbol name) => ToSchema (Media name extra) where
  declareNamedSchema =
    fmap (SW.named (T.pack (symbolVal (Proxy :: Proxy name))))
      . genericDeclareSchema swaggerSchemaOptions

data MediaMeta = MediaMeta
  { _mediaID :: UUID
  , _mediaCachedAt :: Unix
  , _mediaIntroduction :: Maybe UTF8Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (IsMeta, Validity)
makeClassy ''MediaMeta
instance ToJSON MediaMeta where
  toJSON = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions
instance ToSchema MediaMeta where
  declareNamedSchema =
    fmap (swaggerDesc "Media Meta information")
      . genericDeclareNamedSchema swaggerSchemaOptions

type MediaWithMeta = WithMeta "MediaWithMeta" MediaInfo MediaMeta

data SingleTagMediaMeta = SingleTagMediaMeta
  { _stmMeta :: MediaMeta
  , _stmTagMeta :: MediaTagMeta
  }
  deriving (Generic, Show)
  deriving anyclass (Validity)
makeLenses ''SingleTagMediaMeta
instance IsMeta SingleTagMediaMeta
instance ToSchema SingleTagMediaMeta where
  declareNamedSchema =
    fmap (swaggerDesc "media meta information got by a single tag")
      . genericDeclareNamedSchema swaggerSchemaOptions
