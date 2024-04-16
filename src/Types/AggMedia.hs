module Types.AggMedia where

import Data.Kind (Type)
import Data.Swagger
import Data.Swagger.Internal.Schema qualified as SW
import MyPrelude
import Types.General
import Types.Media
import Types.Utils
import Types.Youtube

data AggMedia = AggMedia
  { _amInfo :: MediaInfo
  , _amYtbInfo :: Maybe YoutubeInfo
  }
  deriving (Generic, Show, Eq)
makeClassy ''AggMedia
instance ToJSON AggMedia where
  toJSON = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions
instance SW.ToSchema AggMedia where
  declareNamedSchema =
    fmap (swaggerDesc "Aggregated Media information, which contains all possible information of a media")
      . genericDeclareNamedSchema swaggerSchemaOptions

class AsAggMedia a where
  asAggMedia :: a -> AggMedia
  asAggMediaWithMeta :: WithMeta nameA a m -> WithMeta nameB AggMedia m
  asAggMediaWithMeta wm = wm & entry %~ asAggMedia

instance AsAggMedia YoutubeVideo where
  asAggMedia (Media {..}) = AggMedia _mediaInfo (Just _mediaExtraInfo)

instance AsAggMedia MediaInfo where
  asAggMedia m = AggMedia m Nothing

type AggMediaWithMeta = WithMeta "AggMediaWithMeta" AggMedia MediaMeta
type AggMediaWithSingleTagMeta =
  WithMeta "AggMediaWithSingleTagMeta" AggMedia SingleTagMediaMeta

isoAggMedia :: Iso' MediaInfo AggMedia
isoAggMedia = iso (\mi -> AggMedia mi Nothing) (view amInfo)
