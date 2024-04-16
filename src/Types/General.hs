module Types.General where

import Data.Aeson qualified as J
import Data.Coerce (Coercible, coerce)
import Data.Monoid (Any (..), Ap)
import Data.StaticText (Static)
import Data.StaticText qualified as S
import Data.Swagger qualified as SW
import Data.Swagger.Declare qualified as SW
import Data.Swagger.Internal.Schema qualified as SW
import Data.UnixTime (UnixTime (..), getUnixTime)
import Data.Validity (Validation, Validity)
import Data.Validity qualified as VA
import Data.Validity.Text ()
import Data.Validity.Text qualified as VA
import Foreign.C (CTime (..))
import GHC.TypeLits (KnownNat, KnownSymbol, Symbol, symbolVal)
import GHC.TypeNats (Nat, natVal)
import MyPrelude
import RIO.ByteString qualified as B
import RIO.Char qualified as C
import RIO.List qualified as L
import RIO.Text qualified as T
import RIO.Time (NominalDiffTime)
import Rel8 (DBMax, DBOrd)
import Rel8 qualified as R
import Servant
import Servant.Docs
import System.Posix.Types (EpochTime)
import System.Random
import Text.Read (readsPrec)
import Types.Utils

newtype Offset = Offset Word
  deriving stock (Generic)
  deriving newtype (Eq, Show, FromJSON, FromHttpApiData, Num, Default, Ord, Real, Enum, Integral)
  deriving anyclass (ToJSON)
instance SW.ToParamSchema Offset where
  toParamSchema ty = SW.toParamSchema (Proxy @Word) & withDefault ty
instance BoundedWord Offset where
  bwL = coerced
  bwMax _ = 100000
instance Validity Offset where
  validate = VA.decorate "Offset" . validateBound

newtype Limit = Limit Word
  deriving stock (Generic)
  deriving newtype (Eq, Show, FromJSON, FromHttpApiData, Num, Ord, Real, Enum, Integral)
  deriving anyclass (ToJSON)
instance Default Limit where
  def = Limit 20
instance SW.ToParamSchema Limit where
  toParamSchema ty = SW.toParamSchema (Proxy @Word) & withDefault ty
instance BoundedWord Limit where
  bwL = coerced
  bwMax _ = 100000
instance Validity Limit where
  validate = VA.decorate "Limit" . validateBound

newtype UTF8Text = UTF8Text {fromUTF8Text :: ByteString}
  deriving stock (Generic)
  deriving newtype (Eq, Read, DBType, DBEq, IsString)
instance Show UTF8Text where
  show t = show $ t ^. from isoUTF8Text
instance Display UTF8Text where
  textDisplay = view (from isoUTF8Text)

isoUTF8Text :: Iso' Text UTF8Text
isoUTF8Text = iso (UTF8Text . encodeUtf8) (decodeUtf8Lenient . fromUTF8Text)
instance SW.ToSchema UTF8Text where
  declareNamedSchema _ =
    pure
      $ SW.named "UTF8Text" (SW.toSchema (Proxy :: Proxy Text))
      & swaggerDesc "Text encoded in UTF8"
instance FromJSON UTF8Text where
  parseJSON = fmap (view isoUTF8Text) <$> parseJSON
instance ToJSON UTF8Text where
  toJSON = toJSON . view (from isoUTF8Text)
instance Validity UTF8Text where
  validate (UTF8Text t) =
    VA.declare "UTF8Text should has valid UTF8 encoding" $ B.isValidUtf8 t
instance FromHttpApiData UTF8Text where
  parseUrlPiece = fmap (view isoUTF8Text) <$> parseUrlPiece

newtype DescHTML = DescHTML UTF8Text
  deriving stock (Generic)
  deriving newtype (DBEq, DBType, Eq, Show, FromJSON)
  deriving anyclass (ToJSON, Validity)

instance SW.ToSchema DescHTML where
  declareNamedSchema _ =
    pure
      $ SW.named "DescHTML" (SW.toSchema (Proxy :: Proxy Text))
      & swaggerDesc "Description in HTML form"

newtype Unix = Unix Int64
  deriving (Enum, Generic)
  deriving newtype (DBEq, DBType, DBOrd, DBMax, Eq, Show, Ord, SW.ToParamSchema, FromJSON, ToJSON, FromHttpApiData, Bounded, Default, Num)
type CachedAt = Unix
instance Validity Unix where
  validate (Unix x) = VA.check (x >= 0) "Unix time must be at least 0"

currentUnix :: (MonadIO m) => m Unix
currentUnix = do
  now <- liftIO getUnixTime
  let (CTime sec) = utSeconds now
  return $ Unix sec

instance SW.ToSchema Unix where
  declareNamedSchema =
    pure
      . (SW.schema . SW.minimum_ ?~ 0)
      . swaggerDesc "The number of non-leap seconds which have passed since 00:00:00 UTC on Thursday, 1 January 1970"
      . SW.paramSchemaToNamedSchema swaggerSchemaOptions

class IsMeta a

data WithMeta (name :: Symbol) entry m = WithMeta
  {_entry :: entry, _meta :: m}
  deriving (Generic, Show, Eq)
  deriving anyclass (Validity)
makeLenses ''WithMeta
instance (SW.ToSchema entry, SW.ToSchema m, KnownSymbol name) => SW.ToSchema (WithMeta name entry m) where
  declareNamedSchema =
    fmap (SW.named (T.pack (symbolVal (Proxy :: Proxy name))))
      . SW.genericDeclareSchema swaggerSchemaOptions

instance (ToJSON entry, ToJSON m) => ToJSON (WithMeta name entry m) where
  toJSON = J.genericToJSON aesonOptions
  toEncoding = J.genericToEncoding aesonOptions

type Duration = Unix

newtype FixedLenText len = FixedLenText (Static Text len)
  deriving stock (Generic)
  deriving newtype (Eq, Ord)

decorateFixedLenText :: FixedLenText len -> (Char -> Validation) -> Validation
decorateFixedLenText (FixedLenText t) = VA.decorateText (S.unwrap t)

instance Show (FixedLenText n) where
  show (FixedLenText t) = T.unpack $ S.unwrap t

instance (KnownNat n) => DBType (FixedLenText n) where
  typeInformation = R.parseTypeInformation parseFixedLenText tshow R.typeInformation

instance (KnownNat n) => BoundedLength (FixedLenText n) where
  bLength _ = bMinLength (Proxy @(FixedLenText n))
  bMinLength _ = fromEnum $ natVal (Proxy @n)
  bMaxLength = bMinLength

parseFixedLenText ::
  forall (n :: Nat) s.
  (KnownNat n, IsString s) =>
  Text
  -> Either s (FixedLenText n)
parseFixedLenText t =
  maybeToEither
    ( fromString
        $ show
        $ wrapShow "[" t "]"
        <> " should have length "
        <> expectedLen
        <> " but got "
        <> actualLen
    )
    (S.create t ^? _Just . coerced)
  where
    actualLen = tshow $ T.length t
    expectedLen = tshow $ natVal (Proxy :: Proxy n)

instance ToJSON (FixedLenText len) where
  toJSON (FixedLenText t) = toJSON (S.unwrap t)
  toEncoding (FixedLenText t) = J.toEncoding (S.unwrap t)

instance (KnownNat n) => FromJSON (FixedLenText n) where
  parseJSON v = do
    (t :: Text) <- parseJSON v
    case parseFixedLenText t of
      Left msg -> fail msg
      Right res -> return res

instance (KnownNat n) => SW.ToParamSchema (FixedLenText n) where
  toParamSchema _ =
    SW.toParamSchema (Proxy @Text) & validLengthSchema (Proxy @(FixedLenText n))

instance (KnownNat n) => FromHttpApiData (FixedLenText n) where
  parseQueryParam = parseFixedLenText

instance (KnownNat n) => Validity (FixedLenText n) where validate = validateLength
