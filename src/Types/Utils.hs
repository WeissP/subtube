module Types.Utils (module Rel8, module Data.UUID, module Data.Aeson, swaggerExp, swaggerDesc, aesonOptions, swaggerSchemaOptions, renameLabel, withDefault, BoundedLength (..), prettyValidateText, BoundedWord (..)) where

import Data.Aeson (FromJSON (..), ToJSON (..), genericToEncoding, genericToJSON, parseJSON)
import Data.Aeson qualified as J
import Data.StaticText (Static)
import Data.StaticText qualified as S
import Data.Swagger
import Data.Swagger.Internal.Schema (toNamedSchema)
import Data.Swagger.Lens qualified as SW
import Data.UUID (UUID)
import Data.UnixTime
import Data.Validity (Validation, Validity, check, declare, prettyValidate)
import GHC.TypeLits (KnownNat)
import GHC.TypeNats (Nat, natVal)
import MyPrelude
import RIO.Char qualified as C
import RIO.List qualified as L
import RIO.Text qualified as T
import RIO.Time (NominalDiffTime)
import Rel8 (DBEq, DBType, ReadShow (..), Rel8able)
import Servant
import Servant.Docs
import System.Posix.Types (EpochTime)

swaggerExp :: (HasSchema t a, HasExample a (Maybe J.Value), ToJSON b) => b -> t -> t
swaggerExp e = schema . example ?~ toJSON e

swaggerDesc :: (HasSchema t a, HasDescription a (Maybe Text)) => Text -> t -> t
swaggerDesc desc = schema . description ?~ desc

withDefault ::
  forall a t.
  (HasDefault t (Maybe J.Value), Default a, ToJSON a) =>
  Proxy a
  -> t
  -> t
withDefault _ t = t & default_ ?~ toJSON (def :: a)

aesonOptions :: J.Options
aesonOptions = J.defaultOptions {J.fieldLabelModifier = renameLabel}

swaggerSchemaOptions :: SchemaOptions
swaggerSchemaOptions = fromAesonOptions aesonOptions

renameLabel :: String -> String
renameLabel = removeFirstWord . removeUnderscore
  where
    removeUnderscore ('_' : s) = s
    removeUnderscore s = s
    removeFirstWord v = case break C.isUpper v of
      (s, []) -> s
      (_, s) -> let (cap, rest) = break C.isLower s in (C.toLower <$> cap) <> rest

prettyValidateText :: (Validity a) => a -> Either Text a
prettyValidateText = mapLeft fromString . prettyValidate

class BoundedWord a where
  bwL :: Getter a Word
  bwMax :: Proxy a -> Word
  validateBound :: a -> Validation
  validateBound a = declare ("should be at most " <> show m) (a ^. bwL <= m)
    where
      m = bwMax (Proxy @a)

class BoundedLength a where
  bLength :: a -> Int
  bMinLength :: Proxy a -> Int
  bMaxLength :: Proxy a -> Int
  validateLength :: (Show a) => a -> Validation
  validateLength a = checkMin <> checkMax
    where
      checkMin =
        check
          (bLength a <= bMaxLength (Proxy @a))
          ("The length can not exceed " <> show (bMaxLength (Proxy @a)))
      checkMax =
        check
          (bLength a >= bMinLength (Proxy @a))
          ("The length can not shorter than " <> show (bMinLength (Proxy @a)))
  validLengthSchema ::
    ( HasMinLength s (Maybe Integer)
    , HasMaxLength s (Maybe Integer)
    ) =>
    Proxy a
    -> s
    -> s
  validLengthSchema _ s =
    s
      & SW.minLength
      ?~ toInteger (bMinLength (Proxy @a))
      & SW.maxLength
      ?~ toInteger (bMaxLength (Proxy @a))
