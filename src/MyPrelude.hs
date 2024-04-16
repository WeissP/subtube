{-# OPTIONS_GHC -Wno-orphans #-}

module MyPrelude (
  module X,
  module Data.Default,
  module RIO,
  module Control.Monad.Except,
  maybeToEither,
  wrapShow,
  logException,
  firstJustsM,
  displayWithSep,
  wrapDisplay,
  listDisplay,
  getOrDft,
  replace,
  getEnv,
  hoistMaybe,
) where

import Control.Lens as X
import Control.Monad.Except
import Control.Monad.Trans.Maybe as X
import Data.Default
import Data.Foldable (foldlM)
import Data.Semigroup (Semigroup (..))
import RIO hiding (ASetter, ASetter', Getting, Lens, Lens', lens, over, preview, set, sets, to, view, (%~), (.~), (^.), (^..), (^?))
import RIO.NonEmpty qualified as NE
import System.Environment qualified as S

class ItemsDisplay f where
  displayWithSep :: (Display a) => Builder -> f a -> Utf8Builder
  listDisplay :: (Display a) => f a -> Utf8Builder
  listDisplay items = wrapDisplay "[" (displayWithSep ", " items) "]"

instance ItemsDisplay NonEmpty where
  displayWithSep sep items =
    Utf8Builder
      $ sconcat
      $ NE.intersperse sep (items <&> getUtf8Builder . display)

instance Display String where display = fromString

hoistMaybe :: (Applicative m) => Maybe b -> MaybeT m b
hoistMaybe = MaybeT . pure

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just b) = Right b
maybeToEither a Nothing = Left a

wrapShow :: (Show a) => Text -> a -> Text -> Text
wrapShow l a r = l <> tshow a <> r

wrapDisplay :: (Display a) => Builder -> a -> Builder -> Utf8Builder
wrapDisplay l a r = Utf8Builder $ l <> getUtf8Builder (display a) <> r

logException ::
  ( MonadIO m
  , MonadReader env m
  , HasLogFunc env
  , HasCallStack
  , Exception e
  ) =>
  e
  -> m ()
logException = logWarn . ("Catched Exception: " <>) . fromString . displayException

{- | Takes computations returnings @Maybes@; tries each one in order.
The first one to return a @Just@ wins. Returns @Nothing@ if all computations
return @Nothing@.
(comes from GHC-9.8.2)
-}
firstJustsM :: (Monad m, Foldable f) => f (m (Maybe a)) -> m (Maybe a)
firstJustsM = foldlM go Nothing
  where
    go :: (Monad m) => Maybe a -> m (Maybe a) -> m (Maybe a)
    go Nothing action = action
    go result@(Just _) _action = return result

getOrDft :: (Default a) => Maybe a -> a
getOrDft (Just v) = v
getOrDft Nothing = def

replace :: Lens' a b -> a -> a -> a
replace l base new = base & l .~ new ^. l

getEnv :: (MonadIO m) => String -> m String
getEnv = liftIO . S.getEnv
