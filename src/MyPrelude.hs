module MyPrelude (module X, module RIO, hoistMaybe) where

import Control.Lens as X
import Control.Monad.Trans.Maybe as X
import RIO hiding (ASetter, ASetter', Getting, Lens, Lens', lens, over, preview, set, sets, to, view, (%~), (.~), (^.), (^..), (^?))

hoistMaybe :: (Applicative m) => Maybe b -> MaybeT m b
hoistMaybe = MaybeT . pure
