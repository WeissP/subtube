module Types.Cached where

import MyPrelude
import Types.General

data Cached a = Cached {_cData :: a, _cAt :: Unix}
makeLenses ''Cached
