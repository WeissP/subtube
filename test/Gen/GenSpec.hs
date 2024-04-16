module Gen.GenSpec where

import Gen.GenIns
import MyPrelude
import Test.Syd
import Test.Syd.Validity
import Types

spec :: Spec
spec = do
  genValidSpec @UTF8Text
  genValidSpec @YtbVideoID
  genValidSpec @YtbChannelID
  genValidSpec @MediaTag
  genValidSpec @ChannelVideos
