{-# OPTIONS_GHC -Wno-orphans #-}

module Gen.GenIns where

import Data.GenValidity.Text (genText, genTextBy)
import Data.StaticText (createLeft)
import Data.UUID qualified as UUID
import GHC.TypeNats (KnownNat)
import MyPrelude
import RIO.ByteString qualified as B
import RIO.Partial qualified as RIO'
import RIO.Set qualified as Set
import RIO.Text qualified as T
import Test.QuickCheck hiding (Fixed)
import Test.QuickCheck qualified as Q
import Test.QuickCheck.Utf8
import Test.Syd.Validity
import Types

idChars :: [Char]
idChars = Set.toList idCharSet

genIdChar :: Gen Char
genIdChar = Q.elements idChars

instance GenValid MediaTag where
  genValid = MediaTag . view isoUTF8Text . T.take (bMaxLength (Proxy @MediaTag)) <$> genValidUtf81
  shrinkValid _ = []

instance GenValid UTF8Text where
  genValid = UTF8Text <$> utf8BS
  shrinkValid = fmap UTF8Text . shrinkUtf8BS . fromUTF8Text

instance GenValid TagInfo

instance (KnownNat n) => GenValid (FixedLenText n) where
  genValid = do
    let len = bMinLength (Proxy @(FixedLenText n))
    chars <- replicateM len genIdChar
    return $ FixedLenText $ createLeft '_' (fromString chars)
  shrinkValid _ = []

instance GenValid UUID where
  genValid =
    ( UUID.fromWords
        <$> genValid
        <*> genValid
        <*> genValid
        <*> genValid
    )
      `suchThat` (UUID.nil /=)
  shrinkValid _ = []
instance GenValid Offset where
  genValid = Offset <$> Q.choose (0, bwMax (Proxy @Offset))

instance GenValid Limit where
  genValid = Limit <$> Q.choose (0, bwMax (Proxy @Limit))

instance GenValid YtbVideoID
instance GenValid YtbChannelID
instance GenValid DescHTML
instance GenValid YoutubeInfo
instance GenValid Thumbnail
instance GenValid Unix where
  genValid = Unix . getPositive <$> arbitrary
instance GenValid MediaInfo
instance GenValid ChannelCacheInfo
instance GenValid Channel
instance GenValid ChannelMeta
instance GenValid TagPost
instance (GenValid extra) => GenValid (Media name extra)
instance (GenValid e, GenValid m) => GenValid (WithMeta name e m)

data ChannelVideos = ChannelVideos {_cvChannel :: ChannelWithMeta, _cvVideos :: NonEmpty YoutubeVideo}
  deriving stock (Generic, Show)
makeLenses ''ChannelVideos
instance Validity ChannelVideos where
  validate cv =
    decorate "ChannelVideos" (sameChID <> noDupVID)
    where
      cid = cv ^. cvChannel . entry . chID
      vids = cv ^. cvVideos ^.. traverse . mediaExtraInfo . yiVideoID
      sameChID =
        declare "All videos should have the same Channel ID"
          $ allOf (traverse . mediaExtraInfo . yiChannelID) (cid ==) (cv ^. cvVideos)
      noDupVID = declare "No videos should have the same video ID" $ length vids == length (Set.fromList vids)

instance GenValid ChannelVideos where
  genValid = do
    chan <- genValid
    let cid = chan ^. entry . chID
    videos :: NonEmpty YoutubeVideo <- genValid
    return (ChannelVideos chan $ videos <&> set (mediaExtraInfo . yiChannelID) cid) `suchThat` isValid
