{-# OPTIONS_GHC -Wno-orphans #-}

module Invidious.Parser where

import Data.Aeson (withObject, (.:?))
import Data.Aeson qualified as J
import Data.Aeson.Types ((.:))
import Data.Foldable (maximumBy)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import MyPrelude
import Network.Wreq qualified as W
import RIO.NonEmpty qualified as NE
import RIO.Vector qualified as V
import Types

contParam :: Maybe Text -> W.Options -> W.Options
contParam Nothing opts = opts
contParam (Just t) opts = opts & W.param "continuation" .~ [t]

data WithCont (name :: Symbol) a = WithCont
  { wContItems :: a
  , wCont :: Maybe Text
  }
  deriving stock (Show)
instance (FromJSON a, KnownSymbol name) => FromJSON (WithCont name a) where
  parseJSON = withObject (itemName <> "WithCont") deser
    where
      itemName = symbolVal (Proxy :: Proxy name)
      deser obj = WithCont <$> obj .: fromString itemName <*> obj .:? "continuation"

data ThumbnailObject = ThumbnailObject
  { quality :: Maybe String
  , url :: String
  , width :: Int
  , height :: Int
  }
  deriving (Generic, FromJSON, ToJSON, Show)

asThumbnail :: ThumbnailObject -> Thumbnail
asThumbnail = Thumbnail . fromString . url

bestThumbnail :: NonEmpty ThumbnailObject -> Thumbnail
bestThumbnail = asThumbnail . maximumBy (compare `on` size)
  where
    size x = width x + height x

instance FromJSON Thumbnail where
  parseJSON v@(J.Object _) = parseJSON v <&> asThumbnail
  parseJSON v@(J.Array _) = do
    vs <- parseJSONList v
    case NE.nonEmpty vs of
      Just xs -> return $ bestThumbnail xs
      Nothing -> fail $ "no thumbnails were found in: " <> show v
  parseJSON invalid = fail $ "parse thumbnails failed from: " <> show invalid

instance FromJSON Channel where
  parseJSON = withObject "Channel" deser
    where
      deser obj =
        Channel
          <$> obj
          .: "authorId"
          <*> obj
          .: "author"
          <*> obj
          .: "descriptionHtml"
          <*> obj
          .: "subCount"
          <*> obj
          .: "authorThumbnails"

instance FromJSON YoutubeVideo where
  parseJSON = withObject "YoutubeVideo" deser
    where
      deYoutubeInfo obj = YoutubeInfo <$> obj .: "authorId" <*> obj .: "videoId" <*> obj .: "descriptionHtml"
      deMediaInfo obj = MediaInfo <$> obj .: "lengthSeconds" <*> obj .: "title" <*> obj .: "videoThumbnails" <*> obj .: "published"
      deser obj = Media <$> deYoutubeInfo obj <*> deMediaInfo obj
