module Types.Schemas where

import Data.Type.Equality
import Hasql.Connection qualified as H
import MyPrelude
import RIO.Time (UTCTime)
import Rel8 hiding (each)
import Rel8 qualified as R
import Types.AggMedia
import Types.Cached
import Types.General
import Types.Media
import Types.Utils
import Types.Youtube

class (Rel8able entry) => IsEntry entry where
  entrySchema :: Proxy entry -> TableSchema (entry Name)

class (IsEntry entry) => UUIDEntry entry where
  uuidKey :: entry Expr -> Column Expr UUID

data TagEntry f = TagEntry
  { teID :: Column f UUID
  , teName :: Column f MediaTag
  , teIntroduction :: Column f (Maybe UTF8Text)
  , teUpdatedAt :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)
deriving stock instance (f ~ Result) => Show (TagEntry f)
instance IsEntry TagEntry where
  entrySchema _ =
    TableSchema
      { name = QualifiedName "tag" (Just "subtube")
      , columns =
          TagEntry
            { teID = "gid"
            , teName = "tag_name"
            , teIntroduction = "introduction"
            , teUpdatedAt = "updated_at"
            }
      }
instance UUIDEntry TagEntry where uuidKey = teID
fromTagEntry :: TagEntry Result -> TagInfo
fromTagEntry = TagInfo <$> teID <*> teName <*> teIntroduction

data YtbChannelEntry f = YtbChannelEntry
  { yceID :: Column f UUID
  , yceChanID :: Column f YtbChannelID
  , yceName :: Column f Text
  , yceDesc :: Column f DescHTML
  , yceSubCount :: Column f Int32
  , yceIntroduction :: Column f (Maybe UTF8Text)
  , yceThumbnail :: Column f Thumbnail
  , yceInfoCachedAt :: Column f Unix
  , yceVideosCachedAt :: Column f Unix
  , yceUpdatedAt :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)
deriving stock instance (f ~ Result) => Show (YtbChannelEntry f)
instance UUIDEntry YtbChannelEntry where uuidKey = yceID
instance IsEntry YtbChannelEntry where
  entrySchema _ =
    TableSchema
      { name = QualifiedName "ytb_channel" (Just "subtube")
      , columns =
          YtbChannelEntry
            { yceID = "gid"
            , yceChanID = "ytb_channel_id"
            , yceName = "channel_name"
            , yceDesc = "description"
            , yceSubCount = "sub_count"
            , yceIntroduction = "introduction"
            , yceThumbnail = "thumbnail"
            , yceInfoCachedAt = "info_cached_at"
            , yceVideosCachedAt = "videos_cached_at"
            , yceUpdatedAt = "updated_at"
            }
      }
fromYtbChannelEntry :: YtbChannelEntry Result -> (UUID, ChannelWithMeta)
fromYtbChannelEntry = (,) <$> yceID <*> (WithMeta <$> c <*> cMeta)
  where
    c = Channel <$> yceChanID <*> yceName <*> yceDesc <*> yceSubCount <*> yceThumbnail
    cMeta = ChannelMeta <$> yceInfoCachedAt <*> yceVideosCachedAt <*> yceIntroduction

data MediaEntry f = MediaEntry
  { meID :: Column f UUID
  , meTitle :: Column f MediaTitle
  , meDuration :: Column f Duration
  , meThumbnail :: Column f Thumbnail
  , mePublishedAt :: Column f Unix
  , meCachedAt :: Column f (Maybe Unix)
  , meIntroduction :: Column f (Maybe UTF8Text)
  , meUpdatedAt :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)
deriving stock instance (f ~ Result) => Show (MediaEntry f)
instance IsEntry MediaEntry where
  entrySchema _ =
    TableSchema
      { name = QualifiedName "media" (Just "subtube")
      , columns =
          MediaEntry
            { meID = "gid"
            , meTitle = "title"
            , meDuration = "duration"
            , meThumbnail = "thumbnail"
            , mePublishedAt = "published_at"
            , meCachedAt = "cached_at"
            , meIntroduction = "introduction"
            , meUpdatedAt = "updated_at"
            }
      }
instance UUIDEntry MediaEntry where uuidKey = meID
fromMediaEntry :: MediaEntry Result -> MediaWithMeta
fromMediaEntry = WithMeta <$> mInfo <*> mMeta
  where
    mInfo = MediaInfo <$> meDuration <*> meTitle <*> meThumbnail <*> mePublishedAt
    mMeta = MediaMeta <$> meID <*> getOrDft . meCachedAt <*> meIntroduction

data YtbVideoEntry f = YtbVideoEntry
  { yveID :: Column f UUID
  , yveVideoID :: Column f YtbVideoID
  , yveChanID :: Column f YtbChannelID
  , yveDesc :: Column f DescHTML
  , yveUpdatedAt :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)
deriving stock instance (f ~ Result) => Show (YtbVideoEntry f)
instance IsEntry YtbVideoEntry where
  entrySchema _ =
    TableSchema
      { name = QualifiedName "ytb_video" (Just "subtube")
      , columns =
          YtbVideoEntry
            { yveVideoID = "ytb_video_id"
            , yveID = "gid"
            , yveChanID = "ytb_channel_id"
            , yveDesc = "description"
            , yveUpdatedAt = "updated_at"
            }
      }
instance UUIDEntry YtbVideoEntry where uuidKey = yveID
fromYtbVideoEntry :: YtbVideoEntry Result -> YoutubeInfo
fromYtbVideoEntry = YoutubeInfo <$> yveChanID <*> yveVideoID <*> yveDesc

data YtbVideoAndMediaEntry f = YtbVideoAndMediaEntry
  { yvmMedia :: MediaEntry f
  , yvmYtb :: YtbVideoEntry f
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)
deriving stock instance (f ~ Result) => Show (YtbVideoAndMediaEntry f)
fromYtbVideoAndMediaEntry :: YtbVideoAndMediaEntry Result -> YoutubeVideoWithMeta
fromYtbVideoAndMediaEntry (YtbVideoAndMediaEntry {..}) =
  fromMediaEntry yvmMedia & entry %~ Media (fromYtbVideoEntry yvmYtb)

data TagMemberEntry f = TagMemberEntry
  { tmeID :: Column f UUID
  , tmeMemberID :: Column f UUID
  , tmeTaggedBy :: Column f TaggingMethod
  , tmeUpdatedAt :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)
deriving stock instance (f ~ Result) => Show (TagMemberEntry f)
instance IsEntry TagMemberEntry where
  entrySchema _ =
    TableSchema
      { name = QualifiedName "tag_member" (Just "subtube")
      , columns =
          TagMemberEntry
            { tmeID = "gid"
            , tmeMemberID = "member_gid"
            , tmeTaggedBy = "tagged_by"
            , tmeUpdatedAt = "updated_at"
            }
      }
instance UUIDEntry TagMemberEntry where uuidKey = tmeID

type AggMediaView = (MediaEntry Expr, MaybeTable Expr (YtbVideoEntry Expr))
type AggMediaResult = (MediaEntry Result, Maybe (YtbVideoEntry Result))

fromAggVideosResult :: AggMediaResult -> AggMediaWithMeta
fromAggVideosResult (media, ytb) =
  asAggMediaWithMeta (fromMediaEntry media)
    & entry
    . amYtbInfo
    .~ (ytb <&> fromYtbVideoEntry)

data ChannelCacheInfoView f = ChannelCacheInfoView
  { ccivChID :: Column f YtbChannelID
  , ccivCachedAt :: Column f Unix
  , ccivNewestPublishedAt :: Column f Unix
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)
fromChannelCacheInfoView :: ChannelCacheInfoView Result -> ChannelCacheInfo
fromChannelCacheInfoView = ChannelCacheInfo <$> ccivChID <*> ccivCachedAt <*> ccivNewestPublishedAt
