module Psql.PsqlSpec where

import Data.List.NonEmpty qualified as NE
import Db.Command
import Db.Command qualified as C
import Db.Queries qualified as Q
import Gen.GenIns
import Hasql.Connection qualified as H
import Hasql.Statement qualified as H
import Hasql.Transaction qualified as HT
import Hasql.Transaction.Sessions as HT
import MyPrelude
import RIO.ByteString.Lazy (putStrLn)
import RIO.List qualified as L
import RIO.List.Partial qualified as L'
import RIO.NonEmpty.Partial qualified as NE'
import RIO.Partial qualified as RIO'
import Rel8 qualified
import System.IO (hGetLine, hPutStrLn)
import Test.Syd
import Test.Syd.Validity
import TestEnv
import Types
import UnliftIO.Process

-- withPgData :: (C.Insertable entry ins, C.Upsertable entry) => [ins] -> RIO TestEnv a -> RIO TestEnv a
-- withPgData inses =
--   local
--     $ set pgTestBefore (HT.statement () $ C.insertList inses upsert (pure ()))
--     . set pgTestAfter HT.condemn

-- \$ set pgTestBefore (HT.statement () HT.condemn)

-- noTA :: TestEnv -> RIO TestEnv a -> IO a
-- noTA env = runRIO

rebase :: (MonadIO m) => m ()
rebase = do
  (_, _, _, ph) <- createProcess (shell "yes | sqitch rebase mock") {cwd = Just "/home/weiss/projects/subtube/migration/psql/", std_in = CreatePipe, std_out = CreatePipe}
  _ <- waitForProcess ph
  return ()

-- runWithPgData :: (C.Insertable entry ins, C.Upsertable entry) => TestEnv -> [ins] -> RIO TestEnv a -> IO a
-- runWithPgData env inses task = runRIO env $ withPgData inses task

basicSpec :: TestDef (Env : otherOuters) ()
basicSpec = describe "Basic database operations should work" do
  itWithOuter "Database should read the same Tag that is newly inserted" $ \env -> do
    forAllValid $ \(tp :: TagPost) -> do
      res <- runWithRollback env do
        asTa $ Rel8.run_ $ C.insertOne tp upsert Rel8.NoReturning
        C.tagByName (tp ^. tpTag)
      res `shouldSatisfy` not . null
      let got = RIO'.fromJust res
      (got ^. tagName) `shouldBe` (tp ^. tpTag)
      (got ^. tagIntro) `shouldBe` (tp ^. tpIntro)
  itWithOuter "Database should read the same media that is newly inserted" $ \env -> do
    forAllValid $ \(ca :: CachedAt, mi :: MediaInfo) -> do
      me <- runWithRollback env do
        uuid <- asTa $ Rel8.run1 $ C.insertOne (ca, mi) upsert (Rel8.Returning meID)
        asTa $ Rel8.runMaybe $ Rel8.select $ Q.byUUID @MediaEntry (Rel8.lit uuid)
      me `shouldSatisfy` not . null
      let got = fromMediaEntry $ RIO'.fromJust me
      (got ^. entry) `shouldBe` mi
      (got ^. meta . mediaCachedAt) `shouldBe` ca
  itWithOuter "Database should read the same youtube video that is newly inserted" $ \env -> do
    forAllValid $ \(ca :: CachedAt, mi :: MediaInfo, yi :: YoutubeInfo) -> do
      ye <- runWithRollback env do
        uuid <- asTa $ Rel8.run1 $ C.insertOne (ca, mi) upsert (Rel8.Returning meID)
        asTa $ Rel8.run_ $ C.insertOne (uuid, yi) upsert Rel8.NoReturning
        asTa $ Rel8.runMaybe $ Rel8.select $ Q.byUUID @YtbVideoEntry (Rel8.lit uuid)
      shouldSatisfyNamed ye "Result should not be empty" (not . null)
      let got = fromYtbVideoEntry $ RIO'.fromJust ye
      got `shouldBe` yi

insertChannelVideosSpec :: TestDef (Env : otherOuters) ()
insertChannelVideosSpec = describe "Channel Videos Spec" $ do
  itWithOuter "new channel videos should be inserted correctly" $ \env -> do
    forAllValid $ \(cv :: ChannelVideos, cachedAt :: Unix) -> do
      (gotYtbVids, gotChan, gotCci) <- runWithRollback env do
        asTa $ Rel8.run_ $ C.insertOne (cv ^. cvChannel) upsert Rel8.NoReturning
        insertChannelVideos cachedAt (cv ^. cvVideos)
        gotYtbVids <- traverse ytbVideoByID $ cv ^. cvVideos ^.. traverse . mediaExtraInfo . yiVideoID
        gotChan <- C.ytbChannelByID (cv ^. cvChannel . entry . chID)
        gotCci <- mapM (\(uuid, _) -> C.channelCacheInfo uuid) gotChan
        return (NE.nonEmpty $ catMaybes gotYtbVids, gotChan, join gotCci)
      gotYtbVids `shouldNotBe` Nothing
      let videos = RIO'.fromJust gotYtbVids
          vidsNoMeta = videos <&> _entry
      vidsNoMeta `shouldBe` cv ^. cvVideos
      (cv & cvVideos .~ vidsNoMeta) `shouldSatisfy` isValid
      gotChan `shouldNotBe` Nothing
      let (_, chan) = RIO'.fromJust gotChan
      chan `shouldBe` (cv ^. cvChannel & (meta . cmVideosCachedAt) .~ cachedAt)
      gotCci
        `shouldBe` Just
          ( ChannelCacheInfo
              (cv ^. cvChannel . entry . chID)
              cachedAt
              (maximum1Of (traverse1 . mediaInfo . mediaPublishedAt) $ cv ^. cvVideos)
          )
  itWithOuter "ChannelCacheInfo should be found even if there is only channel info without any videos" $ \env -> do
    forAllValid $ \(cm :: ChannelWithMeta) -> do
      gotCci <- runWithRollback env do
        uuid <- asTa $ Rel8.run1 $ C.insertOne cm upsert (Rel8.Returning yceID)
        C.channelCacheInfo uuid
      gotCci
        `shouldBe` Just
          (ChannelCacheInfo (cm ^. entry . chID) (cm ^. meta . cmVideosCachedAt) 0)

aggVideosSpec :: TestDef (Env : otherOuters) ()
aggVideosSpec = describe "aggregated videos should be read correctly" do
  itWithOuter "aggregated youtube videos should be read correctly" $ \env -> do
    forAllValid $ \(offset@(Offset os) :: Offset, limit@(Limit lmt) :: Limit, cv :: ChannelVideos) -> do
      let vidsID = cv ^. cvVideos ^.. traverse1 . mediaExtraInfo . yiVideoID
      (gotChanAgg, gotVidsAgg, gotAllAgg1, gotAllAgg2) <- runWithRollback env do
        asTa $ Rel8.run_ $ C.insertOne (cv ^. cvChannel) upsert Rel8.NoReturning
        insertChannelVideos (cv ^. cvChannel . meta . cmVideosCachedAt) (cv ^. cvVideos)
        chUUID <- fst . RIO'.fromJust <$> C.ytbChannelByID (cv ^. cvChannel . entry . chID)
        vidsUUID <- fmap (view $ meta . mediaID) . catMaybes <$> traverse ytbVideoByID vidsID
        let agg = aggVideos offset limit
        (,,,)
          <$> agg [(ByYoutubeChannel, [chUUID])]
          <*> agg [(ByYoutubeVideo, vidsUUID)]
          <*> agg [(ByYoutubeChannel, [chUUID]), (ByYoutubeVideo, vidsUUID)]
          <*> agg [(ByYoutubeVideo, vidsUUID), (ByYoutubeChannel, [chUUID])]
      let expected =
            drop (fromEnum os)
              $ take (fromEnum lmt)
              $ L.sortBy (compare `on` view (amInfo . mediaPublishedAt)) (cv ^. cvVideos ^.. traverse . to asAggMedia)
          asExpected name got =
            shouldSatisfyNamed
              (got ^.. traverse . entry)
              ("Got " <> name <> " should be equal to " <> show expected)
              (== expected)
      asExpected "ByYoutubeChannel" gotChanAgg
      asExpected "ByYoutubeVideo" gotVidsAgg
      asExpected "[ByYoutubeChannel,ByYoutubeVideo]" gotAllAgg1
      asExpected "[ByYoutubeVideo,ByYoutubeChannel]" gotAllAgg2
      gotChanAgg `shouldBe` gotVidsAgg
      gotChanAgg `shouldBe` gotAllAgg1
      gotChanAgg `shouldBe` gotAllAgg2

spec :: TestDef (Env : otherOuters) ()
spec = sequential $ do
  basicSpec
  insertChannelVideosSpec
  aggVideosSpec
