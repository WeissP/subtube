module TestEnv where

import Db.Command (pgExecSession)
import Db.Command qualified as C
import Db.Queries
import Gen.GenIns
import Hasql.Connection qualified as H
import Hasql.Statement qualified as H
import Hasql.Transaction (Transaction)
import Hasql.Transaction qualified as HT
import Hasql.Transaction.Sessions as HT
import MyPrelude
import RIO.List.Partial qualified as L'
import RIO.Partial qualified as RIO'
import RIO.Set qualified as Set
import Rel8 qualified
import Test.QuickCheck qualified as Q
import Test.Syd.Validity
import Types

pgRunWithRollback :: (C.PgM m) => Transaction a -> m a
pgRunWithRollback ta = C.pgRunTa $ ta <* HT.condemn

testEnv :: (MonadUnliftIO m) => LogFunc -> m Env
testEnv lf = do
  conn <- connectPsqlViaEnv "PSQL_MOCK_URL"
  ivSess <- apiSessionPoolViaEnvs $ "INVIDIOUS_ROOT" :| ["INVIDIOUS_ROOT_FALLBACK"]
  return $ Env conn ivSess lf

sameElems :: (Ord e, Foldable la, Foldable lb) => la e -> lb e -> Bool
sameElems la lb = Set.fromList (toList la) == Set.fromList (toList lb)

runWithRollback :: Env -> HT.Transaction a -> IO a
runWithRollback env ta = runRIO env $ C.pgRunTa $ ta <* HT.condemn

-- tt = do
--   logOptions <- logOptionsHandle stderr True
--   cv :: ChannelVideos <- Q.generate genValid
--   withLogFunc logOptions $ \lf -> do
--     env <- testEnv lf
--     runRIO env $ C.pgRunTa do
--       let vidsID = cv ^. cvVideos ^.. traverse1 . mediaExtraInfo . yiVideoID
--       C.asTa $ Rel8.run_ $ C.insertOne (cv ^. cvChannel) C.upsert Rel8.NoReturning
--       C.insertChannelVideos (cv ^. cvChannel . meta . cmVideosCachedAt) (cv ^. cvVideos)
--       chUUID <- fst . RIO'.fromJust <$> C.ytbChannelByID (cv ^. cvChannel . entry . chID)
--       vidsUUID <- fmap (view $ meta . mediaID) . catMaybes <$> traverse C.ytbVideoByID vidsID
--       let agg = C.aggVideos 0 limit
--       (,,,)
--         <$> agg [(ByYoutubeChannel, [chUUID])]
--         <*> agg [(ByYoutubeVideo, vidsUUID)]
--         <*> agg [(ByYoutubeChannel, [chUUID]), (ByYoutubeVideo, vidsUUID)]
--         <*> agg [(ByYoutubeVideo, vidsUUID), (ByYoutubeChannel, [chUUID])]

deleteTable :: (IsEntry entry, C.PgM m) => Proxy entry -> m ()
deleteTable ty =
  C.pgRunTa
    $ C.asTa
    $ Rel8.run_
    $ Rel8.delete
      Rel8.Delete
        { from = entrySchema ty
        , using = pure ()
        , deleteWhere = \_ _ -> Rel8.lit True
        , returning = Rel8.NoReturning
        }

deleteAllTables :: (C.PgM m) => m ()
deleteAllTables =
  sequence_
    [ deleteTable (Proxy @YtbChannelEntry)
    , deleteTable (Proxy @YtbVideoEntry)
    , deleteTable (Proxy @MediaEntry)
    , deleteTable (Proxy @TagMemberEntry)
    , deleteTable (Proxy @TagEntry)
    ]
