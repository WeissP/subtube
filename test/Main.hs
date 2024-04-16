module Main where

import Db.Command qualified as C
import Gen.Spec qualified as G
import Hasql.Connection qualified as H
import Hasql.Statement qualified as H
import Hasql.Transaction qualified as HT
import Hasql.Transaction.Sessions as HT
import MyPrelude
import Psql.Spec qualified as P
import Test.Syd
import TestEnv
import Types

main :: IO ()
main = do
  logOptions <- logOptionsHandle stderr True
  withLogFunc logOptions $ \lf -> do
    env <- testEnv lf
    sydTest $ do
      G.spec
      beforeAll (return env) P.spec
