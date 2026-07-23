-- | Toml-vocabulary ↔ DB reconciliation ('Icarium.Categories.syncCategories').
module CategoriesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Icarium.Categories (SyncReport (..), syncCategories)
import Icarium.Config (CategoriesConfig (..))
import Icarium.Repo.Category qualified as RC
import Icarium.Types

import TestHelpers

tests :: TestTree
tests =
    testGroup
        "category sync"
        [ testCase "inserts toml-only categories" testSyncInserts
        , testCase "reports orphans and exits non-zero (no prune)" testSyncOrphanNoPrune
        , testCase "prunes unused orphans when no blockers" testSyncPrunesUnused
        , testCase "blocks on in-use category; no deletions" testSyncBlocksInUse
        ]

emptyCfg :: CategoriesConfig
emptyCfg = CategoriesConfig{catDomains = [], catDisciplines = [], catKinds = []}

testSyncInserts :: IO ()
testSyncInserts = withTestDb $ \conn -> do
    let cfg = emptyCfg{catDomains = ["cli"], catDisciplines = ["haskell"]}
    rpt <- syncCategories conn cfg False
    srInserted rpt @?= [(Domain, "cli"), (Discipline, "haskell")]
    null (srOrphans rpt) @?= True
    null (srPruned rpt) @?= True
    null (srBlocking rpt) @?= True
    cats <- RC.listCategories conn Nothing
    length cats @?= 2

testSyncOrphanNoPrune :: IO ()
testSyncOrphanNoPrune = withTestDb $ \conn -> do
    _ <- RC.insertCategory conn Domain "stale-domain"
    rpt <- syncCategories conn emptyCfg False
    null (srInserted rpt) @?= True
    length (srOrphans rpt) @?= 1
    null (srPruned rpt) @?= True
    null (srBlocking rpt) @?= True
    cats <- RC.listCategories conn Nothing
    length cats @?= 1

testSyncPrunesUnused :: IO ()
testSyncPrunesUnused = withTestDb $ \conn -> do
    _ <- RC.insertCategory conn Domain "stale-domain"
    rpt <- syncCategories conn emptyCfg True
    null (srInserted rpt) @?= True
    null (srOrphans rpt) @?= True
    length (srPruned rpt) @?= 1
    null (srBlocking rpt) @?= True
    cats <- RC.listCategories conn Nothing
    length cats @?= 0

testSyncBlocksInUse :: IO ()
testSyncBlocksInUse = withTestDb $ \conn -> do
    cid <- RC.insertCategory conn Domain "stale-domain"
    tid <- mkTaskRow conn "T"
    RC.attachTaskCategory conn tid cid
    rpt <- syncCategories conn emptyCfg True
    null (srInserted rpt) @?= True
    null (srOrphans rpt) @?= True
    null (srPruned rpt) @?= True
    length (srBlocking rpt) @?= 1
    let (_, nodeIds) = head (srBlocking rpt)
    nodeIds @?= [tid]
    cats <- RC.listCategories conn Nothing
    length cats @?= 1
