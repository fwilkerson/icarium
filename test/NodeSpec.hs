module NodeSpec (tests) where

import Control.Exception (bracket)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Database.SQLite.Simple (Connection, close, open)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Icarium.Bodies (bodiesDir, ctxBodyPath, taskBodyPath)
import Icarium.Db (migrateDb)
import Icarium.Node (autoDeriveDeps, createContextWithBody, createTaskWithBody, inheritedContextCategories)
import Icarium.Repo.Category qualified as RC
import Icarium.Repo.Context qualified as RCx
import Icarium.Repo.Task qualified as RT
import Icarium.Schema (applySchema)
import Icarium.Types

import TestHelpers

tests :: TestTree
tests =
    testGroup
        "Node"
        -- Regression: the reviewer-warn path once wrote only the DB column,
        -- leaving the on-disk body file (what `ctx cat`/`ctx path` read)
        -- missing. These helpers must keep the column and the file in sync,
        -- for both node kinds.
        [ testCase "createContextWithBody writes both the DB body column and the body file" testContext
        , testCase "createTaskWithBody writes both the DB body column and the body file" testTask
        , testGroup
            "autoDeriveDeps"
            [ testCase "ICARIUM_TASK_ID set, no explicit → edge inserted" testAutoDeriveDepsEdgeInserted
            , testCase "explicit --derived-from wins over ICARIUM_TASK_ID" testAutoDeriveDepsExplicitWins
            , testCase "ICARIUM_TASK_ID unset → no auto edge" testAutoDeriveDepsNoEnv
            , testCase "ICARIUM_TASK_ID set but task missing → empty" testAutoDeriveDepsTaskMissing
            ]
        , testGroup
            "inheritedContextCategories"
            [ testCase "inherits only the axes no flag pre-empts" testInheritUnflaggedAxes
            , testCase "both axes flagged → nothing inherited" testInheritBothFlagged
            , testCase "ICARIUM_TASK_ID unset → nothing inherited" testInheritNoEnv
            ]
        ]

{- | Run an action against a fresh on-disk DB (needed so the body file lands
next to a real db path rather than under :memory:'s cwd-relative dir).
-}
withDiskDb :: (FilePath -> Connection -> IO a) -> IO a
withDiskDb act =
    withSystemTempDirectory "icarium-node" $ \dir -> do
        let db = dir </> "icarium.db"
        bracket (open db) close $ \c -> do
            applySchema c
            migrateDb c
            act db c

testContext :: IO ()
testContext = withDiskDb $ \db c -> do
    (cid, fp) <- createContextWithBody c db RCx.NewContext{RCx.ncTitle = "warn", RCx.ncBody = "ctx findings", RCx.ncSourceDispatch = Nothing}
    body <- RCx.getContextBody c cid
    body @?= "ctx findings"
    fp @?= ctxBodyPath (bodiesDir db) cid
    exists <- doesFileExist fp
    assertBool "context body file should exist on disk" exists
    onDisk <- TIO.readFile fp
    onDisk @?= "ctx findings"

testTask :: IO ()
testTask = withDiskDb $ \db c -> do
    (tid, fp) <-
        createTaskWithBody
            c
            db
            RT.NewTask
                { RT.ntTitle = "t"
                , RT.ntBody = "task body"
                , RT.ntState = ReadyHeadless
                , RT.ntPriority = Nothing
                , RT.ntNoCommit = False
                , RT.ntRouting = mempty
                }
    body <- RT.getTaskBody c tid
    body @?= "task body"
    fp @?= taskBodyPath (bodiesDir db) tid
    exists <- doesFileExist fp
    assertBool "task body file should exist on disk" exists
    onDisk <- TIO.readFile fp
    onDisk @?= "task body"

testAutoDeriveDepsEdgeInserted :: IO ()
testAutoDeriveDepsEdgeInserted = withTestDb $ \c -> do
    tid <- mkTaskRow c "Dispatch task"
    result <- autoDeriveDeps c [] (Just (T.unpack tid))
    result @?= [(TaskNode, tid)]

testAutoDeriveDepsExplicitWins :: IO ()
testAutoDeriveDepsExplicitWins = withTestDb $ \c -> do
    tid <- mkTaskRow c "Dispatch task"
    otherTid <- mkTaskRow c "Other task"
    result <- autoDeriveDeps c [otherTid] (Just (T.unpack tid))
    result @?= []

testAutoDeriveDepsNoEnv :: IO ()
testAutoDeriveDepsNoEnv = withTestDb $ \c -> do
    result <- autoDeriveDeps c [] Nothing
    result @?= []

testAutoDeriveDepsTaskMissing :: IO ()
testAutoDeriveDepsTaskMissing = withTestDb $ \c -> do
    result <- autoDeriveDeps c [] (Just "01ZZZZZZZZ0000000000000000")
    result @?= []

-- | A task carrying one category on each axis.
mkCategorizedTask :: Connection -> IO Text
mkCategorizedTask c = do
    tid <- mkTaskRow c "Dispatch task"
    cats <- sequence [mkCat c Domain "cli", mkCat c Discipline "haskell", mkCat c Kind "refactor"]
    mapM_ (RC.attachTaskCategory c tid . categoryId) cats
    pure tid

testInheritUnflaggedAxes :: IO ()
testInheritUnflaggedAxes = withTestDb $ \c -> do
    tid <- mkCategorizedTask c
    -- --domain is explicit, so only discipline (and the never-flagged kind
    -- axis, which attachContextCategory drops later) comes across.
    inherited <- inheritedContextCategories c (Just "other") Nothing (Just (T.unpack tid))
    map categoryAxis inherited @?= [Discipline, Kind]

testInheritBothFlagged :: IO ()
testInheritBothFlagged = withTestDb $ \c -> do
    tid <- mkCategorizedTask c
    inherited <- inheritedContextCategories c (Just "d") (Just "x") (Just (T.unpack tid))
    map categoryAxis inherited @?= []

testInheritNoEnv :: IO ()
testInheritNoEnv = withTestDb $ \c -> do
    _ <- mkCategorizedTask c
    inherited <- inheritedContextCategories c Nothing Nothing Nothing
    map categoryAxis inherited @?= []
