module NodeSpec (tests) where

import Control.Exception (bracket)
import Data.Text.IO qualified as TIO
import Database.SQLite.Simple (Connection, close, open)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Icarium.Bodies (bodiesDir, ctxBodyPath, taskBodyPath)
import Icarium.Db (migrateDb)
import Icarium.Node (createContextWithBody, createTaskWithBody)
import Icarium.Repo.Context qualified as RCx
import Icarium.Repo.Task qualified as RT
import Icarium.Schema (applySchema)
import Icarium.Types (TaskState (..))

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
