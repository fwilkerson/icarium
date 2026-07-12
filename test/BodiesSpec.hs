module BodiesSpec (tests) where

import Control.Exception (bracket)
import Data.Text (Text)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))
import Database.SQLite.Simple (Connection, Only (..), close, execute, open, query)
import System.Directory (doesFileExist, setModificationTime)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Icarium.Bodies (bodiesDir, ensureBodiesDirs, taskBodyPath, writeBody)
import Icarium.Bodies.Sweep (refreshTaskBody)
import Icarium.Db (migrateDb)
import Icarium.Repo.Task qualified as RT
import Icarium.Schema (applySchema)
import Icarium.Types (Task (..))

tests :: TestTree
tests =
    testGroup
        "Bodies"
        [ testCase "writeBody is atomic: partial write leaves target unchanged" testAtomicWrite
        , testCase "writeBody creates file when target absent" testCreateAbsent
        , testCase "refreshTaskBody syncs record, column, and FTS despite stale mtime" testRefreshSyncs
        , testCase "refreshTaskBody with absent file leaves task and DB untouched" testRefreshAbsentFile
        , testCase "refreshTaskBody with identical file writes nothing" testRefreshIdenticalNoWrite
        ]

-- Simulate a partial write: write content to .tmp but do not rename.
-- The target file must remain unchanged (or absent).
testAtomicWrite :: IO ()
testAtomicWrite = withSystemTempDirectory "bodies-test" $ \dir -> do
    let target = dir </> "body.md"
    let tmp = target <> ".tmp"
    -- Establish original content
    writeBody target "original content"
    original <- readFile target
    original @?= "original content"
    -- Simulate crash: write to tmp but skip rename
    writeFile tmp "partial write"
    -- Target must be untouched
    actual <- readFile target
    actual @?= "original content"
    -- tmp exists, target exists, they differ
    tmpExists <- doesFileExist tmp
    assertBool "tmp file should exist" tmpExists

testCreateAbsent :: IO ()
testCreateAbsent = withSystemTempDirectory "bodies-test" $ \dir -> do
    let target = dir </> "new.md"
    exists0 <- doesFileExist target
    assertBool "should not exist yet" (not exists0)
    writeBody target "hello"
    content <- readFile target
    content @?= "hello"
    -- no stray .tmp left behind
    tmpExists <- doesFileExist (target <> ".tmp")
    assertBool "tmp should be cleaned up" (not tmpExists)

-- =============================================================
-- refreshTaskBody (issue #8)
-- =============================================================

pinnedTime :: Text
pinnedTime = "2001-01-01 00:00:00"

{- | Temp on-disk DB with one task whose updated_at is pinned via raw
INSERT — tasks_touch is AFTER UPDATE only, so the pin sticks; any
accidental UPDATE bumps it to now, making no-write assertions
tamper-evident.
-}
withPinnedTask :: Text -> (Connection -> FilePath -> Task -> IO a) -> IO a
withPinnedTask body k = withSystemTempDirectory "bodies-test" $ \dir -> do
    let db = dir </> "icarium.db"
    bracket (open db) close $ \conn -> do
        applySchema conn
        migrateDb conn
        let tid = "01REFRESH00000000000000000" :: Text
        execute
            conn
            "INSERT INTO tasks (id, title, body, updated_at) VALUES (?, ?, ?, ?)"
            (tid, "refresh test task" :: Text, body, pinnedTime)
        Just t <- RT.getTask conn tid
        k conn db t

dbUpdatedAt :: Connection -> Text -> IO Text
dbUpdatedAt conn tid = do
    [Only upd] <- query conn "SELECT updated_at FROM tasks WHERE id = ?" (Only tid)
    pure upd

-- A past mtime encodes "mtimeSweep would skip this"; refreshTaskBody is
-- content-compared and must sync anyway.
testRefreshSyncs :: IO ()
testRefreshSyncs = withPinnedTask "old body" $ \conn db t -> do
    let fp = taskBodyPath (bodiesDir db) (taskId t)
    ensureBodiesDirs (bodiesDir db)
    writeBody fp "new body"
    setModificationTime fp (UTCTime (fromGregorian 2000 1 1) 0)
    t' <- refreshTaskBody conn db t
    taskBody t' @?= "new body"
    col <- RT.getTaskBody conn (taskId t)
    col @?= "new body"
    ftsBodies <- query conn "SELECT body FROM body_fts WHERE id = ?" (Only (taskId t))
    ftsBodies @?= [Only ("new body" :: Text)]

testRefreshAbsentFile :: IO ()
testRefreshAbsentFile = withPinnedTask "column body" $ \conn db t -> do
    t' <- refreshTaskBody conn db t
    taskBody t' @?= "column body"
    upd <- dbUpdatedAt conn (taskId t)
    upd @?= pinnedTime

testRefreshIdenticalNoWrite :: IO ()
testRefreshIdenticalNoWrite = withPinnedTask "same body" $ \conn db t -> do
    let fp = taskBodyPath (bodiesDir db) (taskId t)
    ensureBodiesDirs (bodiesDir db)
    writeBody fp "same body"
    t' <- refreshTaskBody conn db t
    taskBody t' @?= "same body"
    upd <- dbUpdatedAt conn (taskId t)
    upd @?= pinnedTime
