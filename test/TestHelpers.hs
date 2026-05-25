module TestHelpers (
    withTestDb,
    withBaseTestDb,
    mkCat,
    mkContext,
    attachContextCats,
    minTask,
    withTestRepo,
    withCwdLock,
) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.Text (Text)
import Database.SQLite.Simple (Connection, close, open)
import System.IO.Temp (withSystemTempDirectory)
import System.IO.Unsafe (unsafePerformIO)
import System.Process.Typed (proc, readProcess, setWorkingDir)

import Icarium.Db (migrateDb)
import Icarium.Repo.Category qualified as RC
import Icarium.Repo.Context qualified as RK
import Icarium.Schema (applySchema)
import Icarium.Types

-- | In-memory DB at the full current schema (base + all migrations).
withTestDb :: (Connection -> IO a) -> IO a
withTestDb act = bracket (open ":memory:") close $ \conn -> do
    applySchema conn
    migrateDb conn
    act conn

-- | In-memory DB at the base schema only (user_version = schemaVersion, no incremental migrations).
withBaseTestDb :: (Connection -> IO a) -> IO a
withBaseTestDb act = bracket (open ":memory:") close $ \conn -> do
    applySchema conn
    act conn

mkCat :: Connection -> CategoryAxis -> Text -> IO Category
mkCat c axis name = do
    cid <- RC.insertCategory c axis name
    pure (Category cid axis name)

mkContext :: Connection -> Text -> Text -> IO Text
mkContext c title body =
    RK.insertContext c RK.NewContext{RK.ncTitle = title, RK.ncBody = body}

attachContextCats :: Connection -> Text -> [Category] -> IO ()
attachContextCats c kid cats =
    forM_ cats $ \cat -> RC.attachContextCategory c kid (categoryId cat)

minTask :: Task
minTask =
    Task
        { taskId = "01TEST00000000000000000000"
        , taskTitle = "Test task"
        , taskBody = "Body text"
        , taskState = Ready
        , taskPriority = Nothing
        , taskBlockReason = Nothing
        , taskCreatedAt = "2026-01-01T00:00:00Z"
        , taskUpdatedAt = "2026-01-01T00:00:00Z"
        , taskNoCommit = False
        }

{- | Create a throwaway git repo with one commit on main. The directory
is removed after the action returns. Tests that exercise production
code which calls git on the inherited cwd should combine this with
'withCwdLock' and 'withCurrentDirectory'.
-}
withTestRepo :: (FilePath -> IO a) -> IO a
withTestRepo k =
    withSystemTempDirectory "icarium-git-test" $ \dir -> do
        let git args = readProcess (setWorkingDir dir (proc "git" args))
        _ <- git ["init", "-b", "main"]
        _ <- git ["config", "user.email", "test@example.com"]
        _ <- git ["config", "user.name", "Test"]
        writeFile (dir <> "/README") "init"
        _ <- git ["add", "README"]
        _ <- git ["commit", "-m", "initial"]
        k dir

-- Process-wide lock for tests that mutate the parent process's working
-- directory. Production git helpers inherit cwd, so two such tests in
-- different temp dirs would race; they must serialize on this lock.
cwdLock :: MVar ()
cwdLock = unsafePerformIO (newMVar ())
{-# NOINLINE cwdLock #-}

withCwdLock :: IO a -> IO a
withCwdLock = withMVar cwdLock . const
