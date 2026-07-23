module TestHelpers (
    withTestDb,
    withBaseTestDb,
    mkCat,
    mkContext,
    mkCtxFrom,
    mkTaskRow,
    mkTaskIn,
    insertTestDispatch,
    attachContextCats,
    minTask,
    withTestRepo,
    withCwdLock,
    withOutOfTreeDb,
    readEventLog,
    eventField,
) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.Aeson (Key, Object, Value (..), decode)
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple (Connection, Query (..), close, execute, open)
import Icarium.Events (eventLogPath)
import System.Directory (doesFileExist)
import System.IO.Temp (withSystemTempDirectory)
import System.IO.Unsafe (unsafePerformIO)
import System.Process.Typed (proc, readProcess, setWorkingDir)

import Icarium.Db (migrateDb)
import Icarium.Repo.Category qualified as RC
import Icarium.Repo.Context qualified as RK
import Icarium.Repo.Task qualified as RT
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
    RK.insertContext c RK.NewContext{RK.ncTitle = title, RK.ncBody = body, RK.ncSourceDispatch = Nothing}

attachContextCats :: Connection -> Text -> [Category] -> IO ()
attachContextCats c kid cats =
    forM_ cats (RC.attachContextCategory c kid)

minTask :: Task
minTask =
    Task
        { taskId = "01TEST00000000000000000000"
        , taskTitle = "Test task"
        , taskBody = "Body text"
        , taskState = ReadyHeadless
        , taskPriority = Nothing
        , taskBlockReason = Nothing
        , taskCreatedAt = "2026-01-01T00:00:00Z"
        , taskUpdatedAt = "2026-01-01T00:00:00Z"
        , taskNoCommit = False
        , taskClaimedBy = Nothing
        , taskClaimedAt = Nothing
        , taskRouting = mempty
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

{- | A DB path outside any repo under test. 'DispatchCtx' carries the
invoking checkout's DB, never the dispatch worktree's — and the event log
sits beside it, so pointing @dxDbPath@ into the worktree would dirty the
very tree those tests assert on.
-}
withOutOfTreeDb :: (FilePath -> IO a) -> IO a
withOutOfTreeDb k = withSystemTempDirectory "icarium-dbdir" $ \d -> k (d <> "/icarium.db")

-- | Every line of the event log beside @db@, decoded, oldest first.
readEventLog :: FilePath -> IO [Object]
readEventLog db = do
    let path = eventLogPath db
    exists <- doesFileExist path
    if not exists
        then pure []
        else do
            raw <- BLC.readFile path
            pure
                [ case decode l of
                    Just (Object o) -> o
                    _ -> error ("event log line is not a JSON object: " <> BLC.unpack l)
                | l <- BLC.lines raw
                , not (BLC.null l)
                ]

-- | A string field of a logged event, or 'Nothing' when the key is absent.
eventField :: Key -> Object -> Maybe String
eventField k o = case KM.lookup k o of
    Just (String t) -> Just (T.unpack t)
    _ -> Nothing

{- | A dispatch row for @tid@ under id @did@. Enough to satisfy the foreign
keys that hang off a run — anything a test asserts on is set by the code
under test, not here.
-}
insertTestDispatch :: Connection -> Text -> Text -> IO ()
insertTestDispatch c did tid =
    execute
        c
        ( Query
            "INSERT INTO dispatches \
            \(id, task_id, branch, base_branch, base_sha, model, effort) \
            \VALUES (?,?,?,?,?,?,?)"
        )
        ( did
        , tid
        , "dispatch/" <> did :: Text
        , "main" :: Text
        , "0000000000000000000000000000000000000000" :: Text
        , "claude-sonnet-4-6" :: Text
        , "medium" :: Text
        )

-- | A context entry tagged with the dispatch that produced it (Nothing = hand-filed).
mkCtxFrom :: Connection -> Text -> Maybe Text -> IO Text
mkCtxFrom c title mDid =
    RK.insertContext
        c
        RK.NewContext{RK.ncTitle = title, RK.ncBody = "", RK.ncSourceDispatch = mDid}

-- | A minimal headless-ready task; returns its id.
mkTaskRow :: Connection -> Text -> IO Text
mkTaskRow c title = mkTaskIn c title ReadyHeadless

-- | A minimal task in the given state; returns its id.
mkTaskIn :: Connection -> Text -> TaskState -> IO Text
mkTaskIn c title st =
    RT.insertTask
        c
        RT.NewTask
            { RT.ntTitle = title
            , RT.ntBody = ""
            , RT.ntState = st
            , RT.ntPriority = Nothing
            , RT.ntNoCommit = False
            , RT.ntRouting = mempty
            }
