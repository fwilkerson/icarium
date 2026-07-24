module Icarium.Repo.Task (
    NewTask (..),
    TaskUpdate (..),
    emptyUpdate,
    insertTask,
    getTask,
    getTasksByIds,
    getTasksByPrefix,
    resolveTaskId,
    listTasks,
    queueTasks,
    ClaimResult (..),
    claimNextTask,
    claimReadyTask,
    claimTask,
    releaseTask,
    updateTask,
    deleteTask,
    listTaskIdTimes,
    getTaskBody,
    getTaskTitle,
    setTaskBody,
    taskExists,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (throwIO, try)
import Control.Monad (void, when)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple (
    Connection,
    Only (..),
    Query (..),
    SQLData (..),
    SQLError (..),
    ToRow (..),
    execute,
    execute_,
    query,
    query_,
    withImmediateTransaction,
 )
import Database.SQLite.Simple.ToField (toField)
import Database.SQLite3 (Error (..))
import GHC.Clock (getMonotonicTime)

import Icarium.Id (newId)
import Icarium.Repo.Fts qualified as Fts
import Icarium.Repo.Internal (axisFilters, inClause, prefixLookup, qualified, resolveByPrefix)
import Icarium.Types (CategoryAxis, NodeKind (..), Routing, Task (..), TaskState (..), readyStates, taskCols, taskStateText)

data NewTask = NewTask
    { ntTitle :: Text
    , ntBody :: Text
    , ntState :: TaskState
    , ntPriority :: Maybe Int
    , ntNoCommit :: Bool
    , ntRouting :: Routing
    }

data TaskUpdate = TaskUpdate
    { tuTitle :: Maybe Text
    , tuState :: Maybe TaskState
    , tuPriority :: Maybe (Maybe Int)
    , tuBlockReason :: Maybe (Maybe Text)
    , tuNoCommit :: Maybe Bool
    , -- A patch, not a value: it must leave the fields it doesn't name alone,
      -- which a @Maybe Routing@ could not express.
      tuRouting :: Routing -> Routing
    }

emptyUpdate :: TaskUpdate
emptyUpdate = TaskUpdate Nothing Nothing Nothing Nothing Nothing id

{- | Queue ordering, shared by @queueTasks@ and @claimNextTask@ so
`task queue` and `task claim` cannot drift apart.
-}
readyOrder :: Text
readyOrder = "COALESCE(priority, 0) DESC, created_at ASC"

insertTask :: Connection -> NewTask -> IO Text
insertTask conn NewTask{..} = do
    tid <- newId
    execute
        conn
        ( Query
            "INSERT INTO tasks (id, title, body, state, priority, no_commit, model, effort) \
            \VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
        )
        (toRow (tid, ntTitle, ntBody, ntState, ntPriority, ntNoCommit) <> toRow ntRouting)
    Fts.indexEntry conn tid TaskNode ntTitle ntBody
    pure tid

getTask :: Connection -> Text -> IO (Maybe Task)
getTask conn tid = do
    rows <-
        query
            conn
            (Query $ "SELECT " <> qualified "" taskCols <> " FROM tasks WHERE id = ?")
            (Only tid)
    pure $ case rows of
        (t : _) -> Just t
        [] -> Nothing

-- | Fetch tasks by exact ids in a single batch query.
getTasksByIds :: Connection -> [Text] -> IO [Task]
getTasksByIds _ [] = pure []
getTasksByIds conn ids =
    query
        conn
        (Query $ "SELECT " <> qualified "" taskCols <> " FROM tasks WHERE id IN " <> inClause ids)
        (map SQLText ids)

-- | Tasks whose ULID starts with @prefix@.
getTasksByPrefix :: Connection -> Text -> IO [Task]
getTasksByPrefix conn = prefixLookup conn "tasks" (qualified "" taskCols)

-- | Resolve a user-supplied string to a canonical task ULID via prefix match.
resolveTaskId :: Connection -> Text -> IO (Either String Text)
resolveTaskId conn = resolveByPrefix (getTasksByPrefix conn) taskId "task"

{- | List tasks: a pure filter over @tasks@ in creation order. No dependency
gate — that belongs to 'queueTasks'. @filterStates@ narrows by state (empty
= all); @cats@ is a list of @(axis, name)@ category filters, ANDed at the
SQL level; any axis may appear, including workflow axes.
-}
listTasks :: Connection -> [TaskState] -> [(CategoryAxis, Text)] -> IO [Task]
listTasks conn = selectTasks conn "tasks" "created_at ASC"

{- | The ordered worklist: tasks in @states@ drawn from the @ready_tasks@
view, which carries every ready state whose depends_on are all satisfied.
The dependency gate lives in that view and nowhere else; callers narrow to
their own queue — headless or interactive — by state.
-}
queueTasks :: Connection -> [TaskState] -> IO [Task]
queueTasks conn states = selectTasks conn "ready_tasks" readyOrder states []

selectTasks :: Connection -> Text -> Text -> [TaskState] -> [(CategoryAxis, Text)] -> IO [Task]
selectTasks conn tbl ord filterStates cats = do
    rows <- query conn (buildQ tbl ord) params
    pure $ case filterStates of
        [] -> rows
        ss -> filter ((`elem` ss) . taskState) rows
  where
    (clauses, params) = axisFilters "task_categories" "task_id" cats
    whereClause = case clauses of
        [] -> ""
        cs -> " WHERE " <> T.intercalate " AND " cs
    buildQ t o = Query $ "SELECT " <> qualified "" taskCols <> " FROM " <> t <> whereClause <> " ORDER BY " <> o

{- | What a claim attempt found. 'NoCandidate' is an answer — the queue is
empty, or the named task is not claimable. 'LockBusy' is not: the write lock
never came free, so the queue's contents are still unknown. Callers that
collapse the two report an empty queue to a caller that should retry.
-}
data ClaimResult
    = Claimed Task
    | NoCandidate
    | LockBusy
    deriving (Show)

{- | Run a claim inside @BEGIN IMMEDIATE@, retrying while SQLite says busy.

@BEGIN IMMEDIATE@ acquires the write lock before the read, so concurrent
claims serialise and the loser re-reads a queue the winner has already
shortened. A deferred transaction (sqlite-simple's @withTransaction@)
would not do: both callers would read the same head row, then one would
fail to upgrade its read lock — which @busy_timeout@ cannot resolve.

@busy_timeout@ (see "Icarium.Db") waits out a lock held by a slow writer,
but SQLite refuses to wait at all on a WAL snapshot conflict or a lock lost
between the BEGIN and the COMMIT. There the only cure is to start the
transaction over, which is what the backoff below does; exhausting it means
'LockBusy', never 'NoCandidate'.

The deadline bounds the whole thing: an attempt that burned the 5s
@busy_timeout@ has already waited out a slow writer, and stacking seven more
of those would hang a claim for most of a minute to no purpose.
-}
withClaimLock :: Connection -> IO (Maybe Task) -> IO ClaimResult
withClaimLock conn act = do
    started <- getMonotonicTime
    go started [5000, 10000, 20000, 40000, 80000, 160000, 320000]
  where
    go started delays = do
        r <- try (withImmediateTransaction conn act)
        case r of
            Right (Just t) -> pure (Claimed t)
            Right Nothing -> pure NoCandidate
            Left e
                | sqlError e `notElem` [ErrorBusy, ErrorLocked] -> throwIO e
                | otherwise -> do
                    -- sqlite-simple rolls back only when the action threw, so a
                    -- busy COMMIT leaves the transaction open and the retry would
                    -- die on "cannot start a transaction within a transaction".
                    void (try (execute_ conn "ROLLBACK") :: IO (Either SQLError ()))
                    now <- getMonotonicTime
                    case delays of
                        (d : rest) | now - started < claimRetryBudget -> threadDelay d >> go started rest
                        _ -> pure LockBusy

-- | Seconds a claim may spend retrying a busy write lock before giving up.
claimRetryBudget :: Double
claimRetryBudget = 5

{- | Take the head of the queue formed by @states@, mark it in-progress and
stamp @owner@ on it. Dispatch passes 'ReadyHeadless'; the interactive CLI
passes 'ReadyInteractive'.
-}
claimNextTask :: Connection -> [TaskState] -> Text -> IO ClaimResult
claimNextTask conn states owner = withClaimLock conn $ do
    rows <-
        query
            conn
            ( Query $
                "SELECT id FROM ready_tasks WHERE state IN "
                    <> inClause states
                    <> " ORDER BY "
                    <> readyOrder
                    <> " LIMIT 1"
            )
            (map (SQLText . taskStateText) states)
    case rows of
        [] -> pure Nothing
        (Only tid : _) -> do
            stampClaim conn tid owner
            getTask conn tid

{- | Claim a *named* task, provided it is still in a ready-ish state.
'NoCandidate' when it is not — the caller reports why. Unlike
'claimNextTask' this ignores the deps gate: naming the task is the
selection, and the same lock discipline keeps the state test and the stamp
atomic against a racing queue claim.
-}
claimReadyTask :: Connection -> Text -> Text -> IO ClaimResult
claimReadyTask conn tid owner = withClaimLock conn $ do
    rows <-
        query
            conn
            (Query $ "SELECT id FROM tasks WHERE id = ? AND state IN " <> inClause readyStates)
            (SQLText tid : map (SQLText . taskStateText) readyStates)
    case rows :: [Only Text] of
        [] -> pure Nothing
        _ -> do
            stampClaim conn tid owner
            getTask conn tid

{- | Claim a named task regardless of queue position or state. @dispatch
run TASK_ID@ re-runs tasks that are blocked or already in progress, so a
compare-and-swap on 'ReadyHeadless' would refuse the cases it exists to serve;
naming the task is itself the selection.
-}
claimTask :: Connection -> Text -> Text -> IO ClaimResult
claimTask conn tid owner = withClaimLock conn $ do
    stampClaim conn tid owner
    getTask conn tid

stampClaim :: Connection -> Text -> Text -> IO ()
stampClaim conn tid owner =
    execute
        conn
        ( Query
            "UPDATE tasks SET state='in_progress', claimed_by=?, \
            \claimed_at=datetime('now') WHERE id=?"
        )
        (owner, tid)

{- | Hand a claimed task back to the queue: state 'ReadyHeadless' also clears the
claim stamp (see 'updateTask'). For a claim whose work never started —
worktree setup failed, capacity ran out — leaving it in_progress would
strand it outside both the ready queue and @dispatch recover@.
-}
releaseTask :: Connection -> Text -> IO ()
releaseTask conn tid =
    void $ updateTask conn tid emptyUpdate{tuState = Just ReadyHeadless}

{- | Apply a sparse update. Returns True iff a row was affected.
When the title changes, updates the FTS5 entry to keep search current.
-}
updateTask :: Connection -> Text -> TaskUpdate -> IO Bool
updateTask conn tid TaskUpdate{..} = do
    mt <- getTask conn tid
    case mt of
        Nothing -> pure False
        Just t -> do
            let newTitle = fromMaybe (taskTitle t) tuTitle
                newState = fromMaybe (taskState t) tuState
                newPrio = fromMaybe (taskPriority t) tuPriority
                -- Invariant: block_reason is meaningful only for Blocked.
                newBlock =
                    if newState == Blocked
                        then fromMaybe (taskBlockReason t) tuBlockReason
                        else Nothing
                newNoCommit = fromMaybe (taskNoCommit t) tuNoCommit
                newRouting = tuRouting (taskRouting t)
                -- Claims live only while in_progress (spec/schema.sql).
                (newClaimBy, newClaimAt)
                    | newState == InProgress = (taskClaimedBy t, taskClaimedAt t)
                    | otherwise = (Nothing, Nothing)
            execute
                conn
                ( Query
                    "UPDATE tasks SET title=?, state=?, \
                    \priority=?, block_reason=?, no_commit=?, \
                    \claimed_by=?, claimed_at=?, model=?, effort=? WHERE id=?"
                )
                ( toRow (newTitle, newState, newPrio, newBlock, newNoCommit, newClaimBy, newClaimAt)
                    <> toRow newRouting
                    <> [toField tid]
                )
            -- Keep FTS title in sync when title changes.
            when (isJust tuTitle) $
                Fts.indexEntry conn tid TaskNode newTitle (taskBody t)
            pure True

deleteTask :: Connection -> Text -> IO Bool
deleteTask conn tid = do
    mt <- getTask conn tid
    case mt of
        Nothing -> pure False
        Just _ -> do
            execute conn (Query "DELETE FROM tasks WHERE id = ?") (Only tid)
            Fts.removeEntry conn tid
            pure True

listTaskIdTimes :: Connection -> IO [(Text, Text)]
listTaskIdTimes conn = query_ conn "SELECT id, updated_at FROM tasks"

getTaskBody :: Connection -> Text -> IO Text
getTaskBody conn tid = do
    rows <- query conn "SELECT body FROM tasks WHERE id = ?" (Only tid) :: IO [Only Text]
    pure $ case rows of
        (Only b : _) -> b
        [] -> ""

getTaskTitle :: Connection -> Text -> IO (Maybe Text)
getTaskTitle conn tid = do
    rows <- query conn "SELECT title FROM tasks WHERE id = ?" (Only tid) :: IO [Only Text]
    pure $ case rows of
        (Only t : _) -> Just t
        [] -> Nothing

setTaskBody :: Connection -> Text -> Text -> IO ()
setTaskBody conn tid body =
    execute conn "UPDATE tasks SET body = ? WHERE id = ?" (body, tid)

taskExists :: Connection -> Text -> IO Bool
taskExists conn tid = do
    rows <- query conn "SELECT 1 FROM tasks WHERE id = ?" (Only tid) :: IO [Only Int]
    pure (not (null rows))
