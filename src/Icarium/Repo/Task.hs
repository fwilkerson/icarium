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

import Control.Monad (void, when)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple (
    Connection,
    Only (..),
    Query (..),
    SQLData (..),
    execute,
    query,
    query_,
    withImmediateTransaction,
 )

import Icarium.Id (newId)
import Icarium.Repo.Fts qualified as Fts
import Icarium.Repo.Internal (axisFilters, prefixLookup, resolveByPrefix, taskCols)
import Icarium.Types (CategoryAxis, NodeKind (..), Task (..), TaskState (..), readyStates, taskStateText)

data NewTask = NewTask
    { ntTitle :: Text
    , ntBody :: Text
    , ntState :: TaskState
    , ntPriority :: Maybe Int
    , ntNoCommit :: Bool
    }

data TaskUpdate = TaskUpdate
    { tuTitle :: Maybe Text
    , tuState :: Maybe TaskState
    , tuPriority :: Maybe (Maybe Int)
    , tuBlockReason :: Maybe (Maybe Text)
    , tuNoCommit :: Maybe Bool
    }

emptyUpdate :: TaskUpdate
emptyUpdate = TaskUpdate Nothing Nothing Nothing Nothing Nothing

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
            "INSERT INTO tasks (id, title, body, state, priority, no_commit) \
            \VALUES (?, ?, ?, ?, ?, ?)"
        )
        (tid, ntTitle, ntBody, ntState, ntPriority, ntNoCommit)
    Fts.indexEntry conn tid TaskNode ntTitle ntBody
    pure tid

getTask :: Connection -> Text -> IO (Maybe Task)
getTask conn tid = do
    rows <-
        query
            conn
            (Query $ "SELECT " <> taskCols "" <> " FROM tasks WHERE id = ?")
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
        (Query $ "SELECT " <> taskCols "" <> " FROM tasks WHERE id IN " <> ph)
        (map SQLText ids)
  where
    ph = "(" <> T.intercalate "," (replicate (length ids) "?") <> ")"

-- | Tasks whose ULID starts with @prefix@.
getTasksByPrefix :: Connection -> Text -> IO [Task]
getTasksByPrefix conn = prefixLookup conn "tasks" (taskCols "")

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
    buildQ t o = Query $ "SELECT " <> taskCols "" <> " FROM " <> t <> whereClause <> " ORDER BY " <> o

{- | Take the head of the queue formed by @states@, mark it in-progress and
stamp @owner@ on it. Returns the claimed task as it now stands, or Nothing
when that queue is empty. Dispatch passes 'ReadyHeadless'; the interactive CLI
passes 'ReadyInteractive'.

@BEGIN IMMEDIATE@ acquires the write lock before the read, so concurrent
claims serialise and the loser re-reads a queue the winner has already
shortened. A deferred transaction (sqlite-simple's @withTransaction@)
would not do: both callers would read the same head row, then one would
fail to upgrade its read lock — which @busy_timeout@ cannot resolve.
-}
claimNextTask :: Connection -> [TaskState] -> Text -> IO (Maybe Task)
claimNextTask conn states owner = withImmediateTransaction conn $ do
    rows <-
        query
            conn
            ( Query $
                "SELECT id FROM ready_tasks WHERE state IN "
                    <> statePlaceholders states
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
Returns Nothing when it is not — the caller reports why. Unlike
'claimNextTask' this ignores the deps gate: naming the task is the
selection, and the same lock discipline keeps the state test and the stamp
atomic against a racing queue claim.
-}
claimReadyTask :: Connection -> Text -> Text -> IO (Maybe Task)
claimReadyTask conn tid owner = withImmediateTransaction conn $ do
    rows <-
        query
            conn
            (Query $ "SELECT id FROM tasks WHERE id = ? AND state IN " <> statePlaceholders readyStates)
            (SQLText tid : map (SQLText . taskStateText) readyStates)
    case rows :: [Only Text] of
        [] -> pure Nothing
        _ -> do
            stampClaim conn tid owner
            getTask conn tid

statePlaceholders :: [TaskState] -> Text
statePlaceholders ss = "(" <> T.intercalate "," (replicate (length ss) "?") <> ")"

{- | Claim a named task regardless of queue position or state. @dispatch
run TASK_ID@ re-runs tasks that are blocked or already in progress, so a
compare-and-swap on 'ReadyHeadless' would refuse the cases it exists to serve;
naming the task is itself the selection.
-}
claimTask :: Connection -> Text -> Text -> IO (Maybe Task)
claimTask conn tid owner = do
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
                -- Claims live only while in_progress (spec/schema.sql).
                (newClaimBy, newClaimAt)
                    | newState == InProgress = (taskClaimedBy t, taskClaimedAt t)
                    | otherwise = (Nothing, Nothing)
            execute
                conn
                ( Query
                    "UPDATE tasks SET title=?, state=?, \
                    \priority=?, block_reason=?, no_commit=?, \
                    \claimed_by=?, claimed_at=? WHERE id=?"
                )
                (newTitle, newState, newPrio, newBlock, newNoCommit, newClaimBy, newClaimAt, tid)
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
