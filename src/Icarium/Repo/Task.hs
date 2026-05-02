module Icarium.Repo.Task (
    NewTask (..),
    TaskUpdate (..),
    emptyUpdate,
    taskCols,
    taskColsQualified,
    insertTask,
    getTask,
    getTasksByIds,
    getTasksByPrefix,
    resolveTaskId,
    listTasks,
    updateTask,
    deleteTask,
) where

import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple (
    Connection,
    Only (..),
    Query (..),
    SQLData (..),
    execute,
    query,
 )

import Icarium.Id (newId)
import Icarium.Repo.Internal (prefixLookup, resolveByPrefix)
import Icarium.Types (Task (..), TaskState (..))

data NewTask = NewTask
    { ntTitle :: Text
    , ntBody :: Text
    , ntState :: TaskState
    , ntPriority :: Maybe Int
    , ntNoCommit :: Bool
    }

data TaskUpdate = TaskUpdate
    { tuTitle :: Maybe Text
    , tuBody :: Maybe Text
    , tuState :: Maybe TaskState
    , tuPriority :: Maybe (Maybe Int) -- Nothing = unchanged, Just Nothing = clear
    , tuBlockReason :: Maybe (Maybe Text)
    , tuNoCommit :: Maybe Bool
    }

emptyUpdate :: TaskUpdate
emptyUpdate = TaskUpdate Nothing Nothing Nothing Nothing Nothing Nothing

taskColumnNames :: [Text]
taskColumnNames = ["id", "title", "body", "state", "priority", "block_reason", "created_at", "updated_at", "no_commit"]

taskCols :: Text
taskCols = T.intercalate ", " taskColumnNames

taskColsQualified :: Text -> Text
taskColsQualified alias = T.intercalate ", " (map (\c -> alias <> "." <> c) taskColumnNames)

insertTask :: Connection -> NewTask -> IO Text
insertTask conn NewTask{..} = do
    tid <- newId
    execute
        conn
        ( Query
            "INSERT INTO tasks (id, title, body, state, priority, no_commit) \
            \VALUES (?, ?, ?, ?, ?, ?)"
        )
        (tid, ntTitle, ntBody, ntState, ntPriority, boolToInt ntNoCommit)
    pure tid
  where
    boolToInt True = 1 :: Int
    boolToInt False = 0

getTask :: Connection -> Text -> IO (Maybe Task)
getTask conn tid = do
    rows <-
        query
            conn
            (Query $ "SELECT " <> taskCols <> " FROM tasks WHERE id = ?")
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
        (Query $ "SELECT " <> taskCols <> " FROM tasks WHERE id IN " <> ph)
        (map SQLText ids)
  where
    ph = "(" <> T.intercalate "," (replicate (length ids) "?") <> ")"

-- | Tasks whose ULID starts with @prefix@.
getTasksByPrefix :: Connection -> Text -> IO [Task]
getTasksByPrefix conn = prefixLookup conn "tasks" taskCols

-- | Resolve a user-supplied string to a canonical task ULID via prefix match.
resolveTaskId :: Connection -> Text -> IO (Either String Text)
resolveTaskId conn = resolveByPrefix (getTasksByPrefix conn) taskId "task"

{- | List tasks. @readyOnly=True@ pulls from the @ready_tasks@ view
(state='ready' with all depends_on satisfied). Otherwise pulls from
@tasks@ and optionally filters by state client-side. Both @mDomain@
and @mDiscipline@ are category-name filters applied at the SQL level.
-}
listTasks :: Connection -> [TaskState] -> Bool -> Maybe Text -> Maybe Text -> IO [Task]
listTasks conn filterStates readyOnly mDomain mDisc = do
    rows <- query conn (buildQ tbl ord) params
    pure $
        if readyOnly
            then rows
            else case filterStates of
                [] -> rows
                ss -> filter ((`elem` ss) . taskState) rows
  where
    tbl = if readyOnly then "ready_tasks" else "tasks"
    ord =
        if readyOnly
            then "COALESCE(priority, 0) DESC, created_at ASC"
            else "created_at ASC"
    (whereClause, params) = taskCatWhere mDomain mDisc
    buildQ t o = Query $ "SELECT " <> taskCols <> " FROM " <> t <> whereClause <> " ORDER BY " <> o

taskCatWhere :: Maybe Text -> Maybe Text -> (Text, [SQLData])
taskCatWhere mDomain mDisc =
    let catSubq axis =
            "id IN (SELECT task_id FROM task_categories tc"
                <> " JOIN categories c ON c.id = tc.category_id"
                <> " WHERE c.axis = '"
                <> axis
                <> "' AND c.name = ?)"
        filters =
            catMaybes
                [ fmap (\n -> (catSubq "domain", SQLText n)) mDomain
                , fmap (\n -> (catSubq "discipline", SQLText n)) mDisc
                ]
     in case filters of
            [] -> ("", [])
            fs -> (" WHERE " <> T.intercalate " AND " (map fst fs), map snd fs)

-- | Apply a sparse update. Returns True iff a row was affected.
updateTask :: Connection -> Text -> TaskUpdate -> IO Bool
updateTask conn tid TaskUpdate{..} = do
    mt <- getTask conn tid
    case mt of
        Nothing -> pure False
        Just t -> do
            let newTitle = fromMaybe (taskTitle t) tuTitle
                newBody = fromMaybe (taskBody t) tuBody
                newState = fromMaybe (taskState t) tuState
                newPrio = fromMaybe (taskPriority t) tuPriority
                -- Invariant: block_reason is meaningful only for Blocked.
                -- Clear it on any transition out of Blocked so it doesn't
                -- linger as stale text on done/in_progress tasks.
                newBlock =
                    if newState == Blocked
                        then fromMaybe (taskBlockReason t) tuBlockReason
                        else Nothing
                newNoCommit = boolToInt (fromMaybe (taskNoCommit t) tuNoCommit)
            execute
                conn
                ( Query
                    "UPDATE tasks SET title=?, body=?, state=?, \
                    \priority=?, block_reason=?, no_commit=? WHERE id=?"
                )
                (newTitle, newBody, newState, newPrio, newBlock, newNoCommit, tid)
            pure True
  where
    boolToInt True = 1 :: Int
    boolToInt False = 0

deleteTask :: Connection -> Text -> IO Bool
deleteTask conn tid = do
    mt <- getTask conn tid
    case mt of
        Nothing -> pure False
        Just _ -> do
            execute conn (Query "DELETE FROM tasks WHERE id = ?") (Only tid)
            pure True
