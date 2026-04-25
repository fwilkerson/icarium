module Icarium.Repo.Task
    ( NewTask(..)
    , TaskUpdate(..)
    , emptyUpdate
    , insertTask
    , getTask
    , listTasks
    , updateTask
    , deleteTask
    ) where

import           Data.Maybe             (catMaybes, fromMaybe)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Database.SQLite.Simple (Connection, Only (..), Query (..), SQLData (..), execute,
                                         query)

import           Icarium.Id             (newId)
import           Icarium.Types          (Task (..), TaskState (..))

data NewTask = NewTask
    { ntTitle    :: Text
    , ntBody     :: Text
    , ntState    :: TaskState
    , ntPriority :: Maybe Int
    }

data TaskUpdate = TaskUpdate
    { tuTitle       :: Maybe Text
    , tuBody        :: Maybe Text
    , tuState       :: Maybe TaskState
    , tuPriority    :: Maybe (Maybe Int)   -- Nothing = unchanged, Just Nothing = clear
    , tuBlockReason :: Maybe (Maybe Text)
    }

emptyUpdate :: TaskUpdate
emptyUpdate = TaskUpdate Nothing Nothing Nothing Nothing Nothing

taskCols :: Text
taskCols = "id, title, body, state, priority, block_reason, created_at, updated_at"

insertTask :: Connection -> NewTask -> IO Text
insertTask conn NewTask{..} = do
    tid <- newId
    execute conn
        (Query $ "INSERT INTO tasks (id, title, body, state, priority) \
                 \VALUES (?, ?, ?, ?, ?)")
        (tid, ntTitle, ntBody, ntState, ntPriority)
    pure tid

getTask :: Connection -> Text -> IO (Maybe Task)
getTask conn tid = do
    rows <- query conn
        (Query $ "SELECT " <> taskCols <> " FROM tasks WHERE id = ?")
        (Only tid)
    pure $ case rows of
        (t:_) -> Just t
        []    -> Nothing

-- | List tasks. @readyOnly=True@ pulls from the @ready_tasks@ view
-- (state='ready' with all depends_on satisfied). Otherwise pulls from
-- @tasks@ and optionally filters by state client-side. Both @mDomain@
-- and @mDiscipline@ are category-name filters applied at the SQL level.
listTasks :: Connection -> [TaskState] -> Bool -> Maybe Text -> Maybe Text -> IO [Task]
listTasks conn filterStates readyOnly mDomain mDisc = do
    rows <- query conn (buildQ tbl ord) params
    pure $ if readyOnly
        then rows
        else case filterStates of
            [] -> rows
            ss -> filter ((`elem` ss) . taskState) rows
  where
    tbl  = if readyOnly then "ready_tasks" else "tasks"
    ord  = if readyOnly
               then "COALESCE(priority, 0) DESC, created_at ASC"
               else "created_at ASC"
    (whereClause, params) = taskCatWhere mDomain mDisc
    buildQ t o = Query $ "SELECT " <> taskCols <> " FROM " <> t <> whereClause <> " ORDER BY " <> o

taskCatWhere :: Maybe Text -> Maybe Text -> (Text, [SQLData])
taskCatWhere mDomain mDisc =
    let catSubq axis = "id IN (SELECT task_id FROM task_categories tc"
                    <> " JOIN categories c ON c.id = tc.category_id"
                    <> " WHERE c.axis = '" <> axis <> "' AND c.name = ?)"
        filters = catMaybes
            [ fmap (\n -> (catSubq "domain",      SQLText n)) mDomain
            , fmap (\n -> (catSubq "discipline",   SQLText n)) mDisc
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
        Just t  -> do
            let newTitle = fromMaybe (taskTitle t) tuTitle
                newBody  = fromMaybe (taskBody t)  tuBody
                newState = fromMaybe (taskState t) tuState
                newPrio  = fromMaybe (taskPriority t)    tuPriority
                newBlock = fromMaybe (taskBlockReason t) tuBlockReason
            execute conn
                (Query "UPDATE tasks SET title=?, body=?, state=?, \
                       \priority=?, block_reason=? WHERE id=?")
                (newTitle, newBody, newState, newPrio, newBlock, tid)
            pure True

deleteTask :: Connection -> Text -> IO Bool
deleteTask conn tid = do
    mt <- getTask conn tid
    case mt of
        Nothing -> pure False
        Just _  -> do
            execute conn (Query "DELETE FROM tasks WHERE id = ?") (Only tid)
            pure True
