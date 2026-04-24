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

import Data.Text (Text)
import Database.SQLite.Simple
    ( Connection, Only(..), Query(..), execute, query, query_
    )

import Icarium.Id (newId)
import Icarium.Types (Task(..), TaskState(..))

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
-- @tasks@ and optionally filters by state client-side.
listTasks :: Connection -> [TaskState] -> Bool -> IO [Task]
listTasks conn filterStates readyOnly
    | readyOnly = query_ conn
        (Query $ "SELECT " <> taskCols <> " FROM ready_tasks \
                 \ORDER BY COALESCE(priority, 0) DESC, created_at ASC")
    | otherwise = do
        rows <- query_ conn
            (Query $ "SELECT " <> taskCols <> " FROM tasks \
                     \ORDER BY created_at ASC")
        pure $ case filterStates of
            [] -> rows
            ss -> filter ((`elem` ss) . taskState) rows

-- | Apply a sparse update. Returns True iff a row was affected.
updateTask :: Connection -> Text -> TaskUpdate -> IO Bool
updateTask conn tid TaskUpdate{..} = do
    mt <- getTask conn tid
    case mt of
        Nothing -> pure False
        Just t  -> do
            let newTitle = maybe (taskTitle t) id tuTitle
                newBody  = maybe (taskBody t)  id tuBody
                newState = maybe (taskState t) id tuState
                newPrio  = maybe (taskPriority t)    id tuPriority
                newBlock = maybe (taskBlockReason t) id tuBlockReason
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
