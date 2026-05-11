module Icarium.Repo.Search (
    SearchHit (..),
    searchEntries,
) where

import Data.List (sortBy)
import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple (Connection, Query (..), query)

import Icarium.Repo.Context (ctxCols)
import Icarium.Repo.Internal (escapeLike)
import Icarium.Repo.Task (taskCols)
import Icarium.Types

data SearchHit = SearchHit
    { hitId :: Text
    , hitKind :: NodeKind
    , hitTitle :: Text
    , hitBody :: Text
    , hitUpdatedAt :: Text
    , hitTitleMatch :: Bool
    , hitState :: Maybe TaskState
    , hitStale :: Bool
    }

{- | Search tasks and context for @q@. Results ranked: title hits first,
then updated_at DESC. @mKind@ narrows to one table; @Nothing@ = both.
-}
searchEntries :: Connection -> Text -> Maybe NodeKind -> Int -> IO [SearchHit]
searchEntries conn q mKind limit = do
    taskHits <- case mKind of
        Just ContextNode -> pure []
        _ -> searchTasks conn qLower pat
    ctxHits <- case mKind of
        Just TaskNode -> pure []
        _ -> searchContexts conn qLower pat
    let ranked = sortBy rankHit (taskHits ++ ctxHits)
    pure (take limit ranked)
  where
    pat = "%" <> escapeLike q <> "%"
    qLower = T.toLower q

    rankHit a b = case compare (hitTitleMatch b) (hitTitleMatch a) of
        EQ -> compare (hitUpdatedAt b) (hitUpdatedAt a)
        o -> o

searchTasks :: Connection -> Text -> Text -> IO [SearchHit]
searchTasks conn qLower pat = do
    rows <- query conn sql (pat, pat) :: IO [Task]
    pure (map toHit rows)
  where
    sql = Query $ "SELECT " <> taskCols <> " FROM tasks WHERE title LIKE ? ESCAPE '\\' OR body LIKE ? ESCAPE '\\'"
    toHit t =
        SearchHit
            { hitId = taskId t
            , hitKind = TaskNode
            , hitTitle = taskTitle t
            , hitBody = taskBody t
            , hitUpdatedAt = taskUpdatedAt t
            , hitTitleMatch = qLower `T.isInfixOf` T.toLower (taskTitle t)
            , hitState = Just (taskState t)
            , hitStale = False
            }

searchContexts :: Connection -> Text -> Text -> IO [SearchHit]
searchContexts conn qLower pat = do
    rows <- query conn sql (pat, pat) :: IO [Context]
    pure (map toHit rows)
  where
    sql = Query $ "SELECT " <> ctxCols <> " FROM context WHERE title LIKE ? ESCAPE '\\' OR body LIKE ? ESCAPE '\\'"
    toHit k =
        SearchHit
            { hitId = contextId k
            , hitKind = ContextNode
            , hitTitle = contextTitle k
            , hitBody = contextBody k
            , hitUpdatedAt = contextUpdatedAt k
            , hitTitleMatch = qLower `T.isInfixOf` T.toLower (contextTitle k)
            , hitState = Nothing
            , hitStale = contextStale k
            }
