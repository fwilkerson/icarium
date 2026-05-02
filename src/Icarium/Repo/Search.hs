module Icarium.Repo.Search (
    SearchHit (..),
    searchEntries,
) where

import Data.List (sortBy)
import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple (Connection, Query (..), query)

import Icarium.Repo.Internal (escapeLike)
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

taskCols :: Text
taskCols = "id, title, body, state, priority, block_reason, created_at, updated_at, no_commit"

knowCols :: Text
knowCols = "id, title, body, stale, created_at, updated_at"

{- | Search tasks and knowledge for @q@. Results ranked: title hits first,
then updated_at DESC. @mKind@ narrows to one table; @Nothing@ = both.
-}
searchEntries :: Connection -> Text -> Maybe NodeKind -> Int -> IO [SearchHit]
searchEntries conn q mKind limit = do
    taskHits <- case mKind of
        Just KnowledgeNode -> pure []
        _ -> searchTasks conn qLower pat
    knowHits <- case mKind of
        Just TaskNode -> pure []
        _ -> searchKnowledge conn qLower pat
    let ranked = sortBy rankHit (taskHits ++ knowHits)
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

searchKnowledge :: Connection -> Text -> Text -> IO [SearchHit]
searchKnowledge conn qLower pat = do
    rows <- query conn sql (pat, pat) :: IO [Knowledge]
    pure (map toHit rows)
  where
    sql = Query $ "SELECT " <> knowCols <> " FROM knowledge WHERE title LIKE ? ESCAPE '\\' OR body LIKE ? ESCAPE '\\'"
    toHit k =
        SearchHit
            { hitId = knowledgeId k
            , hitKind = KnowledgeNode
            , hitTitle = knowledgeTitle k
            , hitBody = knowledgeBody k
            , hitUpdatedAt = knowledgeUpdatedAt k
            , hitTitleMatch = qLower `T.isInfixOf` T.toLower (knowledgeTitle k)
            , hitState = Nothing
            , hitStale = knowledgeStale k
            }
