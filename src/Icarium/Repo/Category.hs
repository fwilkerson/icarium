module Icarium.Repo.Category
    ( insertCategory
    , findCategory
    , listCategories
    , deleteCategory
    , attachTaskCategory
    , attachKnowledgeCategory
    ) where

import Data.Text (Text)
import Database.SQLite.Simple
    ( Connection, Query(..), execute, query, query_
    )

import Icarium.Id (newId)
import Icarium.Types (Category(..), CategoryAxis(..), categoryAxisText)

insertCategory :: Connection -> CategoryAxis -> Text -> IO Text
insertCategory conn axis name = do
    cid <- newId
    execute conn
        (Query "INSERT INTO categories (id, axis, name) VALUES (?, ?, ?)")
        (cid, axis, name)
    pure cid

findCategory :: Connection -> CategoryAxis -> Text -> IO (Maybe Category)
findCategory conn axis name = do
    rows <- query conn
        (Query "SELECT id, axis, name FROM categories WHERE axis = ? AND name = ?")
        (axis, name)
    pure $ case rows of
        (c:_) -> Just c
        []    -> Nothing

listCategories :: Connection -> Maybe CategoryAxis -> IO [Category]
listCategories conn mAxis = do
    rows <- query_ conn
        (Query "SELECT id, axis, name FROM categories ORDER BY axis, name")
    pure $ case mAxis of
        Nothing -> rows
        Just a  -> filter ((== categoryAxisText a) . categoryAxisText . categoryAxis) rows

deleteCategory :: Connection -> CategoryAxis -> Text -> IO Bool
deleteCategory conn axis name = do
    mc <- findCategory conn axis name
    case mc of
        Nothing -> pure False
        Just _  -> do
            execute conn
                (Query "DELETE FROM categories WHERE axis = ? AND name = ?")
                (axis, name)
            pure True

attachTaskCategory :: Connection -> Text -> Text -> IO ()
attachTaskCategory conn tid cid = execute conn
    (Query "INSERT OR IGNORE INTO task_categories (task_id, category_id) VALUES (?, ?)")
    (tid, cid)

attachKnowledgeCategory :: Connection -> Text -> Text -> IO ()
attachKnowledgeCategory conn kid cid = execute conn
    (Query "INSERT OR IGNORE INTO knowledge_categories (knowledge_id, category_id) VALUES (?, ?)")
    (kid, cid)
