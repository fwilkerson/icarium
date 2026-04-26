module Icarium.Repo.Category
    ( insertCategory
    , findCategory
    , listCategories
    , deleteCategory
    , attachTaskCategory
    , attachKnowledgeCategory
    , taskCategoriesFor
    , taskCategoriesBatch
    , knowledgeCategoriesFor
    ) where

import           Data.List              (groupBy, sortBy)
import           Data.Ord               (comparing)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Database.SQLite.Simple (Connection, Query (..), SQLData (..), execute, query, query_)

import           Icarium.Id             (newId)
import           Icarium.Types          (Category (..), CategoryAxis (..), categoryAxisText)

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

taskCategoriesFor :: Connection -> Text -> IO [Category]
taskCategoriesFor conn tid = query conn
    (Query "SELECT c.id, c.axis, c.name FROM categories c \
           \JOIN task_categories tc ON tc.category_id = c.id \
           \WHERE tc.task_id = ? ORDER BY c.axis, c.name")
    [tid]

knowledgeCategoriesFor :: Connection -> Text -> IO [Category]
knowledgeCategoriesFor conn kid = query conn
    (Query "SELECT c.id, c.axis, c.name FROM categories c \
           \JOIN knowledge_categories kc ON kc.category_id = c.id \
           \WHERE kc.knowledge_id = ? ORDER BY c.axis, c.name")
    [kid]

-- | Fetch categories for multiple task ids in a single query.
-- Returns an association list of (task_id, [Category]).
-- Tasks with no categories are omitted from the result; use @lookup tid result@
-- and default to @[]@.
taskCategoriesBatch :: Connection -> [Text] -> IO [(Text, [Category])]
taskCategoriesBatch _ [] = pure []
taskCategoriesBatch conn ids = do
    rows <- query conn (Query q) params
                :: IO [(Text, Text, CategoryAxis, Text)]
    let pairs = [(tid, Category cid axis catName) | (tid, cid, axis, catName) <- rows]
        grouped = groupBy (\a b -> fst a == fst b)
                . sortBy (comparing fst)
                $ pairs
    pure $ map (\grp -> (fst (head grp), map snd grp)) grouped
  where
    ph = "(" <> T.intercalate "," (replicate (length ids) "?") <> ")"
    q  = "SELECT tc.task_id, c.id, c.axis, c.name \
         \FROM categories c \
         \JOIN task_categories tc ON tc.category_id = c.id \
         \WHERE tc.task_id IN " <> ph <> " \
         \ORDER BY tc.task_id, c.axis, c.name"
    params = map SQLText ids
