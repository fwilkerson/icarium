module Icarium.Repo.Category
    ( insertCategory
    , findCategory
    , listCategories
    , deleteCategory
    , categoryNodeUsages
    , attachTaskCategory
    , attachKnowledgeCategory
    , detachTaskCategoriesByAxis
    , detachKnowledgeCategoriesByAxis
    , taskCategoriesFor
    , taskCategoriesBatch
    , knowledgeCategoriesFor
    , knowledgeCategoriesBatch
    ) where

import           Data.List              (groupBy, sortBy)
import           Data.Ord               (comparing)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Database.SQLite.Simple (Connection, Only (..), Query (..), SQLData (..), execute,
                                         query, query_)

import           Icarium.Id             (newId)
import           Icarium.Types          (Category (..), CategoryAxis (..))

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
listCategories conn mAxis = case mAxis of
    Nothing -> query_ conn
        (Query "SELECT id, axis, name FROM categories ORDER BY axis, name")
    Just a  -> query conn
        (Query "SELECT id, axis, name FROM categories WHERE axis = ? ORDER BY axis, name")
        (Only a)

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

-- | Returns all node ids (task ids and knowledge ids) attached to this category.
categoryNodeUsages :: Connection -> Text -> IO [Text]
categoryNodeUsages conn cid = do
    taskIds <- query conn
        (Query "SELECT task_id FROM task_categories WHERE category_id = ?") [cid]
    knowIds <- query conn
        (Query "SELECT knowledge_id FROM knowledge_categories WHERE category_id = ?") [cid]
    pure $ map (\(Only t) -> t) taskIds ++ map (\(Only k) -> k) knowIds

attachTaskCategory :: Connection -> Text -> Text -> IO ()
attachTaskCategory conn tid cid = execute conn
    (Query "INSERT OR IGNORE INTO task_categories (task_id, category_id) VALUES (?, ?)")
    (tid, cid)

attachKnowledgeCategory :: Connection -> Text -> Text -> IO ()
attachKnowledgeCategory conn kid cid = execute conn
    (Query "INSERT OR IGNORE INTO knowledge_categories (knowledge_id, category_id) VALUES (?, ?)")
    (kid, cid)

detachTaskCategoriesByAxis :: Connection -> Text -> CategoryAxis -> IO ()
detachTaskCategoriesByAxis conn tid axis = execute conn
    (Query "DELETE FROM task_categories WHERE task_id = ? \
           \AND category_id IN (SELECT id FROM categories WHERE axis = ?)")
    (tid, axis)

detachKnowledgeCategoriesByAxis :: Connection -> Text -> CategoryAxis -> IO ()
detachKnowledgeCategoriesByAxis conn kid axis = execute conn
    (Query "DELETE FROM knowledge_categories WHERE knowledge_id = ? \
           \AND category_id IN (SELECT id FROM categories WHERE axis = ?)")
    (kid, axis)

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

-- | Fetch categories for multiple knowledge ids in a single query.
-- Returns an association list of (knowledge_id, [Category]).
-- Entries with no categories are omitted; use @lookup kid result@ and default to @[]@.
knowledgeCategoriesBatch :: Connection -> [Text] -> IO [(Text, [Category])]
knowledgeCategoriesBatch _ [] = pure []
knowledgeCategoriesBatch conn ids = do
    rows <- query conn (Query q) params
                :: IO [(Text, Text, CategoryAxis, Text)]
    let pairs = [(kid, Category cid axis catName) | (kid, cid, axis, catName) <- rows]
        grouped = groupBy (\a b -> fst a == fst b)
                . sortBy (comparing fst)
                $ pairs
    pure $ map (\grp -> (fst (head grp), map snd grp)) grouped
  where
    ph = "(" <> T.intercalate "," (replicate (length ids) "?") <> ")"
    q  = "SELECT kc.knowledge_id, c.id, c.axis, c.name \
         \FROM categories c \
         \JOIN knowledge_categories kc ON kc.category_id = c.id \
         \WHERE kc.knowledge_id IN " <> ph <> " \
         \ORDER BY kc.knowledge_id, c.axis, c.name"
    params = map SQLText ids
