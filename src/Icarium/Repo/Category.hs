module Icarium.Repo.Category (
    insertCategory,
    findCategory,
    listCategories,
    deleteCategory,
    categoryNodeUsages,
    attachTaskCategory,
    attachContextCategory,
    detachTaskCategoriesByAxis,
    detachContextCategoriesByAxis,
    taskCategoriesFor,
    taskCategoriesBatch,
    contextCategoriesFor,
    contextCategoriesBatch,
) where

import Data.List.NonEmpty qualified as NE
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
 )

import Icarium.Id (newId)
import Icarium.Types (Category (..), CategoryAxis (..))

insertCategory :: Connection -> CategoryAxis -> Text -> IO Text
insertCategory conn axis name = do
    cid <- newId
    execute
        conn
        (Query "INSERT INTO categories (id, axis, name) VALUES (?, ?, ?)")
        (cid, axis, name)
    pure cid

findCategory :: Connection -> CategoryAxis -> Text -> IO (Maybe Category)
findCategory conn axis name = do
    rows <-
        query
            conn
            (Query "SELECT id, axis, name FROM categories WHERE axis = ? AND name = ?")
            (axis, name)
    pure $ case rows of
        (c : _) -> Just c
        [] -> Nothing

listCategories :: Connection -> Maybe CategoryAxis -> IO [Category]
listCategories conn mAxis = case mAxis of
    Nothing ->
        query_
            conn
            (Query "SELECT id, axis, name FROM categories ORDER BY axis, name")
    Just a ->
        query
            conn
            (Query "SELECT id, axis, name FROM categories WHERE axis = ? ORDER BY axis, name")
            (Only a)

deleteCategory :: Connection -> CategoryAxis -> Text -> IO Bool
deleteCategory conn axis name = do
    mc <- findCategory conn axis name
    case mc of
        Nothing -> pure False
        Just _ -> do
            execute
                conn
                (Query "DELETE FROM categories WHERE axis = ? AND name = ?")
                (axis, name)
            pure True

-- | Returns all node ids (task ids and context ids) attached to this category.
categoryNodeUsages :: Connection -> Text -> IO [Text]
categoryNodeUsages conn cid = do
    taskIds <-
        query
            conn
            (Query "SELECT task_id FROM task_categories WHERE category_id = ?")
            [cid]
    ctxIds <-
        query
            conn
            (Query "SELECT context_id FROM context_categories WHERE category_id = ?")
            [cid]
    pure $ map (\(Only t) -> t) taskIds ++ map (\(Only k) -> k) ctxIds

attachTaskCategory :: Connection -> Text -> Text -> IO ()
attachTaskCategory conn tid cid =
    execute
        conn
        (Query "INSERT OR IGNORE INTO task_categories (task_id, category_id) VALUES (?, ?)")
        (tid, cid)

attachContextCategory :: Connection -> Text -> Text -> IO ()
attachContextCategory conn kid cid =
    execute
        conn
        (Query "INSERT OR IGNORE INTO context_categories (context_id, category_id) VALUES (?, ?)")
        (kid, cid)

detachTaskCategoriesByAxis :: Connection -> Text -> CategoryAxis -> IO ()
detachTaskCategoriesByAxis conn tid axis =
    execute
        conn
        ( Query
            "DELETE FROM task_categories WHERE task_id = ? \
            \AND category_id IN (SELECT id FROM categories WHERE axis = ?)"
        )
        (tid, axis)

detachContextCategoriesByAxis :: Connection -> Text -> CategoryAxis -> IO ()
detachContextCategoriesByAxis conn kid axis =
    execute
        conn
        ( Query
            "DELETE FROM context_categories WHERE context_id = ? \
            \AND category_id IN (SELECT id FROM categories WHERE axis = ?)"
        )
        (kid, axis)

taskCategoriesFor :: Connection -> Text -> IO [Category]
taskCategoriesFor conn tid =
    query
        conn
        ( Query
            "SELECT c.id, c.axis, c.name FROM categories c \
            \JOIN task_categories tc ON tc.category_id = c.id \
            \WHERE tc.task_id = ? ORDER BY c.axis, c.name"
        )
        [tid]

contextCategoriesFor :: Connection -> Text -> IO [Category]
contextCategoriesFor conn kid =
    query
        conn
        ( Query
            "SELECT c.id, c.axis, c.name FROM categories c \
            \JOIN context_categories cc ON cc.category_id = c.id \
            \WHERE cc.context_id = ? ORDER BY c.axis, c.name"
        )
        [kid]

{- | Fetch categories for multiple task ids in a single query.
Returns an association list of (task_id, [Category]).
Tasks with no categories are omitted from the result; use @lookup tid result@
and default to @[]@.
-}
taskCategoriesBatch :: Connection -> [Text] -> IO [(Text, [Category])]
taskCategoriesBatch c = categoriesBatchBy c "task_categories" "task_id"

{- | Fetch categories for multiple context ids in a single query.
Returns an association list of (context_id, [Category]).
Entries with no categories are omitted; use @lookup kid result@ and default to @[]@.
-}
contextCategoriesBatch :: Connection -> [Text] -> IO [(Text, [Category])]
contextCategoriesBatch c = categoriesBatchBy c "context_categories" "context_id"

categoriesBatchBy ::
    Connection ->
    -- | join table name (e.g. "task_categories")
    Text ->
    -- | FK column on the join table (e.g. "task_id")
    Text ->
    [Text] ->
    IO [(Text, [Category])]
categoriesBatchBy _ _ _ [] = pure []
categoriesBatchBy conn joinTable fkCol ids = do
    rows <-
        query conn (Query q) params ::
            IO [(Text, Text, CategoryAxis, Text)]
    let pairs = [(nid, Category cid axis catName) | (nid, cid, axis, catName) <- rows]
        grouped = NE.groupAllWith fst pairs
    pure $ map (\grp -> (fst (NE.head grp), map snd (NE.toList grp))) grouped
  where
    ph = "(" <> T.intercalate "," (replicate (length ids) "?") <> ")"
    q =
        "SELECT jt."
            <> fkCol
            <> ", c.id, c.axis, c.name \
               \FROM categories c \
               \JOIN "
            <> joinTable
            <> " jt ON jt.category_id = c.id \
               \WHERE jt."
            <> fkCol
            <> " IN "
            <> ph
            <> " \
               \ORDER BY jt."
            <> fkCol
            <> ", c.axis, c.name"
    params = map SQLText ids
