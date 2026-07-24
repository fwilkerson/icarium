module Icarium.Repo.Internal (
    escapeLike,
    prefixLookup,
    resolveByPrefix,
    qualified,
    axisFilters,
    inClause,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple (Connection, FromRow, Only (..), Query (..), SQLData (..), query)

import Icarium.Types (CategoryAxis, categoryAxisText)

{- | Render a column list (see "Icarium.Types") for a query, each name
prefixed with @alias@ (@"t."@ for a join, @""@ unqualified). Every
statement naming a record's columns must go through this: hand-written
copies drift from the @FromRow@ as columns are added, and the failure is a
runtime conversion error, not a type error.
-}
qualified :: Text -> [Text] -> Text
qualified alias = T.intercalate ", " . map (alias <>)

{- | One @id IN (…)@ clause per @(axis, name)@ filter, ANDed by the caller.
@joinTable@/@idCol@ name the link table (@task_categories@/@task_id@ or
@context_categories@/@context_id@). Axis names are interpolated (a closed
enum, not user input); the category name is bound.
-}
axisFilters :: Text -> Text -> [(CategoryAxis, Text)] -> ([Text], [SQLData])
axisFilters joinTable idCol filters =
    (map (clause . fst) filters, map (SQLText . snd) filters)
  where
    clause axis =
        "id IN (SELECT "
            <> idCol
            <> " FROM "
            <> joinTable
            <> " jt JOIN categories c ON c.id = jt.category_id"
            <> " WHERE c.axis = '"
            <> categoryAxisText axis
            <> "' AND c.name = ?)"

{- | The parenthesised placeholder list for an @IN@ clause, one @?@ per
element: @IN " <> inClause xs@. The parens belong here, not at the call
site — half the copies of this used to omit them, and the two spellings
are indistinguishable until SQLite rejects the query.
-}
inClause :: [a] -> Text
inClause xs = "(" <> T.intercalate "," (replicate (length xs) "?") <> ")"

-- | Escape LIKE special characters so they match literally.
escapeLike :: Text -> Text
escapeLike = T.concatMap esc
  where
    esc c
        | c `elem` ['%', '_', '\\'] = T.pack ['\\', c]
        | otherwise = T.singleton c

-- | Select rows from @table@ whose @id@ column starts with @prefix@.
prefixLookup :: (FromRow a) => Connection -> Text -> Text -> Text -> IO [a]
prefixLookup conn table cols prefix =
    query
        conn
        (Query $ "SELECT " <> cols <> " FROM " <> table <> " WHERE id LIKE ? ESCAPE '\\'")
        (Only (escapeLike prefix <> "%"))

{- | Resolve a user-supplied prefix to a canonical ULID.
Single match → Right id; no match → Left "noun not found"; multiple → Left "ambiguous id".
-}
resolveByPrefix :: (Text -> IO [a]) -> (a -> Text) -> Text -> Text -> IO (Either String Text)
resolveByPrefix lookupFn projectId noun input = do
    xs <- lookupFn input
    case xs of
        [x] -> pure (Right (projectId x))
        [] -> pure (Left $ T.unpack noun <> " not found: " <> T.unpack input)
        _ ->
            pure
                ( Left $
                    "ambiguous id: "
                        <> T.unpack input
                        <> " (matches: "
                        <> T.unpack (T.intercalate ", " (map projectId xs))
                        <> ")"
                )
