module Icarium.Repo.Internal (
    escapeLike,
    prefixLookup,
    resolveByPrefix,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple (Connection, FromRow, Only (..), Query (..), query)

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
