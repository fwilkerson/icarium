module Icarium.Slug
    ( titleToSlug
    , uniqueTaskSlug
    , uniqueKnowledgeSlug
    ) where

import           Data.Char              (isAlphaNum)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Database.SQLite.Simple (Connection, Only (..), Query (..), query)

-- | Convert a title to a candidate slug: lowercase, non-alphanumeric → '-',
-- collapse consecutive '-', strip trailing '-', hard-truncate to 30 chars.
titleToSlug :: Text -> Text
titleToSlug title =
    let normed    = T.map (\c -> if isAlphaNum c then c else '-') (T.toLower title)
        collapsed = T.intercalate "-" . filter (not . T.null) . T.splitOn "-" $ normed
        truncated = T.dropWhileEnd (== '-') (T.take 30 collapsed)
    in if T.null truncated then "item" else truncated

-- | Find a unique slug in the @tasks@ table, appending @-2@, @-3@, … on collision.
uniqueTaskSlug :: Connection -> Text -> IO Text
uniqueTaskSlug conn = uniqueIn conn "tasks"

-- | Find a unique slug in the @knowledge@ table.
uniqueKnowledgeSlug :: Connection -> Text -> IO Text
uniqueKnowledgeSlug conn = uniqueIn conn "knowledge"

uniqueIn :: Connection -> Text -> Text -> IO Text
uniqueIn conn tbl base = go (base : [base <> "-" <> T.pack (show n) | n <- [2 :: Int ..]])
  where
    go []     = pure base
    go (c:cs) = do
        rows <- query conn
                    (Query $ "SELECT 1 FROM " <> tbl <> " WHERE slug = ? LIMIT 1")
                    (Only c) :: IO [Only Int]
        if null rows then pure c else go cs
