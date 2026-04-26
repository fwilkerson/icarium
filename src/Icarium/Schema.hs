{-# LANGUAGE TemplateHaskell #-}
module Icarium.Schema
    ( schemaSql
    , schemaVersion
    , applySchema
    , execSql
    ) where

import           Data.FileEmbed                  (embedFile, makeRelativeToProject)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as TE
import           Database.SQLite.Simple          (Connection, Query (..), execute_)
import qualified Database.SQLite.Simple.Internal as Internal
import qualified Database.SQLite3                as Direct

-- | The DDL for the current schema, embedded at compile time from
-- @spec/schema.sql@. This is the single source of truth.
schemaSql :: Text
schemaSql = TE.decodeUtf8 $(makeRelativeToProject "spec/schema.sql" >>= embedFile)

-- | Monotonically-increasing integer. Bumped whenever schema changes.
-- Stored in @PRAGMA user_version@.
schemaVersion :: Int
schemaVersion = 4

-- | Apply the schema to a fresh connection and stamp the version.
-- Uses direct-sqlite's multi-statement exec to run the whole script.
applySchema :: Connection -> IO ()
applySchema conn = do
    Direct.exec (Internal.connectionHandle conn) schemaSql
    execute_ conn (Query (T.pack ("PRAGMA user_version = " <> show schemaVersion)))

-- | Execute arbitrary (potentially multi-statement) SQL against a connection.
-- Useful for applying DDL fixtures in tests.
execSql :: Connection -> Text -> IO ()
execSql conn = Direct.exec (Internal.connectionHandle conn)
