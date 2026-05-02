{-# LANGUAGE TemplateHaskell #-}

module Icarium.Schema (
    schemaSql,
    schemaVersion,
    applySchema,
    execSql,
) where

import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Database.SQLite.Simple (Connection, Query (..), execute_)
import Database.SQLite.Simple.Internal qualified as Internal
import Database.SQLite3 qualified as Direct

{- | The DDL for the current schema, embedded at compile time from
@spec/schema.sql@. This is the single source of truth.
-}
schemaSql :: Text
schemaSql = TE.decodeUtf8 $(makeRelativeToProject "spec/schema.sql" >>= embedFile)

{- | Monotonically-increasing integer. Bumped whenever schema changes.
Stored in @PRAGMA user_version@.
-}
schemaVersion :: Int
schemaVersion = 1

{- | Apply the schema to a fresh connection and stamp the version.
Uses direct-sqlite's multi-statement exec to run the whole script.
-}
applySchema :: Connection -> IO ()
applySchema conn = do
    Direct.exec (Internal.connectionHandle conn) schemaSql
    execute_ conn (Query (T.pack ("PRAGMA user_version = " <> show schemaVersion)))

{- | Execute arbitrary (potentially multi-statement) SQL against a connection.
Useful for applying DDL fixtures in tests.
-}
execSql :: Connection -> Text -> IO ()
execSql conn = Direct.exec (Internal.connectionHandle conn)
