{-# LANGUAGE TemplateHaskell #-}

module Icarium.Schema (
    schemaSql,
    schemaVersion,
    applySchema,
    execSql,
) where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedDir, embedFile, makeRelativeToProject)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Database.SQLite.Simple (Connection, Query (..), execute_)
import Database.SQLite.Simple.Internal qualified as Internal
import Database.SQLite3 qualified as Direct
import Text.Read (readMaybe)

{- | The DDL for the current schema, embedded at compile time from
@spec/schema.sql@. This is the single source of truth.
-}
schemaSql :: Text
schemaSql = TE.decodeUtf8 $(makeRelativeToProject "spec/schema.sql" >>= embedFile)

migrationFiles :: [(FilePath, ByteString)]
migrationFiles = $(makeRelativeToProject "spec/migrations" >>= embedDir)

{- | Highest migration version across all migrations (v1 base plus any files in
@spec/migrations/@). Computed at compile time from embedded filenames.
-}
schemaVersion :: Int
schemaVersion = maximum (1 : mapMaybe versionOf migrationFiles)
  where
    versionOf (fp, _) = readMaybe (take 4 fp)

{- | Apply the schema to a fresh connection and stamp the current schema version.
Uses direct-sqlite's multi-statement exec to run the whole script.
-}
applySchema :: Connection -> IO ()
applySchema conn = do
    Direct.exec (Internal.connectionHandle conn) schemaSql
    execute_ conn $ Query $ "PRAGMA user_version = " <> T.pack (show schemaVersion)

{- | Execute arbitrary (potentially multi-statement) SQL against a connection.
Useful for applying DDL fixtures in tests.
-}
execSql :: Connection -> Text -> IO ()
execSql conn = Direct.exec (Internal.connectionHandle conn)
