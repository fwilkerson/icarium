{-# LANGUAGE TemplateHaskell #-}

module Icarium.Migrations (
    Migration (..),
    migrations,
    mkSqlMigration,
) where

import Control.Exception (SomeException, catch, throwIO)
import Data.ByteString (ByteString)
import Data.FileEmbed (embedDir, makeRelativeToProject)
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Database.SQLite.Simple (Connection)
import Icarium.Schema (applySchema, execSql)
import Text.Read (readMaybe)

{- | A single forward migration. @migrationVersion@ is the schema version
produced by running @migrationUp@. Each @migrationUp@ manages its own
transaction and stamps @PRAGMA user_version@ atomically.
-}
data Migration = Migration
    { migrationVersion :: Int
    , migrationUp :: Connection -> IO ()
    }

embeddedMigrationFiles :: [(FilePath, ByteString)]
embeddedMigrationFiles = $(makeRelativeToProject "spec/migrations" >>= embedDir)

{- | Ordered list of all schema migrations, oldest first.
Files matching @spec/migrations/NNNN_*.sql@ are picked up automatically at
compile time; no edits to this module are needed when adding a new file.
-}
migrations :: [Migration]
migrations =
    Migration 1 applySchema
        : sortBy
            (comparing migrationVersion)
            (mapMaybe toMigration embeddedMigrationFiles)

toMigration :: (FilePath, ByteString) -> Maybe Migration
toMigration (fp, content) = do
    v <- readMaybe (take 4 fp)
    pure $ mkSqlMigration v (decodeUtf8 content)

{- | Build a @Migration@ that wraps arbitrary SQL in a transaction.
A failing statement rolls back the transaction, leaving @user_version@
unchanged. Useful for testing.
-}
mkSqlMigration :: Int -> Text -> Migration
mkSqlMigration v sql = Migration v (runSqlMigration v sql)

runSqlMigration :: Int -> Text -> Connection -> IO ()
runSqlMigration v sql conn = do
    execSql conn "BEGIN"
    ( do
            execSql conn sql
            execSql conn (T.pack ("PRAGMA user_version = " <> show v))
        )
        `catch` \(e :: SomeException) -> do
            execSql conn "ROLLBACK" `catch` \(_ :: SomeException) -> pure ()
            throwIO e
    execSql conn "COMMIT"
