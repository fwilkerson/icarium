{-# LANGUAGE TemplateHaskell #-}

module Icarium.Migrations (
    Migration (..),
    migrations,
) where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedDir, makeRelativeToProject)
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Text.Encoding (decodeUtf8)
import Icarium.Migrations.Internal (Migration (..), mkSqlMigration)
import Icarium.Schema (applySchema)
import Text.Read (readMaybe)

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
