module Icarium.Migrations (
    Migration (..),
    migrations,
) where

import Database.SQLite.Simple (Connection)
import Icarium.Schema (applySchema)

{- | A single forward migration. @migrationVersion@ is the schema version
produced by running @migrationUp@. Each @migrationUp@ manages its own
transaction and stamps @PRAGMA user_version@ atomically.
-}
data Migration = Migration
    { migrationVersion :: Int
    , migrationUp :: Connection -> IO ()
    }

{- | Ordered list of all schema migrations, oldest first.
To add a new migration: append @Migration N migrate_N_minus_1_to_N@.
-}
migrations :: [Migration]
migrations = [Migration 1 applySchema]
