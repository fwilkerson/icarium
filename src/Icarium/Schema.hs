{-# LANGUAGE TemplateHaskell #-}
module Icarium.Schema
    ( schemaSql
    , schemaVersion
    , applySchema
    ) where

import           Data.FileEmbed                  (embedFile, makeRelativeToProject)
import           Data.Text                       (Text)
import qualified Data.Text.Encoding              as TE
import           Database.SQLite.Simple          (Connection, execute_)
import qualified Database.SQLite.Simple.Internal as Internal
import qualified Database.SQLite3                as Direct

-- | The DDL for the current schema, embedded at compile time from
-- @spec/schema.sql@. This is the single source of truth.
schemaSql :: Text
schemaSql = TE.decodeUtf8 $(makeRelativeToProject "spec/schema.sql" >>= embedFile)

-- | Monotonically-increasing integer. Bumped whenever schema changes.
-- Stored in @PRAGMA user_version@.
schemaVersion :: Int
schemaVersion = 2

-- | Apply the schema to a fresh connection and stamp the version.
-- Uses direct-sqlite's multi-statement exec to run the whole script.
applySchema :: Connection -> IO ()
applySchema conn = do
    Direct.exec (Internal.connectionHandle conn) schemaSql
    execute_ conn "PRAGMA user_version = 1"
