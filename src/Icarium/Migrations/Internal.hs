module Icarium.Migrations.Internal (
    Migration (..),
    mkSqlMigration,
) where

import Control.Exception (SomeException, catch, throwIO)
import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple (Connection)
import Icarium.Schema (execSql)

{- | A single forward migration. @migrationVersion@ is the schema version
produced by running @migrationUp@. Each @migrationUp@ manages its own
transaction and stamps @PRAGMA user_version@ atomically.
-}
data Migration = Migration
    { migrationVersion :: Int
    , migrationUp :: Connection -> IO ()
    }

{- | Build a @Migration@ that wraps arbitrary SQL in a transaction.
A failing statement rolls back the transaction, leaving @user_version@
unchanged.
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
