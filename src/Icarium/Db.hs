module Icarium.Db (
    defaultDbPath,
    openDb,
    withDbSync,
    withDbReadOnly,
    initDb,
    dbSchemaVersion,
    migrateDb,
    parseDbTime,
) where

import Control.Exception (bracket)
import Control.Monad (unless, void, when)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple (Connection, Only (..), Query (..), close, execute_, open, query_)
import Icarium.Bodies.Sweep (mtimeSweep)
import Icarium.Migrations (Migration (..), migrations)
import Icarium.Schema (applySchema, schemaVersion)
import Icarium.Time (parseDbTime)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory, (</>))

defaultDbPath :: FilePath
defaultDbPath = ".icarium" </> "icarium.db"

{- | WAL + busy_timeout on every connection: dispatch, its heartbeat
thread, and concurrent commands share one DB file. Both pragmas return a
row, so @query_@ (not @execute_@) is required.
-}
openDb :: FilePath -> IO Connection
openDb path = do
    createDirectoryIfMissing True (takeDirectory path)
    conn <- open path
    void (query_ conn "PRAGMA journal_mode=WAL" :: IO [Only Text])
    void (query_ conn "PRAGMA busy_timeout=5000" :: IO [Only Int64])
    pure conn

{- | Open the DB, run pending migrations, run the mtime sweep, then the action.
Use for commands that write to the DB or need FTS accuracy (dispatch run, search, reindex).
-}
withDbSync :: FilePath -> (Connection -> IO a) -> IO a
withDbSync path action = bracket (openDb path) close $ \conn -> do
    migrateDb conn
    mtimeSweep conn path
    action conn

{- | Open the DB, run pending migrations, then the action — no sweep.
Use for pure-read commands and write commands that don't need FTS sync.
-}
withDbReadOnly :: FilePath -> (Connection -> IO a) -> IO a
withDbReadOnly path action = bracket (openDb path) close $ \conn -> do
    migrateDb conn
    action conn

{- | Run all pending migrations against an open connection, in version order.
Each migration is atomic: it manages its own transaction and stamps
@PRAGMA user_version@ inside that transaction. Idempotent against an
already-current DB.
Re-reads the schema version after each step so a single migration that
jumps multiple versions (e.g. applySchema stamping schemaVersion on a
fresh DB) does not cause subsequent incremental migrations to run.

Special case: when @user_version = 0@ but the @tasks@ table already exists,
the DB was created externally (e.g. a restore script applying spec/schema.sql
directly). Stamp the current schema version and skip migration 1 rather than
letting it fail with "table tasks already exists".
-}
migrateDb :: Connection -> IO ()
migrateDb conn = do
    v <- dbSchemaVersion conn
    when (v == 0) $ do
        rows <- query_ conn "SELECT name FROM sqlite_master WHERE type='table' AND name='tasks'" :: IO [Only T.Text]
        unless (null rows) $
            execute_ conn $
                Query $
                    "PRAGMA user_version = " <> T.pack (show schemaVersion)
    go
  where
    go = do
        current <- fromIntegral <$> dbSchemaVersion conn
        let pending = filter ((> current) . migrationVersion) migrations
        case pending of
            [] -> pure ()
            (m : _) -> migrationUp m conn >> go

{- | Create the DB file and apply the schema. Fails if the file
already exists — the caller decides whether to handle that.
-}
initDb :: FilePath -> IO ()
initDb path = do
    exists <- doesFileExist path
    if exists
        then ioError (userError ("database already exists: " <> path))
        else bracket (openDb path) close applySchema

dbSchemaVersion :: Connection -> IO Int64
dbSchemaVersion conn = do
    rows <- query_ conn "PRAGMA user_version" :: IO [Only Int64]
    pure $ case rows of
        (Only v : _) -> v
        _ -> 0
