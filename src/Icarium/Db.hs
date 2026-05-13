module Icarium.Db (
    defaultDbPath,
    openDb,
    withDb,
    initDb,
    dbSchemaVersion,
    migrateDb,
    parseDbTime,
) where

import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import Database.SQLite.Simple (Connection, Only (..), close, open, query_)
import Icarium.Bodies (mtimeSweep)
import Icarium.Migrations (Migration (..), migrations)
import Icarium.Schema (applySchema)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory, (</>))

defaultDbPath :: FilePath
defaultDbPath = ".icarium" </> "icarium.db"

openDb :: FilePath -> IO Connection
openDb path = do
    createDirectoryIfMissing True (takeDirectory path)
    open path

{- | Open the DB, run pending migrations, run an action, close the DB.
Exception-safe; if migration fails the error propagates and the DB is
left at its original schema version (the migration ran inside a transaction).
-}
withDb :: FilePath -> (Connection -> IO a) -> IO a
withDb path action = bracket (openDb path) close $ \conn -> do
    migrateDb conn
    mtimeSweep conn path
    action conn

{- | Run all pending migrations against an open connection, in version order.
Each migration is atomic: it manages its own transaction and stamps
@PRAGMA user_version@ inside that transaction. Idempotent against an
already-current DB.
-}
migrateDb :: Connection -> IO ()
migrateDb conn = do
    current <- fromIntegral <$> dbSchemaVersion conn
    let pending = filter ((> current) . migrationVersion) migrations
    forM_ pending $ \m -> migrationUp m conn

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

parseDbTime :: Text -> Maybe UTCTime
parseDbTime = parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" . T.unpack
