module Icarium.Db
    ( defaultDbPath
    , openDb
    , initDb
    , dbSchemaVersion
    ) where

import Control.Exception (bracket)
import Data.Int (Int64)
import Database.SQLite.Simple (Connection, Only(..), close, open, query_)
import Icarium.Schema (applySchema)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), takeDirectory)

defaultDbPath :: FilePath
defaultDbPath = ".icarium" </> "icarium.db"

openDb :: FilePath -> IO Connection
openDb path = do
    createDirectoryIfMissing True (takeDirectory path)
    open path

-- | Create the DB file and apply the schema. Fails if the file
-- already exists — the caller decides whether to handle that.
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
        _            -> 0
