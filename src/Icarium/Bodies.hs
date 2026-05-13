module Icarium.Bodies (
    bodiesDir,
    taskBodyPath,
    ctxBodyPath,
    ensureBodiesDirs,
    writeBody,
    readBody,
    mtimeSweep,
) where

import Control.Monad (forM_, unless, when)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Database.SQLite.Simple (Connection, Only (..), execute, query, query_)
import System.Directory (
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getModificationTime,
    listDirectory,
    removeFile,
 )
import System.FilePath (dropExtension, takeDirectory, takeExtension, (</>))
import System.IO (hPutStrLn, stderr)

import Icarium.Repo.Fts qualified as Fts
import Icarium.Types (NodeKind (..))

bodiesDir :: FilePath -> FilePath
bodiesDir dbPath = takeDirectory dbPath </> "bodies"

taskBodyPath :: FilePath -> Text -> FilePath
taskBodyPath bodDir tid = bodDir </> "tasks" </> (T.unpack tid ++ ".md")

ctxBodyPath :: FilePath -> Text -> FilePath
ctxBodyPath bodDir cid = bodDir </> "contexts" </> (T.unpack cid ++ ".md")

ensureBodiesDirs :: FilePath -> IO ()
ensureBodiesDirs bodDir = do
    createDirectoryIfMissing True (bodDir </> "tasks")
    createDirectoryIfMissing True (bodDir </> "contexts")

writeBody :: FilePath -> Text -> IO ()
writeBody = TIO.writeFile

readBody :: FilePath -> IO Text
readBody fp = do
    exists <- doesFileExist fp
    if exists then TIO.readFile fp else pure ""

{- | Parse DB timestamp ("YYYY-MM-DD HH:MM:SS") to UTCTime.
Duplicated from Icarium.Db to avoid a circular import.
-}
parseDbTimeBodies :: Text -> Maybe UTCTime
parseDbTimeBodies = parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" . T.unpack

{- | Compare as integer Unix seconds: avoids sub-second mismatch between
file mtimes (high precision) and SQLite datetime() (second precision).
-}
utcToSec :: UTCTime -> Int
utcToSec = floor . utcTimeToPOSIXSeconds

{- | Check all body files against DB updated_at, re-index stale files,
create missing files from DB body column, and delete orphan files.
-}
mtimeSweep :: Connection -> FilePath -> IO ()
mtimeSweep conn dbPath = do
    let bodDir = bodiesDir dbPath
    ensureBodiesDirs bodDir
    sweepKind conn bodDir TaskNode
    sweepKind conn bodDir ContextNode
    orphanScan conn bodDir TaskNode
    orphanScan conn bodDir ContextNode

sweepKind :: Connection -> FilePath -> NodeKind -> IO ()
sweepKind conn bodDir kind = do
    entries <- listIdTimes conn kind
    forM_ entries $ \(eid, updAt) -> do
        let fp = bodyPath bodDir kind eid
        exists <- doesFileExist fp
        if not exists
            then migrateBodyToFile conn kind eid fp
            else sweepFile conn kind eid fp updAt

-- | First-time migration: write the DB body column out to a body file.
migrateBodyToFile :: Connection -> NodeKind -> Text -> FilePath -> IO ()
migrateBodyToFile conn kind eid fp = do
    body <- getBodyFromDb conn kind eid
    writeBody fp body

sweepFile :: Connection -> NodeKind -> Text -> FilePath -> Text -> IO ()
sweepFile conn kind eid fp updAt = do
    mtime <- getModificationTime fp
    let mtimeSec = utcToSec mtime
    case parseDbTimeBodies updAt of
        Just t
            | mtimeSec > utcToSec t -> do
                body <- TIO.readFile fp
                updateBodyInDb conn kind eid body
                titleRow <- getTitleFromDb conn kind eid
                Fts.indexEntry conn eid kind (fromMaybe "" titleRow) body
        _ -> pure ()

orphanScan :: Connection -> FilePath -> NodeKind -> IO ()
orphanScan conn bodDir kind = do
    let dir = kindDir bodDir kind
    dirExists <- doesDirectoryExist dir
    when dirExists $ do
        files <- listDirectory dir
        let mdFiles = filter ((== ".md") . takeExtension) files
        forM_ mdFiles $ \f -> do
            let eid = T.pack (dropExtension f)
            exists <- rowExists conn kind eid
            unless exists $ do
                hPutStrLn stderr $
                    "warn: orphan body file removed: " ++ f
                removeFile (dir </> f)

-- =============================================================
-- Internal helpers
-- =============================================================

kindDir :: FilePath -> NodeKind -> FilePath
kindDir bodDir TaskNode = bodDir </> "tasks"
kindDir bodDir ContextNode = bodDir </> "contexts"

bodyPath :: FilePath -> NodeKind -> Text -> FilePath
bodyPath bodDir TaskNode eid = taskBodyPath bodDir eid
bodyPath bodDir ContextNode eid = ctxBodyPath bodDir eid

listIdTimes :: Connection -> NodeKind -> IO [(Text, Text)]
listIdTimes conn TaskNode =
    query_ conn "SELECT id, updated_at FROM tasks"
listIdTimes conn ContextNode =
    query_ conn "SELECT id, updated_at FROM context"

getBodyFromDb :: Connection -> NodeKind -> Text -> IO Text
getBodyFromDb conn kind eid = do
    rows <- case kind of
        TaskNode ->
            query conn "SELECT body FROM tasks WHERE id = ?" (Only eid) ::
                IO [Only Text]
        ContextNode ->
            query conn "SELECT body FROM context WHERE id = ?" (Only eid) ::
                IO [Only Text]
    pure $ case rows of
        (Only b : _) -> b
        [] -> ""

getTitleFromDb :: Connection -> NodeKind -> Text -> IO (Maybe Text)
getTitleFromDb conn kind eid = do
    rows <- case kind of
        TaskNode ->
            query conn "SELECT title FROM tasks WHERE id = ?" (Only eid) ::
                IO [Only Text]
        ContextNode ->
            query conn "SELECT title FROM context WHERE id = ?" (Only eid) ::
                IO [Only Text]
    pure $ case rows of
        (Only t : _) -> Just t
        [] -> Nothing

updateBodyInDb :: Connection -> NodeKind -> Text -> Text -> IO ()
updateBodyInDb conn TaskNode eid body =
    execute conn "UPDATE tasks SET body = ? WHERE id = ?" (body, eid)
updateBodyInDb conn ContextNode eid body =
    execute conn "UPDATE context SET body = ? WHERE id = ?" (body, eid)

rowExists :: Connection -> NodeKind -> Text -> IO Bool
rowExists conn kind eid = do
    rows <- case kind of
        TaskNode ->
            query conn "SELECT 1 FROM tasks WHERE id = ?" (Only eid) ::
                IO [Only Int]
        ContextNode ->
            query conn "SELECT 1 FROM context WHERE id = ?" (Only eid) ::
                IO [Only Int]
    pure (not (null rows))
