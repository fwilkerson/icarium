module Icarium.Bodies.Sweep (mtimeSweep) where

import Control.Monad (forM_, unless, when)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Database.SQLite.Simple (Connection)
import System.Directory (
    doesDirectoryExist,
    doesFileExist,
    getModificationTime,
    listDirectory,
    removeFile,
 )
import System.FilePath (dropExtension, takeExtension, (</>))
import System.IO (hPutStrLn, stderr)

import Icarium.Bodies (bodiesDir, ctxBodyPath, ensureBodiesDirs, taskBodyPath, writeBody)
import Icarium.Repo.Context qualified as Repo.Context
import Icarium.Repo.Fts qualified as Fts
import Icarium.Repo.Task qualified as Repo.Task
import Icarium.Time (parseDbTime)
import Icarium.Types (NodeKind (..))

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
    case parseDbTime updAt of
        Just t
            | mtimeSec > utcToSec t -> do
                body <- TIO.readFile fp
                setBodyInDb conn kind eid body
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
listIdTimes conn TaskNode = Repo.Task.listTaskIdTimes conn
listIdTimes conn ContextNode = Repo.Context.listContextIdTimes conn

getBodyFromDb :: Connection -> NodeKind -> Text -> IO Text
getBodyFromDb conn TaskNode eid = Repo.Task.getTaskBody conn eid
getBodyFromDb conn ContextNode eid = Repo.Context.getContextBody conn eid

getTitleFromDb :: Connection -> NodeKind -> Text -> IO (Maybe Text)
getTitleFromDb conn TaskNode eid = Repo.Task.getTaskTitle conn eid
getTitleFromDb conn ContextNode eid = Repo.Context.getContextTitle conn eid

setBodyInDb :: Connection -> NodeKind -> Text -> Text -> IO ()
setBodyInDb conn TaskNode eid body = Repo.Task.setTaskBody conn eid body
setBodyInDb conn ContextNode eid body = Repo.Context.setContextBody conn eid body

rowExists :: Connection -> NodeKind -> Text -> IO Bool
rowExists conn TaskNode eid = Repo.Task.taskExists conn eid
rowExists conn ContextNode eid = Repo.Context.contextExists conn eid
