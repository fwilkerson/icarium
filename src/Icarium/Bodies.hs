module Icarium.Bodies (
    bodiesDir,
    taskBodyPath,
    ctxBodyPath,
    ensureBodiesDirs,
    writeBody,
    readBody,
    persistBody,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Icarium.Types (NodeKind (..))
import System.Directory (createDirectoryIfMissing, doesFileExist, renamePath)
import System.FilePath (takeDirectory, (</>))

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
writeBody fp content = do
    let tmp = fp <> ".tmp"
    TIO.writeFile tmp content
    renamePath tmp fp

readBody :: FilePath -> IO Text
readBody fp = do
    exists <- doesFileExist fp
    if exists then TIO.readFile fp else pure ""

persistBody :: FilePath -> NodeKind -> Text -> Text -> IO FilePath
persistBody db kind nid body = do
    let bodDir = bodiesDir db
        fp = case kind of
            TaskNode -> taskBodyPath bodDir nid
            ContextNode -> ctxBodyPath bodDir nid
    ensureBodiesDirs bodDir
    writeBody fp body
    pure fp
