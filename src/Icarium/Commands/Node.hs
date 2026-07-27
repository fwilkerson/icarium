{- | The command-layer home for the @Node@ supertype: the id-resolution
surface and the CRUD leaves that @task@ and @ctx@ share verbatim. Each runner
takes a 'NodeKind' and dispatches to the task or context repo through the
small tables below — no per-kind behaviour lives in a runner body.
-}
module Icarium.Commands.Node (
    -- * Id argument
    nodeIdArg,

    -- * Resolution
    requireTask,
    requireContext,
    resolveNode,

    -- * Shared CRUD runners
    runRm,
    runPath,
    runCat,
    runExists,
) where

import Control.Monad (when)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Database.SQLite.Simple (Connection)
import Options.Applicative
import System.Directory (doesFileExist, removeFile)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)

import Icarium.Bodies (bodiesDir, nodeBodyPath, readBody)
import Icarium.Commands.Util (fatal, resolveOrFatal)
import Icarium.Db (withDb)
import Icarium.Events qualified as Ev
import Icarium.Repo.Context qualified as RCx
import Icarium.Repo.Task qualified as RT
import Icarium.Types

-- | The id positional argument, its metavar (@TASK_ID@ / @CONTEXT_ID@) named by kind.
nodeIdArg :: NodeKind -> Parser Text
nodeIdArg kind =
    T.pack <$> strArgument (metavar (T.unpack (T.toUpper (nodeKindText kind)) <> "_ID"))

-- =============================================================
-- Per-kind dispatch tables (plumbing, not behaviour)
-- =============================================================

resolveId :: NodeKind -> Connection -> Text -> IO (Either String Text)
resolveId TaskNode = RT.resolveTaskId
resolveId ContextNode = RCx.resolveContextId

deleteRow :: NodeKind -> Connection -> Text -> IO Bool
deleteRow TaskNode = RT.deleteTask
deleteRow ContextNode = RCx.deleteContext

idsByPrefix :: NodeKind -> Connection -> Text -> IO [Text]
idsByPrefix TaskNode c input = map taskId <$> RT.getTasksByPrefix c input
idsByPrefix ContextNode c input = map contextId <$> RCx.getContextsByPrefix c input

-- =============================================================
-- Resolution
-- =============================================================

requireTask :: Connection -> Text -> IO Text
requireTask c input = RT.resolveTaskId c input >>= either (fatal 2) pure

requireContext :: Connection -> Text -> IO Text
requireContext c input = RCx.resolveContextId c input >>= either (fatal 2) pure

resolveNode :: Connection -> Text -> IO (NodeKind, Text)
resolveNode c input = do
    ts <- RT.getTasksByPrefix c input
    cxs <- RCx.getContextsByPrefix c input
    case (ts, cxs) of
        ([t], []) -> pure (TaskNode, taskId t)
        ([], [cx]) -> pure (ContextNode, contextId cx)
        ([], []) -> fatal 2 ("unknown node: " <> T.unpack input)
        _ -> fatal 2 ("ambiguous id: " <> T.unpack input)

-- =============================================================
-- Shared CRUD runners
-- =============================================================

{- | Delete a node: take the row and its body file together, then log the
deletion with the CLI actor for the kind (@task rm@ / @ctx rm@).
-}
runRm :: NodeKind -> FilePath -> Text -> IO ()
runRm kind db input = withDb db $ \c -> do
    nid <- resolveOrFatal (resolveId kind c input)
    ok <- deleteRow kind c nid
    if ok
        then do
            let fp = nodeBodyPath (bodiesDir db) kind nid
            exists <- doesFileExist fp
            when exists $ removeFile fp
            Ev.emit db (nodeKindCli kind <> " rm") (Ev.nodeDeleted kind nid)
            TIO.putStrLn ("deleted " <> nid)
        else fatal 1 (T.unpack (nodeKindText kind) <> " not found: " <> T.unpack input)

runPath :: NodeKind -> FilePath -> Text -> IO ()
runPath kind db input = withDb db $ \c -> do
    nid <- resolveOrFatal (resolveId kind c input)
    TIO.putStrLn (T.pack (nodeBodyPath (bodiesDir db) kind nid))

runCat :: NodeKind -> FilePath -> Text -> IO ()
runCat kind db input = withDb db $ \c -> do
    nid <- resolveOrFatal (resolveId kind c input)
    TIO.putStr =<< readBody (nodeBodyPath (bodiesDir db) kind nid)

{- | The @exists@ contract: exit 0 on a unique prefix match (printing the full
id when @verbose@), 1 on no match, 2 on ambiguity. Callers only script the
exit code, so the codes are the interface.
-}
runExists :: NodeKind -> FilePath -> Bool -> Text -> IO ()
runExists kind db verbose input = withDb db $ \c -> do
    ids <- idsByPrefix kind c input
    case ids of
        [nid] -> when verbose $ TIO.putStrLn nid
        [] -> exitWith (ExitFailure 1)
        _ -> do
            hPutStrLn stderr $
                "ambiguous: "
                    <> T.unpack input
                    <> " matches "
                    <> show (length ids)
                    <> " "
                    <> T.unpack (nodeKindText kind <> "s")
            exitWith (ExitFailure 2)
