module Icarium.Commands.Task (Command, parser, run) where

import Control.Monad (forM_, unless, void, when)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Database.SQLite.Simple (Connection)
import Options.Applicative
import System.Directory (doesFileExist, removeFile)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)

import Icarium.Bodies (bodiesDir, persistBody, readBody, taskBodyPath)
import Icarium.Commands.Util
import Icarium.Db (withDbReadOnly, withDbSync)
import Icarium.Render qualified as Render
import Icarium.Repo.Category qualified as RC
import Icarium.Repo.Context qualified as RCx
import Icarium.Repo.Edge qualified as RE
import Icarium.Repo.Task qualified as RT
import Icarium.Types

data Command
    = Add AddOpts
    | List ListOpts
    | Show ShowOpts
    | Update UpdateOpts
    | Rm RmOpts
    | Next NextOpts
    | Path PathOpts
    | Cat CatOpts
    | Exists ExistsOpts

parser :: Parser Command
parser =
    subparser
        ( subcmd "add" "Add a task. Prints <id> and body path; Write your markdown to that path (no temp draft needed)." (Add <$> addP)
            <> subcmd "list" "List tasks (alias: ls)" (List <$> listP)
            <> subcmd "show" "Show task metadata. The body is intentionally not printed: Read $(icarium task path <id>) so a subsequent Edit can succeed (Claude Code's Edit tool requires a prior Read of the same path)." (Show <$> showP)
            <> subcmd "update" "Update task metadata. To edit the body: Read $(icarium task path <id>) then Edit." (Update <$> updateP)
            <> subcmd "rm" "Delete a task" (Rm <$> rmP)
            <> subcmd "next" "Print next ready task id; exit 1 if empty" (Next <$> nextP)
            <> subcmd "path" "Print body file path for a task (the body is a markdown file you Read/Edit directly)." (Path <$> pathP)
            <> subcmd "cat" "Print body of a task to stdout. Exit non-zero if the body file is missing." (Cat <$> catP)
            <> subcmd "exists" "Check whether a task id or prefix resolves uniquely. Exit 0 = found, 1 = not found, 2 = ambiguous." (Exists <$> existsP)
        )

run :: FilePath -> Command -> IO ()
run db = \case
    Add o -> runAdd db o
    List o -> runList db o
    Show o -> runShow db o
    Update o -> runUpdate db o
    Rm o -> runRm db o
    Next o -> runNext db o
    Path o -> runPath db o
    Cat o -> runCat db o
    Exists o -> runExists db o

-- =============================================================
-- add
-- =============================================================

data AddOpts = AddOpts
    { aTitle :: Text
    , aBody :: BodyInput
    , aState :: TaskState
    , aPriority :: Maybe Int
    , aDomain :: Maybe Text
    , aDiscipline :: Maybe Text
    , aDependsOn :: [Text]
    , aReferences :: [Text]
    , aNoCommit :: Bool
    }

addP :: Parser AddOpts
addP =
    AddOpts . T.pack
        <$> strArgument (metavar "TITLE" <> help "Task title. Keep ≤ 72 chars; longer titles are truncated in `task list`.")
        <*> bodyInputParser
        <*> option
            taskStateReader
            ( long "state"
                <> metavar "STATE"
                <> value Planned
                <> help "idea | planned | ready (default: planned)"
            )
        <*> optional
            ( option
                auto
                ( long "priority"
                    <> metavar "N"
                    <> help "0-10. Higher number = higher priority (sorts first); also more bubbles in the priority bar."
                )
            )
        <*> optional (textOption "domain" "NAME" "Tag with this domain category")
        <*> optional (textOption "discipline" "NAME" "Tag with this discipline category")
        <*> many (textOption "depends-on" "TASK_ID" "Add a depends_on edge to TASK_ID")
        <*> many (textOption "references" "CONTEXT_ID" "Add a references edge to CONTEXT_ID")
        <*> switch (long "no-commit" <> help "Mark task as side-effect-only (no code commits required)")

runAdd :: FilePath -> AddOpts -> IO ()
runAdd db o = withDbReadOnly db $ \c -> do
    body <- resolveBody (aBody o)
    unless (aState o `elem` [Idea, Planned, Ready]) $
        fatal 2 "on add: state must be idea | planned | ready"

    -- Pre-validate referenced categories and nodes so we fail before insert.
    mDomain <- mapM (requireCategory c Domain) (aDomain o)
    mDisc <- mapM (requireCategory c Discipline) (aDiscipline o)
    depIds <- mapM (requireTask c) (aDependsOn o)
    refIds <- mapM (requireContext c) (aReferences o)

    tid <-
        RT.insertTask
            c
            RT.NewTask
                { RT.ntTitle = aTitle o
                , RT.ntBody = body
                , RT.ntState = aState o
                , RT.ntPriority = aPriority o
                , RT.ntNoCommit = aNoCommit o
                }
    forM_ mDomain $ \cat -> RC.attachTaskCategory c tid (categoryId cat)
    forM_ mDisc $ \cat -> RC.attachTaskCategory c tid (categoryId cat)
    forM_ depIds $ \depId ->
        void $ RE.insertEdge c DependsOn TaskNode tid TaskNode depId
    forM_ refIds $ \refId ->
        void $ RE.insertEdge c References TaskNode tid ContextNode refId
    fp <- persistBody db TaskNode tid body
    TIO.putStrLn tid
    TIO.putStrLn (T.pack fp)
    when (T.null body) $ do
        hPutStrLn stderr ("# next: Write your markdown to " <> fp)
        hPutStrLn stderr ("# to edit later: Read $(icarium task path " <> T.unpack tid <> ") then Edit")

-- =============================================================
-- list
-- =============================================================

data ListOpts = ListOpts
    { lStates :: [TaskState]
    , lReady :: Bool
    , lDomain :: Maybe Text
    , lDiscipline :: Maybe Text
    , lAll :: Bool
    , lLimit :: Maybe Int
    }

listP :: Parser ListOpts
listP =
    ListOpts
        -- Canonical state values: src/Icarium/Types.hs parseTaskState
        <$> many
            ( option
                taskStateReader
                ( long "state"
                    <> metavar "STATE"
                    <> help "Filter by task state (idea | planned | ready | in-progress | blocked | done | abandoned). Repeatable."
                )
            )
        <*> switch (long "ready" <> help "Only state=ready tasks with all depends-on satisfied (matches what `dispatch run` and `task next` pick)")
        <*> optional (textOption "domain" "NAME" "Filter by domain category")
        <*> optional (textOption "discipline" "NAME" "Filter by discipline category")
        <*> switch (long "all" <> help "Include done and abandoned tasks")
        <*> optional
            ( option
                auto
                ( long "limit"
                    <> metavar "N"
                    <> help "Return at most N tasks"
                )
            )

defaultActiveStates :: [TaskState]
defaultActiveStates = [Idea, Planned, Ready, InProgress, Blocked]

runList :: FilePath -> ListOpts -> IO ()
runList db o = withDbReadOnly db $ \c -> do
    forM_ (lDomain o) $ \n -> void $ requireCategory c Domain n
    forM_ (lDiscipline o) $ \n -> void $ requireCategory c Discipline n
    let effectiveStates
            | not (null (lStates o)) = lStates o
            | lAll o = []
            | otherwise = defaultActiveStates
    ts0 <- RT.listTasks c effectiveStates (lReady o) (lDomain o) (lDiscipline o)
    let ts = maybe ts0 (`take` ts0) (lLimit o)
    taskRows <- buildTaskRows c ts
    utf8 <- detectUtf8
    TIO.putStr (Render.renderTaskList utf8 taskRows)

-- | Load per-task categories and edge counts in batch, build TaskRow list.
buildTaskRows :: Connection -> [Task] -> IO [Render.TaskRow]
buildTaskRows c ts = do
    let ids = map taskId ts
    catsBatch <- RC.taskCategoriesBatch c ids
    countsBatch <- RE.taskEdgeCounts c ids
    pure
        [ Render.TaskRow
            { Render.trTask = t
            , Render.trCats = fromMaybe [] (lookup (taskId t) catsBatch)
            , Render.trDeps = fst counts
            , Render.trRefs = snd counts
            }
        | t <- ts
        , let counts = fromMaybe (0, 0) (lookup (taskId t) countsBatch)
        ]

-- =============================================================
-- show
-- =============================================================

data ShowOpts = ShowOpts
    { sId :: Text
    , sPrompt :: Bool
    }

showP :: Parser ShowOpts
showP =
    ShowOpts . T.pack
        <$> strArgument (metavar "TASK_ID")
        <*> switch (long "prompt" <> help "Render task as an LLM prompt context block")

runShow :: FilePath -> ShowOpts -> IO ()
runShow db o = openDb db $ \c -> do
    tid <- resolveOrFatal (RT.resolveTaskId c (sId o))
    mt <- RT.getTask c tid
    t <- maybe (fatal 1 ("task not found: " <> T.unpack tid)) pure mt
    if sPrompt o
        then do
            bodyFromFile <- readBody (taskBodyPath (bodiesDir db) tid)
            let t' = t{taskBody = bodyFromFile}
            refs <- RE.referencedContexts c (taskId t')
            deps <- RE.dependencyTasks c (taskId t')
            cats <- RC.taskCategoriesFor c (taskId t')
            catMatch <- RCx.categoryMatchedContexts c cats 5
            let refIds = map contextId refs
                dedupedCat = filter (\cx -> contextId cx `notElem` refIds) catMatch
            TIO.putStr (Render.renderTaskPrompt t' refs dedupedCat deps)
        else do
            refs <- RE.referencedContexts c (taskId t)
            deps <- RE.dependencyTasks c (taskId t)
            cats <- RC.taskCategoriesFor c (taskId t)
            utf8 <- detectUtf8
            let bodyPath = T.pack (taskBodyPath (bodiesDir db) tid)
            TIO.putStr (Render.renderTaskHuman utf8 t bodyPath refs deps cats)
  where
    openDb = if sPrompt o then withDbSync else withDbReadOnly

-- =============================================================
-- update
-- =============================================================

data UpdateOpts = UpdateOpts
    { uId :: Text
    , uState :: Maybe TaskState
    , uPriority :: Maybe Int
    , uTitle :: Maybe Text
    , uBlockReason :: Maybe Text
    , uDomain :: Maybe Text
    , uDiscipline :: Maybe Text
    , uNoCommit :: Maybe Bool
    }

updateP :: Parser UpdateOpts
updateP =
    UpdateOpts . T.pack
        <$> strArgument (metavar "TASK_ID")
        -- Canonical state values: src/Icarium/Types.hs parseTaskState
        <*> optional
            ( option
                taskStateReader
                ( long "state"
                    <> metavar "STATE"
                    <> help "Set new task state (idea | planned | ready | in-progress | blocked | done | abandoned)."
                )
            )
        <*> optional
            ( option
                auto
                ( long "priority"
                    <> metavar "N"
                    <> help "0-10. Higher number = higher priority (sorts first); also more bubbles in the priority bar."
                )
            )
        <*> optional (textOption "title" "TEXT" "Replace task title. Keep ≤ 72 chars; longer titles are truncated in `task list`.")
        <*> optional (textOption "block-reason" "TEXT" "Reason for blocked state (required with --state blocked)")
        <*> optional (textOption "domain" "NAME" "Replace domain category; empty string clears")
        <*> optional (textOption "discipline" "NAME" "Replace discipline category; empty string clears")
        <*> optional
            ( flag' True (long "no-commit" <> help "Mark as side-effect-only (no code commits required)")
                <|> flag' False (long "commit-required" <> help "Clear no-commit flag (commit required)")
            )

runUpdate :: FilePath -> UpdateOpts -> IO ()
runUpdate db o = withDbReadOnly db $ \c -> do
    when (uState o == Just Blocked && isNothing (uBlockReason o)) $
        fatal 2 "--state blocked requires --block-reason"
    tid <- resolveOrFatal (RT.resolveTaskId c (uId o))
    -- Validate categories before any mutation.
    mDomCat <- resolveAxisFlag c Domain (uDomain o)
    mDiscCat <- resolveAxisFlag c Discipline (uDiscipline o)
    -- Apply replacements: detach all of that axis, then attach new one if given.
    forM_ mDomCat $ \mCat -> do
        RC.detachTaskCategoriesByAxis c tid Domain
        forM_ mCat $ \cat -> RC.attachTaskCategory c tid (categoryId cat)
    forM_ mDiscCat $ \mCat -> do
        RC.detachTaskCategoriesByAxis c tid Discipline
        forM_ mCat $ \cat -> RC.attachTaskCategory c tid (categoryId cat)
    let upd =
            RT.emptyUpdate
                { RT.tuTitle = uTitle o
                , RT.tuState = uState o
                , RT.tuPriority = fmap Just (uPriority o)
                , RT.tuBlockReason = fmap Just (uBlockReason o)
                , RT.tuNoCommit = uNoCommit o
                }
    ok <- RT.updateTask c tid upd
    if ok
        then TIO.putStrLn ("updated " <> tid)
        else fatal 1 ("task not found: " <> T.unpack (uId o))

-- =============================================================
-- rm
-- =============================================================

newtype RmOpts = RmOpts {rId :: Text}

rmP :: Parser RmOpts
rmP = RmOpts . T.pack <$> strArgument (metavar "TASK_ID")

runRm :: FilePath -> RmOpts -> IO ()
runRm db o = withDbReadOnly db $ \c -> do
    tid <- resolveOrFatal (RT.resolveTaskId c (rId o))
    ok <- RT.deleteTask c tid
    if ok
        then do
            let fp = taskBodyPath (bodiesDir db) tid
            exists <- doesFileExist fp
            when exists $ removeFile fp
            TIO.putStrLn ("deleted " <> tid)
        else fatal 1 ("task not found: " <> T.unpack (rId o))

-- =============================================================
-- next
-- =============================================================

data NextOpts = NextOpts

nextP :: Parser NextOpts
nextP = pure NextOpts

runNext :: FilePath -> NextOpts -> IO ()
runNext db _ = withDbReadOnly db $ \c -> do
    ts <- RT.listTasks c [] True Nothing Nothing
    case ts of
        [] -> exitWith (ExitFailure 1)
        (t : _) -> TIO.putStrLn (taskId t)

-- =============================================================
-- path
-- =============================================================

newtype PathOpts = PathOpts {pId :: Text}

pathP :: Parser PathOpts
pathP = PathOpts . T.pack <$> strArgument (metavar "TASK_ID")

runPath :: FilePath -> PathOpts -> IO ()
runPath db o = withDbReadOnly db $ \c -> do
    tid <- resolveOrFatal (RT.resolveTaskId c (pId o))
    TIO.putStrLn (T.pack (taskBodyPath (bodiesDir db) tid))

-- =============================================================
-- cat
-- =============================================================

newtype CatOpts = CatOpts {catId :: Text}

catP :: Parser CatOpts
catP = CatOpts . T.pack <$> strArgument (metavar "TASK_ID")

runCat :: FilePath -> CatOpts -> IO ()
runCat db o = withDbReadOnly db $ \c -> do
    tid <- resolveOrFatal (RT.resolveTaskId c (catId o))
    TIO.putStr =<< readBody (taskBodyPath (bodiesDir db) tid)

-- =============================================================
-- exists
-- =============================================================

data ExistsOpts = ExistsOpts
    { exId :: Text
    , exVerbose :: Bool
    }

existsP :: Parser ExistsOpts
existsP =
    ExistsOpts . T.pack
        <$> strArgument (metavar "TASK_ID")
        <*> switch (long "verbose" <> short 'v' <> help "Print the resolved full id on stdout")

runExists :: FilePath -> ExistsOpts -> IO ()
runExists db o = withDbReadOnly db $ \c -> do
    ts <- RT.getTasksByPrefix c (exId o)
    case ts of
        [t] -> do
            when (exVerbose o) $ TIO.putStrLn (taskId t)
        [] -> exitWith (ExitFailure 1)
        _ -> do
            hPutStrLn stderr $
                "ambiguous: "
                    <> T.unpack (exId o)
                    <> " matches "
                    <> show (length ts)
                    <> " tasks"
            exitWith (ExitFailure 2)
