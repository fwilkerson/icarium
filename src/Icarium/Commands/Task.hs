module Icarium.Commands.Task (Command, parser, run) where

import           Control.Monad          (forM_, unless, void, when)
import           Data.Maybe             (fromMaybe, isNothing)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Database.SQLite.Simple (Connection)
import           Options.Applicative
import           System.Exit            (ExitCode (..), exitWith)

import           Icarium.Commands.Util
import           Icarium.Db             (withDb)
import qualified Icarium.Render         as Render
import qualified Icarium.Repo.Category  as RC
import qualified Icarium.Repo.Edge      as RE
import qualified Icarium.Repo.Knowledge as RK
import qualified Icarium.Repo.Task      as RT
import           Icarium.Types

data Command
    = Add AddOpts
    | List ListOpts
    | Show ShowOpts
    | Update UpdateOpts
    | Rm RmOpts
    | Next NextOpts

parser :: Parser Command
parser = subparser
    ( subcmd "add"    "Add a task"                                (Add    <$> addP)
   <> subcmd "list"   "List tasks"                                (List   <$> listP)
   <> subcmd "show"   "Show a task"                               (Show   <$> showP)
   <> subcmd "update" "Update a task"                             (Update <$> updateP)
   <> subcmd "rm"     "Delete a task"                             (Rm     <$> rmP)
   <> subcmd "next"   "Print next ready task id; exit 1 if empty" (Next   <$> nextP)
    )

run :: FilePath -> Command -> IO ()
run db = \case
    Add o     -> runAdd db o
    List o    -> runList db o
    Show o    -> runShow db o
    Update o  -> runUpdate db o
    Rm o      -> runRm db o
    Next o    -> runNext db o

-- =============================================================
-- add
-- =============================================================

data AddOpts = AddOpts
    { aTitle      :: Text
    , aBody       :: BodyInput
    , aState      :: TaskState
    , aPriority   :: Maybe Int
    , aDomain     :: Maybe Text
    , aDiscipline :: Maybe Text
    , aDependsOn  :: [Text]
    , aReferences :: [Text]
    }

addP :: Parser AddOpts
addP = AddOpts . T.pack
    <$> strArgument (metavar "TITLE" <> help "Task title. Keep ≤ 72 chars; longer titles are truncated in `task list`.")
    <*> bodyInputParser
    <*> option taskStateReader
            ( long "state" <> metavar "STATE" <> value Planned
           <> help "idea | planned | ready (default: planned)" )
    <*> optional (option auto (long "priority" <> metavar "N"
           <> help "0-10. Higher number = higher priority (sorts first); also more bubbles in the priority bar."))
    <*> optional (T.pack <$> strOption (long "domain" <> metavar "NAME"
           <> help "Tag with this domain category"))
    <*> optional (T.pack <$> strOption (long "discipline" <> metavar "NAME"
           <> help "Tag with this discipline category"))
    <*> many (T.pack <$> strOption (long "depends-on" <> metavar "TASK_ID"
           <> help "Add a depends_on edge to TASK_ID"))
    <*> many (T.pack <$> strOption (long "references" <> metavar "KNOWLEDGE_ID"
           <> help "Add a references edge to KNOWLEDGE_ID"))

runAdd :: FilePath -> AddOpts -> IO ()
runAdd db o = withDb db $ \c -> do
    body <- resolveBody (aBody o)
    unless (aState o `elem` [Idea, Planned, Ready]) $
        fatal 2 "on add: state must be idea | planned | ready"

    -- Pre-validate referenced categories and nodes so we fail before insert.
    mDomain <- mapM (requireCategory c Domain)     (aDomain o)
    mDisc   <- mapM (requireCategory c Discipline) (aDiscipline o)
    depIds  <- mapM (requireTask c)      (aDependsOn o)
    refIds  <- mapM (requireKnowledge c) (aReferences o)

    tid <- RT.insertTask c RT.NewTask
        { RT.ntTitle    = aTitle o
        , RT.ntBody     = body
        , RT.ntState    = aState o
        , RT.ntPriority = aPriority o
        }
    forM_ mDomain $ \cat -> RC.attachTaskCategory c tid (categoryId cat)
    forM_ mDisc   $ \cat -> RC.attachTaskCategory c tid (categoryId cat)
    forM_ depIds $ \depId ->
        void $ RE.insertEdge c DependsOn TaskNode tid TaskNode depId
    forM_ refIds $ \refId ->
        void $ RE.insertEdge c References TaskNode tid KnowledgeNode refId
    TIO.putStrLn tid

-- =============================================================
-- list
-- =============================================================

data ListOpts = ListOpts
    { lStates     :: [TaskState]
    , lReady      :: Bool
    , lDomain     :: Maybe Text
    , lDiscipline :: Maybe Text
    , lAll        :: Bool
    }

listP :: Parser ListOpts
listP = ListOpts
    -- Canonical state values: src/Icarium/Types.hs parseTaskState
    <$> many (option taskStateReader (long "state" <> metavar "STATE"
           <> help "Filter by task state (idea | planned | ready | in-progress | blocked | done | abandoned). Repeatable."))
    <*> switch (long "ready" <> help "Only state=ready tasks with all depends-on satisfied (matches what `dispatch run` and `task next` pick)")
    <*> optional (T.pack <$> strOption (long "domain"     <> metavar "NAME"
           <> help "Filter by domain category"))
    <*> optional (T.pack <$> strOption (long "discipline" <> metavar "NAME"
           <> help "Filter by discipline category"))
    <*> switch (long "all" <> help "Include done and abandoned tasks")

defaultActiveStates :: [TaskState]
defaultActiveStates = [Idea, Planned, Ready, InProgress, Blocked]

runList :: FilePath -> ListOpts -> IO ()
runList db o = withDb db $ \c -> do
    forM_ (lDomain o)     $ \n -> void $ requireCategory c Domain     n
    forM_ (lDiscipline o) $ \n -> void $ requireCategory c Discipline n
    let effectiveStates
            | not (null (lStates o)) = lStates o
            | lAll o                 = []
            | otherwise              = defaultActiveStates
    ts <- RT.listTasks c effectiveStates (lReady o) (lDomain o) (lDiscipline o)
    taskRows <- buildTaskRows c ts
    utf8     <- detectUtf8
    TIO.putStr (Render.renderTaskList utf8 taskRows)

-- | Load per-task categories and edge counts in batch, build TaskRow list.
buildTaskRows :: Connection -> [Task] -> IO [Render.TaskRow]
buildTaskRows c ts = do
    let ids = map taskId ts
    catsBatch   <- RC.taskCategoriesBatch c ids
    countsBatch <- RE.taskEdgeCounts      c ids
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
    { sId     :: Text
    , sPrompt :: Bool
    , sBody   :: Bool
    }

showP :: Parser ShowOpts
showP = ShowOpts . T.pack
    <$> strArgument (metavar "TASK_ID")
    <*> switch (long "prompt" <> help "Render task as an LLM prompt context block")
    <*> switch (long "body"   <> help "Print only the task body and nothing else")

runShow :: FilePath -> ShowOpts -> IO ()
runShow db o = do
    when (sPrompt o && sBody o) $
        fatal 2 "--body and --prompt are mutually exclusive"
    withDb db $ \c -> do
        tid <- resolveOrFatal (RT.resolveTaskId c (sId o))
        mt  <- RT.getTask c tid
        t   <- maybe (fatal 1 ("task not found: " <> T.unpack tid)) pure mt
        if sBody o
            then TIO.putStr (taskBody t)
            else if sPrompt o
                then do
                    refs <- RE.referencedKnowledge c (taskId t)
                    deps <- RE.dependencyTasks     c (taskId t)
                    cats <- RC.taskCategoriesFor   c (taskId t)
                    catMatch <- RK.categoryMatchedKnowledge c cats 5
                    let refIds     = map knowledgeId refs
                        dedupedCat = filter (\k -> knowledgeId k `notElem` refIds) catMatch
                    TIO.putStr (Render.renderTaskPrompt t refs dedupedCat deps)
                else do
                    refs <- RE.referencedKnowledge c (taskId t)
                    deps <- RE.dependencyTasks     c (taskId t)
                    cats <- RC.taskCategoriesFor   c (taskId t)
                    utf8 <- detectUtf8
                    TIO.putStr (Render.renderTaskHuman utf8 t refs deps cats)

-- =============================================================
-- update
-- =============================================================

data UpdateOpts = UpdateOpts
    { uId          :: Text
    , uState       :: Maybe TaskState
    , uPriority    :: Maybe Int
    , uTitle       :: Maybe Text
    , uBody        :: BodyInput
    , uBlockReason :: Maybe Text
    , uDomain      :: Maybe Text
    , uDiscipline  :: Maybe Text
    }

updateP :: Parser UpdateOpts
updateP = UpdateOpts . T.pack
    <$> strArgument (metavar "TASK_ID")
    -- Canonical state values: src/Icarium/Types.hs parseTaskState
    <*> optional (option taskStateReader (long "state" <> metavar "STATE"
           <> help "Set new task state (idea | planned | ready | in-progress | blocked | done | abandoned)."))
    <*> optional (option auto (long "priority" <> metavar "N"
           <> help "0-10. Higher number = higher priority (sorts first); also more bubbles in the priority bar."))
    <*> optional (T.pack <$> strOption (long "title" <> metavar "TEXT"
           <> help "Replace task title. Keep ≤ 72 chars; longer titles are truncated in `task list`."))
    <*> bodyInputParser
    <*> optional (T.pack <$> strOption (long "block-reason" <> metavar "TEXT"
           <> help "Reason for blocked state (required with --state blocked)"))
    <*> optional (T.pack <$> strOption (long "domain" <> metavar "NAME"
           <> help "Replace domain category; empty string clears"))
    <*> optional (T.pack <$> strOption (long "discipline" <> metavar "NAME"
           <> help "Replace discipline category; empty string clears"))

runUpdate :: FilePath -> UpdateOpts -> IO ()
runUpdate db o = withDb db $ \c -> do
    when (uState o == Just Blocked && isNothing (uBlockReason o)) $
        fatal 2 "--state blocked requires --block-reason"
    tid <- resolveOrFatal (RT.resolveTaskId c (uId o))
    body <- case uBody o of
        BodyNone -> pure Nothing
        b        -> Just <$> resolveBody b
    -- Validate categories before any mutation.
    mDomCat  <- resolveAxisFlag c Domain     (uDomain o)
    mDiscCat <- resolveAxisFlag c Discipline (uDiscipline o)
    -- Apply replacements: detach all of that axis, then attach new one if given.
    forM_ mDomCat $ \mCat -> do
        RC.detachTaskCategoriesByAxis c tid Domain
        forM_ mCat $ \cat -> RC.attachTaskCategory c tid (categoryId cat)
    forM_ mDiscCat $ \mCat -> do
        RC.detachTaskCategoriesByAxis c tid Discipline
        forM_ mCat $ \cat -> RC.attachTaskCategory c tid (categoryId cat)
    let upd = RT.emptyUpdate
            { RT.tuTitle       = uTitle o
            , RT.tuBody        = body
            , RT.tuState       = uState o
            , RT.tuPriority    = fmap Just (uPriority o)
            , RT.tuBlockReason = fmap Just (uBlockReason o)
            }
    ok <- RT.updateTask c tid upd
    if ok then TIO.putStrLn ("updated " <> tid)
          else fatal 1 ("task not found: " <> T.unpack (uId o))

-- =============================================================
-- rm
-- =============================================================

newtype RmOpts = RmOpts { rId :: Text }

rmP :: Parser RmOpts
rmP = RmOpts . T.pack <$> strArgument (metavar "TASK_ID")

runRm :: FilePath -> RmOpts -> IO ()
runRm db o = withDb db $ \c -> do
    tid <- resolveOrFatal (RT.resolveTaskId c (rId o))
    ok <- RT.deleteTask c tid
    if ok then TIO.putStrLn ("deleted " <> tid)
          else fatal 1 ("task not found: " <> T.unpack (rId o))

-- =============================================================
-- next
-- =============================================================

data NextOpts = NextOpts

nextP :: Parser NextOpts
nextP = pure NextOpts

runNext :: FilePath -> NextOpts -> IO ()
runNext db _ = withDb db $ \c -> do
    ts <- RT.listTasks c [] True Nothing Nothing
    case ts of
        []      -> exitWith (ExitFailure 1)
        (t : _) -> TIO.putStrLn (taskId t)
