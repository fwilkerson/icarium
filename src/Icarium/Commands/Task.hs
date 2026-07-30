module Icarium.Commands.Task (Command, parser, run) where

import Control.Monad (forM, forM_, unless, void, when)
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.Char (isSpace)
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Database.SQLite.Simple (Connection)
import Options.Applicative
import System.Exit (ExitCode (..), exitWith)
import System.IO (stderr)

import Icarium.Bodies (bodiesDir, readBody, taskBodyPath)
import Icarium.Commands.Node qualified as Node
import Icarium.Commands.Util
import Icarium.Db (withDb, withDbSync)
import Icarium.Events qualified as Ev
import Icarium.Node (createTaskWithBody)
import Icarium.Prompt (taskPromptBody)
import Icarium.Render qualified as Render
import Icarium.Render.Json qualified as Json
import Icarium.Repo.Category qualified as RC
import Icarium.Repo.Curation qualified as RCur
import Icarium.Repo.Edge qualified as RE
import Icarium.Repo.Task qualified as RT
import Icarium.Types

data Command
    = Add AddOpts
    | List ListOpts
    | Queue QueueOpts
    | Show ShowOpts
    | Update UpdateOpts
    | Rm Text
    | Next NextOpts
    | Claim ClaimOpts
    | Path Text
    | Cat Text
    | Exists ExistsOpts

parser :: Parser Command
parser =
    subparser
        ( subcmd "add" "Add a task. Prints <id> and body path; Write your markdown to that path (no temp draft needed)." (Add <$> addP)
            <> subcmd "list" "List tasks (alias: ls). A pure filter: no dependency gate, no queue semantics — use `task queue` for actionable work." (List <$> listP)
            <> subcmd "queue" "Print the ordered worklist: both ready states, dependency-gated, priority then age. Narrow with --headless / --interactive." (Queue <$> queueP)
            <> subcmd "show" "Show task metadata. The body is intentionally not printed: Read $(icarium task path <id>) so a subsequent Edit can succeed (Claude Code's Edit tool requires a prior Read of the same path)." (Show <$> showP)
            <> subcmd "update" "Update task metadata. To edit the body: Read $(icarium task path <id>) then Edit." (Update <$> updateP)
            <> subcmd "start" "Set state to in-progress. Shorthand for `update <id> --state in-progress`." (Update <$> stateShorthandP InProgress)
            <> subcmd "done" "Set state to done. Shorthand for `update <id> --state done`." (Update <$> stateShorthandP Done)
            <> subcmd "rm" "Delete a task" (Rm <$> Node.nodeIdArg TaskNode)
            <> subcmd "next" "Print the next ready-interactive task id; exit 1 if empty. Same row as the head of `task queue --interactive`." (Next <$> nextP)
            <> subcmd "claim" "Atomically claim a task: marks it in-progress, stamps an owner, prints its id. With TASK_ID, claims that task if it is in either ready state. Without, takes the head of the ready-interactive queue; exit 1 if empty, exit 3 if the database write lock stayed busy (retry)." (Claim <$> claimP)
            <> subcmd "path" "Print body file path for a task (the body is a markdown file you Read/Edit directly)." (Path <$> Node.nodeIdArg TaskNode)
            <> subcmd "cat" "Print body of a task to stdout. Exit non-zero if the body file is missing." (Cat <$> Node.nodeIdArg TaskNode)
            <> subcmd "exists" "Check whether a task id or prefix resolves uniquely. Exit 0 = found, 1 = not found, 2 = ambiguous." (Exists <$> existsP)
        )

run :: FilePath -> Command -> IO ()
run db = \case
    Add o -> runAdd db o
    List o -> runList db o
    Queue o -> runQueue db o
    Show o -> runShow db o
    Update o -> runUpdate db o
    Rm t -> Node.runRm TaskNode db t
    Next o -> runNext db o
    Claim o -> runClaim db o
    Path t -> Node.runPath TaskNode db t
    Cat t -> Node.runCat TaskNode db t
    Exists o -> Node.runExists TaskNode db (exVerbose o) (exId o)

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
    , aKind :: Maybe Text
    , aDependsOn :: [Text]
    , aReferences :: [Text]
    , aNoCommit :: Bool
    , aRouting :: Routing
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
                <> help "idea | planned | ready-headless | ready-interactive (default: planned)"
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
        <*> optional (textOption "kind" "NAME" "Tag with this kind category (shape of the work, e.g. bug | enhancement). Task-only; not used to pull related context.")
        <*> many (textOption "depends-on" "TASK_ID" "Add a depends_on edge to TASK_ID")
        <*> many (textOption "references" "CONTEXT_ID" "Add a references edge to CONTEXT_ID")
        <*> switch (long "no-commit" <> help "Mark task as side-effect-only (no code commits required)")
        <*> (($ mempty) <$> routingP "this task")

runAdd :: FilePath -> AddOpts -> IO ()
runAdd db o = withDb db $ \c -> do
    body <- resolveBody (aBody o)
    unless (aState o `elem` ([Idea, Planned] <> readyStates)) $
        fatal 2 "on add: state must be idea | planned | ready-headless | ready-interactive"

    -- Pre-validate referenced categories and nodes so we fail before insert.
    mDomain <- mapM (requireCategory c Domain) (aDomain o)
    mDisc <- mapM (requireCategory c Discipline) (aDiscipline o)
    mKind <- mapM (requireCategory c Kind) (aKind o)
    depIds <- mapM (Node.requireTask c) (aDependsOn o)
    refIds <- mapM (Node.requireContext c) (aReferences o)

    (tid, fp) <-
        createTaskWithBody
            c
            db
            RT.NewTask
                { RT.ntTitle = aTitle o
                , RT.ntBody = body
                , RT.ntState = aState o
                , RT.ntPriority = aPriority o
                , RT.ntNoCommit = aNoCommit o
                , RT.ntRouting = aRouting o
                }
    forM_ (catMaybes [mDomain, mDisc, mKind]) $ \cat ->
        RC.attachTaskCategory c tid (categoryId cat)
    forM_ depIds $ \depId ->
        void $ RE.insertEdge c DependsOn TaskNode tid TaskNode depId
    forM_ refIds $ \refId ->
        void $ RE.insertEdge c References TaskNode tid ContextNode refId
    Ev.emit db "task add" (Ev.TaskCreated tid (aState o))
    TIO.putStrLn tid
    TIO.putStrLn (T.pack fp)
    when (T.null body) $
        mapM_ (TIO.hPutStrLn stderr) (Render.emptyBodyNudge TaskNode tid fp)
    -- Read back rather than inspecting the flags: same predicate, same source
    -- of truth as the prompt-render and dispatch guards, so the three can't drift.
    attached <- RC.taskCategoriesFor c tid
    unless (hasRetrievalAxis attached) $
        mapM_ (TIO.hPutStrLn stderr) (Render.untaggedAddNudge tid)

-- =============================================================
-- list
-- =============================================================

data ListOpts = ListOpts
    { lStates :: [TaskState]
    , lDomain :: Maybe Text
    , lDiscipline :: Maybe Text
    , lKind :: Maybe Text
    , lAll :: Bool
    , lLimit :: Maybe Int
    , lJson :: Bool
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
                    <> help ("Filter by task state (" <> stateChoices <> "). Repeatable.")
                )
            )
        <*> optional (textOption "domain" "NAME" "Filter by domain category")
        <*> optional (textOption "discipline" "NAME" "Filter by discipline category")
        <*> optional (textOption "kind" "NAME" "Filter by kind category (e.g. bug | enhancement)")
        <*> switch (long "all" <> help "Include done and abandoned tasks")
        <*> optional
            ( option
                auto
                ( long "limit"
                    <> metavar "N"
                    <> help "Return at most N tasks"
                )
            )
        <*> jsonFlag

defaultActiveStates :: [TaskState]
defaultActiveStates = [Idea, Planned, ReadyHeadless, ReadyInteractive, InProgress, Blocked]

runList :: FilePath -> ListOpts -> IO ()
runList db o = withDb db $ \c -> do
    catFilters <-
        resolveCatFilters
            c
            [(Domain, lDomain o), (Discipline, lDiscipline o), (Kind, lKind o)]
    let effectiveStates
            | not (null (lStates o)) = lStates o
            | lAll o = []
            | otherwise = defaultActiveStates
    ts0 <- RT.listTasks c effectiveStates catFilters
    emitTasks c (lLimit o) (lJson o) ts0

-- | Shared tail of @task list@ and @task queue@: limit, then render.
emitTasks :: Connection -> Maybe Int -> Bool -> [Task] -> IO ()
emitTasks c limit asJson ts0 = do
    let ts = maybe ts0 (`take` ts0) limit
    taskRows <- buildTaskRows c ts
    if asJson
        then BLC.putStrLn (Json.renderTaskListJson taskRows)
        else do
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
-- queue
-- =============================================================

data QueueOpts = QueueOpts
    { qHeadless :: Bool
    , qInteractive :: Bool
    , qLimit :: Maybe Int
    , qJson :: Bool
    }

queueP :: Parser QueueOpts
queueP =
    QueueOpts
        <$> switch (long "headless" <> help "Only the headless queue: what `dispatch run` will take")
        <*> switch (long "interactive" <> help "Only the interactive queue: what `task next` and bare `task claim` take")
        <*> optional
            ( option
                auto
                ( long "limit"
                    <> metavar "N"
                    <> help "Return at most N tasks"
                )
            )
        <*> jsonFlag

{- | The ordered worklist. Deliberately no category filters: @queue@ is what
is actionable now, @list@ is how you find specific work — keeping them
distinct stops the two from collapsing into near-duplicates.
-}
runQueue :: FilePath -> QueueOpts -> IO ()
runQueue db o = do
    when (qHeadless o && qInteractive o) $
        fatal 2 "--headless and --interactive are mutually exclusive; drop one, or pass neither for both queues"
    withDb db $ \c -> do
        let states
                | qHeadless o = [ReadyHeadless]
                | qInteractive o = [ReadyInteractive]
                | otherwise = readyStates
        ts <- RT.queueTasks c states
        emitTasks c (qLimit o) (qJson o) ts

-- =============================================================
-- show
-- =============================================================

data ShowOpts = ShowOpts
    { sId :: Text
    , sPrompt :: Bool
    , sJson :: Bool
    }

showP :: Parser ShowOpts
showP =
    ShowOpts
        <$> Node.nodeIdArg TaskNode
        <*> switch (long "prompt" <> help "Render task as an LLM prompt context block")
        <*> jsonFlag

runShow :: FilePath -> ShowOpts -> IO ()
runShow db o = do
    -- Flag conflict is pure usage error; reject before any DB I/O so a
    -- bad invocation can't create/migrate a store as a side effect.
    when (sPrompt o && sJson o) $
        fatal 2 "--prompt and --json are mutually exclusive"
    openDb db $ \c -> do
        tid <- resolveOrFatal (RT.resolveTaskId c (sId o))
        mt <- RT.getTask c tid
        t <- maybe (fatal 1 ("task not found: " <> T.unpack tid)) pure mt
        if sPrompt o
            then do
                bodyFromFile <- readBody (taskBodyPath (bodiesDir db) tid)
                TIO.putStr =<< taskPromptBody c t{taskBody = bodyFromFile}
            else do
                refs <- RE.referencedContexts c (taskId t)
                deps <- RE.dependencyTasks c (taskId t)
                cats <- RC.taskCategoriesFor c (taskId t)
                derived <- RE.derivedFromTasks c (taskId t)
                retiredIds <- RCur.retiredContextIds c (map contextId refs)
                let bodyPath = T.pack (taskBodyPath (bodiesDir db) tid)
                if sJson o
                    then BLC.putStrLn (Json.renderTaskShowJson t bodyPath refs deps derived cats retiredIds)
                    else do
                        utf8 <- detectUtf8
                        TIO.putStr (Render.renderTaskHuman utf8 t bodyPath refs deps derived cats retiredIds)
  where
    openDb = if sPrompt o then withDbSync else withDb

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
    , uKind :: Maybe Text
    , uNoCommit :: Maybe Bool
    , uRouting :: Routing -> Routing
    }

updateP :: Parser UpdateOpts
updateP =
    UpdateOpts
        <$> Node.nodeIdArg TaskNode
        -- Canonical state values: src/Icarium/Types.hs parseTaskState
        <*> optional
            ( option
                taskStateReader
                ( long "state"
                    <> metavar "STATE"
                    <> help ("Set new task state (" <> stateChoices <> ").")
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
        <*> optional (textOption "kind" "NAME" "Replace kind category; empty string clears")
        <*> optional
            ( flag' True (long "no-commit" <> help "Mark as side-effect-only (no code commits required)")
                <|> flag' False (long "commit-required" <> help "Clear no-commit flag (commit required)")
            )
        <*> routingP "this task"

-- | @task start@ / @task done@: an update that only sets the state.
stateShorthandP :: TaskState -> Parser UpdateOpts
stateShorthandP st = mk <$> Node.nodeIdArg TaskNode
  where
    mk tid =
        UpdateOpts
            { uId = tid
            , uState = Just st
            , uPriority = Nothing
            , uTitle = Nothing
            , uBlockReason = Nothing
            , uDomain = Nothing
            , uDiscipline = Nothing
            , uKind = Nothing
            , uNoCommit = Nothing
            , uRouting = id
            }

runUpdate :: FilePath -> UpdateOpts -> IO ()
runUpdate db o = withDb db $ \c -> do
    when (uState o == Just Blocked && isNothing (uBlockReason o)) $
        fatal 2 "--state blocked requires --block-reason"
    tid <- resolveOrFatal (RT.resolveTaskId c (uId o))
    -- Read the pre-update row here: the event needs the state it moved from.
    mBefore <- RT.getTask c tid
    -- Validate every axis before any mutation.
    changes <-
        forM [(Domain, uDomain o), (Discipline, uDiscipline o), (Kind, uKind o)] $
            \(ax, flg) -> (ax,) <$> resolveAxisFlag c ax flg
    -- Apply replacements: detach all of that axis, then attach new one if given.
    forM_ changes $ \(ax, mChange) -> forM_ mChange $ \mCat -> do
        RC.detachTaskCategoriesByAxis c tid ax
        forM_ mCat $ \cat -> RC.attachTaskCategory c tid (categoryId cat)
    let upd =
            RT.emptyUpdate
                { RT.tuTitle = uTitle o
                , RT.tuState = uState o
                , RT.tuPriority = fmap Just (uPriority o)
                , RT.tuBlockReason = fmap Just (uBlockReason o)
                , RT.tuNoCommit = uNoCommit o
                , RT.tuRouting = uRouting o
                }
    ok <- RT.updateTask c tid upd
    if ok
        then do
            -- Only a transition is an event: a title or priority edit has no
            -- from/to, and logging one with from == to invents a move.
            forM_ mBefore $ \before ->
                forM_ (uState o) $ \new ->
                    when (new /= taskState before) $
                        Ev.emit db "task update" (Ev.TaskUpdated tid (taskState before) new)
            TIO.putStrLn ("updated " <> tid)
        else fatal 1 ("task not found: " <> T.unpack (uId o))

-- =============================================================
-- next
-- =============================================================

data NextOpts = NextOpts

nextP :: Parser NextOpts
nextP = pure NextOpts

-- | The CLI queue serves the human; headless work is dispatch's to select.
runNext :: FilePath -> NextOpts -> IO ()
runNext db _ = withDb db $ \c -> do
    ts <- RT.queueTasks c [ReadyInteractive]
    case ts of
        [] -> exitWith (ExitFailure 1)
        (t : _) -> TIO.putStrLn (taskId t)

-- =============================================================
-- claim
-- =============================================================

data ClaimOpts = ClaimOpts
    { clId :: Maybe Text
    , clOwner :: Maybe Text
    }

claimP :: Parser ClaimOpts
claimP =
    ClaimOpts
        <$> optional (T.pack <$> strArgument (metavar "TASK_ID"))
        <*> optional
            ( textOption
                "owner"
                "NAME"
                "Record NAME as the claim owner (default: $ICARIUM_OWNER, else <user>@<host>)"
            )

runClaim :: FilePath -> ClaimOpts -> IO ()
runClaim db o = do
    when (maybe False (T.all isSpace) (clOwner o)) $
        fatal 2 "--owner must not be empty"
    withDb db $ \c -> do
        owner <- maybe RT.defaultOwner pure (clOwner o)
        mtid <- traverse (resolveOrFatal . RT.resolveTaskId c) (clId o)
        res <- case mtid of
            Nothing -> RT.claimNextTask c [ReadyInteractive] owner
            Just tid -> RT.claimReadyTask c tid owner
        case res of
            RT.Claimed t from -> do
                Ev.emit db "task claim" (Ev.TaskClaimed (taskId t) from owner)
                TIO.putStrLn (taskId t)
            -- A retry that dropped the id claims whatever heads the queue, and
            -- one that dropped the owner stamps the wrong name. Replay both.
            RT.LockBusy ->
                lockBusy $
                    "icarium task claim"
                        <> maybe "" (" " <>) mtid
                        <> maybe "" (" --owner " <>) (clOwner o)
            -- Exit 1 with no output is the empty-queue signal scripts read;
            -- a named task that was refused gets the reason instead.
            RT.NoCandidate -> maybe (exitWith (ExitFailure 1)) (refuseClaim c) mtid

-- | Only reached when the claim was refused; report the state that refused it.
refuseClaim :: Connection -> Text -> IO ()
refuseClaim c tid = do
    mt <- RT.getTask c tid
    let st = maybe "missing" (taskStateText . taskState) mt
    fatal 1 . T.unpack $
        "not claimable: "
            <> tid
            <> " is "
            <> st
            <> "; claim takes ready-headless | ready-interactive tasks. \
               \Specify it first, then: icarium task update "
            <> tid
            <> " --state ready-interactive"

-- =============================================================
-- exists
-- =============================================================

data ExistsOpts = ExistsOpts
    { exId :: Text
    , exVerbose :: Bool
    }

existsP :: Parser ExistsOpts
existsP =
    ExistsOpts
        <$> Node.nodeIdArg TaskNode
        <*> switch (long "verbose" <> short 'v' <> help "Print the resolved full id on stdout")
