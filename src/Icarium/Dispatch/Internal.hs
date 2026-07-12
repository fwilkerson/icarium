module Icarium.Dispatch.Internal (
    DispatchRequest (..),
    DispatchResult (..),
    DispatchCtx (..),
    dispatch,
    dispatchBranchName,
    applyOutcomeToTask,
) where

import Control.Exception (onException)
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Database.SQLite.Simple (Connection)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

import Icarium.Bodies.Sweep (refreshTaskBody)
import Icarium.Config (
    Config (..),
    DispatchConfig (..),
    ProjectConfig (..),
    ReviewConfig (..),
 )
import Icarium.Dispatch.Claude (RunCtx (..), claudeArgs, runClaudeStreaming)
import Icarium.Dispatch.Outcome (
    DispatchCtx (..),
    DispatchResult (..),
    applyOutcomeToTask,
 )
import Icarium.Dispatch.PostClaude (PostClaudeResult (..), handlePostClaudeWithReview)
import Icarium.Dispatch.Worktree (
    WorktreeError,
    createDispatchWorktree,
    teardownWorktree,
    worktreeErrorText,
    worktreePath,
 )
import Icarium.Git qualified as Git
import Icarium.Id (newId)
import Icarium.Render (renderTaskPrompt)
import Icarium.Repo.Category qualified as RC
import Icarium.Repo.Context qualified as RCx
import Icarium.Repo.Dispatch qualified as RD
import Icarium.Repo.Edge qualified as RE
import Icarium.Repo.Task qualified as RT
import Icarium.Types

-- =============================================================
-- Request / result
-- =============================================================

data DispatchRequest = DispatchRequest
    { drTask :: Task
    , drConfig :: Config
    , drDbPath :: FilePath
    , drDryRun :: Bool
    , drModelOverride :: Maybe Text
    , drEffortOverride :: Maybe Effort
    , drBaseOverride :: Maybe Text
    }

dispatchBranchName :: Text -> Text
dispatchBranchName did = "dispatch/" <> did

data ResolvedOpts = ResolvedOpts
    { roModel :: Text
    , roEffort :: Effort
    , roBase :: Text
    }

resolveDispatchOpts :: DispatchRequest -> ResolvedOpts
resolveDispatchOpts req =
    ResolvedOpts
        { roModel = fromMaybe (dcModel (cfgDispatch (drConfig req))) (drModelOverride req)
        , roEffort = fromMaybe (dcEffort (cfgDispatch (drConfig req))) (drEffortOverride req)
        , roBase = fromMaybe (pcIntegrationBranch (cfgProject (drConfig req))) (drBaseOverride req)
        }

-- =============================================================
-- Entry
-- =============================================================

{- | Run one dispatch. 'Left' means the worktree could not be provisioned
(no capacity, setup error) before any dispatch row existed — the task is
untouched and the caller decides whether to stop or fail loudly.
-}
dispatch :: Connection -> DispatchRequest -> IO (Either WorktreeError DispatchResult)
dispatch conn req
    | drDryRun req = Right <$> doDryRun conn req
    | otherwise = doReal conn req

-- =============================================================
-- Dry run
-- =============================================================

doDryRun :: Connection -> DispatchRequest -> IO DispatchResult
doDryRun conn req = do
    task <- refreshTaskBody conn (drDbPath req) (drTask req)
    prompt <- buildPrompt conn task Nothing
    fakeId <- newId
    let dcfg = cfgDispatch (drConfig req)
        branch = dispatchBranchName fakeId
        opts = resolveDispatchOpts req
        tools = dcTools dcfg
        allowed = dcAllowedTools dcfg
        scratchDir = dcScratchDir dcfg

    TIO.putStrLn "=== DRY RUN ==="
    TIO.putStrLn $ "dispatch id (simulated): " <> fakeId
    TIO.putStrLn $ "task id:                 " <> taskId task
    TIO.putStrLn $ "base branch:             " <> roBase opts
    TIO.putStrLn $ "dispatch branch:         " <> branch
    TIO.putStrLn $ "worktree:                " <> T.pack (worktreePath fakeId)
    TIO.putStrLn $ "model:                   " <> roModel opts
    TIO.putStrLn $ "effort:                  " <> effortText (roEffort opts)
    TIO.putStrLn $ "tools:                   " <> T.intercalate "," tools
    TIO.putStrLn $ "allowed_tools:           " <> T.intercalate "," allowed
    TIO.putStrLn $ "scratch_dir:             " <> scratchDir
    TIO.putStrLn ""
    TIO.putStrLn "--- claude invocation ---"
    TIO.putStrLn (renderCmdPreview (roModel opts) (roEffort opts) tools allowed)
    TIO.putStrLn ""
    TIO.putStrLn "--- prompt (via stdin) ---"
    TIO.putStr prompt

    pure
        DispatchResult
            { dresDispatchId = Nothing
            , dresOutcome = OSuccess
            , dresBranch = branch
            , dresNotes = "dry-run"
            , dresLogPath = Nothing
            , dresBaseSha = Nothing
            }

{- | Renders 'claudeArgs', quoting the comma-joined tool lists for
human readability.
-}
renderCmdPreview :: Text -> Effort -> [Text] -> [Text] -> Text
renderCmdPreview model effort tools allowed =
    T.unwords ("claude" : quoteToolLists (claudeArgs model effort tools allowed))
  where
    quoteToolLists (flag : val : rest)
        | flag `elem` ["--tools", "--allowedTools"] =
            flag : ("\"" <> val <> "\"") : quoteToolLists rest
        | otherwise = flag : quoteToolLists (val : rest)
    quoteToolLists xs = xs

-- =============================================================
-- Real dispatch (with retry loop)
-- =============================================================

doReal :: Connection -> DispatchRequest -> IO (Either WorktreeError DispatchResult)
doReal conn req = doRealAttempt conn req 1 Nothing

doRealAttempt :: Connection -> DispatchRequest -> Int -> Maybe Text -> IO (Either WorktreeError DispatchResult)
doRealAttempt conn req attempt mFindings = do
    let cfg = drConfig req
        dcfg = cfgDispatch cfg
        dbPath = drDbPath req
        opts = resolveDispatchOpts req
        base = roBase opts
        model = roModel opts
        effort = roEffort opts
        maxAttempts = maybe 1 rcMaxAttempts (cfgReview cfg)

    -- Re-read per attempt: retries must see body-file edits from the
    -- previous attempt (the record in the request is a dispatch-start
    -- snapshot of the DB column).
    task <- refreshTaskBody conn dbPath (drTask req)
    baseSha <- either (ioFail . show) pure =<< Git.revParse "." base

    did <- newId
    let branch = dispatchBranchName did
        logDir = ".icarium" </> "logs"
        logPath = logDir </> T.unpack did <> ".jsonl"
    createDirectoryIfMissing True logDir

    mWt <- createDispatchWorktree "." dcfg did branch base
    case mWt of
        Left err -> pure (Left err)
        Right wt -> do
            createDirectoryIfMissing True (wt </> T.unpack (dcScratchDir dcfg))

            RD.insertDispatch
                conn
                did
                RD.NewDispatch
                    { RD.ndTaskId = taskId task
                    , RD.ndBranch = branch
                    , RD.ndBaseBranch = base
                    , RD.ndBaseSha = baseSha
                    , RD.ndModel = model
                    , RD.ndEffort = effort
                    , RD.ndLogPath = Just logPath
                    , RD.ndPid = Nothing
                    }

            void $
                RT.updateTask
                    conn
                    (taskId task)
                    RT.emptyUpdate
                        { RT.tuState = Just InProgress
                        }

            prompt <- buildPrompt conn task mFindings

            let dx =
                    DispatchCtx
                        { dxConn = conn
                        , dxDbPath = dbPath
                        , dxDid = did
                        , dxBranch = branch
                        , dxBase = base
                        , dxWorkDir = wt
                        }
                ctx =
                    RunCtx
                        { rcDbPath = dbPath
                        , rcDid = did
                        , rcTask = task
                        , rcPrompt = prompt
                        , rcModel = model
                        , rcEffort = effort
                        , rcLogPath = logPath
                        , rcWorkDir = wt
                        }
            -- Teardown must run on every exit, including exceptions;
            -- checkpointing of dirty state happens inside post-claude first.
            pcResult <-
                ( do
                    exit <- runClaudeStreaming ctx dcfg
                    handlePostClaudeWithReview dx cfg task (taskNoCommit task) exit baseSha logPath
                )
                    `onException` teardownWorktree "." dcfg wt
            teardownWorktree "." dcfg wt
            -- A no-commit success leaves an empty branch (sha == baseSha,
            -- verified by the post-claude guard); delete it once the worktree
            -- no longer has it checked out. Force: `-d` checks merged-ness
            -- against HEAD, which may be an unrelated checkout.
            case pcResult of
                PCDone dr
                    | dresOutcome dr == OSuccess && taskNoCommit task ->
                        void (Git.deleteBranchForce "." branch)
                _ -> pure ()
            case pcResult of
                PCDone dr -> pure (Right dr)
                PCRetry dr findings
                    | attempt < maxAttempts -> do
                        next <- doRealAttempt conn req (attempt + 1) (Just findings)
                        case next of
                            Right dr' -> pure (Right dr')
                            Left err -> do
                                -- The first attempt already recorded a failed
                                -- dispatch; report that rather than losing the
                                -- findings to a provisioning error.
                                hPutStrLn stderr $
                                    "icarium: retry skipped: "
                                        <> T.unpack (worktreeErrorText err)
                                pure (Right dr)
                    | otherwise -> pure (Right dr)

buildPrompt :: Connection -> Task -> Maybe Text -> IO Text
buildPrompt conn t mFindings = do
    refs <- RE.referencedContexts conn (taskId t)
    cats <- RC.taskCategoriesFor conn (taskId t)
    catMatch <- RCx.categoryMatchedContexts conn cats 5
    deps <- RE.dependencyTasks conn (taskId t)
    let refIds = map contextId refs
        dedupedCat = filter (\cx -> contextId cx `notElem` refIds) catMatch
        base = renderTaskPrompt t refs dedupedCat deps
    pure $ case mFindings of
        Nothing -> base
        Just f -> base <> "\n## Reviewer findings from previous attempt\n\n" <> f <> "\n"

ioFail :: String -> IO a
ioFail = ioError . userError
