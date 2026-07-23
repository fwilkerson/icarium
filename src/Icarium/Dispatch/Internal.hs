module Icarium.Dispatch.Internal (
    DispatchRequest (..),
    DispatchResult (..),
    DispatchCtx (..),
    dispatch,
    buildPrompt,
    applyOutcomeToTask,
) where

import Control.Exception (onException)
import Control.Monad (void, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, withExceptT)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Database.SQLite.Simple (Connection)
import System.Directory (createDirectoryIfMissing, makeAbsolute)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

import Icarium.Bodies.Sweep (refreshTaskBody)
import Icarium.Config (
    Config (..),
    DispatchConfig (..),
    ProjectConfig (..),
    ReviewConfig (..),
 )
import Icarium.Dispatch.Agreement (agreementSection, loadAgreementFile, scratchSection)
import Icarium.Dispatch.Claude (RunCtx (..), claudeArgs, runClaudeStreaming)
import Icarium.Dispatch.Outcome (
    DispatchCtx (..),
    DispatchResult (..),
    applyOutcomeToTask,
 )
import Icarium.Dispatch.PostClaude (PostClaudeArgs (..), PostClaudeResult (..), handlePostClaudeWithReview)
import Icarium.Dispatch.Reviewer (loadReviewerPrompt)
import Icarium.Dispatch.Worktree (
    WorktreeError (..),
    createDispatchWorktree,
    teardownWorktree,
    worktreeErrorText,
    worktreePath,
 )
import Icarium.Events qualified as Ev
import Icarium.Git qualified as Git
import Icarium.Id (newId)
import Icarium.Prompt (taskPromptBody)
import Icarium.Repo.Dispatch qualified as RD
import Icarium.Types

-- =============================================================
-- Request / result
-- =============================================================

data DispatchRequest = DispatchRequest
    { drTask :: Task
    , drConfig :: Config
    , drDbPath :: FilePath
    , drDryRun :: Bool
    , drRouting :: Routing
    , drBaseOverride :: Maybe Text
    }

dispatchBranchName :: Text -> Text
dispatchBranchName did = "dispatch/" <> did

{- | The worker's scratch directory inside its worktree. Absolute: the
worker's cwd is the worktree, and a relative path in the prompt would
resolve differently for any child process that changes directory.
One resolver so the directory we create and the path we name can't drift.
-}
resolveScratch :: FilePath -> DispatchConfig -> IO FilePath
resolveScratch wt dcfg = makeAbsolute (wt </> T.unpack (dcScratchDir dcfg))

data ResolvedOpts = ResolvedOpts
    { roModel :: Text
    , roEffort :: Effort
    , roBase :: Text
    }

{- | Precedence: the @dispatch run@ flag, then the task's own override, then
the @[dispatch]@ config default. The flag is a deliberate one-off, so it
outranks the task's standing routing choice.
-}
resolveDispatchOpts :: DispatchRequest -> ResolvedOpts
resolveDispatchOpts req =
    ResolvedOpts
        { roModel = fromMaybe (dcModel dcfg) (rtModel routing)
        , roEffort = fromMaybe (dcEffort dcfg) (rtEffort routing)
        , roBase = fromMaybe (pcIntegrationBranch (cfgProject (drConfig req))) (drBaseOverride req)
        }
  where
    dcfg = cfgDispatch (drConfig req)
    routing = drRouting req <> taskRouting (drTask req)

-- =============================================================
-- Entry
-- =============================================================

{- | Run one dispatch. 'Left' means provisioning failed before any dispatch
row existed — either a preflight check (an unreadable reviewer prompt_path
or agreement_path, see 'loadPreflight') or the worktree itself (no capacity,
setup error). The task is untouched and the caller decides whether to stop
or fail loudly.
-}
dispatch :: Connection -> DispatchRequest -> IO (Either WorktreeError DispatchResult)
dispatch conn req
    | drDryRun req = doDryRun conn req
    | otherwise = doReal conn req

-- =============================================================
-- Dry run
-- =============================================================

{- | Loads the reviewer system prompt and working agreement, the two
externally-named files that must fail closed: an unreadable prompt_path or
agreement_path has to stop the run before the worker starts (and before any
dispatch row or task state exists), not silently degrade to a weaker
built-in once the work is already done. A dry run runs the same checks —
previewing a prompt the real run would refuse to build is a lie.
-}
loadPreflight :: Config -> IO (Either Text (Maybe Text, Maybe Text))
loadPreflight cfg = do
    mSysPromptResult <- case cfgReview cfg of
        Just rc | rcEnabled rc -> loadReviewerPrompt (rcPromptPath rc)
        _ -> pure (Right Nothing)
    mAgreementResult <- loadAgreementFile (dcAgreementPath (cfgDispatch cfg))
    pure ((,) <$> mSysPromptResult <*> mAgreementResult)

doDryRun :: Connection -> DispatchRequest -> IO (Either WorktreeError DispatchResult)
doDryRun conn req = do
    task <- refreshTaskBody conn (drDbPath req) (drTask req)
    preflightResult <- loadPreflight (drConfig req)
    case preflightResult of
        Left err -> pure (Left (WtPreflightFailed err))
        Right (_mSysPrompt, mAgreement) -> Right <$> dryRunPreview conn req task mAgreement

dryRunPreview :: Connection -> DispatchRequest -> Task -> Maybe Text -> IO DispatchResult
dryRunPreview conn req task mAgreement = do
    fakeId <- newId
    let dcfg = cfgDispatch (drConfig req)
    absScratch <- resolveScratch (worktreePath fakeId) dcfg
    prompt <- buildPrompt conn task absScratch mAgreement Nothing
    let branch = dispatchBranchName fakeId
        opts = resolveDispatchOpts req
        tools = dcTools dcfg
        allowed = dcAllowedTools dcfg
        scratchDir = dcScratchDir dcfg
        mcpConfig = dcMcpConfig dcfg

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
    TIO.putStrLn (renderCmdPreview (roModel opts) (roEffort opts) tools allowed mcpConfig)
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
            , dresPayload = Nothing
            }

{- | Renders 'claudeArgs' for human readability: the comma-joined tool lists
get quoted, and the worker schema is elided. The preview exists to show what
/varies/ with task and config — the schema is an icarium-owned constant, and
inlining ~2 KB of JSON buries everything else on the line.
-}
renderCmdPreview :: Text -> Effort -> [Text] -> [Text] -> Maybe Text -> Text
renderCmdPreview model effort tools allowed mcpConfig =
    T.unwords ("claude" : readable (claudeArgs model effort tools allowed mcpConfig))
  where
    readable (flag : val : rest)
        | flag `elem` ["--tools", "--allowedTools"] =
            flag : ("\"" <> val <> "\"") : readable rest
        | flag == "--json-schema" = flag : "<worker payload schema>" : readable rest
        | otherwise = flag : readable (val : rest)
    readable xs = xs

-- =============================================================
-- Real dispatch (with retry loop)
-- =============================================================

doReal :: Connection -> DispatchRequest -> IO (Either WorktreeError DispatchResult)
doReal conn req = doRealAttempt conn req 1 Nothing Nothing

doRealAttempt :: Connection -> DispatchRequest -> Int -> Maybe Text -> Maybe Text -> IO (Either WorktreeError DispatchResult)
doRealAttempt conn req attempt mFindings mBaseline = do
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
    -- The reviewer's tamper baseline is the body at FIRST attempt start:
    -- a retry diffs against the original contract, not its own attempt
    -- start, else attempt 1's tampering becomes attempt 2's clean
    -- baseline and the surviving dispatch reports "no".
    let baselineBody = fromMaybe (taskBody task) mBaseline
    baseSha <- either (ioFail . show) pure =<< Git.revParse "." base

    provisioned <- runExceptT $ do
        (mSysPrompt, mAgreement) <-
            withExceptT WtPreflightFailed (ExceptT (loadPreflight cfg))
        did <- lift newId
        wt <- ExceptT (createDispatchWorktree "." dcfg did (dispatchBranchName did) base)
        pure (mSysPrompt, mAgreement, did, wt)

    case provisioned of
        Left err -> pure (Left err)
        Right (mSysPrompt, mAgreement, did, wt) -> do
            let branch = dispatchBranchName did
                logDir = ".icarium" </> "logs"
                logPath = logDir </> T.unpack did <> ".jsonl"
            createDirectoryIfMissing True logDir
            absScratch <- resolveScratch wt dcfg
            createDirectoryIfMissing True absScratch

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
            Ev.emit dbPath "dispatch" (Ev.DispatchStarted did (taskId task) branch)

            prompt <- buildPrompt conn task absScratch mAgreement mFindings

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
                    handlePostClaudeWithReview
                        PostClaudeArgs
                            { pcaCtx = dx
                            , pcaConfig = cfg
                            , pcaTask = task
                            , pcaBaselineBody = baselineBody
                            , pcaSysPrompt = mSysPrompt
                            , pcaExit = exit
                            , pcaBaseSha = baseSha
                            , pcaLogPath = logPath
                            }
                )
                    `onException` teardownWorktree "." dcfg wt
            teardownWorktree "." dcfg wt
            case pcResult of
                PCDone dr -> do
                    -- A no-commit success leaves an empty branch (sha ==
                    -- baseSha, verified by the post-claude guard); delete it
                    -- once the worktree no longer has it checked out. Force:
                    -- `-d` checks merged-ness against HEAD, which may be an
                    -- unrelated checkout.
                    when (dresOutcome dr == OSuccess && taskNoCommit task) $
                        void (Git.deleteBranchForce "." branch)
                    pure (Right dr)
                PCRetry dr findings
                    | attempt < maxAttempts -> do
                        next <- doRealAttempt conn req (attempt + 1) (Just findings) (Just baselineBody)
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

{- | @mAgreement@ is the loaded agreement_path content (Nothing = built-in),
appended after the shared task content — see 'agreementSection'.
@absScratch@ is the worker's scratch directory, already resolved: the
prompt names the path, never an env var (see 'scratchSection').
-}
buildPrompt :: Connection -> Task -> FilePath -> Maybe Text -> Maybe Text -> IO Text
buildPrompt conn t absScratch mAgreement mFindings = do
    body <- taskPromptBody conn t
    let base =
            body
                <> agreementSection mAgreement
                <> "\n"
                <> scratchSection absScratch
    pure $ case mFindings of
        Nothing -> base
        Just f -> base <> "\n## Reviewer findings from previous attempt\n\n" <> f <> "\n"

ioFail :: String -> IO a
ioFail = ioError . userError
