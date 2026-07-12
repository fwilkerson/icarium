module Icarium.Dispatch.Internal (
    DispatchRequest (..),
    DispatchResult (..),
    DispatchCtx (..),
    dispatch,
    dispatchBranchName,
    applyOutcomeToTask,
) where

import Control.Monad (unless, void, when)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Database.SQLite.Simple (Connection)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

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
    FinishArgs (..),
    applyOutcomeToTask,
    finishWith,
 )
import Icarium.Dispatch.PostClaude (PostClaudeResult (..), handlePostClaudeWithReview)
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

dispatch :: Connection -> DispatchRequest -> IO DispatchResult
dispatch conn req
    | drDryRun req = doDryRun conn req
    | otherwise = doReal conn req

-- =============================================================
-- Dry run
-- =============================================================

doDryRun :: Connection -> DispatchRequest -> IO DispatchResult
doDryRun conn req = do
    prompt <- buildPrompt conn (drTask req) Nothing
    fakeId <- newId
    let dcfg = cfgDispatch (drConfig req)
        branch = dispatchBranchName fakeId
        opts = resolveDispatchOpts req
        tools = dcTools dcfg
        allowed = dcAllowedTools dcfg
        scratchDir = dcScratchDir dcfg

    TIO.putStrLn "=== DRY RUN ==="
    TIO.putStrLn $ "dispatch id (simulated): " <> fakeId
    TIO.putStrLn $ "task id:                 " <> taskId (drTask req)
    TIO.putStrLn $ "base branch:             " <> roBase opts
    TIO.putStrLn $ "dispatch branch:         " <> branch
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

doReal :: Connection -> DispatchRequest -> IO DispatchResult
doReal conn req = doRealAttempt conn req 1 Nothing

doRealAttempt :: Connection -> DispatchRequest -> Int -> Maybe Text -> IO DispatchResult
doRealAttempt conn req attempt mFindings = do
    let cfg = drConfig req
        dcfg = cfgDispatch cfg
        task = drTask req
        dbPath = drDbPath req
        opts = resolveDispatchOpts req
        base = roBase opts
        model = roModel opts
        effort = roEffort opts
        maxAttempts = maybe 1 rcMaxAttempts (cfgReview cfg)

    checkPreconditions base
    baseSha <- either (ioFail . show) pure =<< Git.revParse "." base

    did <- newId
    let branch = dispatchBranchName did
        logDir = ".icarium" </> "logs"
        logPath = logDir </> T.unpack did <> ".jsonl"
    createDirectoryIfMissing True logDir
    createDirectoryIfMissing True (T.unpack (dcScratchDir dcfg))

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

    let retention = dcLogRetentionRuns (cfgDispatch cfg)
        dx =
            DispatchCtx
                { dxConn = conn
                , dxDbPath = dbPath
                , dxDid = did
                , dxBranch = branch
                , dxBase = base
                }
    mBr <- Git.createBranch "." branch base
    case mBr of
        Left e ->
            finishWith
                dx
                FinishArgs
                    { faOutcome = OFailure
                    , faSha = Nothing
                    , faNotes = "git checkout -b failed: " <> T.pack (show e)
                    , faRetention = retention
                    , faLogPath = Nothing
                    , faBaseSha = Just baseSha
                    }
        Right () -> do
            let ctx =
                    RunCtx
                        { rcDbPath = dbPath
                        , rcDid = did
                        , rcTask = task
                        , rcPrompt = prompt
                        , rcModel = model
                        , rcEffort = effort
                        , rcLogPath = logPath
                        }
            exit <- runClaudeStreaming ctx dcfg
            pcResult <- handlePostClaudeWithReview dx cfg task (taskNoCommit task) exit baseSha logPath
            case pcResult of
                PCDone dr -> pure dr
                PCRetry dr findings ->
                    if attempt < maxAttempts
                        then doRealAttempt conn req (attempt + 1) (Just findings)
                        else pure dr

checkPreconditions :: Text -> IO ()
checkPreconditions base = do
    clean <- Git.isClean "."
    unless clean $
        ioFail
            "working tree not clean; commit or stash before dispatch"
    mCur <- Git.currentBranch "."
    case mCur of
        Left e -> ioFail ("git: " <> show e)
        Right b ->
            when (b /= base) $
                ioFail
                    ( "not on base branch "
                        <> T.unpack base
                        <> "; currently on "
                        <> T.unpack b
                    )

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
