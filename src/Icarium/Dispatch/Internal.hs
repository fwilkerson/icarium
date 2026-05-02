module Icarium.Dispatch.Internal (
    DispatchRequest (..),
    DispatchResult (..),
    DispatchCtx (..),
    dispatch,
    dispatchBranchName,
    applyOutcomeToTask,
    postClaudeGuard,
    raceTimeout,
    timeoutSentinel,
    handlePostClaude,
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
 )
import Icarium.Dispatch.Claude (RunCtx (..), raceTimeout, runClaudeStreaming, timeoutSentinel)
import Icarium.Dispatch.Outcome (
    DispatchCtx (..),
    DispatchResult (..),
    applyOutcomeToTask,
    finishWith,
 )
import Icarium.Dispatch.PostClaude (handlePostClaude, postClaudeGuard)
import Icarium.Git qualified as Git
import Icarium.Id (newId)
import Icarium.Render (renderTaskPrompt)
import Icarium.Repo.Category qualified as RC
import Icarium.Repo.Dispatch qualified as RD
import Icarium.Repo.Edge qualified as RE
import Icarium.Repo.Knowledge qualified as RK
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
    prompt <- buildPrompt conn (drTask req)
    fakeId <- newId
    let dcfg = cfgDispatch (drConfig req)
        branch = dispatchBranchName fakeId
        model = effectiveModel req
        effort = effectiveEffort req
        base = effectiveBase req
        tools = dcTools dcfg
        allowed = dcAllowedTools dcfg
        scratchDir = dcScratchDir dcfg

    TIO.putStrLn "=== DRY RUN ==="
    TIO.putStrLn $ "dispatch id (simulated): " <> fakeId
    TIO.putStrLn $ "task id:                 " <> taskId (drTask req)
    TIO.putStrLn $ "base branch:             " <> base
    TIO.putStrLn $ "dispatch branch:         " <> branch
    TIO.putStrLn $ "model:                   " <> model
    TIO.putStrLn $ "effort:                  " <> effortText effort
    TIO.putStrLn $ "tools:                   " <> T.intercalate "," tools
    TIO.putStrLn $ "allowed_tools:           " <> T.intercalate "," allowed
    TIO.putStrLn $ "scratch_dir:             " <> scratchDir
    TIO.putStrLn ""
    TIO.putStrLn "--- claude invocation ---"
    TIO.putStrLn (renderCmdPreview model effort tools allowed)
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

renderCmdPreview :: Text -> Effort -> [Text] -> [Text] -> Text
renderCmdPreview model effort tools allowed =
    T.unwords
        [ "claude -p"
        , "--model"
        , model
        , "--effort"
        , effortText effort
        , "--output-format stream-json"
        , "--verbose"
        , "--tools \"" <> T.intercalate "," tools <> "\""
        , "--disable-slash-commands"
        , "--allowedTools \"" <> T.intercalate "," allowed <> "\""
        ]

-- =============================================================
-- Real dispatch
-- =============================================================

doReal :: Connection -> DispatchRequest -> IO DispatchResult
doReal conn req = do
    let cfg = drConfig req
        dcfg = cfgDispatch cfg
        task = drTask req
        dbPath = drDbPath req
        base = effectiveBase req
        model = effectiveModel req
        effort = effectiveEffort req

    checkPreconditions base
    baseSha <- either (ioFail . show) pure =<< Git.revParse base

    -- Generate id up front so branch name and log path can embed it.
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

    prompt <- buildPrompt conn task

    let retention = dcLogRetentionRuns (cfgDispatch cfg)
        dx =
            DispatchCtx
                { dxConn = conn
                , dxDid = did
                , dxBranch = branch
                , dxBase = base
                }
    mBr <- Git.createBranch branch base
    case mBr of
        Left e ->
            finishWith
                dx
                OFailure
                Nothing
                ("git checkout -b failed: " <> T.pack (show e))
                retention
                Nothing
                (Just baseSha)
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
            handlePostClaude dx cfg exit baseSha logPath

checkPreconditions :: Text -> IO ()
checkPreconditions base = do
    clean <- Git.isClean
    unless clean $
        ioFail
            "working tree not clean; commit or stash before dispatch"
    mCur <- Git.currentBranch
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

buildPrompt :: Connection -> Task -> IO Text
buildPrompt conn t = do
    refs <- RE.referencedKnowledge conn (taskId t)
    cats <- RC.taskCategoriesFor conn (taskId t)
    catMatch <- RK.categoryMatchedKnowledge conn cats 5
    deps <- RE.dependencyTasks conn (taskId t)
    let refIds = map knowledgeId refs
        dedupedCat = filter (\k -> knowledgeId k `notElem` refIds) catMatch
    pure (renderTaskPrompt t refs dedupedCat deps)

effectiveModel :: DispatchRequest -> Text
effectiveModel req =
    fromMaybe (dcModel (cfgDispatch (drConfig req))) (drModelOverride req)

effectiveEffort :: DispatchRequest -> Effort
effectiveEffort req =
    fromMaybe (dcEffort (cfgDispatch (drConfig req))) (drEffortOverride req)

effectiveBase :: DispatchRequest -> Text
effectiveBase req =
    fromMaybe
        (pcIntegrationBranch (cfgProject (drConfig req)))
        (drBaseOverride req)

ioFail :: String -> IO a
ioFail = ioError . userError
