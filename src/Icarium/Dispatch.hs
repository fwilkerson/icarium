module Icarium.Dispatch
    ( DispatchRequest(..)
    , DispatchResult(..)
    , dispatch
    , dispatchBranchName
    , applyOutcomeToTask
    ) where

import           Control.Concurrent     (forkIO)
import           Control.Exception      (SomeException, bracket, try)
import           Control.Monad          (unless, void, when)
import qualified Data.ByteString.Char8  as BC
import qualified Data.ByteString.Lazy   as BL
import           Data.Maybe             (fromMaybe)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import qualified Data.Text.IO           as TIO
import           Database.SQLite.Simple (Connection, close)
import           System.Directory       (createDirectoryIfMissing, doesFileExist, removeFile)
import           System.Environment     (getEnvironment)
import           System.Exit            (ExitCode (..))
import           System.FilePath        ((</>))
import           System.IO              (BufferMode (..), Handle, IOMode (..), hClose, hIsEOF,
                                         hPutStrLn, hSetBuffering, openFile, stderr)
import           System.Process.Typed   (byteStringInput, createPipe, getPid, getStdout, proc,
                                         runProcess, setEnv, setStdin, setStdout, shell,
                                         waitExitCode, withProcessWait)

import           Icarium.Config         (CommandsConfig (..), Config (..), DispatchConfig (..),
                                         ProjectConfig (..))
import           Icarium.Db             (defaultDbPath, openDb)
import qualified Icarium.Git            as Git
import           Icarium.Id             (newId)
import           Icarium.Render         (renderTaskPrompt)
import qualified Icarium.Repo.Category  as RC
import qualified Icarium.Repo.Dispatch  as RD
import qualified Icarium.Repo.Edge      as RE
import qualified Icarium.Repo.Knowledge as RK
import qualified Icarium.Repo.Task      as RT
import           Icarium.Types

-- =============================================================
-- Request / result
-- =============================================================

data DispatchRequest = DispatchRequest
    { drTask           :: Task
    , drConfig         :: Config
    , drDryRun         :: Bool
    , drModelOverride  :: Maybe Text
    , drEffortOverride :: Maybe Effort
    , drBaseOverride   :: Maybe Text
    }

data DispatchResult = DispatchResult
    { dresDispatchId :: Maybe Text
    , dresOutcome    :: DispatchOutcome
    , dresBranch     :: Text
    , dresNotes      :: Text
    , dresLogPath    :: Maybe FilePath
    , dresBaseSha    :: Maybe Text
    }

dispatchBranchName :: Text -> Text
dispatchBranchName did = "dispatch/" <> did

-- =============================================================
-- Entry
-- =============================================================

dispatch :: Connection -> DispatchRequest -> IO DispatchResult
dispatch conn req
    | drDryRun req = doDryRun  conn req
    | otherwise    = doReal    conn req

-- =============================================================
-- Dry run
-- =============================================================

doDryRun :: Connection -> DispatchRequest -> IO DispatchResult
doDryRun conn req = do
    prompt <- buildPrompt conn (drTask req)
    fakeId <- newId
    let branch = dispatchBranchName fakeId
        model  = effectiveModel  req
        effort = effectiveEffort req
        base   = effectiveBase   req
        tools  = dcAllowedTools (cfgDispatch (drConfig req))

    TIO.putStrLn "=== DRY RUN ==="
    TIO.putStrLn $ "dispatch id (simulated): " <> fakeId
    TIO.putStrLn $ "task id:                 " <> taskId (drTask req)
    TIO.putStrLn $ "base branch:             " <> base
    TIO.putStrLn $ "dispatch branch:         " <> branch
    TIO.putStrLn $ "model:                   " <> model
    TIO.putStrLn $ "effort:                  " <> effortText effort
    TIO.putStrLn $ "allowed_tools:           " <> T.intercalate "," tools
    TIO.putStrLn ""
    TIO.putStrLn "--- claude invocation ---"
    TIO.putStrLn (renderCmdPreview model tools)
    TIO.putStrLn ""
    TIO.putStrLn "--- prompt (via stdin) ---"
    TIO.putStr prompt

    pure DispatchResult
        { dresDispatchId = Nothing
        , dresOutcome    = OSuccess
        , dresBranch     = branch
        , dresNotes      = "dry-run"
        , dresLogPath    = Nothing
        , dresBaseSha    = Nothing
        }

renderCmdPreview :: Text -> [Text] -> Text
renderCmdPreview model tools = T.unwords
    [ "claude -p"
    , "--model", model
    , "--output-format stream-json"
    , "--verbose"
    , "--allowedTools \"" <> T.intercalate "," tools <> "\""
    ]

-- =============================================================
-- Real dispatch
-- =============================================================

doReal :: Connection -> DispatchRequest -> IO DispatchResult
doReal conn req = do
    let cfg   = drConfig req
        task  = drTask   req
        base  = effectiveBase   req
        model = effectiveModel  req
        effort= effectiveEffort req
        tools = dcAllowedTools (cfgDispatch cfg)

    checkPreconditions base
    baseSha <- either (ioFail . show) pure =<< Git.revParse base

    -- Generate id up front so branch name and log path can embed it.
    did <- newId
    let branch  = dispatchBranchName did
        logDir  = ".icarium" </> "logs"
        logPath = logDir </> T.unpack did <> ".jsonl"
    createDirectoryIfMissing True logDir

    RD.insertDispatch conn did RD.NewDispatch
        { RD.ndTaskId     = taskId task
        , RD.ndBranch     = branch
        , RD.ndBaseBranch = base
        , RD.ndBaseSha    = baseSha
        , RD.ndModel      = model
        , RD.ndEffort     = effort
        , RD.ndLogPath    = Just logPath
        , RD.ndPid        = Nothing
        }

    void $ RT.updateTask conn (taskId task) RT.emptyUpdate
        { RT.tuState = Just InProgress }

    prompt <- buildPrompt conn task

    let retention = dcLogRetentionRuns (cfgDispatch cfg)
    mBr <- Git.createBranch branch base
    case mBr of
        Left e ->
            finishWith conn did branch OFailure Nothing
                ("git checkout -b failed: " <> T.pack (show e)) retention
                Nothing (Just baseSha)
        Right () -> do
            exit <- runClaudeStreaming did task prompt model tools logPath
            handlePostClaude conn did branch base cfg exit baseSha logPath

checkPreconditions :: Text -> IO ()
checkPreconditions base = do
    clean <- Git.isClean
    unless clean $ ioFail
        "working tree not clean; commit or stash before dispatch"
    mCur <- Git.currentBranch
    case mCur of
        Left e  -> ioFail ("git: " <> show e)
        Right b -> when (b /= base) $ ioFail
            ("not on base branch " <> T.unpack base <>
             "; currently on " <> T.unpack b)

-- =============================================================
-- Claude invocation with live event streaming
-- =============================================================

runClaudeStreaming
    :: Text -> Task -> Text -> Text -> [Text] -> FilePath
    -> IO ExitCode
runClaudeStreaming did task prompt model tools logPath = do
    parentEnv <- getEnvironment
    let promptBytes = BL.fromStrict (TE.encodeUtf8 prompt)
        args =
            [ "-p"
            , "--model", T.unpack model
            , "--output-format", "stream-json"
            , "--verbose"
            , "--allowedTools", T.unpack (T.intercalate "," tools)
            ]
        -- Inherit parent env so claude can find ~/.claude credentials
        -- via $HOME; append icarium's own vars.
        env = parentEnv ++
            [ ("ICARIUM_DISPATCH_ID", T.unpack did)
            , ("ICARIUM_TASK_ID",     T.unpack (taskId task))
            ]
        pcfg = setStdin  (byteStringInput promptBytes)
             $ setStdout createPipe
             $ setEnv    env
             $ proc "claude" args

    withLogHandle logPath $ \logH ->
        withProcessWait pcfg $ \p -> do
            -- Record the child PID so recovery can detect a dead process.
            mPid <- getPid p
            case mPid of
                Just pid -> bracket (openDb defaultDbPath) close $ \c ->
                    RD.setPid c did (fromIntegral pid)
                Nothing  -> pure ()
            _ <- forkIO (teeAndHeartbeat (getStdout p) logH did)
            waitExitCode p

-- | Tail the child's stdout, copy each line to the log file, and bump
-- the heartbeat row per event. Runs in its own thread with its own DB
-- connection so we don't share sqlite-simple's Connection between
-- threads. On any failure the thread just exits; the caller is not
-- blocked waiting for it.
teeAndHeartbeat :: Handle -> Handle -> Text -> IO ()
teeAndHeartbeat src logH did = do
    r <- try (bracket (openDb defaultDbPath) close (loop src logH did))
    case r :: Either SomeException () of
        Left e  -> hPutStrLn stderr ("icarium: heartbeat thread died: " <> show e)
        Right _ -> pure ()
  where
    loop h lh d c = do
        eof <- hIsEOF h
        if eof then pure ()
        else do
            line <- BC.hGetLine h
            BC.hPutStrLn lh line
            RD.updateHeartbeat c d
            let short = T.pack (BC.unpack (BC.take 120 line))
                tag   = "[" <> T.take 8 d <> "] "
            hPutStrLn stderr (T.unpack (tag <> short))
            loop h lh d c

withLogHandle :: FilePath -> (Handle -> IO a) -> IO a
withLogHandle path act = do
    h <- openFile path WriteMode
    hSetBuffering h LineBuffering
    r <- act h
    hClose h
    pure r

-- =============================================================
-- Post-claude gates: build, test, FF-merge
-- =============================================================

handlePostClaude
    :: Connection -> Text -> Text -> Text -> Config -> ExitCode
    -> Text -> FilePath
    -> IO DispatchResult
handlePostClaude conn did branch base cfg exit baseSha logPath = case exit of
    ExitFailure c ->
        finishWith conn did branch OFailure Nothing
            ("claude exited " <> T.pack (show c)) ret
            (Just logPath) (Just baseSha)
    ExitSuccess -> do
        mSha <- Git.revParse branch
        case mSha of
            Right sha -> RD.setLastCommit conn did sha
            Left  _   -> pure ()
        let cc = cfgCommands cfg
        gated <- runGate (ccBuild cc) >>= \case
            Left n  -> pure (Left n)
            Right _ -> runGate (ccTest cc)
        case gated of
            Left notes ->
                finishWith conn did branch OFailure Nothing notes ret
                    (Just logPath) (Just baseSha)
            Right () -> do
                e1 <- Git.checkout base
                case e1 of
                    Left err -> finishWith conn did branch OFailure Nothing
                        ("checkout base: " <> T.pack (show err)) ret
                        (Just logPath) (Just baseSha)
                    Right () -> do
                        e2 <- Git.ffMerge branch
                        case e2 of
                            Left err -> finishWith conn did branch OFailure Nothing
                                ("ff-merge: " <> T.pack (show err)) ret
                                (Just logPath) (Just baseSha)
                            Right () -> do
                                -- Branch is fully reachable from base; delete it.
                                _ <- Git.deleteBranch branch
                                mShaBase <- Git.revParse base
                                let mergeSha = either (const Nothing) Just mShaBase
                                finishWith conn did branch OSuccess mergeSha "merged" ret
                                    (Just logPath) (Just baseSha)
  where
    ret = dcLogRetentionRuns (cfgDispatch cfg)

-- | Run a shell command (as a single string, so users can include
-- pipes and &&). Returns () on exit 0; otherwise a short note.
runGate :: Text -> IO (Either Text ())
runGate cmdText
    | T.null (T.strip cmdText) = pure (Right ())
    | otherwise = do
        code <- runProcess (shell (T.unpack cmdText))
        pure $ case code of
            ExitSuccess   -> Right ()
            ExitFailure c -> Left (cmdText <> " -> exit " <> T.pack (show c))

-- =============================================================
-- Plumbing
-- =============================================================

finishWith
    :: Connection -> Text -> Text -> DispatchOutcome -> Maybe Text -> Text -> Int
    -> Maybe FilePath -> Maybe Text
    -> IO DispatchResult
finishWith conn did branch outcome mSha notes retention mLogPath mBaseSha = do
    RD.finishDispatch conn did outcome mSha (Just notes)
    pruneLogFiles conn retention
    pure DispatchResult
        { dresDispatchId = Just did
        , dresOutcome    = outcome
        , dresBranch     = branch
        , dresNotes      = notes
        , dresLogPath    = mLogPath
        , dresBaseSha    = mBaseSha
        }

pruneLogFiles :: Connection -> Int -> IO ()
pruneLogFiles conn retention = do
    paths <- RD.logPathsOutsideRetention conn retention
    mapM_ deleteIfExists paths
  where
    deleteIfExists p = do
        exists <- doesFileExist p
        when exists (removeFile p)

buildPrompt :: Connection -> Task -> IO Text
buildPrompt conn t = do
    refs     <- RE.referencedKnowledge conn (taskId t)
    cats     <- RC.taskCategoriesFor   conn (taskId t)
    catMatch <- RK.categoryMatchedKnowledge conn cats 5
    deps     <- RE.dependencyTasks     conn (taskId t)
    let refIds     = map knowledgeId refs
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
    fromMaybe (pcIntegrationBranch (cfgProject (drConfig req)))
              (drBaseOverride req)

ioFail :: String -> IO a
ioFail = ioError . userError

-- | Reconcile task state with the dispatch outcome. Intended to be
-- called from the CLI layer after @dispatch@ returns.
--
-- * success and task still 'ready' -> mark 'done' (the agent
--   presumably didn't self-update; we don't want to re-pick it).
-- * failure -> mark 'blocked' with the dispatch notes as reason.
-- * interrupted -> leave to @icarium recover@.
-- * dry-run (dispatch id absent) -> no-op.
applyOutcomeToTask :: Connection -> Task -> DispatchResult -> IO ()
applyOutcomeToTask conn t res
    | Nothing <- dresDispatchId res = pure ()
    | otherwise = case dresOutcome res of
        OSuccess -> do
            mFresh <- RT.getTask conn (taskId t)
            case mFresh of
                Just t' | taskState t' `elem` [InProgress, Ready] ->
                    void $ RT.updateTask conn (taskId t') RT.emptyUpdate
                        { RT.tuState = Just Done }
                _ -> pure ()
        OFailure ->
            void $ RT.updateTask conn (taskId t) RT.emptyUpdate
                { RT.tuState       = Just Blocked
                , RT.tuBlockReason = Just (Just (dresNotes res))
                }
        OInterrupted -> pure ()
