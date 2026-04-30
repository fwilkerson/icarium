module Icarium.Dispatch.Internal
    ( DispatchRequest(..)
    , DispatchResult(..)
    , dispatch
    , dispatchBranchName
    , applyOutcomeToTask
    , postClaudeGuard
    , raceTimeout
    , timeoutSentinel
    , handlePostClaude
    ) where

import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Exception          (SomeException, bracket, try)
import           Control.Monad              (unless, void, when)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except (runExceptT, throwE)
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy       as BL
import           Data.Maybe                 (fromMaybe)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.Text.IO               as TIO
import           Data.Time                  (defaultTimeLocale, formatTime, getCurrentTime)
import           Database.SQLite.Simple     (Connection, close)
import           System.Directory           (createDirectoryIfMissing, doesFileExist, removeFile)
import           System.Environment         (getEnvironment)
import           System.Exit                (ExitCode (..))
import           System.FilePath            ((</>))
import           System.IO                  (BufferMode (..), Handle, IOMode (..), hIsEOF,
                                             hPutStrLn, hSetBuffering, stderr, withFile)
import           System.Posix.Signals       (sigINT, sigKILL, signalProcessGroup)
import           System.Posix.Types         (CPid (..))
import           System.Process.Typed       (byteStringInput, createPipe, getPid, getStdout, proc,
                                             runProcess, setCreateGroup, setEnv, setStdin,
                                             setStdout, shell, waitExitCode, withProcessWait)
import           System.Timeout             (timeout)

import           Icarium.Config             (CommandsConfig (..), Config (..), DispatchConfig (..),
                                             ProjectConfig (..))
import           Icarium.Db                 (openDb)
import           Icarium.Dispatch.Tick      (emptyTickState, summariseTick)
import qualified Icarium.Git                as Git
import           Icarium.Id                 (newId)
import           Icarium.Render             (renderTaskPrompt)
import qualified Icarium.Repo.Category      as RC
import qualified Icarium.Repo.Dispatch      as RD
import qualified Icarium.Repo.Edge          as RE
import qualified Icarium.Repo.Knowledge     as RK
import qualified Icarium.Repo.Task          as RT
import           Icarium.Types

-- =============================================================
-- Request / result
-- =============================================================

data DispatchRequest = DispatchRequest
    { drTask           :: Task
    , drConfig         :: Config
    , drDbPath         :: FilePath
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
    let dcfg       = cfgDispatch (drConfig req)
        branch     = dispatchBranchName fakeId
        model      = effectiveModel  req
        effort     = effectiveEffort req
        base       = effectiveBase   req
        tools      = dcTools         dcfg
        allowed    = dcAllowedTools  dcfg
        scratchDir = dcScratchDir    dcfg

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

    pure DispatchResult
        { dresDispatchId = Nothing
        , dresOutcome    = OSuccess
        , dresBranch     = branch
        , dresNotes      = "dry-run"
        , dresLogPath    = Nothing
        , dresBaseSha    = Nothing
        }

renderCmdPreview :: Text -> Effort -> [Text] -> [Text] -> Text
renderCmdPreview model effort tools allowed = T.unwords
    [ "claude -p"
    , "--model", model
    , "--effort", effortText effort
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
    let cfg    = drConfig req
        dcfg   = cfgDispatch cfg
        task   = drTask   req
        dbPath = drDbPath req
        base   = effectiveBase   req
        model  = effectiveModel  req
        effort = effectiveEffort req

    checkPreconditions base
    baseSha <- either (ioFail . show) pure =<< Git.revParse base

    -- Generate id up front so branch name and log path can embed it.
    did <- newId
    let branch  = dispatchBranchName did
        logDir  = ".icarium" </> "logs"
        logPath = logDir </> T.unpack did <> ".jsonl"
    createDirectoryIfMissing True logDir
    createDirectoryIfMissing True (T.unpack (dcScratchDir dcfg))

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
            finishWith conn did branch base OFailure Nothing
                ("git checkout -b failed: " <> T.pack (show e)) retention
                Nothing (Just baseSha)
        Right () -> do
            let ctx = RunCtx
                    { rcDbPath  = dbPath
                    , rcDid     = did
                    , rcTask    = task
                    , rcPrompt  = prompt
                    , rcModel   = model
                    , rcEffort  = effort
                    , rcLogPath = logPath
                    }
            exit <- runClaudeStreaming ctx dcfg
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

-- | ExitCode returned when the wall-clock limit fires. @handlePostClaude@
-- translates this sentinel into an OFailure with a timeout note.
timeoutSentinel :: ExitCode
timeoutSentinel = ExitFailure 124

-- | Race an IO action against a deadline (microseconds).
-- Returns 'Left ()' if the deadline fires first, 'Right a' otherwise.
raceTimeout :: Int -> IO a -> IO (Either () a)
raceTimeout usecs act = maybe (Left ()) Right <$> timeout usecs act

-- | Send SIGINT to a process group, wait 10 s for a clean exit, then
-- SIGKILL. Errors from signalProcessGroup (e.g. ESRCH if already dead)
-- are silently ignored.
killGroupGracefully :: CPid -> IO ()
killGroupGracefully pgid = do
    void $ (try :: IO () -> IO (Either SomeException ())) (signalProcessGroup sigINT pgid)
    threadDelay (10 * 1_000_000)
    void $ (try :: IO () -> IO (Either SomeException ())) (signalProcessGroup sigKILL pgid)

data RunCtx = RunCtx
    { rcDbPath  :: FilePath
    , rcDid     :: Text
    , rcTask    :: Task
    , rcPrompt  :: Text
    , rcModel   :: Text
    , rcEffort  :: Effort
    , rcLogPath :: FilePath
    }

runClaudeStreaming :: RunCtx -> DispatchConfig -> IO ExitCode
runClaudeStreaming ctx dcfg = do
    let dbPath     = rcDbPath  ctx
        did        = rcDid     ctx
        task       = rcTask    ctx
        prompt     = rcPrompt  ctx
        model      = rcModel   ctx
        effort     = rcEffort  ctx
        logPath    = rcLogPath ctx
        tools      = dcTools                dcfg
        allowed    = dcAllowedTools         dcfg
        scratchDir = dcScratchDir           dcfg
        maxMinutes = dcMaxMinutesPerDispatch dcfg
    parentEnv <- getEnvironment
    let promptBytes = BL.fromStrict (TE.encodeUtf8 prompt)
        args =
            [ "-p"
            , "--model", T.unpack model
            , "--effort", T.unpack (effortText effort)
            , "--output-format", "stream-json"
            , "--verbose"
            , "--tools", T.unpack (T.intercalate "," tools)
            , "--disable-slash-commands"
            , "--allowedTools", T.unpack (T.intercalate "," allowed)
            ]
        -- Inherit parent env so claude can find ~/.claude credentials
        -- via $HOME; append icarium's own vars.
        env = parentEnv ++
            [ ("ICARIUM_DISPATCH_ID",  T.unpack did)
            , ("ICARIUM_TASK_ID",      T.unpack (taskId task))
            , ("ICARIUM_SCRATCH_DIR",  T.unpack scratchDir)
            ]
        pcfg = setStdin       (byteStringInput promptBytes)
             $ setStdout      createPipe
             $ setEnv         env
             $ setCreateGroup True
             $ proc "claude" args
        maxUsecs = maxMinutes * 60 * 1_000_000

    withLogHandle logPath $ \logH ->
        withProcessWait pcfg $ \p -> do
            -- Record the child PID so recovery can detect a dead process.
            mPid <- getPid p
            case mPid of
                Just pid -> bracket (openDb dbPath) close $ \c ->
                    RD.setPid c did (fromIntegral pid)
                Nothing  -> pure ()
            _ <- forkIO (teeAndHeartbeat dbPath (getStdout p) logH did (taskTitle task))
            result <- raceTimeout maxUsecs (waitExitCode p)
            case result of
                Right exit -> pure exit
                Left () -> do
                    mapM_ (killGroupGracefully . CPid . fromIntegral) mPid
                    pure timeoutSentinel

-- | Tail the child's stdout, copy each line to the log file, and bump
-- the heartbeat row per event. Runs in its own thread with its own DB
-- connection so we don't share sqlite-simple's Connection between
-- threads. On any failure the thread just exits; the caller is not
-- blocked waiting for it.
teeAndHeartbeat :: FilePath -> Handle -> Handle -> Text -> Text -> IO ()
teeAndHeartbeat dbPath src logH did title = do
    hPutStrLn stderr $ "[" ++ T.unpack (T.take 8 did) ++ "] " ++ T.unpack (T.take 60 title)
    r <- try (bracket (openDb dbPath) close (loop src logH did emptyTickState))
    case r :: Either SomeException () of
        Left e  -> hPutStrLn stderr ("icarium: heartbeat thread died: " <> show e)
        Right _ -> pure ()
  where
    loop h lh d st c = do
        eof <- hIsEOF h
        if eof then pure ()
        else do
            line <- BC.hGetLine h
            BC.hPutStrLn lh line
            RD.updateHeartbeat c d
            now <- getCurrentTime
            let ts = formatTime defaultTimeLocale "%H:%M:%S" now
                (outLines, st') = summariseTick ts line st
            mapM_ (hPutStrLn stderr) outLines
            loop h lh d st' c

withLogHandle :: FilePath -> (Handle -> IO a) -> IO a
withLogHandle path act =
    withFile path WriteMode $ \h -> do
        hSetBuffering h LineBuffering
        act h

-- =============================================================
-- Post-claude gates: build, test, FF-merge
-- =============================================================

handlePostClaude
    :: Connection -> Text -> Text -> Text -> Config -> ExitCode
    -> Text -> FilePath
    -> IO DispatchResult
handlePostClaude conn did branch base cfg exit baseSha logPath = do
    let ret     = dcLogRetentionRuns        (cfgDispatch cfg)
        maxMins = dcMaxMinutesPerDispatch   (cfgDispatch cfg)
        cc      = cfgCommands cfg
        finish o mSha notes =
            finishWith conn did branch base o mSha notes ret (Just logPath) (Just baseSha)
        step = do
            case exit of
                ExitFailure 124 -> throwE ("timed out after " <> T.pack (show maxMins) <> " minutes")
                ExitFailure c   -> throwE ("claude exited " <> T.pack (show c))
                ExitSuccess     -> pure ()
            porcelain  <- liftIO Git.statusPorcelain
            mBranchSha <- liftIO (Git.revParse branch)
            mapM_ throwE (postClaudeGuard porcelain mBranchSha baseSha)
            liftIO $ case mBranchSha of
                Right sha -> RD.setLastCommit conn did sha
                Left  _   -> pure ()
            liftIO (runGate (ccBuild cc)) >>= either throwE pure
            liftIO (runGate (ccTest  cc)) >>= either throwE pure
            liftIO (Git.checkout base) >>= \case
                Left err -> throwE ("checkout base: " <> T.pack (show err))
                Right () -> pure ()
            liftIO (Git.ffMerge branch) >>= \case
                Left err -> throwE ("ff-merge: " <> T.pack (show err))
                Right () -> pure ()
            liftIO (void (Git.deleteBranch branch))
            either (const Nothing) Just <$> liftIO (Git.revParse base)
    runExceptT step >>= \case
        Left notes -> finish OFailure Nothing notes
        Right mSha -> finish OSuccess mSha "merged"

-- | Pure guard logic for the post-claude checks. Returns Just an error
-- message if a guard fires, Nothing if both pass.
--   * Dirty-tree guard fires when @porcelain@ (raw `git status --porcelain`
--     output) is non-empty after stripping. The porcelain content is
--     embedded in the message so the operator can see *what* was left
--     behind without digging through the log.
--   * Empty-diff guard fires when the dispatch branch SHA equals baseSha
--     (agent exited success but made no commits).
postClaudeGuard :: Text -> Either e Text -> Text -> Maybe Text
postClaudeGuard porcelain mBranchSha baseSha
    | not (T.null porcStripped) = Just dirtyMsg
    | branchSha == Just baseSha = Just "agent made no commits on dispatch branch"
    | otherwise                 = Nothing
  where
    porcStripped = T.strip porcelain
    branchSha    = either (const Nothing) Just mBranchSha
    dirtyMsg     = "agent left uncommitted changes; refusing to merge\nuncommitted:\n"
                <> T.intercalate "\n" (map ("  " <>) (T.lines porcStripped))

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
    :: Connection -> Text -> Text -> Text -> DispatchOutcome -> Maybe Text -> Text -> Int
    -> Maybe FilePath -> Maybe Text
    -> IO DispatchResult
finishWith conn did branch base outcome mSha notes retention mLogPath mBaseSha = do
    -- Best-effort: on failure, return to the base branch so the next
    -- dispatch (e.g. drain mode) doesn't fail its on-base-branch
    -- precondition. Ignore errors here so we don't mask the original
    -- failure note.
    when (outcome == OFailure) $ void (Git.checkout base)
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
-- * interrupted -> leave to @icarium dispatch recover@.
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
