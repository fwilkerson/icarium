module Icarium.Dispatch
    ( DispatchRequest(..)
    , DispatchResult(..)
    , dispatch
    , dispatchBranchName
    , applyOutcomeToTask
    , postClaudeGuard
    ) where

import           Control.Concurrent     (forkIO)
import           Control.Exception      (SomeException, bracket, try)
import           Control.Monad          (unless, void, when)
import           Data.Aeson             (Object, Result (..), Value (..), decodeStrict, fromJSON)
import qualified Data.Aeson.Key         as AK
import qualified Data.Aeson.KeyMap      as AKM
import qualified Data.ByteString.Char8  as BC
import qualified Data.ByteString.Lazy   as BL
import           Data.Maybe             (fromMaybe, mapMaybe, maybeToList)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import qualified Data.Text.IO           as TIO
import           Data.Time              (defaultTimeLocale, formatTime, getCurrentTime)
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
    TIO.putStrLn (renderCmdPreview model tools allowed)
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

renderCmdPreview :: Text -> [Text] -> [Text] -> Text
renderCmdPreview model tools allowed = T.unwords
    [ "claude -p"
    , "--model", model
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
    let cfg        = drConfig req
        dcfg       = cfgDispatch cfg
        task       = drTask   req
        base       = effectiveBase   req
        model      = effectiveModel  req
        effort     = effectiveEffort req
        tools      = dcTools         dcfg
        allowed    = dcAllowedTools  dcfg
        scratchDir = dcScratchDir    dcfg

    checkPreconditions base
    baseSha <- either (ioFail . show) pure =<< Git.revParse base

    -- Generate id up front so branch name and log path can embed it.
    did <- newId
    let branch  = dispatchBranchName did
        logDir  = ".icarium" </> "logs"
        logPath = logDir </> T.unpack did <> ".jsonl"
    createDirectoryIfMissing True logDir
    createDirectoryIfMissing True (T.unpack scratchDir)

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
            exit <- runClaudeStreaming did task prompt model tools allowed scratchDir logPath
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
    :: Text -> Task -> Text -> Text -> [Text] -> [Text] -> Text -> FilePath
    -> IO ExitCode
runClaudeStreaming did task prompt model tools allowed scratchDir logPath = do
    parentEnv <- getEnvironment
    let promptBytes = BL.fromStrict (TE.encodeUtf8 prompt)
        args =
            [ "-p"
            , "--model", T.unpack model
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
            _ <- forkIO (teeAndHeartbeat (getStdout p) logH did (taskTitle task))
            waitExitCode p

-- | Tail the child's stdout, copy each line to the log file, and bump
-- the heartbeat row per event. Runs in its own thread with its own DB
-- connection so we don't share sqlite-simple's Connection between
-- threads. On any failure the thread just exits; the caller is not
-- blocked waiting for it.
teeAndHeartbeat :: Handle -> Handle -> Text -> Text -> IO ()
teeAndHeartbeat src logH did title = do
    hPutStrLn stderr $ "[" ++ T.unpack (T.take 8 did) ++ "] " ++ T.unpack (T.take 60 title)
    r <- try (bracket (openDb defaultDbPath) close (loop src logH did emptyTickState))
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

-- =============================================================
-- Tick parsing: structured per-event stderr summary
-- =============================================================

data TickState = TickState
    { tsEventCount :: !Int
    , tsLastIn     :: !Int
    , tsLastOut    :: !Int
    , tsLastCache  :: !Int
    }

emptyTickState :: TickState
emptyTickState = TickState 0 0 0 0

-- | Parse one JSONL line and return lines to emit on stderr.
-- Increments the event counter and prints a usage summary every 20 events.
summariseTick :: String -> BC.ByteString -> TickState -> ([String], TickState)
summariseTick ts bytes st0 =
    let st = st0 { tsEventCount = tsEventCount st0 + 1 }
    in case decodeStrict bytes :: Maybe Value of
        Nothing  -> ([row '?' "unknown" (BC.unpack (BC.take 120 bytes))], st)
        Just val -> case val of
            Object obj -> parseEvent st obj
            _          -> ([row '?' "unknown" (BC.unpack (BC.take 120 bytes))], st)
  where
    pad k      = k ++ replicate (max 0 (14 - length k)) ' '
    row sym kw body = ts ++ "  " ++ [sym] ++ " " ++ pad kw ++ body

    parseEvent st obj = case lookStr "type" obj of
        Just "system"           -> handleSystem st obj
        Just "assistant"        -> handleAssistant st obj
        Just "user"             -> handleUser st obj
        Just "result"           -> handleResult st obj
        Just "rate_limit_event" -> ([], st)
        _                       -> ([row '?' "unknown" (BC.unpack (BC.take 120 bytes))], st)

    handleSystem st obj =
        let model  = maybe "?" T.unpack (lookStr "model"      obj)
            sessId = maybe "?" (take 8 . T.unpack) (lookStr "session_id" obj)
        in ([row '.' "system" ("model=" ++ model ++ " session=" ++ sessId)], st)

    handleAssistant st obj =
        let msg       = lookObj "message" obj
            usageObj  = msg >>= lookObj "usage"
            inToks    = usageObj >>= lookInt "input_tokens"
            outToks   = usageObj >>= lookInt "output_tokens"
            cacheToks = usageObj >>= lookInt "cache_read_input_tokens"
            st1 = st { tsLastIn    = fromMaybe (tsLastIn    st) inToks
                     , tsLastOut   = fromMaybe (tsLastOut   st) outToks
                     , tsLastCache = fromMaybe (tsLastCache st) cacheToks
                     }
            contents  = msg >>= lookArr "content"
            eventLine = do
                xs <- contents
                c  <- case xs of { (x:_) -> Just x; [] -> Nothing }
                parseContent c
            (usageLines, st2) = checkUsagePeriodic st1
        in (maybeToList eventLine ++ usageLines, st2)

    parseContent (Object c) = case lookStr "type" c of
        Just "thinking" ->
            let txt = maybe "" (take 80 . T.unpack) (lookStr "thinking" c)
            in Just (row '>' "thinking" txt)
        Just "text" ->
            let txt = maybe "" (take 80 . T.unpack) (lookStr "text" c)
            in Just (row '>' "assistant" txt)
        Just "tool_use" ->
            let name    = maybe "?" T.unpack (lookStr "name" c)
                inputV  = lookObj "input" c
                summary = summariseToolInput name inputV
            in Just (row '*' "tool" (name ++ ": " ++ summary))
        _ -> Nothing
    parseContent _ = Nothing

    handleUser st obj =
        let msg      = lookObj "message" obj
            contents = fromMaybe [] (msg >>= lookArr "content")
        in (mapMaybe toolResultError contents, st)

    -- | A tool_result content block whose @is_error@ is true. Returns a
    -- one-line summary; non-errors and non-tool_result blocks return Nothing.
    toolResultError (Object o)
        | Just (String "tool_result") <- lookRaw "type"     o
        , Just (Bool True)            <- lookRaw "is_error" o
        = Just (row 'x' "tool_result" (errBody o))
      where
        errBody c = case lookRaw "content" c of
            Just (String t) -> take 80 (T.unpack t)
            _               -> "error"
    toolResultError _ = Nothing

    handleResult st obj =
        let subtype   = maybe "?" T.unpack (lookStr "subtype" obj)
            result    = maybe "" (take 60 . T.unpack) (lookStr "result" obj)
            usageObj  = lookObj "usage" obj
            inToks    = usageObj >>= lookInt "input_tokens"
            outToks   = usageObj >>= lookInt "output_tokens"
            cacheToks = usageObj >>= lookInt "cache_read_input_tokens"
            resultLine = row '+' "result" (subtype ++ ": " ++ result)
            usageLine  = case (inToks, outToks, cacheToks) of
                (Just i, Just o, Just c) ->
                    [row '=' "usage" ("in " ++ show i ++ " / out " ++ show o
                                    ++ " / cache_read " ++ show c)]
                _ -> []
        in (resultLine : usageLine, st)

    checkUsagePeriodic st
        | tsEventCount st >= 20 =
            let line = row '=' "usage" ("in " ++ show (tsLastIn st)
                                      ++ " / out " ++ show (tsLastOut st)
                                      ++ " / cache_read " ++ show (tsLastCache st))
            in ([line], st { tsEventCount = 0 })
        | otherwise = ([], st)

-- | Brief summary of a tool's input arguments for display.
summariseToolInput :: String -> Maybe Object -> String
summariseToolInput "Bash"  (Just o) = take 80 $ maybe "?" T.unpack (lookStr "command"     o)
summariseToolInput "Read"  (Just o) = maybe "?" T.unpack (lookStr "file_path"   o)
summariseToolInput "Edit"  (Just o) = maybe "?" T.unpack (lookStr "file_path"   o)
summariseToolInput "Write" (Just o) = maybe "?" T.unpack (lookStr "file_path"   o)
summariseToolInput "Glob"  (Just o) = maybe "?" T.unpack (lookStr "pattern"     o)
summariseToolInput "Grep"  (Just o) = maybe "?" T.unpack (lookStr "pattern"     o)
summariseToolInput "Agent" (Just o) = maybe "?" T.unpack (lookStr "description" o)
summariseToolInput _       _        = "..."

-- Aeson helpers

lookRaw :: Text -> Object -> Maybe Value
lookRaw k = AKM.lookup (AK.fromText k)

lookStr :: Text -> Object -> Maybe Text
lookStr k obj = case lookRaw k obj of
    Just (String t) -> Just t
    _               -> Nothing

lookObj :: Text -> Object -> Maybe Object
lookObj k obj = case lookRaw k obj of
    Just (Object o) -> Just o
    _               -> Nothing

lookArr :: Text -> Object -> Maybe [Value]
lookArr k obj = case lookRaw k obj of
    Just v  -> case fromJSON v :: Result [Value] of
                   Success xs -> Just xs
                   Error _    -> Nothing
    Nothing -> Nothing

lookInt :: Text -> Object -> Maybe Int
lookInt k obj = case lookRaw k obj of
    Just v  -> case fromJSON v :: Result Int of
                   Success n -> Just n
                   Error _   -> Nothing
    Nothing -> Nothing

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
        finishWith conn did branch base OFailure Nothing
            ("claude exited " <> T.pack (show c)) ret
            (Just logPath) (Just baseSha)
    ExitSuccess -> do
        porcelain  <- Git.statusPorcelain
        mBranchSha <- Git.revParse branch
        case postClaudeGuard porcelain mBranchSha baseSha of
            Just msg ->
                finishWith conn did branch base OFailure Nothing msg ret
                    (Just logPath) (Just baseSha)
            Nothing -> do
                case mBranchSha of
                    Right sha -> RD.setLastCommit conn did sha
                    Left  _   -> pure ()
                let cc = cfgCommands cfg
                gated <- runGate (ccBuild cc) >>= \case
                    Left n  -> pure (Left n)
                    Right _ -> runGate (ccTest cc)
                case gated of
                    Left notes ->
                        finishWith conn did branch base OFailure Nothing notes ret
                            (Just logPath) (Just baseSha)
                    Right () -> do
                        e1 <- Git.checkout base
                        case e1 of
                            Left err -> finishWith conn did branch base OFailure Nothing
                                ("checkout base: " <> T.pack (show err)) ret
                                (Just logPath) (Just baseSha)
                            Right () -> do
                                e2 <- Git.ffMerge branch
                                case e2 of
                                    Left err -> finishWith conn did branch base OFailure Nothing
                                        ("ff-merge: " <> T.pack (show err)) ret
                                        (Just logPath) (Just baseSha)
                                    Right () -> do
                                        _ <- Git.deleteBranch branch
                                        mShaBase <- Git.revParse base
                                        let mergeSha = either (const Nothing) Just mShaBase
                                        finishWith conn did branch base OSuccess mergeSha "merged" ret
                                            (Just logPath) (Just baseSha)
  where
    ret = dcLogRetentionRuns (cfgDispatch cfg)

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
