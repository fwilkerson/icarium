module Icarium.Dispatch.Claude (
    RunCtx (..),
    claudeArgs,
    raceTimeout,
    timeoutSentinel,
    killGroupGracefully,
    killGroupAfter,
    runClaudeStreaming,
    withLogHandle,
) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, bracket, handle, try)
import Control.Monad (void, when)
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Database.SQLite.Simple (close)
import System.Directory (makeAbsolute)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.IO (
    BufferMode (..),
    Handle,
    IOMode (..),
    hIsEOF,
    hPutStrLn,
    hSetBuffering,
    stderr,
    withFile,
 )
import System.Posix.Signals (sigINT, sigKILL, signalProcessGroup)
import System.Posix.Types (CPid (..))
import System.Process.Typed (
    byteStringInput,
    createPipe,
    getPid,
    getStdout,
    proc,
    setCreateGroup,
    setEnv,
    setStdin,
    setStdout,
    setWorkingDir,
    waitExitCode,
    withProcessWait,
 )
import System.Timeout (timeout)

import Icarium.Config (DispatchConfig (..))
import Icarium.Db (openDb)
import Icarium.Dispatch.Payload (jsonSchemaArgs, workerSchema)
import Icarium.Dispatch.Tick (TickAction (..), TickState (..), emptyTickState, summariseTick)
import Icarium.Repo.Dispatch qualified as RD
import Icarium.Types

{- | ExitCode returned when the wall-clock limit fires.
@handlePostClaudeWithReview@ translates this sentinel into an OFailure with a
timeout note.
-}
timeoutSentinel :: ExitCode
timeoutSentinel = ExitFailure 124

{- | Race an IO action against a deadline (microseconds).
Returns 'Left ()' if the deadline fires first, 'Right a' otherwise.
-}
raceTimeout :: Int -> IO a -> IO (Either () a)
raceTimeout usecs act = maybe (Left ()) Right <$> timeout usecs act

{- | Send SIGINT to a process group, wait 10 s for a clean exit, then
SIGKILL. Errors from signalProcessGroup (e.g. ESRCH if already dead)
are silently ignored.
-}
killGroupGracefully :: CPid -> IO ()
killGroupGracefully = killGroupAfter (threadDelay killGrace)

{- | 'killGroupGracefully' for a caller that holds the child and can reap it:
the SIGKILL sweep follows as soon as @reap@ returns instead of after the
full grace period. Reaping between the signals is also what makes the sweep
meaningful — an unreaped zombie leader keeps the group alive on paper.
-}
killGroupAfter :: IO () -> CPid -> IO ()
killGroupAfter reap pgid = do
    signalQuietly sigINT
    _ <- raceTimeout killGrace reap
    signalQuietly sigKILL
  where
    signalQuietly s =
        handle (\(_ :: SomeException) -> pure ()) (signalProcessGroup s pgid)

-- | How long a process group gets to honour SIGINT before SIGKILL.
killGrace :: Int
killGrace = 10 * 1_000_000

data RunCtx = RunCtx
    { rcDbPath :: FilePath
    , rcDid :: Text
    , rcTask :: Task
    , rcPrompt :: Text
    , rcModel :: Text
    , rcEffort :: Effort
    , rcLogPath :: FilePath
    , rcWorkDir :: FilePath
    -- ^ The dispatch worktree the worker runs in.
    }

{- | Full claude(1) worker argument list. Shared with the dry-run preview
so the two can't drift out of sync with each other.

@--strict-mcp-config@ with no @--mcp-config@ means zero MCP servers; when
'Just' a path is given, @--mcp-config@ grants exactly that file.

@--disable-slash-commands@ is derived from @tools@: listing @Skill@ opts the
worker into slash commands and skills (ADR 0003).

@--json-schema@ makes the final message a validated 'workerSchema' payload
rather than prose; the gate ingests it in 'Icarium.Dispatch.Outcome.applyOutcomeToTask'.
-}
claudeArgs :: Text -> Effort -> [Text] -> [Text] -> Maybe Text -> [Text]
claudeArgs model effort tools allowed mcpConfig =
    [ "-p"
    , "--model"
    , model
    , "--effort"
    , effortText effort
    , "--output-format"
    , "stream-json"
    , "--verbose"
    , "--tools"
    , T.intercalate "," tools
    ]
        ++ ["--disable-slash-commands" | "Skill" `notElem` tools]
        ++ [ "--allowedTools"
           , T.intercalate "," allowed
           , "--permission-mode"
           , "dontAsk"
           , "--strict-mcp-config"
           ]
        ++ maybe [] (\p -> ["--mcp-config", p]) mcpConfig
        ++ jsonSchemaArgs workerSchema

runClaudeStreaming :: RunCtx -> DispatchConfig -> IO ExitCode
runClaudeStreaming ctx dcfg = do
    let dbPath = rcDbPath ctx
        did = rcDid ctx
        task = rcTask ctx
        prompt = rcPrompt ctx
        model = rcModel ctx
        effort = rcEffort ctx
        logPath = rcLogPath ctx
        tools = dcTools dcfg
        allowed = dcAllowedTools dcfg
        maxMinutes = dcMaxMinutesPerDispatch dcfg
        mcpConfig = dcMcpConfig dcfg
    parentEnv <- getEnvironment
    -- Absolute: the worker's cwd is the worktree and child `icarium`
    -- invocations resolve ICARIUM_DB against it — a relative db path
    -- would silently create a nested store inside the worktree.
    absDb <- makeAbsolute dbPath
    let promptBytes = BL.fromStrict (TE.encodeUtf8 prompt)
        args = map T.unpack (claudeArgs model effort tools allowed mcpConfig)
        -- Inherit parent env so claude can find ~/.claude credentials
        -- via $HOME; append icarium's own vars.
        env =
            parentEnv
                ++ [ ("ICARIUM_DISPATCH_ID", T.unpack did)
                   , ("ICARIUM_TASK_ID", T.unpack (taskId task))
                   , ("ICARIUM_DB", absDb)
                   ]
        pcfg =
            setStdin (byteStringInput promptBytes) $
                setStdout createPipe $
                    setEnv env $
                        setCreateGroup True $
                            setWorkingDir (rcWorkDir ctx) $
                                proc "claude" args
        maxUsecs = maxMinutes * 60 * 1_000_000
        retryThreshold = dcRetryStormThreshold dcfg

    withLogHandle logPath $ \logH ->
        withProcessWait pcfg $ \p -> do
            -- Record the child PID so recovery can detect a dead process.
            mPid <- getPid p
            mapM_ (\pid -> bracket (openDb dbPath) close $ \c -> RD.setPid c did (fromIntegral pid)) mPid
            _ <- forkIO (teeAndHeartbeat retryThreshold dbPath (getStdout p) logH did (taskTitle task))
            result <- raceTimeout maxUsecs (waitExitCode p)
            case result of
                Right exit -> pure exit
                Left () -> do
                    mapM_ (killGroupGracefully . CPid . fromIntegral) mPid
                    pure timeoutSentinel

{- | Tail the child's stdout, copy each line to the log file, and bump
the heartbeat row per event. Runs in its own thread with its own DB
connection so we don't share sqlite-simple's Connection between
threads. On any failure the thread just exits; the caller is not
blocked waiting for it.
-}
teeAndHeartbeat :: Int -> FilePath -> Handle -> Handle -> Text -> Text -> IO ()
teeAndHeartbeat retryThreshold dbPath src logH did title = do
    hPutStrLn stderr $ "[" ++ T.unpack (T.take 8 did) ++ "] " ++ T.unpack (T.take 60 title)
    r <- try (bracket (openDb dbPath) close (loop src logH did emptyTickState))
    case r :: Either SomeException () of
        Left e -> hPutStrLn stderr ("icarium: heartbeat thread died: " <> show e)
        Right _ -> pure ()
  where
    loop h lh d st c = do
        eof <- hIsEOF h
        if eof
            then pure ()
            else do
                line <- BC.hGetLine h
                BC.hPutStrLn lh line
                RD.updateHeartbeat c d
                now <- getCurrentTime
                let ts = formatTime defaultTimeLocale "%H:%M:%S" now
                    (outLines, st', action) = summariseTick retryThreshold ts line st
                mapM_ (hPutStrLn stderr) outLines
                let tokensChanged =
                        tsTokIn st' /= tsTokIn st
                            || tsTokOut st' /= tsTokOut st
                            || tsTokCache st' /= tsTokCache st
                when tokensChanged $
                    void $
                        (try :: IO () -> IO (Either SomeException ())) $
                            RD.updateTokens c d (tsTokIn st') (tsTokOut st') (tsTokCache st')
                case action of
                    TickContinue -> loop h lh d st' c
                    TickKill reason -> do
                        hPutStrLn stderr ("icarium: watchdog: " ++ T.unpack reason)
                        void $
                            (try :: IO () -> IO (Either SomeException ())) $
                                RD.updateNotes c d reason
                        mDispatch <- RD.getDispatch c d
                        case mDispatch >>= dispatchPid of
                            Just pid ->
                                void $ forkIO $ killGroupGracefully (CPid (fromIntegral pid))
                            Nothing -> pure ()
                        drainLoop h lh

    drainLoop h lh = do
        eof <- hIsEOF h
        if eof
            then pure ()
            else do
                line <- BC.hGetLine h
                BC.hPutStrLn lh line
                drainLoop h lh

withLogHandle :: FilePath -> (Handle -> IO a) -> IO a
withLogHandle path act =
    withFile path WriteMode $ \h -> do
        hSetBuffering h LineBuffering
        act h
