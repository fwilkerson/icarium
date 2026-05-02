module Icarium.Dispatch.Claude (
    RunCtx (..),
    raceTimeout,
    timeoutSentinel,
    killGroupGracefully,
    runClaudeStreaming,
    withLogHandle,
    teeAndHeartbeat,
) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (void)
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Database.SQLite.Simple (close)
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
    waitExitCode,
    withProcessWait,
 )
import System.Timeout (timeout)

import Icarium.Config (DispatchConfig (..))
import Icarium.Db (openDb)
import Icarium.Dispatch.Tick (emptyTickState, summariseTick)
import Icarium.Repo.Dispatch qualified as RD
import Icarium.Types

{- | ExitCode returned when the wall-clock limit fires. @handlePostClaude@
translates this sentinel into an OFailure with a timeout note.
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
killGroupGracefully pgid = do
    void $ (try :: IO () -> IO (Either SomeException ())) (signalProcessGroup sigINT pgid)
    threadDelay (10 * 1_000_000)
    void $ (try :: IO () -> IO (Either SomeException ())) (signalProcessGroup sigKILL pgid)

data RunCtx = RunCtx
    { rcDbPath :: FilePath
    , rcDid :: Text
    , rcTask :: Task
    , rcPrompt :: Text
    , rcModel :: Text
    , rcEffort :: Effort
    , rcLogPath :: FilePath
    }

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
        scratchDir = dcScratchDir dcfg
        maxMinutes = dcMaxMinutesPerDispatch dcfg
    parentEnv <- getEnvironment
    let promptBytes = BL.fromStrict (TE.encodeUtf8 prompt)
        args =
            [ "-p"
            , "--model"
            , T.unpack model
            , "--effort"
            , T.unpack (effortText effort)
            , "--output-format"
            , "stream-json"
            , "--verbose"
            , "--tools"
            , T.unpack (T.intercalate "," tools)
            , "--disable-slash-commands"
            , "--allowedTools"
            , T.unpack (T.intercalate "," allowed)
            ]
        -- Inherit parent env so claude can find ~/.claude credentials
        -- via $HOME; append icarium's own vars.
        env =
            parentEnv
                ++ [ ("ICARIUM_DISPATCH_ID", T.unpack did)
                   , ("ICARIUM_TASK_ID", T.unpack (taskId task))
                   , ("ICARIUM_SCRATCH_DIR", T.unpack scratchDir)
                   ]
        pcfg =
            setStdin (byteStringInput promptBytes) $
                setStdout createPipe $
                    setEnv env $
                        setCreateGroup True $
                            proc "claude" args
        maxUsecs = maxMinutes * 60 * 1_000_000

    withLogHandle logPath $ \logH ->
        withProcessWait pcfg $ \p -> do
            -- Record the child PID so recovery can detect a dead process.
            mPid <- getPid p
            case mPid of
                Just pid -> bracket (openDb dbPath) close $ \c ->
                    RD.setPid c did (fromIntegral pid)
                Nothing -> pure ()
            _ <- forkIO (teeAndHeartbeat dbPath (getStdout p) logH did (taskTitle task))
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
teeAndHeartbeat :: FilePath -> Handle -> Handle -> Text -> Text -> IO ()
teeAndHeartbeat dbPath src logH did title = do
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
                    (outLines, st') = summariseTick ts line st
                mapM_ (hPutStrLn stderr) outLines
                loop h lh d st' c

withLogHandle :: FilePath -> (Handle -> IO a) -> IO a
withLogHandle path act =
    withFile path WriteMode $ \h -> do
        hSetBuffering h LineBuffering
        act h
