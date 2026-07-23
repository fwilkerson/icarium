{-# LANGUAGE ScopedTypeVariables #-}

{- | The build and test gates: which commands run, in what order, and under
what wall-clock budget. Owned here rather than by a caller so the
post-claude check and the merge rebase path cannot drift apart.

A gate is a shell command that can spawn an arbitrary tree (@cabal@ ->
@ghc@ -> a test binary), so it is run the same way the worker is: stdin
closed, its own process group, a deadline, and a kill that reaps the whole
group. Anything less and a single wedged child holds the dispatcher open
forever.
-}
module Icarium.Dispatch.Gate (
    GateEnv (..),
    GateHeartbeat (..),
    gateBudgetUsecs,
    runGates,
    runGate,
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, bracket, finally, handle, try)
import Control.Monad (forM_, void)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Data.Aeson (object, (.=))
import Data.Aeson qualified as A
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Database.SQLite.Simple (close)
import System.Exit (ExitCode (..))
import System.IO (Handle, IOMode (..), hFlush, hPutStrLn, stderr, stdout, withFile)
import System.Posix.Types (CPid (..))
import System.Process.Typed (
    ProcessConfig,
    createPipe,
    getPid,
    getStderr,
    getStdout,
    nullStream,
    proc,
    readProcessStdout,
    setCreateGroup,
    setStderr,
    setStdin,
    setStdout,
    setWorkingDir,
    shell,
    waitExitCode,
    withProcessWait,
 )
import Text.Printf (printf)

import Icarium.Config (CommandsConfig (..), DispatchConfig (..))
import Icarium.Db (openDb)
import Icarium.Dispatch.Claude (killGroupAfter, raceTimeout)
import Icarium.Repo.Dispatch qualified as RD
import Icarium.Text (tshow)

-- | Where a gate runs, how long it may take, and what it reports to.
data GateEnv = GateEnv
    { geDir :: FilePath
    , geBudgetUsecs :: Int
    , geLogPath :: Maybe FilePath
    -- ^ Dispatch log expiry forensics are appended to.
    , geHeartbeat :: Maybe GateHeartbeat
    -- ^ 'Nothing' past the merge rebase gates: that row ended when it parked.
    }

-- | The still-running dispatch row a gate keeps warm.
data GateHeartbeat = GateHeartbeat
    { ghDbPath :: FilePath
    , ghDid :: Text
    }

{- | The per-gate budget from @dispatch.max_minutes_per_gate@: a full build
plus suite is not the same shape of work as one worker turn, so it is not
@max_minutes_per_dispatch@.
-}
gateBudgetUsecs :: DispatchConfig -> Int
gateBudgetUsecs dcfg = dcMaxMinutesPerGate dcfg * 60 * 1_000_000

{- | Run the configured build and test gates in order, stopping at the
first failure.
-}
runGates :: GateEnv -> Maybe CommandsConfig -> IO (Either Text ())
runGates env mcc = runExceptT $ do
    cc <- maybe (throwE "no [commands] section configured") pure mcc
    ExceptT (runGate env (ccBuild cc))
    ExceptT (runGate env (ccTest cc))

{- | Run one shell command (a single string, so users can include pipes and
@&&@) under the environment's budget. Returns () on exit 0; otherwise a
short note naming the command and how it failed.
-}
runGate :: GateEnv -> Text -> IO (Either Text ())
runGate env cmdText
    | T.null (T.strip cmdText) = pure (Right ())
    | otherwise = do
        let pcfg =
                -- stdin closed, not inherited: a child that reads the
                -- dispatcher's terminal would block there forever, and the
                -- dispatcher is not there to answer it.
                setStdin nullStream $
                    setStdout createPipe $
                        setStderr createPipe $
                            setCreateGroup True $
                                setWorkingDir (geDir env) (shell (T.unpack cmdText))
        withProcessWait pcfg $ \p -> do
            mPid <- getPid p
            joinOut <- forkJoin (teeAndBeat (geHeartbeat env) (getStdout p) stdout)
            joinErr <- forkJoin (teeAndBeat (geHeartbeat env) (getStderr p) stderr)
            -- Output the child already wrote must land before the gate's
            -- verdict does; bounded, because a surviving grandchild can hold
            -- the write end of the pipe open indefinitely.
            let drained = void (raceTimeout drainGrace (joinOut >> joinErr))
            raceTimeout (geBudgetUsecs env) (waitExitCode p) >>= \case
                Right ExitSuccess -> drained >> pure (Right ())
                Right (ExitFailure c) -> do
                    drained
                    pure (Left (cmdText <> " -> exit " <> tshow c))
                Left () -> do
                    forM_ mPid $ \pid -> do
                        let pgid = CPid (fromIntegral pid)
                        recordExpiry env cmdText pgid
                        killGroupAfter (void (waitExitCode p)) pgid
                    drained
                    pure (Left (cmdText <> " -> timed out after " <> fmtBudget (geBudgetUsecs env)))

{- | Copy the child's output through to our own stream, bumping the dispatch
heartbeat as it goes — a gate that stops producing output goes stale in
@dispatch list@ instead of only in front of whoever is watching the
terminal.

Chunks, not lines: tasty flushes a test's name before blocking on that
test's result, and that partial line is what identifies which test wedged.
Its own connection because it is its own thread; on any failure the thread
just exits, since output is not worth failing a gate over.
-}
teeAndBeat :: Maybe GateHeartbeat -> Handle -> Handle -> IO ()
teeAndBeat mgh src dst =
    handle (\(_ :: SomeException) -> pure ()) $ case mgh of
        Nothing -> loop (pure ())
        Just gh ->
            bracket (openDb (ghDbPath gh)) close $ \c ->
                loop (quietly (RD.updateHeartbeat c (ghDid gh)))
  where
    loop :: IO () -> IO ()
    loop beat = do
        chunk <- BS.hGetSome src 4096
        if BS.null chunk
            then pure ()
            else do
                BS.hPut dst chunk
                hFlush dst
                beat
                loop beat

{- | Dump the gate's surviving process tree before it is killed. The group is
about to be SIGKILLed, so this is the only chance to record what was stuck
and where; without it the next wedge is as undiagnosable as the first.
-}
recordExpiry :: GateEnv -> Text -> CPid -> IO ()
recordExpiry env cmdText (CPid pgid) = do
    tree <- processGroupSnapshot (fromIntegral pgid)
    ts <- getCurrentTime
    hPutStrLn stderr $
        "icarium: gate timed out after "
            <> T.unpack (fmtBudget (geBudgetUsecs env))
            <> "; process group "
            <> show pgid
    mapM_ (hPutStrLn stderr . ("  " <>) . T.unpack) tree
    forM_ (geLogPath env) $ \logPath ->
        quietly . withFile logPath AppendMode $ \h ->
            BL.hPut h (A.encode (line ts tree) <> "\n")
  where
    line ts tree =
        object
            [ "type" .= ("icarium.gate_timeout" :: Text)
            , "ts" .= iso8601Show ts
            , "gate" .= cmdText
            , "budget" .= fmtBudget (geBudgetUsecs env)
            , "pgid" .= (fromIntegral pgid :: Int)
            , "tree" .= tree
            ]

{- | Every process still in the gate's process group: pid, parent, state and
command, one row per line, header first. Empty when @ps@ is unavailable or
itself hangs — forensics must never become the reason a kill is delayed.
-}
processGroupSnapshot :: Int -> IO [Text]
processGroupSnapshot pgid = do
    r <- raceTimeout (5 * 1_000_000) (try' (readProcessStdout psProc))
    pure $ case r of
        Right (Right (ExitSuccess, out)) -> select (T.lines (TE.decodeUtf8Lenient (BL.toStrict out)))
        _ -> []
  where
    psProc :: ProcessConfig () () ()
    psProc = setStdin nullStream (proc "ps" ["-eo", "pid,ppid,pgid,state,etime,command"])
    try' :: IO a -> IO (Either SomeException a)
    try' = try
    select ls = take 1 ls <> filter inGroup (drop 1 ls)
    inGroup l = case T.words l of
        (_ : _ : g : _) -> g == tshow pgid
        _ -> False

-- | How long the tee threads get to finish after the child is gone.
drainGrace :: Int
drainGrace = 5 * 1_000_000

-- | Run an action in a new thread; the result is the action to wait on it.
forkJoin :: IO () -> IO (IO ())
forkJoin act = do
    done <- newEmptyMVar
    _ <- forkIO (act `finally` putMVar done ())
    pure (takeMVar done)

{- | The budget as the note names it. Configs set whole minutes; sub-minute
budgets exist only in tests, where seconds read better than "0 minutes".
-}
fmtBudget :: Int -> Text
fmtBudget usecs
    | usecs >= 60_000_000 = tshow (usecs `div` 60_000_000) <> " minutes"
    | otherwise = T.pack (printf "%.1f seconds" (fromIntegral usecs / 1e6 :: Double))

quietly :: IO () -> IO ()
quietly = handle (\(_ :: SomeException) -> pure ())
