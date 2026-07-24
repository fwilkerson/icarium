{- | What happened to a /run/ of dispatches, and why it stopped.

Companion to "Icarium.Dispatch.Decide", and deliberately its mirror: that
module concludes what happened to one dispatch, this one concludes what
happened to a run of them. The participants report what they observed and one
pure function concludes what it means.

A drain is a dispatch in a loop. There is no second code path and no mode
flag: a single named dispatch is a drain whose 'Selector' yields once. The
only axis that survives is selection, and it owns the asymmetry — a selector
that finds nothing is @no such task@ when the task was named and @queue empty@
when the queue was the selector.

Why a run ended is a named 'StopReason'; what it exits with is derived from
that reason together with the accumulated outcomes and merge results
('runExit', ADR 0009), never picked at whichever branch noticed.

Only 'nextStep', 'interpretStep' and 'runExit' are pure. The loop performs IO
and streams its progress as it goes, because a long drain must be watchable;
it is covered at the CLI tier against a real worktree and a real worker, not
by injecting a fake.
-}
module Icarium.Dispatch.Drain (
    -- * Selection
    Selector (..),

    -- * The pure tier
    StepResult (..),
    StopReason (..),
    StepContext (..),
    StepVerdict (..),
    Tally (..),
    StopReport (..),
    nextStep,
    interpretStep,
    stopReport,

    -- * The run
    DrainRequest (..),
    RunReport (..),
    drain,
    runExit,
) where

import Control.Applicative ((<|>))
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT, throwE)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Database.SQLite.Simple (Connection)
import System.IO (stderr)
import System.Posix.Signals (Handler (..), installHandler, raiseSignal, sigINT)

import Icarium.Config (Config)
import Icarium.Dispatch qualified as D
import Icarium.Dispatch.LogResult (readLogResult)
import Icarium.Dispatch.Merge (MergeOutcome (..), mergeParked)
import Icarium.Dispatch.Worktree (WorktreeError (..), worktreeErrorText)
import Icarium.Events qualified as Ev
import Icarium.Git qualified as Git
import Icarium.Render.Dispatch qualified as Render
import Icarium.Repo.Dispatch qualified as RD
import Icarium.Repo.Task qualified as RT
import Icarium.Types

{- | How a step picks its task. The one axis on which a named dispatch differs
from a queue drain — and so the one place the "found nothing" asymmetry is
stated.
-}
data Selector
    = -- | A named task, already resolved to a full id. Yields once.
      Named Text
    | -- | The head of the headless queue. Yields until the queue empties.
      QueueHead
    deriving (Show, Eq)

-- | What one pipeline step observed. 'interpretStep' says what it means.
data StepResult
    = -- | The selector had nothing to give.
      StepNoCandidate
    | -- | The database write lock never came free; nothing was claimed.
      StepLockBusy
    | -- | Worktree back-pressure at setup; the claim was released.
      StepNoCapacity Text
    | -- | Setup failed before the worker ran; the claim was released.
      StepSetupFailed Text
    | {- | A dispatch ran to an outcome, and the landing attempt it earned.
      'Nothing' for a merge means there was nothing to land.
      -}
      StepDispatched DispatchOutcome (Maybe MergeOutcome)

{- | Why a run ended. Named rather than inline at the site that noticed,
because the exit code is derived from it ('runExit') rather than asserted
per branch.
-}
data StopReason
    = -- | The queue selector found no actionable task.
      QueueEmpty
    | {- | The selector yields once and already has: a named task, or a dry
      run, which claims nothing and so would preview the same head forever.
      -}
      SelectorSpent
    | -- | The named selector resolved to no claimable task.
      NoSuchTask Text
    | {- | The write lock never came free. Not an empty queue: stopping
      quietly here would leave ready work behind.
      -}
      LockContended
    | CapReached Int
    | -- | SIGINT arrived; the run stops after the dispatch in flight.
      Interrupted
    | {- | Machine-level back-pressure: the next dispatch would hit the same
      wall, so there is no point taking another.
      -}
      BackPressure Text
    | -- | Setup failed before a worker ran; a retry would just repeat it.
      SetupFailed Text
    deriving (Show, Eq)

-- | The run's shape, as the pure tier sees it at one step.
data StepContext = StepContext
    { scSelector :: Selector
    , scDryRun :: Bool
    , scCap :: Maybe Int
    , scInterrupted :: Bool
    -- ^ Whether a SIGINT has arrived by the end of this step.
    }
    deriving (Show, Eq)

-- | What a run leaves behind, accumulated as it goes.
data Tally = Tally
    { tFailed :: Int
    , tParked :: Int
    }
    deriving (Show, Eq)

emptyTally :: Tally
emptyTally = Tally 0 0

-- | What one step did to the run: whether it was the last, and what it added.
data StepVerdict = StepVerdict
    { svStop :: Maybe StopReason
    -- ^ 'Nothing' to take another step.
    , svFailed :: Int
    , svParked :: Int
    }
    deriving (Show, Eq)

{- | Whether an @i@-th step may run at all, before anything is selected. A
selector that yields once is spent after it has; a cap is the user's own
limit on a queue that has not.
-}
nextStep :: StepContext -> Int -> Either StopReason ()
nextStep ctx i
    | i > 0, yieldsOnce = Left SelectorSpent
    | Just cap <- scCap ctx, i >= cap = Left (CapReached cap)
    | otherwise = Right ()
  where
    -- A dry run previews without claiming, so the head it saw is still the
    -- head; a named selector's one task is likewise still named.
    yieldsOnce =
        scDryRun ctx || case scSelector ctx of
            Named _ -> True
            QueueHead -> False

-- | What a step's observation means for the run.
interpretStep :: StepContext -> StepResult -> StepVerdict
interpretStep ctx = \case
    StepNoCandidate -> stop $ case scSelector ctx of
        Named tid -> NoSuchTask tid
        QueueHead -> QueueEmpty
    StepLockBusy -> stop LockContended
    StepNoCapacity note -> stop (BackPressure note)
    StepSetupFailed note -> stop (SetupFailed note)
    StepDispatched outcome mMerge ->
        StepVerdict
            { svStop = mergeStop <|> interrupted
            , svFailed = if outcome == OSuccess then 0 else 1
            , svParked = parked
            }
      where
        (parked, mergeStop) = case mMerge of
            Nothing -> (0, Nothing)
            Just (MergeLanded _) -> (0, Nothing)
            Just (MergeBlocked _ _) -> (1, Nothing)
            Just (MergeStopped note) -> (1, Just (BackPressure note))
        interrupted = if scInterrupted ctx then Just Interrupted else Nothing
  where
    stop reason = StepVerdict{svStop = Just reason, svFailed = 0, svParked = 0}

-- | Everything one run needs; the command layer supplies the selector and cap.
data DrainRequest = DrainRequest
    { dqConn :: Connection
    , dqDbPath :: FilePath
    , dqConfig :: Config
    , dqSelector :: Selector
    , dqCap :: Maybe Int
    , dqDryRun :: Bool
    , dqRouting :: Routing
    , dqBaseOverride :: Maybe Text
    }

-- | What a finished run has to say for itself.
data RunReport = RunReport
    { rrSelector :: Selector
    , rrStop :: StopReason
    , rrTally :: Tally
    }
    deriving (Show, Eq)

{- | Run the pipeline until a stop reason, streaming progress as it goes.
Installs the SIGINT handler for the run: the first interrupt stops the drain
after the dispatch in flight, a second one is the operator insisting.
-}
drain :: DrainRequest -> IO RunReport
drain req = do
    sigCount <- installSigintCounter
    tally <- newIORef emptyTally
    let go !i = case nextStep (shapeAt False) i of
            Left reason -> finish reason
            Right () -> do
                result <- runStep req
                interrupted <- (>= 1) <$> readIORef sigCount
                let verdict = interpretStep (shapeAt interrupted) result
                modifyIORef' tally (add verdict)
                maybe (go (i + 1)) finish (svStop verdict)
        finish reason = do
            case stopReport (dqSelector req) reason of
                StopSaid line -> progress line
                _ -> pure ()
            RunReport (dqSelector req) reason <$> readIORef tally
    go (0 :: Int)
  where
    shapeAt interrupted =
        StepContext
            { scSelector = dqSelector req
            , scDryRun = dqDryRun req
            , scCap = dqCap req
            , scInterrupted = interrupted
            }
    add v t = Tally (tFailed t + svFailed v) (tParked t + svParked v)

-- | What a stop reason is worth: a line, silence, or a failure to exit with.
data StopReport
    = -- | A normal end, and the line the run prints as it stops.
      StopSaid Text
    | -- | Nothing to say: the selector simply had one task and ran it.
      StopSilent
    | -- | The run failed here; exit code and message.
      StopFailed Int Text
    deriving (Show, Eq)

{- | What each way of stopping is worth. One cascade rather than two, so a new
'StopReason' cannot fall through both and end up neither printed nor exited on.
-}
stopReport :: Selector -> StopReason -> StopReport
stopReport sel = \case
    QueueEmpty -> StopSaid Render.renderQueueEmpty
    SelectorSpent -> StopSilent
    CapReached n -> StopSaid (Render.renderCapReached n)
    Interrupted -> StopSaid Render.renderSigint
    BackPressure note -> StopSaid (Render.renderStopping note)
    -- The selector failed, not the work — a bad argument is exit 1.
    NoSuchTask tid -> StopFailed 1 (Render.renderTaskNotFound tid)
    LockContended -> StopFailed 3 (Render.renderLockBusy (retryCommand sel))
    SetupFailed note -> StopFailed 3 note

{- | What the run exits with: the stop reason's own failure if it has one,
otherwise what the dispatches accumulated (ADR 0009). 'Nothing' is exit 0.
One function, so the two forms of @dispatch run@ cannot disagree.
-}
runExit :: RunReport -> Maybe (Int, Text)
runExit rr = stopFailure <|> tallyFailure (rrTally rr)
  where
    stopFailure = case stopReport (rrSelector rr) (rrStop rr) of
        StopFailed code msg -> Just (code, msg)
        _ -> Nothing
    tallyFailure (Tally failed parked)
        | failed == 0, parked == 0 = Nothing
        | otherwise = Just (3, Render.renderRunFailure (failed > 0) (parked > 0))

-- | The command to run again after a claim that took nothing.
retryCommand :: Selector -> Text
retryCommand = \case
    Named tid -> "icarium dispatch run " <> tid
    QueueHead -> "icarium dispatch run"

{- | One pipeline step: select a task, run it, conclude an outcome, attempt
to land the branch. Every way the step can end early is a 'throwE' carrying
what it observed; the interpreter names the stop.
-}
runStep :: DrainRequest -> IO StepResult
runStep req = fmap collapse . runExceptT $ do
    task <-
        liftIO (selectTask req) >>= \case
            RT.Claimed t _ -> pure t
            RT.NoCandidate -> throwE StepNoCandidate
            RT.LockBusy -> throwE StepLockBusy
    liftIO $ progress (Render.renderDispatching (taskId task))
    res <-
        liftIO (D.dispatch (dqConn req) (dispatchRequest req task)) >>= \case
            -- No work started, so the claim goes back either way: capacity
            -- may free up later, while a setup error would just repeat.
            Left err@(WtNoCapacity _) -> release task >> throwE (StepNoCapacity (worktreeErrorText err))
            Left err -> release task >> throwE (StepSetupFailed (worktreeErrorText err))
            Right res -> pure res
    liftIO $ do
        D.applyOutcomeToTask (dqConn req) (dqDbPath req) task res
        progress (Render.renderRunOutcome res)
        printSummary res
        mMerge <- autoMerge (dqConfig req) (dqConn req) res
        mapM_ (uncurry reportMerge) mMerge
        pure (StepDispatched (D.dresOutcome res) (snd <$> mMerge))
  where
    collapse = either id id
    release t = liftIO (releaseClaim req t)

{- | Take the task this run is about: the named one, or the queue head.
Claiming /is/ the selection — taking the head and marking it in_progress is
one atomic step, so racing drains cannot pick the same task. Headless only:
'ReadyInteractive' is work a human must do. A dry run previews instead,
moving nothing and emitting no claim event.
-}
selectTask :: DrainRequest -> IO RT.ClaimResult
selectTask req = case (dqDryRun req, dqSelector req) of
    (True, Named tid) -> maybe RT.NoCandidate preview <$> RT.getTask conn tid
    (True, QueueHead) -> maybe RT.NoCandidate preview . listToMaybe <$> RT.queueTasks conn [ReadyHeadless]
    (False, Named tid) -> claim (RT.claimTask conn tid)
    (False, QueueHead) -> claim (RT.claimNextTask conn [ReadyHeadless])
  where
    conn = dqConn req
    -- A dry run takes the same branch as a claim but moves nothing, so the
    -- task never left the state it is in.
    preview t = RT.Claimed t (taskState t)
    -- A busy lock claimed nothing; the log is append-only, so a claim event
    -- written here could never be retracted.
    claim withOwner = do
        owner <- RT.defaultOwner
        r <- withOwner owner
        case r of
            RT.Claimed t from ->
                Ev.emit (dqDbPath req) "dispatch run" (Ev.TaskClaimed (taskId t) from owner)
            _ -> pure ()
        pure r

dispatchRequest :: DrainRequest -> Task -> D.DispatchRequest
dispatchRequest req task =
    D.DispatchRequest
        { D.drTask = task
        , D.drConfig = dqConfig req
        , D.drDbPath = dqDbPath req
        , D.drDryRun = dqDryRun req
        , D.drRouting = dqRouting req
        , D.drBaseOverride = dqBaseOverride req
        }

{- | Undo a claim whose dispatch never started (no-op under @--dry-run@,
which never claimed).
-}
releaseClaim :: DrainRequest -> Task -> IO ()
releaseClaim req t = unless (dqDryRun req) $ do
    RT.releaseTask (dqConn req) (taskId t)
    Ev.emit (dqDbPath req) "dispatch run" (Ev.TaskUpdated (taskId t) InProgress ReadyHeadless)

{- | Land a just-successful dispatch immediately (attempt-then-park).
'Nothing' when there is nothing to land: dry-run, non-success, or already
merged (no-commit successes are pre-stamped merged).
-}
autoMerge :: Config -> Connection -> D.DispatchResult -> IO (Maybe (Dispatch, MergeOutcome))
autoMerge cfg c res
    | Just did <- D.dresDispatchId res
    , OSuccess <- D.dresOutcome res =
        RD.getDispatch c did >>= \case
            Just d | Nothing <- dispatchMergeSha d -> Just . (d,) <$> mergeParked cfg c d
            _ -> pure Nothing
    | otherwise = pure Nothing

-- | What one landing attempt printed; the tally is the interpreter's job.
reportMerge :: Dispatch -> MergeOutcome -> IO ()
reportMerge d = \case
    MergeLanded sha -> TIO.putStrLn (Render.renderLanded d sha)
    MergeBlocked _ note -> progress (Render.renderStillParked d note)
    MergeStopped note -> progress (Render.renderStopping note)

-- | Print the enriched summary block.
printSummary :: D.DispatchResult -> IO ()
printSummary r = do
    mLog <- maybe (pure Nothing) readLogResult (D.dresLogPath r)
    -- Diff against the dispatch branch, which still exists here: the
    -- auto-merge (which deletes it) runs after this summary. The branch may
    -- be gone for no-commit runs — changedFiles returns [] then.
    files <- maybe (pure []) (\sha -> Git.changedFiles "." sha (D.dresBranch r)) (D.dresBaseSha r)
    TIO.putStr (Render.renderRunSummary r mLog files)

-- | Everything the run says as it goes shares one prefix and one stream.
progress :: Text -> IO ()
progress msg = TIO.hPutStrLn stderr ("icarium: " <> msg)

{- | Count SIGINTs for the run. The first is a request to stop after the
dispatch in flight; a second means the operator is not waiting, so hand the
signal back to the default handler.

Installed for every run, including a named one — a drain of one is still a
drain, and letting Ctrl-C kill it mid-dispatch only to leave an orphan for
@dispatch recover@ is the per-mode difference this module exists to delete.
-}
installSigintCounter :: IO (IORef Int)
installSigintCounter = do
    count <- newIORef (0 :: Int)
    let handler = do
            modifyIORef' count (+ 1)
            n <- readIORef count
            when (n >= 2) $ do
                void $ installHandler sigINT Default Nothing
                raiseSignal sigINT
    void $ installHandler sigINT (Catch handler) Nothing
    pure count
