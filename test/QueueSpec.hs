{- | The ready queue: which tasks the @ready_tasks@ view admits, and how the
two claim paths (headless dispatch, interactive CLI) take from it.
-}
module QueueSpec (tests) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (bracket)
import Control.Monad (void)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Database.SQLite.Simple (Connection, close, execute_, open)
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import Icarium.Db (migrateDb)
import Icarium.Repo.Dispatch qualified as RD
import Icarium.Repo.Edge qualified as RE
import Icarium.Repo.Task qualified as RT
import Icarium.Schema (applySchema)
import Icarium.Types

import TestHelpers

tests :: TestTree
tests =
    testGroup
        "ready queue"
        [ testGroup
            "queue split (headless vs interactive)"
            [ testCase "ready_tasks spans both ready states; callers filter" testReadyTasksSpansBothStates
            , testCase "the deps gate applies to ready_interactive too" testInteractiveDepsGated
            , testCase "claimNextTask honours the state filter" testClaimNextTaskStateFiltered
            , testCase "claimReadyTask claims a named task in either ready state" testClaimReadyTaskNamed
            , testCase "claimReadyTask refuses a task that is not ready-ish" testClaimReadyTaskRefusesNonReady
            , testCase "a busy write lock is not an empty queue" testClaimReportsLockBusy
            , testCase "a claim retries until the write lock frees" testClaimRetriesUntilLockFrees
            ]
        , testGroup
            "dependency gating on merged"
            [ testCase "parked dependency blocks dependent" testParkedDepBlocks
            , testCase "merged dependency unblocks dependent" testMergedDepUnblocks
            , testCase "manually-done dependency with no dispatches unblocks" testManualDoneDepUnblocks
            ]
        ]

-- =============================================================
-- Queue split
-- =============================================================

{- | The view is the one place the deps-satisfaction gate lives; it must
admit both ready states so callers can pick their queue by state filter.
-}
testReadyTasksSpansBothStates :: IO ()
testReadyTasksSpansBothStates = withTestDb $ \c -> do
    headless <- mkTaskIn c "Headless work" ReadyHeadless
    interactive <- mkTaskIn c "Interactive work" ReadyInteractive
    _ <- mkTaskIn c "Not ready" Planned

    both <- readyIds c []
    assertBool "view carries the headless task" (headless `elem` both)
    assertBool "view carries the interactive task" (interactive `elem` both)
    length both @?= 2

    onlyHeadless <- readyIds c [ReadyHeadless]
    onlyHeadless @?= [headless]
    onlyInteractive <- readyIds c [ReadyInteractive]
    onlyInteractive @?= [interactive]

-- | Generalizing the view must not lose the gate for the new state.
testInteractiveDepsGated :: IO ()
testInteractiveDepsGated = withTestDb $ \c -> do
    dep <- mkTaskIn c "Unfinished dependency" ReadyHeadless
    dependent <- mkTaskIn c "Interactive dependent" ReadyInteractive
    _ <- RE.insertEdge c DependsOn TaskNode dependent TaskNode dep

    ready <- readyIds c [ReadyInteractive]
    assertBool "interactive dependent held behind an open dependency" (dependent `notElem` ready)

    void $ RT.updateTask c dep RT.emptyUpdate{RT.tuState = Just Done}
    ready' <- readyIds c [ReadyInteractive]
    ready' @?= [dependent]

-- | Uncontended claims can only be Claimed or NoCandidate; drop to a Maybe.
claimedTask :: RT.ClaimResult -> Maybe Task
claimedTask = \case
    RT.Claimed t -> Just t
    RT.NoCandidate -> Nothing
    RT.LockBusy -> error "LockBusy on a connection nothing else is using"

{- | Dispatch and the interactive CLI share one claim path and differ only
in which states they will take.
-}
testClaimNextTaskStateFiltered :: IO ()
testClaimNextTaskStateFiltered = withTestDb $ \c -> do
    interactive <- mkTaskIn c "Interactive work" ReadyInteractive

    headlessClaim <- RT.claimNextTask c [ReadyHeadless] "dispatch"
    assertBool "headless queue does not see interactive work" (isNothing (claimedTask headlessClaim))

    claimed <- claimedTask <$> RT.claimNextTask c [ReadyInteractive] "human"
    fmap taskId claimed @?= Just interactive
    fmap taskState claimed @?= Just InProgress

testClaimReadyTaskNamed :: IO ()
testClaimReadyTaskNamed = withTestDb $ \c -> do
    headless <- mkTaskIn c "Headless work" ReadyHeadless
    interactive <- mkTaskIn c "Interactive work" ReadyInteractive

    h <- claimedTask <$> RT.claimReadyTask c headless "human"
    fmap taskState h @?= Just InProgress
    fmap taskClaimedBy h @?= Just (Just "human")

    i <- claimedTask <$> RT.claimReadyTask c interactive "human"
    fmap taskState i @?= Just InProgress

-- | Naming a task selects it; it does not license claiming unready work.
testClaimReadyTaskRefusesNonReady :: IO ()
testClaimReadyTaskRefusesNonReady = withTestDb $ \c -> do
    planned <- mkTaskIn c "Under-specified" Planned
    r <- RT.claimReadyTask c planned "human"
    assertBool "planned task refused" (isNothing (claimedTask r))
    still <- RT.getTask c planned
    fmap taskState still @?= Just Planned

{- | Two connections onto one file DB, neither with a busy_timeout, so a
held write lock surfaces as SQLITE_BUSY immediately — the contention the
retry has to survive, without the wall-clock wait a real timeout adds.
-}
withRacingConns :: (Connection -> Connection -> IO a) -> IO a
withRacingConns act =
    withSystemTempFile "icarium-race.db" $ \path h -> do
        hClose h
        bracket (open path) close $ \claimer -> do
            applySchema claimer
            migrateDb claimer
            bracket (open path) close (act claimer)

{- | An empty queue and a lock we could not take are different answers.
Conflating them made `task claim` exit 1 — the empty-queue signal — while
four ready tasks sat in the queue.
-}
testClaimReportsLockBusy :: IO ()
testClaimReportsLockBusy = withRacingConns $ \claimer holder -> do
    _ <- mkTaskIn claimer "Contended work" ReadyInteractive
    execute_ holder "BEGIN IMMEDIATE"
    r <- RT.claimNextTask claimer [ReadyInteractive] "human"
    case r of
        RT.LockBusy -> pure ()
        other -> assertFailure ("expected LockBusy, got " <> show other)
    execute_ holder "ROLLBACK"

-- | A lock held only briefly must still yield a claim, not LockBusy.
testClaimRetriesUntilLockFrees :: IO ()
testClaimRetriesUntilLockFrees = withRacingConns $ \claimer holder -> do
    tid <- mkTaskIn claimer "Contended work" ReadyInteractive
    execute_ holder "BEGIN IMMEDIATE"
    _ <- forkIO (threadDelay 50000 >> execute_ holder "ROLLBACK")
    r <- RT.claimNextTask claimer [ReadyInteractive] "human"
    case r of
        RT.Claimed t -> do
            taskId t @?= tid
            taskState t @?= InProgress
        other -> assertFailure ("expected a claim, got " <> show other)

-- =============================================================
-- Dependency gating on merged
-- =============================================================

{- | Insert a done dependency and a ready dependent linked by depends_on;
returns (depId, dependentId). Callers vary the dependency's dispatch state.
-}
mkDepPair :: Connection -> IO (Text, Text)
mkDepPair c = do
    dep <- mkTaskRow c "Dependency task"
    dependent <- mkTaskRow c "Dependent task"
    _ <- RE.insertEdge c DependsOn TaskNode dependent TaskNode dep
    void $ RT.updateTask c dep RT.emptyUpdate{RT.tuState = Just Done}
    pure (dep, dependent)

readyIds :: Connection -> [TaskState] -> IO [Text]
readyIds c states = map taskId <$> RT.queueTasks c states

testParkedDepBlocks :: IO ()
testParkedDepBlocks = withTestDb $ \c -> do
    (dep, dependent) <- mkDepPair c
    let did = "01DEPGATE00000000000000001" :: Text
    insertTestDispatch c did dep
    RD.finishDispatch c did OSuccess Nothing Nothing
    ready <- readyIds c []
    assertBool "dependent held while dependency is parked" (dependent `notElem` ready)

testMergedDepUnblocks :: IO ()
testMergedDepUnblocks = withTestDb $ \c -> do
    (dep, dependent) <- mkDepPair c
    let did = "01DEPGATE00000000000000002" :: Text
    insertTestDispatch c did dep
    RD.finishDispatch c did OSuccess Nothing Nothing
    RD.setMerged c did "deadbeef"
    ready <- readyIds c []
    assertBool "dependent eligible once dependency merged" (dependent `elem` ready)

testManualDoneDepUnblocks :: IO ()
testManualDoneDepUnblocks = withTestDb $ \c -> do
    (_, dependent) <- mkDepPair c
    ready <- readyIds c []
    assertBool "dependent eligible behind manually-done dependency" (dependent `elem` ready)
