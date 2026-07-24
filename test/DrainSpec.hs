{- | The drain's pure tier: what a step may run, what a step's observation
means for the run, and what the run exits with.

A table rather than a scattering of cases, because the rules are genuinely
combinatorial — selector x step result x the four merge results x SIGINT x dry
run — and the CLI suite can only afford one path per expensive run. The loop
itself is not here on purpose: its dependency is a real worktree and a real
worker, and it stays covered at the CLI tier (see the ticket, not an oversight
to close with a mock).

Mirrors "DecideSpec", which does the same for one dispatch.
-}
module DrainSpec (tests) where

import Data.Text (Text)
import Data.Text qualified as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Icarium.Dispatch.Drain (
    RunReport (..),
    Selector (..),
    StepContext (..),
    StepResult (..),
    StepVerdict (..),
    StopReason (..),
    StopReport (..),
    Tally (..),
    interpretStep,
    nextStep,
    runExit,
    stopReport,
 )
import Icarium.Dispatch.Merge (MergeOutcome (..))
import Icarium.Types (DispatchOutcome (..))

tests :: TestTree
tests =
    testGroup
        "Drain"
        [ testGroup "nextStep" (map runNextStepCase nextStepCases)
        , testGroup "interpretStep" (map runStepCase stepCases)
        , testGroup "stopReport" (map runStopCase stopCases)
        , testGroup "runExit" (map runExitCase exitCases)
        ]

-- | A queue drain with no cap, nothing dry, no interrupt: the plain case.
draining :: StepContext
draining =
    StepContext
        { scSelector = QueueHead
        , scDryRun = False
        , scCap = Nothing
        , scInterrupted = False
        }

taskId :: Text
taskId = "01KYANN7YCKQ4V37J901Y2K5YH"

-- =============================================================
-- nextStep: may an i-th step run at all?
-- =============================================================

data NextCase = NextCase String StepContext Int (Either StopReason ())

runNextStepCase :: NextCase -> TestTree
runNextStepCase (NextCase name ctx i expected) =
    testCase name (nextStep ctx i @?= expected)

nextStepCases :: [NextCase]
nextStepCases =
    [ NextCase "an uncapped queue drain keeps going" draining 0 (Right ())
    , NextCase "an uncapped queue drain has no built-in limit" draining 99 (Right ())
    , NextCase "a cap stops the step that would exceed it" capped 3 (Left (CapReached 3))
    , NextCase "a cap allows the step below it" capped 2 (Right ())
    , NextCase "a named selector yields its one task" named 0 (Right ())
    , NextCase "a named selector is spent after it" named 1 (Left SelectorSpent)
    , NextCase
        "a spent selector outranks a cap that would still allow a step"
        named{scCap = Just 3}
        1
        (Left SelectorSpent)
    , NextCase "a dry run previews once" dry 0 (Right ())
    , -- A preview claims nothing, so the head it saw is still the head. Spent
      -- is not QueueEmpty: the queue it previewed may be full.
      NextCase "a dry run is spent after its preview" dry 1 (Left SelectorSpent)
    ]
  where
    capped = draining{scCap = Just 3}
    named = draining{scSelector = Named taskId}
    dry = draining{scDryRun = True}

-- =============================================================
-- interpretStep: what one step's observation means
-- =============================================================

data StepCase = StepCase String StepContext StepResult StepVerdict

runStepCase :: StepCase -> TestTree
runStepCase (StepCase name ctx result expected) =
    testCase name (interpretStep ctx result @?= expected)

-- | Continue, adding nothing.
onwards :: StepVerdict
onwards = StepVerdict{svStop = Nothing, svFailed = 0, svParked = 0}

stopping :: StopReason -> StepVerdict
stopping reason = onwards{svStop = Just reason}

stepCases :: [StepCase]
stepCases =
    [ -- Selection: the one asymmetry, decided in one place.
      StepCase
        "an empty queue is a normal stop"
        draining
        StepNoCandidate
        (stopping QueueEmpty)
    , StepCase
        "a named selector that found nothing is no such task"
        namedCtx
        StepNoCandidate
        (stopping (NoSuchTask taskId))
    , -- A busy lock is not an empty queue: ready work would be left behind.
      StepCase "a busy write lock stops the run" draining StepLockBusy (stopping LockContended)
    , StepCase "a busy write lock reads the same when named" namedCtx StepLockBusy (stopping LockContended)
    , StepCase
        "worktree back-pressure stops the run"
        draining
        (StepNoCapacity "no worktree capacity")
        (stopping (BackPressure "no worktree capacity"))
    , StepCase
        "a setup failure stops the run"
        draining
        (StepSetupFailed "worktree setup failed")
        (stopping (SetupFailed "worktree setup failed"))
    , -- The four merge results, against a successful dispatch.
      StepCase
        "a success with nothing to land continues"
        draining
        (StepDispatched OSuccess Nothing)
        onwards
    , StepCase
        "a landed success continues clean"
        draining
        (StepDispatched OSuccess (Just (MergeLanded "abc123")))
        onwards
    , StepCase
        "a blocked merge parks one and continues"
        draining
        (StepDispatched OSuccess (Just (MergeBlocked 1 "dirty base")))
        onwards{svParked = 1}
    , StepCase
        "a merge stopped by back-pressure parks one and stops the run"
        draining
        (StepDispatched OSuccess (Just (MergeStopped "no worktree capacity")))
        (stopping (BackPressure "no worktree capacity")){svParked = 1}
    , -- Outcomes other than success.
      StepCase
        "a failed dispatch counts and the run continues"
        draining
        (StepDispatched OFailure Nothing)
        onwards{svFailed = 1}
    , StepCase
        "an interrupted dispatch counts as a failure too"
        draining
        (StepDispatched OInterrupted Nothing)
        onwards{svFailed = 1}
    , -- SIGINT: stop after the dispatch in flight, keeping what it produced.
      StepCase
        "SIGINT stops the run after the current dispatch"
        interrupted
        (StepDispatched OSuccess (Just (MergeLanded "abc123")))
        (stopping Interrupted)
    , StepCase
        "a dispatch that failed before SIGINT still counts"
        interrupted
        (StepDispatched OFailure Nothing)
        (stopping Interrupted){svFailed = 1}
    , StepCase
        "back-pressure outranks SIGINT: both stop, the machine's reason is why"
        interrupted
        (StepDispatched OSuccess (Just (MergeStopped "no worktree capacity")))
        (stopping (BackPressure "no worktree capacity")){svParked = 1}
    , -- A one-shot selector stops at nextStep, so the step itself is ordinary.
      StepCase
        "a dry run's step decides nothing about stopping"
        draining{scDryRun = True}
        (StepDispatched OSuccess Nothing)
        onwards
    , StepCase
        "a dry run that caught a SIGINT stops for the interrupt"
        draining{scDryRun = True, scInterrupted = True}
        (StepDispatched OSuccess Nothing)
        (stopping Interrupted)
    , StepCase
        "a named selector's step decides nothing about stopping"
        namedCtx
        (StepDispatched OSuccess (Just (MergeLanded "abc123")))
        onwards
    ]
  where
    namedCtx = draining{scSelector = Named taskId}
    interrupted = draining{scInterrupted = True}

-- =============================================================
-- stopReport: what each way of stopping is worth
-- =============================================================

data StopCase = StopCase String Selector StopReason StopReport

runStopCase :: StopCase -> TestTree
runStopCase (StopCase name sel reason expected) =
    testCase name (stopReport sel reason @?= expected)

stopCases :: [StopCase]
stopCases =
    [ StopCase
        "an empty queue says so"
        QueueHead
        QueueEmpty
        (StopSaid "ready queue empty; stopping")
    , -- The bug this pins: a preview or a named task must not claim the
      -- queue was empty, because it never looked past its own one task.
      StopCase "a spent selector says nothing" QueueHead SelectorSpent StopSilent
    , StopCase "a spent named selector says nothing" (Named taskId) SelectorSpent StopSilent
    , StopCase
        "a cap says which cap"
        QueueHead
        (CapReached 3)
        (StopSaid "reached max dispatches (3); stopping")
    , StopCase
        "back-pressure says the machine's own reason"
        QueueHead
        (BackPressure "no worktree capacity")
        (StopSaid "no worktree capacity; stopping")
    , StopCase
        "a named task that resolved to nothing is a failure, not a line"
        (Named taskId)
        (NoSuchTask taskId)
        (StopFailed 1 ("task not found: " <> taskId))
    ]

-- =============================================================
-- runExit: what the run exits with (ADR 0009)
-- =============================================================

data ExitCase = ExitCase String RunReport (Maybe Int) (Text -> Bool)

runExitCase :: ExitCase -> TestTree
runExitCase (ExitCase name report code says) = testCase name $ do
    let actual = runExit report
    fmap fst actual @?= code
    assertBool ("message: " <> show (fmap snd actual)) (maybe True (says . snd) actual)

report :: StopReason -> Tally -> RunReport
report = RunReport QueueHead

clean :: Tally
clean = Tally 0 0

anything :: Text -> Bool
anything = const True

exitCases :: [ExitCase]
exitCases =
    [ ExitCase "an empty queue is a clean drain" (report QueueEmpty clean) Nothing anything
    , ExitCase "a spent selector is a clean run" (report SelectorSpent clean) Nothing anything
    , ExitCase "a cap reached is not by itself a failure" (report (CapReached 2) clean) Nothing anything
    , ExitCase "SIGINT is not by itself a failure" (report Interrupted clean) Nothing anything
    , ExitCase
        "back-pressure is not by itself a failure"
        (report (BackPressure "no worktree capacity") clean)
        Nothing
        anything
    , ExitCase
        "a dispatch that failed makes the run exit 3"
        (report QueueEmpty (Tally 1 0))
        (Just 3)
        (has "dispatch list --outcome failure")
    , ExitCase
        "a dispatch that never landed makes the run exit 3"
        (report QueueEmpty (Tally 0 1))
        (Just 3)
        (has "dispatch merge --all")
    , ExitCase
        "both are reported together"
        (report QueueEmpty (Tally 2 1))
        (Just 3)
        (\m -> has "--outcome failure" m && has "merge --all" m)
    , -- Stopping early must not hide a failure that already happened.
      ExitCase
        "a failure before a cap still counts"
        (report (CapReached 2) (Tally 1 0))
        (Just 3)
        anything
    , ExitCase
        "a failure before SIGINT still counts"
        (report Interrupted (Tally 1 0))
        (Just 3)
        anything
    , -- The selector failing is not the work failing.
      ExitCase
        "a named task that resolved to nothing is exit 1"
        (RunReport (Named taskId) (NoSuchTask taskId) clean)
        (Just 1)
        (has "task not found")
    , ExitCase
        "a setup failure is exit 3 in its own words"
        (report (SetupFailed "worktree setup failed: exit 1") clean)
        (Just 3)
        (has "worktree setup failed")
    , ExitCase
        "a busy lock names the drain to retry"
        (report LockContended clean)
        (Just 3)
        (\m -> has "nothing was claimed" m && has "Retry: icarium dispatch run" m)
    , ExitCase
        "a busy lock on a named task names that task in the retry"
        (RunReport (Named taskId) LockContended clean)
        (Just 3)
        (has ("Retry: icarium dispatch run " <> taskId))
    ]
  where
    has = T.isInfixOf
