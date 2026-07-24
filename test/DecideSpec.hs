module DecideSpec (tests) where

import Data.Text (Text)
import System.Exit (ExitCode (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Icarium.Dispatch.Decide (
    Decision (..),
    DecisionInput (..),
    DecisionReason (..),
    GitSignals (..),
    decideOutcome,
    renderReason,
 )
import Icarium.Dispatch.Payload (
    Finding (..),
    FindingAxis (..),
    Severity (..),
    WorkerPayload (..),
    WorkerStatus (..),
    renderFindings,
 )
import Icarium.Types (DispatchOutcome (..), TaskState (..))

tests :: TestTree
tests =
    testGroup
        "Decide"
        [ testGroup
            "exit code"
            [ testCase "a non-zero exit fails with its code" testExitFailure
            , testCase "exit 124 is the timeout sentinel" testExitTimeout
            , testCase "exit beats a worker escalation" testExitBeatsEscalation
            ]
        , testGroup
            "escalation"
            [ testCase "a blocked worker fails with its own reason" testEscalation
            , testCase "a blocked worker with no reason still fails" testEscalationNoReason
            , testCase "escalation beats a dirty tree" testEscalationBeatsGuard
            , testCase "a submitted worker decides nothing" testSubmittedIsSilent
            ]
        , testGroup
            "guards"
            [ testCase "a dirty tree fails, listing what was left" testGuardDirtyTree
            , testCase "no commits on the branch fails" testGuardEmptyDiff
            , testCase "a dirty tree outranks an empty diff" testGuardDirtyFirst
            , testCase "an unresolvable branch sha is not an empty diff" testGuardRevParseError
            , testCase "no-commit: a dirty tree still fails" testGuardNoCommitDirty
            , testCase "no-commit: commits on the branch fail" testGuardNoCommitLeftCommits
            , testCase "no-commit: a clean tree at base has nothing to land" testNoCommitClean
            ]
        , testGroup
            "gates and review"
            [ testCase "a failing gate fails with the gate's own note" testGateFailed
            , testCase "passing gates with no reviewer is clean" testGatesPassed
            , testCase "no findings is a pass" testReviewPass
            , testCase "warn findings pass but are recorded" testReviewWarn
            , testCase "a fail finding fails and is retryable" testReviewFail
            , testCase "a reviewer that never reported fails closed" testReviewFailsClosed
            , testCase "a failing gate outranks the reviewer" testGateBeatsReview
            ]
        , testGroup
            "task transition"
            [ testCase "a success targets done with no block reason" testTransitionSuccess
            , testCase "a failure targets blocked, carrying the note" testTransitionFailure
            ]
        , testGroup
            "renderReason"
            [ testCase "every reason renders its note" testRenderReason
            ]
        ]

baseSha :: Text
baseSha = "aaaa0000"

newSha :: Text
newSha = "bbbb1111"

{- | A run that got as far as gates with nothing wrong: clean tree, a commit on
the branch, no reviewer. Each test overrides the one signal it is about.
-}
cleanInput :: DecisionInput
cleanInput =
    DecisionInput
        { diNoCommit = False
        , diExit = ExitSuccess
        , diTimeoutMinutes = 30
        , diPayload = Just (WorkerPayload{wpStatus = WSubmitted, wpBlockReason = Nothing, wpForFutureAgents = []})
        , diGit = GitSignals{gsPorcelain = "", gsBranchSha = Right newSha, gsBaseSha = baseSha}
        , diGate = Nothing
        , diReview = Nothing
        }

blocked :: Maybe Text -> Maybe WorkerPayload
blocked reason = Just (WorkerPayload{wpStatus = WBlocked, wpBlockReason = reason, wpForFutureAgents = []})

dirty :: GitSignals
dirty = (diGit cleanInput){gsPorcelain = "?? leftover.txt"}

testExitFailure :: IO ()
testExitFailure = do
    let d = decideOutcome cleanInput{diExit = ExitFailure 3}
    dReason d @?= ExitFailed 3
    dOutcome d @?= OFailure

testExitTimeout :: IO ()
testExitTimeout =
    dReason (decideOutcome cleanInput{diExit = ExitFailure 124, diTimeoutMinutes = 45})
        @?= TimedOut 45

-- | The worker cannot report an escalation it never lived to write.
testExitBeatsEscalation :: IO ()
testExitBeatsEscalation =
    dReason (decideOutcome cleanInput{diExit = ExitFailure 1, diPayload = blocked (Just "policy")})
        @?= ExitFailed 1

testEscalation :: IO ()
testEscalation = do
    let d = decideOutcome cleanInput{diPayload = blocked (Just "policy forbids a force-push")}
    dReason d @?= Escalated "policy forbids a force-push"
    dOutcome d @?= OFailure

testEscalationNoReason :: IO ()
testEscalationNoReason =
    dReason (decideOutcome cleanInput{diPayload = blocked Nothing})
        @?= Escalated "no reason given"

{- | The recorded reason must be the worker's own, not the guard's generic
"made no commits" — which is why the escalation outranks the guards.
-}
testEscalationBeatsGuard :: IO ()
testEscalationBeatsGuard =
    dReason (decideOutcome cleanInput{diPayload = blocked (Just "policy"), diGit = dirty})
        @?= Escalated "policy"

testSubmittedIsSilent :: IO ()
testSubmittedIsSilent =
    dReason (decideOutcome cleanInput) @?= Clean

-- | Decide with the given git signals and nothing else wrong.
withGit :: Bool -> Text -> Either Text Text -> DecisionReason
withGit noCommit porcelain branchSha =
    dReason $
        decideOutcome
            cleanInput
                { diNoCommit = noCommit
                , diGit = GitSignals{gsPorcelain = porcelain, gsBranchSha = branchSha, gsBaseSha = baseSha}
                }

testGuardDirtyTree :: IO ()
testGuardDirtyTree =
    withGit False "?? snapshot-test.json\n M src/Foo.hs" (Right newSha)
        @?= GuardFailed
            "agent left uncommitted changes; refusing to accept\n\
            \uncommitted:\n\
            \  ?? snapshot-test.json\n\
            \   M src/Foo.hs"

testGuardEmptyDiff :: IO ()
testGuardEmptyDiff =
    withGit False "" (Right baseSha) @?= GuardFailed "agent made no commits on dispatch branch"

testGuardDirtyFirst :: IO ()
testGuardDirtyFirst =
    withGit False "?? leftover.txt" (Right baseSha)
        @?= GuardFailed
            "agent left uncommitted changes; refusing to accept\n\
            \uncommitted:\n\
            \  ?? leftover.txt"

testGuardRevParseError :: IO ()
testGuardRevParseError = withGit False "" (Left "git error") @?= Clean

testGuardNoCommitDirty :: IO ()
testGuardNoCommitDirty =
    withGit True "?? leftover.txt" (Right baseSha)
        @?= GuardFailed
            "agent left uncommitted changes; refusing to accept\n\
            \uncommitted:\n\
            \  ?? leftover.txt"

testGuardNoCommitLeftCommits :: IO ()
testGuardNoCommitLeftCommits =
    withGit True "" (Right newSha)
        @?= GuardFailed "no-commit task: agent left commits on dispatch branch (branch retained for inspection)"

{- | The expected end of a no-commit run: nothing to land, so neither the gates
nor the reviewer have anything to say about it.
-}
testNoCommitClean :: IO ()
testNoCommitClean = do
    let d = decideOutcome cleanInput{diNoCommit = True, diGit = (diGit cleanInput){gsBranchSha = Right baseSha}}
    dReason d @?= NoCommitClean
    dOutcome d @?= OSuccess

warnFinding :: Finding
warnFinding = Finding AxisStandards SevWarn (Just "src/Foo.hs") "possible Duplicated Code"

failFinding :: Finding
failFinding = Finding AxisSpec SevFail (Just "src/Bar.hs") "requirement not implemented"

-- | Decide with the gates passed and the reviewer having reported @r@.
withReview :: Maybe (Either Text [Finding]) -> Decision
withReview r = decideOutcome cleanInput{diGate = Just (Right ()), diReview = r}

testGateFailed :: IO ()
testGateFailed = do
    let d = decideOutcome cleanInput{diGate = Just (Left "exit 7 -> exit 7")}
    dReason d @?= GateFailed "exit 7 -> exit 7"
    dOutcome d @?= OFailure
    dRetry d @?= Nothing

testGatesPassed :: IO ()
testGatesPassed = dReason (withReview Nothing) @?= Clean

testReviewPass :: IO ()
testReviewPass = dReason (withReview (Just (Right []))) @?= Clean

testReviewWarn :: IO ()
testReviewWarn = do
    let d = withReview (Just (Right [warnFinding]))
    dReason d @?= ReviewerWarn [warnFinding]
    dOutcome d @?= OSuccess
    dRetry d @?= Nothing

testReviewFail :: IO ()
testReviewFail = do
    let fs = [warnFinding, failFinding]
        d = withReview (Just (Right fs))
    dReason d @?= ReviewerFailed (Right fs)
    dOutcome d @?= OFailure
    dRetry d @?= Just (renderFindings fs)

{- | A reviewer whose own run died has no findings, but that is not a pass —
a broken gate must not wave the diff through.
-}
testReviewFailsClosed :: IO ()
testReviewFailsClosed = do
    let d = withReview (Just (Left "reviewer timed out"))
    dReason d @?= ReviewerFailed (Left "reviewer timed out")
    dOutcome d @?= OFailure
    dRetry d @?= Just "reviewer timed out"

testGateBeatsReview :: IO ()
testGateBeatsReview =
    dReason (decideOutcome cleanInput{diGate = Just (Left "build failed"), diReview = Just (Right [])})
        @?= GateFailed "build failed"

testTransitionSuccess :: IO ()
testTransitionSuccess = do
    dTaskTransition (withReview (Just (Right []))) @?= Just (Done, Nothing)
    dTaskTransition (withReview (Just (Right [warnFinding]))) @?= Just (Done, Nothing)

-- | The block reason a failure records is its own note.
testTransitionFailure :: IO ()
testTransitionFailure =
    dTaskTransition (decideOutcome cleanInput{diExit = ExitFailure 2})
        @?= Just (Blocked, Just "claude exited 2")

testRenderReason :: IO ()
testRenderReason = do
    renderReason Clean @?= "gates passed"
    renderReason NoCommitClean @?= "no-commit task"
    renderReason (ReviewerWarn [warnFinding]) @?= "reviewer warn\n" <> renderFindings [warnFinding]
    renderReason (ExitFailed 7) @?= "claude exited 7"
    renderReason (TimedOut 30) @?= "timed out after 30 minutes"
    renderReason (Escalated "policy forbids a force-push") @?= "worker blocked: policy forbids a force-push"
    renderReason (GuardFailed "agent made no commits on dispatch branch")
        @?= "agent made no commits on dispatch branch"
    renderReason (GateFailed "exit 7 -> exit 7") @?= "exit 7 -> exit 7"
    renderReason (ReviewerFailed (Right [failFinding]))
        @?= "reviewer: fail\n" <> renderFindings [failFinding]
    renderReason (ReviewerFailed (Left "reviewer timed out")) @?= "reviewer: fail\nreviewer timed out"
