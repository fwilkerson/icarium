{- | Worktree dispatch + merge, driven end to end against a throwaway git
repo with the stub @claude@ on PATH. See "CliDispatchHelpers" for the
scaffolding these share.
-}
module CliWorktreeSpec (tests) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (try)
import Control.Monad (forM_, replicateM)
import Data.List (isInfixOf, isPrefixOf)
import Database.SQLite.Simple (Query (..), close, execute, open)
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import CliDispatchHelpers (
    addReadyTask,
    agreementToml,
    badgedId,
    countLines,
    dispatchBranches,
    gitOut,
    parkedId,
    runDispatch,
    runDispatchParked,
    stubToml,
    stubTomlWith,
    withDispatchRepo,
    worktreeCount,
 )

tests :: TestTree
tests =
    testGroup
        "worktree dispatch and merge"
        [ testCase "dispatch: invoking checkout untouched from dirty feature branch" testDispatchInvokingCheckoutUntouched
        , testCase "dispatch: child icarium hits parent DB, no nested store" testDispatchNoNestedStore
        , testCase "dispatch: success auto-merges (base advances, branch deleted, task done)" testDispatchAutoMerges
        , testCase "dispatch: blocked auto-merge stays parked, exit 3 names fixing command" testDispatchAutoMergeBlockedParks
        , testCase "dispatch run (drain): dependency chain lands in one invocation" testDrainAutoMergesChain
        , testCase "dispatch merge: fast-forward in place when base checked out clean" testMergeFFInPlace
        , testCase "dispatch merge: fast-forward via temp worktree when base checked out elsewhere" testMergeFFTempWorktree
        , testCase "dispatch merge: dirty base checkout is fatal" testMergeDirtyBaseFatal
        , testCase "dispatch merge: base moved rebases and re-runs gates" testMergeRebaseRegate
        , testCase "dispatch merge: rebase conflict stays parked, exit 3" testMergeConflictParked
        , testCase "dispatch merge --all: lands clean branches, conflict stays parked, exit 3" testMergeAllPartial
        , testCase "dispatch merge --all: nothing parked is a friendly no-op" testMergeAllEmpty
        , testCase "dispatch merge: --all and DISPATCH_ID are mutually exclusive" testMergeArgValidation
        , testCase "dispatch run (drain): task claimed (in_progress + owner) while worker runs" testDrainClaimsTask
        , testCase "dispatch run TASK_ID: task claimed while worker runs" testTargetedDispatchClaimsTask
        , testCase "dispatch run: racing drains never select the same task" testDrainClaimIsAtomic
        , testCase "dispatch: claude failure blocks task, retains branch, removes worktree" testDispatchFailBlocks
        , testCase "dispatch: dirty tree checkpointed as wip on branch" testDispatchDirtyCheckpoint
        , testCase "dispatch run (drain): a failed dispatch exits 3" testDrainFailedDispatchExits3
        , testCase "dispatch: worktree_setup exit 75 stops drain cleanly" testWorktreeSetup75
        , testCase "dispatch: worktree_setup nonzero errors a single run" testWorktreeSetupErr
        , testCase "dispatch: worktree_teardown runs on success and failure" testWorktreeTeardownRuns
        , testCase "dispatch --dry-run previews dontAsk and worktree path" testDispatchDryRun
        , testCase "dispatch --dry-run previews --mcp-config when set" testDispatchDryRunMcpConfig
        , testCase "dispatch --dry-run: Skill in tools drops --disable-slash-commands" testDispatchDryRunSkillTool
        , testCase "dispatch --dry-run prompt carries built-in agreement with no-user rules" testDryRunBuiltInAgreement
        , testCase "dispatch: agreement_path file wholly replaces built-in body" testDispatchAgreementFile
        , testCase "dispatch: prompt names the resolved scratch path, override or not" testDispatchScratchPathResolved
        , testCase "dispatch: unreadable agreement_path fails closed before worker starts" testAgreementPathUnreadableFailsClosed
        , testCase "dispatch recover: orphaned worktree checkpointed and removed" testDispatchRecoverWorktree
        , testCase "dispatch: no-commit task success is not parked, branch deleted" testDispatchNoCommitSuccess
        , testCase "dispatch review: reviewer sees worker's body-file edits" testReviewerSeesBodyFileEdits
        , testCase "dispatch review: body tamper reported to reviewer, flag persisted" testReviewerBodyTamperReport
        , testCase "dispatch review: retry diffs against first-attempt baseline (no laundering)" testReviewerRetryKeepsTamperBaseline
        , testCase "dispatch review: unreadable prompt_path fails closed before worker starts, dry-run too" testReviewerPromptUnreadableFailsClosed
        , testCase "dispatch: dependent held while dependency parked, eligible after merge" testDispatchDepGateOnMerged
        , testCase "dispatch: untagged task warns on stderr, dry-run and live" testDispatchUntaggedWarns
        ]

-- Scenario 1: dispatch from a checkout on a different branch with a dirty
-- tree; the invoking checkout's branch and dirtiness are untouched, the
-- auto-merge lands on base via a temp worktree, and none are left behind.
testDispatchInvokingCheckoutUntouched :: IO ()
testDispatchInvokingCheckoutUntouched = withDispatchRepo $ \dir db -> do
    tid <- addReadyTask dir db "stub task"
    baseSha <- gitOut dir ["rev-parse", "main"]
    _ <- gitOut dir ["checkout", "-b", "feature"]
    writeFile (dir </> "founder.txt") "uncommitted founder work\n"

    (code, _, _) <- runDispatch dir db (Just "commit") ["dispatch", "run", tid]
    code @?= ExitSuccess

    branch <- gitOut dir ["rev-parse", "--abbrev-ref", "HEAD"]
    branch @?= "feature"
    status <- gitOut dir ["status", "--porcelain"]
    assertBool "founder's dirty file survives" ("founder.txt" `isInfixOf` status)
    mainParent <- gitOut dir ["rev-parse", "main~1"]
    assertBool "base fast-forwarded from its old tip" (mainParent == baseSha)

    brs <- dispatchBranches dir
    assertBool "dispatch branch deleted after landing" (null brs)
    wc <- worktreeCount dir
    wc @?= 1

-- Scenario 2: a child `icarium` run by the worker must resolve ICARIUM_DB
-- (absolute) to the parent store, never create a nested .icarium/icarium.db.
testDispatchNoNestedStore :: IO ()
testDispatchNoNestedStore = withDispatchRepo $ \dir db -> do
    tid <- addReadyTask dir db "nested probe task"
    (code, _, _) <- runDispatch dir db (Just "icarium") ["dispatch", "run", tid]
    code @?= ExitSuccess
    report <- readFile (dir </> ".icarium" </> ("stub-report-" <> tid <> ".txt"))
    assertBool "child icarium exited 0 against the parent DB" ("rc=0" `isInfixOf` report)
    assertBool "no nested .icarium/icarium.db created in the worktree" ("nested=no" `isInfixOf` report)
    (_, listOut, _) <- runDispatch dir db Nothing ["dispatch", "list"]
    assertBool "dispatch landed" ("[success]" `isInfixOf` listOut)

-- Scenario 3: a committed success auto-merges — base advances, branch
-- deleted, dispatch stamped merged, task done, no parked leftovers.
testDispatchAutoMerges :: IO ()
testDispatchAutoMerges = withDispatchRepo $ \dir db -> do
    tid <- addReadyTask dir db "auto-merge task"
    baseSha <- gitOut dir ["rev-parse", "main"]
    (code, out, _) <- runDispatch dir db (Just "commit") ["dispatch", "run", tid]
    code @?= ExitSuccess
    assertBool "landing reported" ("merged" `isInfixOf` out)

    mainAfter <- gitOut dir ["rev-parse", "main"]
    assertBool "base advanced" (mainAfter /= baseSha)
    brs <- dispatchBranches dir
    assertBool "dispatch branch deleted after landing" (null brs)
    (_, parkedOut, _) <- runDispatch dir db Nothing ["dispatch", "list", "--parked"]
    assertBool "nothing parked" (not ("[parked]" `isInfixOf` parkedOut))
    (_, listOut, _) <- runDispatch dir db Nothing ["dispatch", "list"]
    assertBool "badge is [success], not [parked]" ("[success]" `isInfixOf` listOut)
    (_, taskOut, _) <- runDispatch dir db Nothing ["task", "show", tid]
    assertBool "task moved to done" ("done" `isInfixOf` taskOut)
    wc <- worktreeCount dir
    wc @?= 1

-- Scenario 3b: auto-merge blocked (dirty base checkout) — dispatch stays
-- parked, task stays done, exit 3, error names the fixing command.
testDispatchAutoMergeBlockedParks :: IO ()
testDispatchAutoMergeBlockedParks = withDispatchRepo $ \dir db -> do
    tid <- addReadyTask dir db "blocked auto-merge task"
    baseSha <- gitOut dir ["rev-parse", "main"]
    writeFile (dir </> "local.txt") "uncommitted local edit\n"
    (code, _, err) <- runDispatch dir db (Just "commit") ["dispatch", "run", tid]
    code @?= ExitFailure 3
    assertBool "error names the fixing command" ("icarium dispatch merge" `isInfixOf` err)
    park <- parkedId dir db
    assertBool "dispatch parked" (not (null park))
    (_, showOut, _) <- runDispatch dir db Nothing ["dispatch", "show", park]
    assertBool "blocked reason persisted to notes" ("dirty tree" `isInfixOf` showOut)
    mainAfter <- gitOut dir ["rev-parse", "main"]
    mainAfter @?= baseSha
    (_, taskOut, _) <- runDispatch dir db Nothing ["task", "show", tid]
    assertBool "task still done" ("done" `isInfixOf` taskOut)

-- Scenario 3c: the drain auto-merges per success, so a dependency chain
-- lands in a single `dispatch run` with no manual merge interleaving.
testDrainAutoMergesChain :: IO ()
testDrainAutoMergesChain = withDispatchRepo $ \dir db -> do
    depTid <- addReadyTask dir db "chain dependency"
    (_, addOut, _) <-
        runDispatch dir db Nothing ["task", "add", "chain dependent", "--state", "ready-headless", "--depends-on", depTid]
    let dependentTid = head (words addOut)
    (code, out, _) <- runDispatch dir db (Just "commit") ["dispatch", "run"]
    code @?= ExitSuccess
    length (filter ("merged " `isPrefixOf`) (lines out)) @?= 2
    (_, t1, _) <- runDispatch dir db Nothing ["task", "show", depTid]
    assertBool "dependency done" ("done" `isInfixOf` t1)
    (_, t2, _) <- runDispatch dir db Nothing ["task", "show", dependentTid]
    assertBool "dependent done" ("done" `isInfixOf` t2)
    brs <- dispatchBranches dir
    assertBool "no leftover dispatch branches" (null brs)

{- | The drain claims mechanically: by the time the worker runs, the task
is in_progress with an owner stamp — not merely hidden from the ready
queue by its open dispatch row.
-}
testDrainClaimsTask :: IO ()
testDrainClaimsTask = withDispatchRepo $ \dir db -> do
    tid <- addReadyTask dir db "claim probe task"
    (code, _, _) <- runDispatch dir db (Just "claimprobe") ["dispatch", "run"]
    code @?= ExitSuccess
    assertClaimedDuringRun dir tid

-- | A targeted dispatch claims the same way; only selection differs.
testTargetedDispatchClaimsTask :: IO ()
testTargetedDispatchClaimsTask = withDispatchRepo $ \dir db -> do
    tid <- addReadyTask dir db "targeted claim probe"
    (code, _, _) <- runDispatch dir db (Just "claimprobe") ["dispatch", "run", tid]
    code @?= ExitSuccess
    assertClaimedDuringRun dir tid

-- | Read the claimprobe stub's snapshot of `task show` taken mid-dispatch.
assertClaimedDuringRun :: FilePath -> String -> IO ()
assertClaimedDuringRun dir tid = do
    probe <- readFile (dir </> ".icarium" </> ("stub-claim-" <> tid <> ".txt"))
    assertBool ("state in_progress during dispatch: " <> probe) ("in_progress" `isInfixOf` probe)
    assertBool ("owner stamped during dispatch: " <> probe) ("owner:  " `isInfixOf` probe)
    assertBool ("claim time stamped during dispatch: " <> probe) ("claimed:  " `isInfixOf` probe)

{- | Selection and the in-progress transition are one atomic claim, so
racing drains partition the queue. Asserted on the dispatch rows rather
than exit codes: a claimer that loses the write lock is a separate defect
(01KXSQB39R). @nocommit@ keeps the two runs off each other's merges, so
the only thing under test is selection.
-}
testDrainClaimIsAtomic :: IO ()
testDrainClaimIsAtomic = withDispatchRepo $ \dir db -> do
    _ <- addReadyTask dir db "race alpha"
    _ <- addReadyTask dir db "race beta"
    -- Widen the window the old read-then-dispatch shape raced in: without
    -- an atomic claim both drains sit in setup holding the same task.
    writeFile (dir </> "icarium.toml") (stubTomlWith "true" (Just "sleep 1") Nothing)
    boxes <- replicateM 2 newEmptyMVar
    forM_ boxes $ \box -> forkIO $ do
        r <-
            try (runDispatch dir db (Just "nocommit") ["dispatch", "run", "--max", "1"]) ::
                IO (Either IOError (ExitCode, String, String))
        putMVar box r
    mapM_ takeMVar boxes
    (_, listOut, _) <- runDispatch dir db Nothing ["dispatch", "list"]
    let rows = filter (not . null) (lines listOut)
        dispatched t = length (filter (t `isInfixOf`) rows)
    dispatched "race alpha" @?= 1
    dispatched "race beta" @?= 1

-- Scenario 4a: base checked out here and clean -> FF in place, HEAD advances.
testMergeFFInPlace :: IO ()
testMergeFFInPlace = withDispatchRepo $ \dir db -> do
    tid <- addReadyTask dir db "ff task"
    _ <- runDispatchParked dir db [tid]
    before <- gitOut dir ["rev-parse", "HEAD"]
    did <- parkedId dir db
    (code, out, _) <- runDispatch dir db Nothing ["dispatch", "merge", did]
    code @?= ExitSuccess
    assertBool "merge reports landing" ("merged" `isInfixOf` out)
    after <- gitOut dir ["rev-parse", "HEAD"]
    assertBool "HEAD advanced in place" (before /= after)
    onMain <- gitOut dir ["rev-parse", "--abbrev-ref", "HEAD"]
    onMain @?= "main"
    (_, listOut, _) <- runDispatch dir db Nothing ["dispatch", "list"]
    assertBool "badge flips to [success]" ("[success]" `isInfixOf` listOut)
    brs <- dispatchBranches dir
    assertBool "dispatch branch deleted after merge" (null brs)

-- Scenario 4b: base checked out nowhere (HEAD on a feature branch) -> FF via
-- a throwaway worktree; the invoking checkout stays on its branch.
testMergeFFTempWorktree :: IO ()
testMergeFFTempWorktree = withDispatchRepo $ \dir db -> do
    tid <- addReadyTask dir db "ff-temp task"
    baseSha <- gitOut dir ["rev-parse", "main"]
    _ <- runDispatchParked dir db [tid]
    _ <- gitOut dir ["checkout", "-b", "feature"]
    did <- parkedId dir db
    (code, out, _) <- runDispatch dir db Nothing ["dispatch", "merge", did]
    code @?= ExitSuccess
    assertBool "merge reports landing" ("merged" `isInfixOf` out)
    mainAfter <- gitOut dir ["rev-parse", "main"]
    assertBool "base advanced" (mainAfter /= baseSha)
    onFeature <- gitOut dir ["rev-parse", "--abbrev-ref", "HEAD"]
    onFeature @?= "feature"
    brs <- dispatchBranches dir
    assertBool "dispatch branch deleted after merge" (null brs)
    wc <- worktreeCount dir
    wc @?= 1

-- Scenario 4c: base checked out here but dirty -> fatal, stays parked.
testMergeDirtyBaseFatal :: IO ()
testMergeDirtyBaseFatal = withDispatchRepo $ \dir db -> do
    tid <- addReadyTask dir db "dirty-base task"
    _ <- runDispatchParked dir db [tid]
    did <- parkedId dir db
    writeFile (dir </> "local.txt") "uncommitted local edit\n"
    (code, _, err) <- runDispatch dir db Nothing ["dispatch", "merge", did]
    code @?= ExitFailure 1
    assertBool "error names the dirty base tree" ("dirty tree" `isInfixOf` err)
    park <- parkedId dir db
    assertBool "dispatch still parked" (not (null park))

-- Scenario 5a: base moved since park -> merge rebases and re-runs the gates
-- (observable: the test gate appends to a file), then lands.
testMergeRebaseRegate :: IO ()
testMergeRebaseRegate = withDispatchRepo $ \dir db -> do
    let gateLog = dir </> ".icarium" </> "gate.log"
    writeFile (dir </> "icarium.toml") (stubTomlWith ("echo gate-ran >> " <> gateLog) Nothing Nothing)
    tid <- addReadyTask dir db "rebase task"
    _ <- runDispatchParked dir db [tid]
    did <- parkedId dir db
    atPark <- countLines gateLog

    -- move base forward, non-conflicting, while parked
    writeFile (dir </> "base-moved.txt") "founder moved base\n"
    _ <- gitOut dir ["add", "-A"]
    _ <- gitOut dir ["commit", "-m", "founder: base moves"]

    (code, _, err) <- runDispatch dir db Nothing ["dispatch", "merge", did]
    code @?= ExitSuccess
    assertBool "rebase announced on stderr" ("rebasing" `isInfixOf` err)
    atMerge <- countLines gateLog
    assertBool "gates re-ran during merge" (atMerge > atPark)
    (_, listOut, _) <- runDispatch dir db Nothing ["dispatch", "list"]
    assertBool "badge flips to [success]" ("[success]" `isInfixOf` listOut)

-- Scenario 5b: base moved with a conflicting change -> rebase conflict,
-- dispatch stays parked with a note, exit 3, no leftover worktree.
testMergeConflictParked :: IO ()
testMergeConflictParked = withDispatchRepo $ \dir db -> do
    tid <- addReadyTask dir db "conflict task"
    _ <- runDispatchParked dir db [tid]
    did <- parkedId dir db
    -- create a conflicting commit on main touching the stub's file
    writeFile (dir </> ("stub-" <> tid <> ".txt")) "conflicting content\n"
    _ <- gitOut dir ["add", "-A"]
    _ <- gitOut dir ["commit", "-m", "founder: conflicts with parked branch"]

    (code, _, _) <- runDispatch dir db Nothing ["dispatch", "merge", did]
    code @?= ExitFailure 3
    (_, showOut, _) <- runDispatch dir db Nothing ["dispatch", "show", did]
    assertBool "conflict note recorded" ("merge conflict; needs manual rebase onto main" `isInfixOf` showOut)
    park <- parkedId dir db
    assertBool "dispatch still parked" (not (null park))
    wc <- worktreeCount dir
    wc @?= 1

-- Scenario 5c: merge --all drains the parked queue oldest-first. Landing the
-- first moves base, so the rest go through rebase + re-gate; a conflicting
-- one stays parked with a note while later ones still land; exit 3.
testMergeAllPartial :: IO ()
testMergeAllPartial = withDispatchRepo $ \dir db -> do
    tid1 <- addReadyTask dir db "all task one"
    tid2 <- addReadyTask dir db "all task two"
    tid3 <- addReadyTask dir db "all task three"
    -- blocked auto-merges leave all three parked; the drain reports that
    -- as exit 3 (some dispatches stayed parked)
    (code0, _, err0) <- runDispatchParked dir db []
    code0 @?= ExitFailure 3
    assertBool "drain names the bulk fixing command" ("icarium dispatch merge --all" `isInfixOf` err0)
    -- conflict with the third parked branch's file on main
    writeFile (dir </> ("stub-" <> tid3 <> ".txt")) "conflicting content\n"
    _ <- gitOut dir ["add", "-A"]
    _ <- gitOut dir ["commit", "-m", "founder: conflicts with third parked branch"]

    (code, out, err) <- runDispatch dir db Nothing ["dispatch", "merge", "--all"]
    code @?= ExitFailure 3
    length (filter ("merged " `isPrefixOf`) (lines out)) @?= 2
    assertBool "rebase path exercised" ("rebasing" `isInfixOf` err)
    assertBool "blocked line names the conflict" ("blocked" `isInfixOf` out)
    assertBool "summary counts the outcomes" ("2 of 3 landed; 1 still parked" `isInfixOf` out)

    (_, parkedOut, _) <- runDispatch dir db Nothing ["dispatch", "list", "--parked"]
    length (filter (not . null) (lines parkedOut)) @?= 1
    brs <- dispatchBranches dir
    length brs @?= 1
    wc <- worktreeCount dir
    wc @?= 1
    one <- doesFileExist (dir </> ("stub-" <> tid1 <> ".txt"))
    two <- doesFileExist (dir </> ("stub-" <> tid2 <> ".txt"))
    assertBool "both landed branches' files reached main" (one && two)

testMergeAllEmpty :: IO ()
testMergeAllEmpty = withDispatchRepo $ \dir db -> do
    (code, out, _) <- runDispatch dir db Nothing ["dispatch", "merge", "--all"]
    code @?= ExitSuccess
    assertBool "friendly no-op message" ("no parked dispatches" `isInfixOf` out)

testMergeArgValidation :: IO ()
testMergeArgValidation = withDispatchRepo $ \dir db -> do
    (code1, _, err1) <- runDispatch dir db Nothing ["dispatch", "merge", "--all", "deadbeef"]
    code1 @?= ExitFailure 2
    assertBool "mutual exclusion reported" ("mutually exclusive" `isInfixOf` err1)
    (code2, _, err2) <- runDispatch dir db Nothing ["dispatch", "merge"]
    code2 @?= ExitFailure 2
    assertBool "asks for an id or --all" ("DISPATCH_ID or --all" `isInfixOf` err2)

-- Scenario 6a: claude exits nonzero -> failure, task blocked, branch retained,
-- worktree removed, invoking checkout pristine.
testDispatchFailBlocks :: IO ()
testDispatchFailBlocks = withDispatchRepo $ \dir db -> do
    tid <- addReadyTask dir db "fail task"
    baseSha <- gitOut dir ["rev-parse", "main"]
    (code, out, err) <- runDispatch dir db (Just "fail") ["dispatch", "run", tid]
    code @?= ExitFailure 3
    assertBool "reports failure" ("dispatch did not succeed" `isInfixOf` err)
    assertBool "notes carry the exit code" ("claude exited 2" `isInfixOf` out)

    (_, taskOut, _) <- runDispatch dir db Nothing ["task", "show", tid]
    assertBool "task blocked" ("blocked" `isInfixOf` taskOut)
    brs <- dispatchBranches dir
    assertBool "dispatch branch retained for inspection" (not (null brs))
    wc <- worktreeCount dir
    wc @?= 1
    headAfter <- gitOut dir ["rev-parse", "HEAD"]
    headAfter @?= baseSha
    status <- gitOut dir ["status", "--porcelain"]
    assertBool "invoking checkout clean" (null status)

{- | ADR 0009: a drain that dispatched and failed is not a clean drain.
Both tasks fail, the queue then empties, and the run still exits 3 naming
where to look — the drain agrees with the single named form.
-}
testDrainFailedDispatchExits3 :: IO ()
testDrainFailedDispatchExits3 = withDispatchRepo $ \dir db -> do
    tidA <- addReadyTask dir db "drain fail alpha"
    tidB <- addReadyTask dir db "drain fail beta"
    (code, _, err) <- runDispatch dir db (Just "fail") ["dispatch", "run"]
    code @?= ExitFailure 3
    assertBool "drain reports the failures" ("dispatches failed" `isInfixOf` err)
    assertBool "message names where to look" ("dispatch list --outcome failure" `isInfixOf` err)
    assertBool "drain still ran to an empty queue" ("ready queue empty" `isInfixOf` err)
    forM_ [tidA, tidB] $ \tid -> do
        (_, taskOut, _) <- runDispatch dir db Nothing ["task", "show", tid]
        assertBool "task blocked" ("blocked" `isInfixOf` taskOut)

-- Scenario 6b: agent leaves a dirty tree -> failure, the dirty state is
-- checkpointed as a wip commit on the retained dispatch branch.
testDispatchDirtyCheckpoint :: IO ()
testDispatchDirtyCheckpoint = withDispatchRepo $ \dir db -> do
    tid <- addReadyTask dir db "dirty task"
    (code, _, _) <- runDispatch dir db (Just "dirty") ["dispatch", "run", tid]
    code @?= ExitFailure 3
    (_, taskOut, _) <- runDispatch dir db Nothing ["task", "show", tid]
    assertBool "task blocked" ("blocked" `isInfixOf` taskOut)
    brs <- dispatchBranches dir
    assertBool "dispatch branch retained" (not (null brs))
    logOut <- gitOut dir ["log", head brs, "--oneline"]
    assertBool "wip checkpoint on the dispatch branch" ("wip: dispatch" `isInfixOf` logOut)
    wc <- worktreeCount dir
    wc @?= 1
    status <- gitOut dir ["status", "--porcelain"]
    assertBool "invoking checkout clean" (null status)

-- Scenario 7a: worktree_setup exit 75 is back-pressure: drain stops cleanly
-- (exit 0), no dispatch row is created, the task stays ready.
testWorktreeSetup75 :: IO ()
testWorktreeSetup75 = withDispatchRepo $ \dir db -> do
    tid <- addReadyTask dir db "capacity task"
    writeFile (dir </> "icarium.toml") (stubTomlWith "true" (Just "exit 75") Nothing)
    (code, _, err) <- runDispatch dir db (Just "commit") ["dispatch", "run"]
    code @?= ExitSuccess
    assertBool "drain reports no capacity and stops" ("no worktree capacity" `isInfixOf` err)
    (_, listOut, _) <- runDispatch dir db Nothing ["dispatch", "list"]
    assertBool "no dispatch row was created" ("(no dispatches)" `isInfixOf` listOut)
    (_, taskOut, _) <- runDispatch dir db Nothing ["task", "show", tid]
    assertBool "task still ready" ("ready" `isInfixOf` taskOut)

-- Scenario 7b: worktree_setup other-nonzero is an error: single run exits 3,
-- no dispatch row, task untouched.
testWorktreeSetupErr :: IO ()
testWorktreeSetupErr = withDispatchRepo $ \dir db -> do
    tid <- addReadyTask dir db "setup-err task"
    writeFile (dir </> "icarium.toml") (stubTomlWith "true" (Just "exit 1") Nothing)
    (code, _, err) <- runDispatch dir db (Just "commit") ["dispatch", "run", tid]
    code @?= ExitFailure 3
    assertBool "reports a setup failure" ("worktree setup failed" `isInfixOf` err)
    (_, listOut, _) <- runDispatch dir db Nothing ["dispatch", "list"]
    assertBool "no dispatch row was created" ("(no dispatches)" `isInfixOf` listOut)
    (_, taskOut, _) <- runDispatch dir db Nothing ["task", "show", tid]
    assertBool "task still ready" ("ready" `isInfixOf` taskOut)

-- Scenario 7c: worktree_teardown runs on both the success and failure paths.
testWorktreeTeardownRuns :: IO ()
testWorktreeTeardownRuns = withDispatchRepo $ \dir db -> do
    let tdLog = dir </> ".icarium" </> "teardown.log"
    writeFile (dir </> "icarium.toml") (stubTomlWith "true" Nothing (Just ("echo torn >> " <> tdLog)))
    okTid <- addReadyTask dir db "teardown ok task"
    _ <- runDispatch dir db (Just "commit") ["dispatch", "run", okTid]
    afterOk <- countLines tdLog
    afterOk @?= 1
    failTid <- addReadyTask dir db "teardown fail task"
    _ <- runDispatch dir db (Just "fail") ["dispatch", "run", failTid]
    afterFail <- countLines tdLog
    afterFail @?= 2

-- Scenario 8: --dry-run previews the containment flag and the worktree path.
testDispatchDryRun :: IO ()
testDispatchDryRun = withDispatchRepo $ \dir db -> do
    tid <- addReadyTask dir db "dry-run task"
    (code, out, _) <- runDispatch dir db Nothing ["dispatch", "run", tid, "--dry-run"]
    code @?= ExitSuccess
    assertBool "preview shows --permission-mode dontAsk" ("--permission-mode dontAsk" `isInfixOf` out)
    assertBool "preview shows the worktree path" ("worktree:" `isInfixOf` out)
    assertBool "worktree path under .icarium/wt" (".icarium/wt/" `isInfixOf` out)
    assertBool "preview shows --strict-mcp-config" ("--strict-mcp-config" `isInfixOf` out)
    assertBool "no --mcp-config when key absent" (not ("--mcp-config" `isInfixOf` out))
    assertBool "slash commands disabled when Skill not in tools" ("--disable-slash-commands" `isInfixOf` out)

-- Scenario 8c: listing Skill in [dispatch] tools drops --disable-slash-commands
-- (ADR 0003 -- the tools list is the only gate on skill loading).
testDispatchDryRunSkillTool :: IO ()
testDispatchDryRunSkillTool = withDispatchRepo $ \dir db -> do
    let toml =
            unlines
                [ "[project]"
                , "integration_branch = \"main\""
                , "[commands]"
                , "build = \"true\""
                , "test  = \"true\""
                , "[dispatch]"
                , "model  = \"stub\""
                , "effort = \"low\""
                , "tools = [\"Bash\", \"Skill\"]"
                , "allowed_tools = [\"Bash\"]"
                , "scratch_dir = \".icarium/scratch\""
                , "max_minutes_per_dispatch = 2"
                , "heartbeat_stale_seconds  = 300"
                , "log_retention_runs       = 5"
                , "[categories]"
                , "domains     = [\"core\"]"
                , "disciplines = [\"development\"]"
                ]
    writeFile (dir </> "icarium.toml") toml
    tid <- addReadyTask dir db "dry-run skill task"
    (code, out, _) <- runDispatch dir db Nothing ["dispatch", "run", tid, "--dry-run"]
    code @?= ExitSuccess
    assertBool "preview lists the Skill tool" ("Bash,Skill" `isInfixOf` out)
    assertBool "no --disable-slash-commands when Skill in tools" (not ("--disable-slash-commands" `isInfixOf` out))

-- Scenario 8b: --dry-run previews --mcp-config <path> when dispatch.mcp_config is set.
testDispatchDryRunMcpConfig :: IO ()
testDispatchDryRunMcpConfig = withDispatchRepo $ \dir db -> do
    let toml =
            unlines
                [ "[project]"
                , "integration_branch = \"main\""
                , "[commands]"
                , "build = \"true\""
                , "test  = \"true\""
                , "[dispatch]"
                , "model  = \"stub\""
                , "effort = \"low\""
                , "tools = [\"Bash\"]"
                , "allowed_tools = [\"Bash\"]"
                , "scratch_dir = \".icarium/scratch\""
                , "max_minutes_per_dispatch = 2"
                , "heartbeat_stale_seconds  = 300"
                , "log_retention_runs       = 5"
                , "mcp_config = \".mcp.json\""
                , "[categories]"
                , "domains     = [\"core\"]"
                , "disciplines = [\"development\"]"
                ]
    writeFile (dir </> "icarium.toml") toml
    tid <- addReadyTask dir db "dry-run mcp task"
    (code, out, _) <- runDispatch dir db Nothing ["dispatch", "run", tid, "--dry-run"]
    code @?= ExitSuccess
    assertBool "preview shows --strict-mcp-config" ("--strict-mcp-config" `isInfixOf` out)
    assertBool "preview shows --mcp-config .mcp.json" ("--mcp-config .mcp.json" `isInfixOf` out)

-- Scenario: with no agreement_path the dispatch prompt carries the built-in
-- agreement and its no-user counterweight, and drives no tracker CLI — every
-- mutation rides the worker payload through the gate. (The agreement's
-- absence from `task show --prompt` is asserted in testTaskShowPrompt.)
testDryRunBuiltInAgreement :: IO ()
testDryRunBuiltInAgreement = withDispatchRepo $ \dir db -> do
    tid <- addReadyTask dir db "agreement task"
    (code, out, _) <- runDispatch dir db Nothing ["dispatch", "run", tid, "--dry-run"]
    code @?= ExitSuccess
    assertBool "prompt carries the agreement" ("## Working agreement" `isInfixOf` out)
    assertBool "no-user counterweight present" ("Permission denials are policy" `isInfixOf` out)
    assertBool "no CLI escalation command" (not ("icarium task update" `isInfixOf` out))
    assertBool "no CLI learnings recipe" (not ("icarium ctx add" `isInfixOf` out))

-- Scenario: agreement_path file content wholly replaces the built-in
-- agreement body — icarium appends nothing of its own to it.
testDispatchAgreementFile :: IO ()
testDispatchAgreementFile = withDispatchRepo $ \dir db -> do
    tid <- addReadyTask dir db "custom agreement task"
    writeFile (dir </> "agreement.md") "Custom lane contract xagree1.\n"
    writeFile (dir </> "icarium.toml") (agreementToml "agreement.md")
    (code, out, _) <- runDispatch dir db Nothing ["dispatch", "run", tid, "--dry-run"]
    code @?= ExitSuccess
    assertBool "built-in body replaced" (not ("headless dispatch working on this task" `isInfixOf` out))
    -- The whole section, not just "the file is in there": icarium appending
    -- anything of its own after the override is the regression to catch.
    let section =
            filter (not . null)
                . takeWhile (not . ("## " `isPrefixOf`))
                . drop 1
                . dropWhile (/= "## Working agreement")
                $ lines out
    section @?= ["Custom lane contract xagree1."]

{- | Scenario: the worker is told where scratch is by absolute path, not by an
env var it has no permitted way to expand. The path rides a section outside
the agreement body, so an agreement_path override cannot drop it.
-}
testDispatchScratchPathResolved :: IO ()
testDispatchScratchPathResolved = withDispatchRepo $ \dir db -> do
    tid <- addReadyTask dir db "scratch path task"
    (code, out, _) <- runDispatch dir db Nothing ["dispatch", "run", tid, "--dry-run"]
    code @?= ExitSuccess
    assertBool "prompt names the worktree-absolute scratch dir" (namesAbsScratch out)
    -- The override replaces the agreement body; the scratch section survives.
    writeFile (dir </> "agreement.md") "Custom lane contract xagree2.\n"
    writeFile (dir </> "icarium.toml") (agreementToml "agreement.md")
    (oCode, oOut, _) <- runDispatch dir db Nothing ["dispatch", "run", tid, "--dry-run"]
    oCode @?= ExitSuccess
    assertBool "override prompt still names the scratch path" (namesAbsScratch oOut)
  where
    -- Both segments on one line, both absolute: the dry-run header prints
    -- `scratch_dir: .icarium/scratch` relative, so a plain substring test
    -- would pass on the header even with the prompt section deleted.
    namesAbsScratch =
        any (\l -> "/.icarium/wt/" `isInfixOf` l && "/.icarium/scratch" `isInfixOf` l)
            . lines

-- Scenario: an unreadable agreement_path must fail closed before the worker
-- starts (same posture as reviewer prompt_path) — and in dry-run too, which
-- must not preview a prompt the real run would refuse to build.
testAgreementPathUnreadableFailsClosed :: IO ()
testAgreementPathUnreadableFailsClosed = withDispatchRepo $ \dir db -> do
    tid <- addReadyTask dir db "agreement-path task"
    writeFile (dir </> "icarium.toml") (agreementToml "missing-agreement.md")
    (dCode, _, dErr) <- runDispatch dir db Nothing ["dispatch", "run", tid, "--dry-run"]
    dCode @?= ExitFailure 3
    assertBool "dry-run error names the unreadable path" ("missing-agreement.md" `isInfixOf` dErr)
    (code, _, err) <- runDispatch dir db (Just "commit") ["dispatch", "run", tid]
    code @?= ExitFailure 3
    assertBool "error names the unreadable path" ("missing-agreement.md" `isInfixOf` err)
    (_, listOut, _) <- runDispatch dir db Nothing ["dispatch", "list"]
    assertBool "no dispatch row was created" ("(no dispatches)" `isInfixOf` listOut)
    (_, taskOut, _) <- runDispatch dir db Nothing ["task", "show", tid]
    assertBool "task still ready" ("ready" `isInfixOf` taskOut)

-- Scenario 9: recover reconciles an orphaned open dispatch whose worktree
-- survived a crash: dirty state is checkpointed, the worktree is removed,
-- the task is blocked.
testDispatchRecoverWorktree :: IO ()
testDispatchRecoverWorktree = withDispatchRepo $ \dir db -> do
    tid <- addReadyTask dir db "recover task"
    baseSha <- gitOut dir ["rev-parse", "main"]
    let did = "01RECOVER" <> replicate 16 '0' <> "1"
        branch = "dispatch/" <> did
    _ <- gitOut dir ["worktree", "add", ".icarium/wt/" <> did, "-b", branch, "main"]
    writeFile (dir </> ".icarium" </> "wt" </> did </> "orphan.txt") "orphan work\n"
    -- open dispatch row with a dead pid and a stale heartbeat
    conn <- open db
    execute
        conn
        ( Query
            "INSERT INTO dispatches \
            \(id, task_id, branch, base_branch, base_sha, pid, model, effort, \
            \ started_at, heartbeat_at) \
            \VALUES (?,?,?,?,?,?,?,?,datetime('now','-1 hour'),datetime('now','-1 hour'))"
        )
        ( did
        , tid
        , branch
        , "main" :: String
        , baseSha
        , 999999 :: Int
        , "stub" :: String
        , "low" :: String
        )
    close conn
    _ <- runDispatch dir db Nothing ["task", "update", tid, "--state", "in-progress"]

    (code, out, _) <- runDispatch dir db Nothing ["dispatch", "recover"]
    code @?= ExitSuccess
    assertBool "recover reports the worktree was removed" ("worktree=removed" `isInfixOf` out)
    (_, showOut, _) <- runDispatch dir db Nothing ["dispatch", "show", did]
    assertBool "outcome interrupted" ("interrupted" `isInfixOf` showOut)
    assertBool "dirty state was checkpointed" ("uncommitted=yes" `isInfixOf` showOut)
    (_, taskOut, _) <- runDispatch dir db Nothing ["task", "show", tid]
    assertBool "task blocked" ("blocked" `isInfixOf` taskOut)
    wc <- worktreeCount dir
    wc @?= 1

-- Scenario 10: a no-commit task success is stamped merged immediately — it
-- never surfaces as parked, and its empty branch is deleted.
testDispatchNoCommitSuccess :: IO ()
testDispatchNoCommitSuccess = withDispatchRepo $ \dir db -> do
    (_, addOut, _) <- runDispatch dir db Nothing ["task", "add", "no-commit task", "--state", "ready-headless", "--no-commit"]
    let tid = head (words addOut)
    (code, out, _) <- runDispatch dir db (Just "nocommit") ["dispatch", "run", tid]
    code @?= ExitSuccess
    assertBool "outcome success" ("success" `isInfixOf` out)
    (_, taskOut, _) <- runDispatch dir db Nothing ["task", "show", tid]
    assertBool "task moved to done" ("done" `isInfixOf` taskOut)
    (_, listOut, _) <- runDispatch dir db Nothing ["dispatch", "list"]
    assertBool "badge is [success], not [parked]" ("[success]" `isInfixOf` listOut)
    (_, parkedOut, _) <- runDispatch dir db Nothing ["dispatch", "list", "--parked"]
    assertBool "not in the parked list" ("(no dispatches)" `isInfixOf` parkedOut)
    brs <- dispatchBranches dir
    assertBool "empty no-commit branch deleted" (null brs)

{- | Regression for issue #8 (the observed live failure): the stub worker
appends a ## Proof section to the body FILE mid-run; the reviewer must be
judged against that fresh body, not the dispatch-start snapshot. The stub's
reviewer branch records the prompt it received next to the repo.
-}
testReviewerSeesBodyFileEdits :: IO ()
testReviewerSeesBodyFileEdits = withDispatchRepo $ \dir db -> do
    writeFile (dir </> "icarium.toml") (stubToml <> unlines ["[review]", "enabled = true"])
    (_, addOut, _) <- runDispatch dir db Nothing ["task", "add", "proof task", "--state", "ready-headless", "--body", "criteria: prove it"]
    let tid = head (words addOut)
    (code, _, _) <- runDispatch dir db (Just "proof") ["dispatch", "run", tid]
    code @?= ExitSuccess
    reviewerPrompt <- readFile (dir </> ".icarium" </> "stub-reviewer-prompt.txt")
    assertBool "reviewer prompt contains the mid-run body edit" ("xproof1" `isInfixOf` reviewerPrompt)
    -- An appended ## Proof section is an exempt addition: the body-change
    -- report must not flag it.
    assertBool "proof-only addition reports no change" ("task body changed during run: no" `isInfixOf` reviewerPrompt)
    -- The reviewer-time refresh also wrote through to column + FTS. The
    -- sweep cannot explain this: the Done update bumped updated_at past
    -- the file's mtime.
    (_, sOut, _) <- runDispatch dir db Nothing ["search", "xproof1"]
    assertBool "mid-run edit searchable after dispatch" ("proof task" `isInfixOf` sOut)

{- | The tamper signal: a worker that rewrites existing body text (rather
than appending an exempt ## Proof section) produces a reviewer prompt whose
body-change report says yes, quotes the old and new text, and the flag
persists on the dispatch row for post-rotation audit.
-}
testReviewerBodyTamperReport :: IO ()
testReviewerBodyTamperReport = withDispatchRepo $ \dir db -> do
    writeFile (dir </> "icarium.toml") (stubToml <> unlines ["[review]", "enabled = true"])
    (_, addOut, _) <- runDispatch dir db Nothing ["task", "add", "tamper task", "--state", "ready-headless", "--body", "criteria: prove it"]
    let tid = head (words addOut)
    (code, _, _) <- runDispatch dir db (Just "tamper") ["dispatch", "run", tid]
    code @?= ExitSuccess
    reviewerPrompt <- readFile (dir </> ".icarium" </> "stub-reviewer-prompt.txt")
    assertBool "report says yes" ("task body changed during run: yes" `isInfixOf` reviewerPrompt)
    assertBool "old text quoted" ("> criteria: prove it" `isInfixOf` reviewerPrompt)
    assertBool "new text quoted" ("> criteria: tampered xtamper1" `isInfixOf` reviewerPrompt)
    did <- badgedId dir db "[success]"
    (_, showOut, _) <- runDispatch dir db Nothing ["dispatch", "show", did]
    assertBool
        "dispatch show records body_changed yes"
        (any (\l -> words l == ["body_changed:", "yes"]) (lines showOut))

{- | Retry laundering: attempt 1 tampers and is failed by its reviewer;
attempt 2 changes nothing. The retry must diff against the FIRST-attempt
baseline, so attempt 2's report (the captured one — the stub overwrites per
review) still says yes and the surviving dispatch records the flag.
-}
testReviewerRetryKeepsTamperBaseline :: IO ()
testReviewerRetryKeepsTamperBaseline = withDispatchRepo $ \dir db -> do
    writeFile
        (dir </> "icarium.toml")
        (stubToml <> unlines ["[review]", "enabled = true", "max_attempts = 2"])
    (_, addOut, _) <- runDispatch dir db Nothing ["task", "add", "launder task", "--state", "ready-headless", "--body", "criteria: prove it"]
    let tid = head (words addOut)
    (code, _, _) <- runDispatch dir db (Just "launder") ["dispatch", "run", tid]
    code @?= ExitSuccess
    (_, listOut, _) <- runDispatch dir db Nothing ["dispatch", "list"]
    assertBool "attempt 1 recorded as failure" ("[failure]" `isInfixOf` listOut)
    assertBool "attempt 2 landed as success" ("[success]" `isInfixOf` listOut)
    reviewerPrompt <- readFile (dir </> ".icarium" </> "stub-reviewer-prompt.txt")
    assertBool "attempt-2 report still says yes" ("task body changed during run: yes" `isInfixOf` reviewerPrompt)
    assertBool "inherited tamper quoted" ("> criteria: tampered xlaunder1" `isInfixOf` reviewerPrompt)
    did <- badgedId dir db "[success]"
    (_, showOut, _) <- runDispatch dir db Nothing ["dispatch", "show", did]
    assertBool
        "surviving dispatch records body_changed yes"
        (any (\l -> words l == ["body_changed:", "yes"]) (lines showOut))

-- Scenario: an unreadable [review] prompt_path must fail closed before the
-- worker starts, not silently fall back to the built-in prompt — and in
-- dry-run too, which must not preview a run the real run would refuse.
testReviewerPromptUnreadableFailsClosed :: IO ()
testReviewerPromptUnreadableFailsClosed = withDispatchRepo $ \dir db -> do
    tid <- addReadyTask dir db "prompt-path task"
    writeFile
        (dir </> "icarium.toml")
        (stubToml <> unlines ["[review]", "enabled = true", "prompt_path = \"missing-reviewer-prompt.md\""])
    (dCode, _, dErr) <- runDispatch dir db Nothing ["dispatch", "run", tid, "--dry-run"]
    dCode @?= ExitFailure 3
    assertBool "dry-run error names the unreadable path" ("missing-reviewer-prompt.md" `isInfixOf` dErr)
    (code, _, err) <- runDispatch dir db (Just "commit") ["dispatch", "run", tid]
    code @?= ExitFailure 3
    assertBool "error names the unreadable path" ("missing-reviewer-prompt.md" `isInfixOf` err)
    (_, listOut, _) <- runDispatch dir db Nothing ["dispatch", "list"]
    assertBool "no dispatch row was created" ("(no dispatches)" `isInfixOf` listOut)
    (_, taskOut, _) <- runDispatch dir db Nothing ["task", "show", tid]
    assertBool "task still ready" ("ready" `isInfixOf` taskOut)

-- Scenario: a dependency going `done` is not enough for its dependents —
-- its work must be IN base. When the auto-merge blocks (dispatch parked),
-- the merged gate must keep holding the dependent; without it the drain
-- would dispatch against a base missing the dependency's changes.
testDispatchDepGateOnMerged :: IO ()
testDispatchDepGateOnMerged = withDispatchRepo $ \dir db -> do
    depTid <- addReadyTask dir db "dep gate dependency"
    (_, addOut, _) <-
        runDispatch dir db Nothing ["task", "add", "dep gate dependent", "--state", "ready-headless", "--depends-on", depTid]
    let dependentTid = head (words addOut)
    -- drain with the auto-merge blocked: the dependency parks, so the
    -- dependent must not become eligible; the drain reports parked leftovers
    (code, _, drainErr) <- runDispatchParked dir db []
    code @?= ExitFailure 3
    assertBool "drain stopped on empty queue after parking the dependency" ("ready queue empty" `isInfixOf` drainErr)
    -- land the dependency; the dependent becomes eligible
    did <- parkedId dir db
    (mcode, _, _) <- runDispatch dir db Nothing ["dispatch", "merge", did]
    mcode @?= ExitSuccess
    (dCode, _, _) <- runDispatch dir db (Just "commit") ["dispatch", "run"]
    dCode @?= ExitSuccess
    (_, showOut, _) <- runDispatch dir db Nothing ["task", "show", dependentTid]
    assertBool "dependent dispatched once its dependency landed" ("done" `isInfixOf` showOut)

{- | Dispatch consumes the same prompt as `task show --prompt`, so it must
surface the same signal rather than silently running a context-free task.
-}
testDispatchUntaggedWarns :: IO ()
testDispatchUntaggedWarns = withDispatchRepo $ \dir db -> do
    tid <- addReadyTask dir db "untagged dispatch task"
    (code, _, err) <- runDispatch dir db Nothing ["dispatch", "run", tid, "--dry-run"]
    code @?= ExitSuccess
    assertBool "dry-run warns about missing tags" ("warn:" `isInfixOf` err)
    assertBool "warning names the fixing command" ("task update" `isInfixOf` err)

    -- The dry-run path is cheap to cover, but the criterion is about not
    -- silently dispatching for real -- so pin the live path too.
    tid2 <- addReadyTask dir db "untagged live task"
    (code2, _, err2) <- runDispatch dir db (Just "commit") ["dispatch", "run", tid2]
    code2 @?= ExitSuccess
    assertBool "live run warns about missing tags" ("warn:" `isInfixOf` err2)
    assertBool "live warning names the fixing command" ("task update" `isInfixOf` err2)
