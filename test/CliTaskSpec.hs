-- | CLI contract for @icarium task@: CRUD, the ready queue, claims, bodies.
module CliTaskSpec (tests) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (try)
import Control.Monad (forM, forM_, replicateM)
import Data.Aeson.KeyMap qualified as KM
import Data.Char (toLower)
import Data.List (isInfixOf, isPrefixOf, nub, sort, tails)
import System.Exit (ExitCode (..))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase, (@?=))

import CliHelpers (
    commonPrefix,
    decodeOut,
    expectField,
    expectObject,
    jsonIds,
    minimalIcariumToml,
    runIcarium,
    runIcariumIn,
    runIcariumStdin,
    withTempDb,
 )

tests :: TestTree
tests =
    testGroup
        "task"
        [ testCase "task add/list/show roundtrip" testTaskRoundtrip
        , testCase "task ids sort into creation order" testTaskIdsSortByCreation
        , testCase "task update --state changes state" testTaskUpdateState
        , testCase "task list --limit caps rows" testTaskListLimit
        , testCase "task next exits 1 on empty queue" testTaskNextEmpty
        , testCase "task next prints id on non-empty" testTaskNextNonEmpty
        , testCase "task claim exits 1 on empty queue" testTaskClaimEmpty
        , testCase "task claim takes each task at most once" testTaskClaimDistinct
        , testCase "task claim: racing processes partition the queue" testTaskClaimConcurrent
        , testCase "task claim records owner, shown by task show" testTaskClaimOwner
        , testCase "task claim --owner empty exits 2, claims nothing" testTaskClaimEmptyOwner
        , testCase "task done clears the claim" testTaskClaimClearedOnDone
        , testCase "task next/claim serve the interactive queue only" testInteractiveQueueSurfaces
        , testCase "task queue excludes tasks with unsatisfied dependencies" testQueueGatesOnDependencies
        , testCase "task queue interleaves both ready states in priority order" testQueueBothStates
        , testCase "task queue --headless / --interactive narrow; both is refused" testQueueNarrowingFlags
        , testCase "task queue on an empty queue says so" testQueueEmpty
        , testCase "task queue --limit and --json" testQueueLimitAndJson
        , testCase "task queue --interactive head equals task next" testQueueHeadMatchesNext
        , testCase "--ready and bare --state ready are gone" testRemovedReadySurfaces
        , testCase "task claim TASK_ID takes a named task in either ready state" testTaskClaimNamed
        , testCase "task claim TASK_ID refuses a task that is not ready" testTaskClaimNamedNotReady
        , testCase "task add --depends-on bad id exits 2" testTaskAddBadDependsOn
        , testCase "task add --state blocked exits 2" testTaskAddStateBlocked
        , testCase "dispatch quarantine: blocked upstream excludes dependent from ready queue" testDispatchQuarantine
        , testCase "task path → body file contains body" testTaskShowBody
        , testCase "task show --prompt works" testTaskShowPrompt
        , testCase "task add prints id and body path; path matches" testTaskBodyRoundTrip
        , testCase "task show (human) prints body path, not body content" testTaskShowBodyPath
        , testCase "task cat prints body to stdout" testTaskCat
        , testCase "task cat on no-body task prints empty and exits 0" testTaskCatNoBody
        , testCase "task add --no-commit sets flag; task show displays it" testTaskNoCommitAddShow
        , testCase "task update --no-commit and --commit-required toggle flag" testTaskNoCommitUpdate
        , testCase "task add --model/--effort shown by task show; unset omits" testTaskModelEffortAddShow
        , testCase "task update --model/--effort sets and clears" testTaskModelEffortUpdate
        , testCase "task add --effort rejects an unknown value" testTaskEffortInvalid
        , testCase "task exists: found exits 0, not-found exits 1, ambiguous exits 2" testTaskExists
        , testCase "task exists --verbose prints full id on match" testTaskExistsVerbose
        , testCase "task start/done shorthands transition state" testTaskStartDone
        , testCase "task update --state accepts underscore spelling in_progress" testTaskStateUnderscoreAccepted
        , testCase "task add --body-stdin with empty stdin exits 2, files nothing" testAddEmptyBodyStdin
        , testCase "add --body with empty/whitespace text exits 2 (task and ctx)" testAddEmptyBodyInline
        , testCase "task list/show --json: valid JSON, ids, body_path not body" testTaskJson
        , testCase "task show --prompt --json exits 2" testTaskShowPromptJsonConflict
        , testCase "task show --prompt: retired ref delivered, stale ref never" testPromptRetiredRefs
        , testCase "task show --prompt: untagged task warns on stderr" testPromptUntaggedWarns
        , testCase "task show --prompt: kind-only task warns too" testPromptKindOnlyWarns
        , testCase "task show: refs, deps and cats reach both branches" testShowBranchesAgree
        , testCase "task show --prompt: one retrieval axis is quiet" testPromptTaggedQuiet
        , testCase "task add: untagged capture nudges without blocking" testTaskAddUntaggedNudge
        ]

testTaskRoundtrip :: IO ()
testTaskRoundtrip = withTempDb $ \db -> do
    (addCode, addOut, _) <- runIcarium db ["task", "add", "My roundtrip task", "--state", "ready-headless"]
    addCode @?= ExitSuccess
    let tid = head (words addOut)
    assertBool "task id non-empty" (not (null tid))

    (lCode, lOut, _) <- runIcarium db ["task", "list"]
    lCode @?= ExitSuccess
    assertBool "list shows title" ("My roundtrip task" `isInfixOf` lOut)
    assertBool "list shows id prefix" (take 10 tid `isInfixOf` lOut)

    (sCode, sOut, _) <- runIcarium db ["task", "show", take 10 tid]
    sCode @?= ExitSuccess
    assertBool "show contains full id" (tid `isInfixOf` sOut)
    assertBool "show contains title" ("My roundtrip task" `isInfixOf` sOut)

{- | Agents compare task ids to tell which task is newer, so ids must sort
into creation order. See "Icarium.Id" for why that holds across invocations
but not within a single process.
-}
testTaskIdsSortByCreation :: IO ()
testTaskIdsSortByCreation = withTempDb $ \db -> do
    ids <- forM [1 .. 5 :: Int] $ \i -> do
        (code, out, _) <- runIcarium db ["task", "add", "Ordered " ++ show i]
        code @?= ExitSuccess
        pure (head (words out))
    assertBool "ids are 26-char ULIDs" (all ((== 26) . length) ids)
    assertEqual "ids are distinct" 5 (length (nub ids))
    assertEqual "ids sort into creation order" ids (sort ids)

testTaskUpdateState :: IO ()
testTaskUpdateState = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["task", "add", "State change task", "--state", "planned"]
    let tid = head (words addOut)

    (uCode, _, _) <- runIcarium db ["task", "update", tid, "--state", "ready-headless"]
    uCode @?= ExitSuccess

    (lCode, lOut, _) <- runIcarium db ["task", "list", "--state", "ready-headless"]
    lCode @?= ExitSuccess
    assertBool "updated task appears in ready list" ("State change task" `isInfixOf` lOut)

    (lCode2, lOut2, _) <- runIcarium db ["task", "list", "--state", "planned"]
    lCode2 @?= ExitSuccess
    assertBool "task no longer in planned list" (not ("State change task" `isInfixOf` lOut2))

testTaskListLimit :: IO ()
testTaskListLimit = withTempDb $ \db -> do
    mapM_ (\i -> runIcarium db ["task", "add", "Task " ++ show (i :: Int), "--state", "ready-headless"]) [1 .. 5 :: Int]
    (code, out, _) <- runIcarium db ["task", "list", "--limit", "3"]
    code @?= ExitSuccess
    let rows = filter (not . null) (lines out)
    length rows @?= 3

testTaskNextEmpty :: IO ()
testTaskNextEmpty = withTempDb $ \db -> do
    (code, _, _) <- runIcarium db ["task", "next"]
    code @?= ExitFailure 1

testTaskNextNonEmpty :: IO ()
testTaskNextNonEmpty = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["task", "add", "Next task", "--state", "ready-interactive"]
    let tid = head (words addOut)

    (code, nextOut, _) <- runIcarium db ["task", "next"]
    code @?= ExitSuccess
    assertBool "next output is the task id" (tid `isInfixOf` nextOut)

testTaskClaimEmpty :: IO ()
testTaskClaimEmpty = withTempDb $ \db -> do
    (code, out, _) <- runIcarium db ["task", "claim"]
    code @?= ExitFailure 1
    out @?= ""

{- | Two claims against a two-task queue must hand out both ids and never
repeat one, in `task next` priority order; a third finds the queue drained.
-}
testTaskClaimDistinct :: IO ()
testTaskClaimDistinct = withTempDb $ \db -> do
    (_, loOut, _) <- runIcarium db ["task", "add", "Low", "--state", "ready-interactive", "--priority", "1"]
    (_, hiOut, _) <- runIcarium db ["task", "add", "High", "--state", "ready-interactive", "--priority", "9"]
    let loId = head (words loOut)
        hiId = head (words hiOut)

    (_, nextOut, _) <- runIcarium db ["task", "next"]
    (c1, out1, _) <- runIcarium db ["task", "claim"]
    c1 @?= ExitSuccess
    words out1 @?= [hiId]
    assertBool "claim agrees with next" (words nextOut == words out1)

    (c2, out2, _) <- runIcarium db ["task", "claim"]
    c2 @?= ExitSuccess
    words out2 @?= [loId]

    (c3, _, _) <- runIcarium db ["task", "claim"]
    c3 @?= ExitFailure 1

{- | The atomicity guarantee, observed rather than reasoned about: claims
racing from separate processes must partition the queue. BEGIN IMMEDIATE
plus busy_timeout means every claimer succeeds; none may repeat an id.
-}
testTaskClaimConcurrent :: IO ()
testTaskClaimConcurrent = withTempDb $ \db -> do
    ids <- forM [1 .. 4 :: Int] $ \i -> do
        (_, out, _) <- runIcarium db ["task", "add", "Race " ++ show i, "--state", "ready-interactive"]
        pure (head (words out))
    boxes <- replicateM 4 newEmptyMVar
    forM_ boxes $ \box -> forkIO $ do
        r <- try (runIcarium db ["task", "claim"]) :: IO (Either IOError (ExitCode, String, String))
        putMVar box r
    results <- mapM takeMVar boxes
    claimed <- forM results $ \case
        Left e -> error ("claim process failed to run: " <> show e)
        Right (code, out, err) -> do
            assertEqual ("claim exited " <> show code <> "; stderr: " <> err) ExitSuccess code
            assertBool ("no busy error: " <> err) (not ("busy" `isInfixOf` map toLower err))
            pure (head (words out))
    sort claimed @?= sort ids
    (c5, _, _) <- runIcarium db ["task", "claim"]
    c5 @?= ExitFailure 1

testTaskClaimOwner :: IO ()
testTaskClaimOwner = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["task", "add", "Claimable", "--state", "ready-interactive"]
    let tid = head (words addOut)

    (code, claimOut, _) <- runIcarium db ["task", "claim", "--owner", "agent-7"]
    code @?= ExitSuccess
    words claimOut @?= [tid]

    (_, showOut, _) <- runIcarium db ["task", "show", tid]
    assertBool "state is in_progress" ("in_progress" `isInfixOf` showOut)
    assertBool "owner shown" ("owner:     agent-7" `isInfixOf` showOut)
    assertBool "claim time shown" ("claimed:   " `isInfixOf` showOut)

-- | Guard runs before any DB I/O, so an empty owner never reaches the queue.
testTaskClaimEmptyOwner :: IO ()
testTaskClaimEmptyOwner = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["task", "add", "Claimable", "--state", "ready-interactive"]
    let tid = head (words addOut)

    (code, _, err) <- runIcarium db ["task", "claim", "--owner", "  "]
    code @?= ExitFailure 2
    assertBool "error names the flag" ("--owner" `isInfixOf` err)

    -- The task must still be claimable.
    (nCode, nOut, _) <- runIcarium db ["task", "next"]
    nCode @?= ExitSuccess
    words nOut @?= [tid]

-- | A claim must not outlive the work: leaving in_progress drops the stamp.
testTaskClaimClearedOnDone :: IO ()
testTaskClaimClearedOnDone = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["task", "add", "Claimable", "--state", "ready-interactive"]
    let tid = head (words addOut)
    (_, _, _) <- runIcarium db ["task", "claim", "--owner", "agent-7"]

    (code, _, _) <- runIcarium db ["task", "done", tid]
    code @?= ExitSuccess
    (_, showOut, _) <- runIcarium db ["task", "show", tid]
    assertBool "owner cleared" (not ("agent-7" `isInfixOf` showOut))
    assertBool "claim time cleared" (not ("claimed:" `isInfixOf` showOut))

{- | The CLI queue serves the human. Headless work sitting in `ready` is
dispatch's to take, and must never be handed to `task next`/`task claim`.
-}
testInteractiveQueueSurfaces :: IO ()
testInteractiveQueueSurfaces = withTempDb $ \db -> do
    (_, hOut, _) <- runIcarium db ["task", "add", "Headless", "--state", "ready-headless"]
    let hId = head (words hOut)

    (nCode, _, _) <- runIcarium db ["task", "next"]
    nCode @?= ExitFailure 1
    (cCode, _, _) <- runIcarium db ["task", "claim"]
    cCode @?= ExitFailure 1

    (_, iOut, _) <- runIcarium db ["task", "add", "Interactive", "--state", "ready-interactive"]
    let iId = head (words iOut)
    (nCode2, nOut, _) <- runIcarium db ["task", "next"]
    nCode2 @?= ExitSuccess
    words nOut @?= [iId]

    (cCode2, cOut, _) <- runIcarium db ["task", "claim"]
    cCode2 @?= ExitSuccess
    words cOut @?= [iId]

    -- The headless task is untouched by either surface.
    (_, showOut, _) <- runIcarium db ["task", "show", hId]
    assertBool "headless task still ready" ("ready" `isInfixOf` showOut)
    assertBool "headless task not claimed" (not ("claimed:" `isInfixOf` showOut))

{- | The gate's user-visible contract: `queue` hides a task whose
dependency is unmet, `list` — a pure filter — still shows it.
-}
testQueueGatesOnDependencies :: IO ()
testQueueGatesOnDependencies = withTempDb $ \db -> do
    (_, depOut, _) <- runIcarium db ["task", "add", "Blocker", "--state", "planned"]
    let depId = head (words depOut)
    (_, bOut, _) <- runIcarium db ["task", "add", "Blocked dependent", "--state", "ready-headless", "--depends-on", depId]
    let bId = head (words bOut)

    (qCode, qOut, _) <- runIcarium db ["task", "queue"]
    qCode @?= ExitSuccess
    assertBool "unsatisfied dependency is out of the queue" (not (take 10 bId `isInfixOf` qOut))

    (lCode, lOut, _) <- runIcarium db ["task", "list", "--state", "ready-headless"]
    lCode @?= ExitSuccess
    assertBool "state filter applies no gate" (take 10 bId `isInfixOf` lOut)

-- | Bare `queue` interleaves both queues in priority order, badged by state.
testQueueBothStates :: IO ()
testQueueBothStates = withTempDb $ \db -> do
    (_, lowOut, _) <- runIcarium db ["task", "add", "Low headless", "--state", "ready-headless", "--priority", "1"]
    (_, hiOut, _) <- runIcarium db ["task", "add", "High interactive", "--state", "ready-interactive", "--priority", "9"]
    let lowId = head (words lowOut)
        hiId = head (words hiOut)

    (code, out, _) <- runIcarium db ["task", "queue"]
    code @?= ExitSuccess
    assertBool "headless row present" (take 10 lowId `isInfixOf` out)
    assertBool "interactive row present" (take 10 hiId `isInfixOf` out)
    assertBool "state badges distinguish rows" ("[ready-headless]" `isInfixOf` out)
    assertBool "state badges distinguish rows" ("[ready-interactive]" `isInfixOf` out)
    let idx sub = length (takeWhile (not . isPrefixOf sub) (tails out))
    assertBool "higher priority first" (idx (take 10 hiId) < idx (take 10 lowId))

testQueueNarrowingFlags :: IO ()
testQueueNarrowingFlags = withTempDb $ \db -> do
    (_, hOut, _) <- runIcarium db ["task", "add", "Headless", "--state", "ready-headless"]
    (_, iOut, _) <- runIcarium db ["task", "add", "Interactive", "--state", "ready-interactive"]
    let hId = take 10 (head (words hOut))
        iId = take 10 (head (words iOut))

    (hCode, hQ, _) <- runIcarium db ["task", "queue", "--headless"]
    hCode @?= ExitSuccess
    assertBool "--headless keeps headless" (hId `isInfixOf` hQ)
    assertBool "--headless drops interactive" (not (iId `isInfixOf` hQ))

    (iCode, iQ, _) <- runIcarium db ["task", "queue", "--interactive"]
    iCode @?= ExitSuccess
    assertBool "--interactive keeps interactive" (iId `isInfixOf` iQ)
    assertBool "--interactive drops headless" (not (hId `isInfixOf` iQ))

    -- A contradictory request has no honest answer: refuse rather than pick one.
    (xCode, _, xErr) <- runIcarium db ["task", "queue", "--headless", "--interactive"]
    xCode @?= ExitFailure 2
    assertBool "error names both flags" ("--headless" `isInfixOf` xErr && "--interactive" `isInfixOf` xErr)

testQueueEmpty :: IO ()
testQueueEmpty = withTempDb $ \db -> do
    (code, out, _) <- runIcarium db ["task", "queue"]
    code @?= ExitSuccess
    assertBool "empty queue says so rather than printing nothing" ("no tasks" `isInfixOf` out)

testQueueLimitAndJson :: IO ()
testQueueLimitAndJson = withTempDb $ \db -> do
    mapM_
        (\i -> runIcarium db ["task", "add", "Q" ++ show (i :: Int), "--state", "ready-headless", "--priority", show (10 - i)])
        [1 .. 3 :: Int]

    (lCode, lOut, _) <- runIcarium db ["task", "queue", "--limit", "1"]
    lCode @?= ExitSuccess
    length (filter (isInfixOf "[ready-headless]") (lines lOut)) @?= 1

    (jCode, jOut, _) <- runIcarium db ["task", "queue", "--json"]
    jCode @?= ExitSuccess
    assertBool "json carries the stored state spelling" ("ready_headless" `isInfixOf` jOut)

{- | Pins `task next` to the queue it prints from, so the two commands
cannot drift apart on ordering.
-}
testQueueHeadMatchesNext :: IO ()
testQueueHeadMatchesNext = withTempDb $ \db -> do
    -- Two tasks tie on priority, so the tiebreak is exercised, and one outranks
    -- both — the head is only unambiguous if queue and next order identically.
    _ <- runIcarium db ["task", "add", "First", "--state", "ready-interactive", "--priority", "5"]
    _ <- runIcarium db ["task", "add", "Second", "--state", "ready-interactive", "--priority", "5"]
    _ <- runIcarium db ["task", "add", "Top", "--state", "ready-interactive", "--priority", "8"]

    (qCode, qOut, _) <- runIcarium db ["task", "queue", "--interactive", "--limit", "1"]
    qCode @?= ExitSuccess
    (nCode, nOut, _) <- runIcarium db ["task", "next"]
    nCode @?= ExitSuccess
    let nextId = head (words nOut)
    assertBool "queue head is what next prints" (take 10 nextId `isInfixOf` qOut)

-- | The removed surfaces must fail visibly, not drift.
testRemovedReadySurfaces :: IO ()
testRemovedReadySurfaces = withTempDb $ \db -> do
    (fCode, _, _) <- runIcarium db ["task", "list", "--ready"]
    fCode @?= ExitFailure 1

    (sCode, _, sErr) <- runIcarium db ["task", "list", "--state", "ready"]
    sCode @?= ExitFailure 1
    assertBool "error lists the valid states" ("ready-headless" `isInfixOf` sErr && "ready-interactive" `isInfixOf` sErr)

testTaskClaimNamed :: IO ()
testTaskClaimNamed = withTempDb $ \db -> do
    (_, hOut, _) <- runIcarium db ["task", "add", "Headless", "--state", "ready-headless"]
    (_, iOut, _) <- runIcarium db ["task", "add", "Interactive", "--state", "ready-interactive"]
    let hId = head (words hOut)
        iId = head (words iOut)

    (hCode, hClaim, _) <- runIcarium db ["task", "claim", hId, "--owner", "agent-7"]
    hCode @?= ExitSuccess
    words hClaim @?= [hId]
    (_, hShow, _) <- runIcarium db ["task", "show", hId]
    assertBool "named claim marks in_progress" ("in_progress" `isInfixOf` hShow)
    assertBool "named claim stamps the owner" ("owner:     agent-7" `isInfixOf` hShow)

    (iCode, iClaim, _) <- runIcarium db ["task", "claim", iId]
    iCode @?= ExitSuccess
    words iClaim @?= [iId]

testTaskClaimNamedNotReady :: IO ()
testTaskClaimNamedNotReady = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["task", "add", "Under-specified", "--state", "planned"]
    let tid = head (words addOut)

    (code, _, err) <- runIcarium db ["task", "claim", tid]
    code @?= ExitFailure 1
    assertBool "error names the state" ("planned" `isInfixOf` err)
    assertBool "error names the fixing command" ("task update" `isInfixOf` err)

    (_, showOut, _) <- runIcarium db ["task", "show", tid]
    assertBool "task left alone" (not ("in_progress" `isInfixOf` showOut))

testTaskAddBadDependsOn :: IO ()
testTaskAddBadDependsOn = withTempDb $ \db -> do
    (code, _, err) <- runIcarium db ["task", "add", "Dependent", "--depends-on", "01NONEXISTENT"]
    code @?= ExitFailure 2
    assertBool "error on stderr" (not (null err))

testTaskAddStateBlocked :: IO ()
testTaskAddStateBlocked = withTempDb $ \db -> do
    (code, _, err) <- runIcarium db ["task", "add", "Bad state", "--state", "blocked"]
    code @?= ExitFailure 2
    assertBool "error mentions state restriction" ("state" `isInfixOf` err)

{- | Quarantine contract: a failed dispatch sets its task to 'blocked'.
The ready_tasks view (used by dispatch run and task next) excludes any
task whose depends_on target is not 'done', so the dependent is silently
quarantined until the upstream is resolved. Independent tasks keep
draining normally.

We simulate a failed dispatch by blocking task A directly; the view
doesn't care how it got there.
-}
testDispatchQuarantine :: IO ()
testDispatchQuarantine = withTempDb $ \db -> do
    -- A: will be blocked (simulating a failed dispatch)
    (_, aOut, _) <- runIcarium db ["task", "add", "Upstream task A", "--state", "ready-headless"]
    let aId = head (words aOut)

    -- B: depends on A; must be quarantined when A is blocked
    (_, bOut, _) <- runIcarium db ["task", "add", "Dependent task B", "--state", "ready-interactive", "--depends-on", aId]
    let bId = head (words bOut)

    -- C: independent; must still be drainable after A is blocked
    (_, cOut, _) <- runIcarium db ["task", "add", "Independent task C", "--state", "ready-interactive"]
    let cId = head (words cOut)

    -- Simulate a dispatch failure: mark A blocked with a reason
    (uCode, _, _) <- runIcarium db ["task", "update", aId, "--state", "blocked", "--block-reason", "simulated dispatch failure"]
    uCode @?= ExitSuccess

    -- the queue (task queue / task next) must exclude B but include C
    (lCode, lOut, _) <- runIcarium db ["task", "queue"]
    lCode @?= ExitSuccess
    assertBool "dependent B absent from ready queue" (not ("Dependent task B" `isInfixOf` lOut))
    assertBool "independent C present in ready queue" ("Independent task C" `isInfixOf` lOut)

    -- task next returns C's full id (the head of what drain would pick), not B's
    (nCode, nOut, _) <- runIcarium db ["task", "next"]
    nCode @?= ExitSuccess
    assertBool "task next picks C, not B" (cId `isInfixOf` nOut)
    assertBool "task next does not pick B" (not (bId `isInfixOf` nOut))

testTaskShowBody :: IO ()
testTaskShowBody = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["task", "add", "Body test task", "--body", "hello world body"]
    let tid = head (words addOut)

    (pCode, pathOut, _) <- runIcarium db ["task", "path", tid]
    pCode @?= ExitSuccess
    let bodyPath = head (lines pathOut)
    contents <- readFile bodyPath
    contents @?= "hello world body"

testTaskShowPrompt :: IO ()
testTaskShowPrompt = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["task", "add", "Prompt test task", "--state", "ready-headless"]
    let tid = head (words addOut)

    (code, out, _) <- runIcarium db ["task", "show", tid, "--prompt"]
    code @?= ExitSuccess
    assertBool "prompt output contains task id" (tid `isInfixOf` out)
    assertBool "prompt output contains title" ("Prompt test task" `isInfixOf` out)
    -- The agreement is dispatch-only; interactive builders consume this
    -- output and must not inherit headless lane rules (issue #11).
    assertBool "no working agreement in shared prompt" (not ("## Working agreement" `isInfixOf` out))

testTaskBodyRoundTrip :: IO ()
testTaskBodyRoundTrip = withSystemTempDirectory "icarium-test" $ \dir -> do
    let db = dir <> "/icarium.db"

    (_, addOut, _) <- runIcarium db ["task", "add", "Round-trip task", "--body", "original body\nline two"]
    let outLines = lines addOut
        tid = head outLines
        bodyPath = outLines !! 1

    -- Read the body file written by `task add`.
    contents <- readFile bodyPath
    contents @?= "original body\nline two"

    -- The path printed by `task path` should match the path from `task add`.
    (_, pathOut, _) <- runIcarium db ["task", "path", tid]
    head (lines pathOut) @?= bodyPath

testTaskShowBodyPath :: IO ()
testTaskShowBodyPath = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["task", "add", "Body path test task", "--body", "secret body content"]
    let outLines = lines addOut
        tid = head outLines
        bodyPath = outLines !! 1

    (code, out, _) <- runIcarium db ["task", "show", tid]
    code @?= ExitSuccess
    assertBool "show contains body path" (bodyPath `isInfixOf` out)
    assertBool "show does not contain body content" (not ("secret body content" `isInfixOf` out))
    assertBool "show does not have ## Body header" (not ("## Body" `isInfixOf` out))

testTaskCat :: IO ()
testTaskCat = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["task", "add", "Cat task", "--body", "body line one\nbody line two"]
    let tid = head (lines addOut)

    (code, out, _) <- runIcarium db ["task", "cat", tid]
    code @?= ExitSuccess
    out @?= "body line one\nbody line two"

testTaskCatNoBody :: IO ()
testTaskCatNoBody = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["task", "add", "No body task"]
    let tid = head (lines addOut)

    (code, out, _) <- runIcarium db ["task", "cat", tid]
    code @?= ExitSuccess
    out @?= ""

testTaskNoCommitAddShow :: IO ()
testTaskNoCommitAddShow = withTempDb $ \db -> do
    (addCode, addOut, _) <- runIcarium db ["task", "add", "Side-effect task", "--no-commit"]
    addCode @?= ExitSuccess
    let tid = head (words addOut)

    (showCode, showOut, _) <- runIcarium db ["task", "show", tid]
    showCode @?= ExitSuccess
    assertBool "no-commit shown in task show" ("no-commit" `isInfixOf` showOut)
    assertBool "no-commit value is yes" ("yes" `isInfixOf` showOut)

    (addCode2, addOut2, _) <- runIcarium db ["task", "add", "Regular task"]
    addCode2 @?= ExitSuccess
    let tid2 = head (words addOut2)

    (showCode2, showOut2, _) <- runIcarium db ["task", "show", tid2]
    showCode2 @?= ExitSuccess
    assertBool "no-commit absent for regular task" (not ("no-commit" `isInfixOf` showOut2))

testTaskNoCommitUpdate :: IO ()
testTaskNoCommitUpdate = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["task", "add", "Update flag task"]
    let tid = head (words addOut)

    (uCode, _, _) <- runIcarium db ["task", "update", tid, "--no-commit"]
    uCode @?= ExitSuccess
    (_, showOut, _) <- runIcarium db ["task", "show", tid]
    assertBool "no-commit set after --no-commit" ("no-commit" `isInfixOf` showOut)

    (uCode2, _, _) <- runIcarium db ["task", "update", tid, "--commit-required"]
    uCode2 @?= ExitSuccess
    (_, showOut2, _) <- runIcarium db ["task", "show", tid]
    assertBool "no-commit cleared after --commit-required" (not ("no-commit" `isInfixOf` showOut2))

testTaskModelEffortAddShow :: IO ()
testTaskModelEffortAddShow = withTempDb $ \db -> do
    (addCode, addOut, _) <-
        runIcarium db ["task", "add", "Cheap task", "--model", "claude-haiku-4-5-20251001", "--effort", "low"]
    addCode @?= ExitSuccess
    let tid = head (words addOut)

    (showCode, showOut, _) <- runIcarium db ["task", "show", tid]
    showCode @?= ExitSuccess
    assertBool "model shown in task show" ("claude-haiku-4-5-20251001" `isInfixOf` showOut)
    assertBool "effort shown in task show" ("low" `isInfixOf` showOut)

    (jCode, jOut, _) <- runIcarium db ["task", "show", tid, "--json"]
    jCode @?= ExitSuccess
    let obj = expectObject (decodeOut jOut)
    expectField "model" obj @?= "claude-haiku-4-5-20251001"
    expectField "effort" obj @?= "low"

    (_, addOut2, _) <- runIcarium db ["task", "add", "Default task"]
    let tid2 = head (words addOut2)
    (_, showOut2, _) <- runIcarium db ["task", "show", tid2]
    assertBool "model line absent when unset" (not ("model:" `isInfixOf` showOut2))
    assertBool "effort line absent when unset" (not ("effort:" `isInfixOf` showOut2))

testTaskModelEffortUpdate :: IO ()
testTaskModelEffortUpdate = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["task", "add", "Retier me"]
    let tid = head (words addOut)

    (uCode, _, _) <- runIcarium db ["task", "update", tid, "--model", "claude-opus-4-8", "--effort", "xhigh"]
    uCode @?= ExitSuccess
    (_, showOut, _) <- runIcarium db ["task", "show", tid]
    assertBool "model set" ("claude-opus-4-8" `isInfixOf` showOut)
    assertBool "effort set" ("xhigh" `isInfixOf` showOut)

    -- Empty string clears, matching the category-axis flags.
    (cCode, _, _) <- runIcarium db ["task", "update", tid, "--model", "", "--effort", ""]
    cCode @?= ExitSuccess
    (_, showOut2, _) <- runIcarium db ["task", "show", tid]
    assertBool "model cleared" (not ("model:" `isInfixOf` showOut2))
    assertBool "effort cleared" (not ("effort:" `isInfixOf` showOut2))

testTaskEffortInvalid :: IO ()
testTaskEffortInvalid = withTempDb $ \db -> do
    (code, _, err) <- runIcarium db ["task", "add", "Bad effort", "--effort", "turbo"]
    code @?= ExitFailure 1
    assertBool "names the bad effort" ("turbo" `isInfixOf` err)

testTaskExists :: IO ()
testTaskExists = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["task", "add", "Exists task"]
    let tid = head (words addOut)

    -- found: full id → exit 0
    (foundCode, foundOut, _) <- runIcarium db ["task", "exists", tid]
    foundCode @?= ExitSuccess
    foundOut @?= ""

    -- found: prefix → exit 0
    (prefCode, _, _) <- runIcarium db ["task", "exists", take 10 tid]
    prefCode @?= ExitSuccess

    -- not found → exit 1
    (missCode, _, _) <- runIcarium db ["task", "exists", "01ZZZZZZZZZZZZZZZZZZZZZZZZ"]
    missCode @?= ExitFailure 1

    -- ambiguous: add a second task and use the prefix the two ids share
    (_, addOut2, _) <- runIcarium db ["task", "add", "Exists task 2"]
    let tid2 = head (words addOut2)
    let sharedPrefix = commonPrefix tid tid2
    assertBool "ULIDs from one DB share a leading prefix" (not (null sharedPrefix))
    (ambCode, _, ambErr) <- runIcarium db ["task", "exists", sharedPrefix]
    ambCode @?= ExitFailure 2
    ambErr @?= "ambiguous: " <> sharedPrefix <> " matches 2 tasks\n"

testTaskExistsVerbose :: IO ()
testTaskExistsVerbose = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["task", "add", "Verbose exists task"]
    let tid = head (words addOut)

    (code, out, _) <- runIcarium db ["task", "exists", "--verbose", take 10 tid]
    code @?= ExitSuccess
    assertBool "verbose output contains full id" (tid `isInfixOf` out)

testTaskStartDone :: IO ()
testTaskStartDone = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["task", "add", "shorthand task", "--state", "ready-headless"]
    let tid = head (words addOut)
    (sCode, sOut, _) <- runIcarium db ["task", "start", tid]
    sCode @?= ExitSuccess
    assertBool "start prints updated" ("updated" `isInfixOf` sOut)
    (_, showOut, _) <- runIcarium db ["task", "show", tid]
    assertBool "state is in_progress after start" ("in_progress" `isInfixOf` showOut)
    (dCode, _, _) <- runIcarium db ["task", "done", tid]
    dCode @?= ExitSuccess
    (_, showOut2, _) <- runIcarium db ["task", "show", tid]
    assertBool "state is done after done" ("done" `isInfixOf` showOut2)

testTaskStateUnderscoreAccepted :: IO ()
testTaskStateUnderscoreAccepted = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["task", "add", "underscore state task"]
    let tid = head (words addOut)
    (code, _, _) <- runIcarium db ["task", "update", tid, "--state", "in_progress"]
    code @?= ExitSuccess

testAddEmptyBodyStdin :: IO ()
testAddEmptyBodyStdin = withTempDb $ \db -> do
    (code, _, err) <- runIcariumStdin db "" ["task", "add", "empty stdin task", "--body-stdin"]
    code @?= ExitFailure 2
    assertBool "error names the empty body" ("empty body" `isInfixOf` err)
    (_, lOut, _) <- runIcarium db ["task", "list"]
    assertBool "nothing was filed" (not ("empty stdin task" `isInfixOf` lOut))

testAddEmptyBodyInline :: IO ()
testAddEmptyBodyInline = withTempDb $ \db -> do
    (tCode, _, tErr) <- runIcarium db ["task", "add", "ws body task", "--body", "  \n "]
    tCode @?= ExitFailure 2
    assertBool "task error names empty body" ("empty body" `isInfixOf` tErr)
    (cCode, _, cErr) <- runIcarium db ["ctx", "add", "ws body ctx", "--body", ""]
    cCode @?= ExitFailure 2
    assertBool "ctx error names empty body" ("empty body" `isInfixOf` cErr)

testTaskJson :: IO ()
testTaskJson = withTempDb $ \db -> do
    (emptyCode, emptyOut, _) <- runIcarium db ["task", "list", "--json"]
    emptyCode @?= ExitSuccess
    jsonIds emptyOut @?= []

    (_, addOut, _) <-
        runIcarium db ["task", "add", "Json task", "--state", "ready-headless", "--body", "unmistakable body prose"]
    let tid = head (words addOut)

    (lCode, lOut, _) <- runIcarium db ["task", "list", "--json"]
    lCode @?= ExitSuccess
    jsonIds lOut @?= [tid]

    (sCode, sOut, _) <- runIcarium db ["task", "show", take 10 tid, "--json"]
    sCode @?= ExitSuccess
    let o = expectObject (decodeOut sOut)
    expectField "id" o @?= tid
    expectField "state" o @?= "ready_headless"
    assertBool "show carries body_path" (KM.member "body_path" o)
    assertBool "show omits body content" (not ("unmistakable body prose" `isInfixOf` sOut))

testTaskShowPromptJsonConflict :: IO ()
testTaskShowPromptJsonConflict = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["task", "add", "conflicting flags task"]
    let tid = head (words addOut)
    (code, out, err) <- runIcarium db ["task", "show", tid, "--prompt", "--json"]
    code @?= ExitFailure 2
    out @?= ""
    assertBool "stderr names both flags" ("--prompt" `isInfixOf` err && "--json" `isInfixOf` err)

testPromptRetiredRefs :: IO ()
testPromptRetiredRefs = withTempDb $ \db -> do
    (_, tOut, _) <- runIcarium db ["task", "add", "prompt task", "--state", "ready-headless"]
    let tid = take 10 (head (words tOut))
    (_, rOut, _) <- runIcarium db ["ctx", "add", "refactored ref", "--body", "xrefactorbody"]
    let rId = take 10 (head (words rOut))
    (_, sOut, _) <- runIcarium db ["ctx", "add", "staled ref", "--body", "xstalebody"]
    let sId = take 10 (head (words sOut))
    (_, _, _) <- runIcarium db ["link", "add", tid, "references", rId]
    (_, _, _) <- runIcarium db ["link", "add", tid, "references", sId]
    (_, tOut2, _) <- runIcarium db ["task", "add", "refactor target", "--state", "ready-headless"]
    let tid2 = take 10 (head (words tOut2))
    (_, _, _) <- runIcarium db ["ctx", "curate", rId, "refactor", "--artifact", tid2]
    (_, _, _) <- runIcarium db ["ctx", "curate", sId, "stale"]
    (code, out, _) <- runIcarium db ["task", "show", tid, "--prompt"]
    code @?= ExitSuccess
    assertBool "refactor-retired ref still injected" ("xrefactorbody" `isInfixOf` out)
    assertBool "stale ref never injected" (not ("xstalebody" `isInfixOf` out))

{- | A task with no retrieval axis (domain/discipline) auto-pulls nothing, so
the prompt must say so rather than render a context-free block in silence.
-}
testPromptUntaggedWarns :: IO ()
testPromptUntaggedWarns = withSystemTempDirectory "icarium-test" $ \dir -> do
    let db = dir <> "/icarium.db"
    writeFile (dir <> "/icarium.toml") minimalIcariumToml
    (_, aOut, _) <- runIcariumIn dir db ["task", "add", "untagged task", "--state", "ready-headless"]
    let tid = head (words aOut)

    (code, out, err) <- runIcariumIn dir db ["task", "show", tid, "--prompt"]
    code @?= ExitSuccess
    assertBool "prompt still renders on stdout" ("untagged task" `isInfixOf` out)
    assertBool "warning is on stderr, not stdout" (not ("warn:" `isInfixOf` out))
    assertBool "warns about missing tags" ("warn:" `isInfixOf` err)
    assertBool "warning names the fixing command" ("task update" `isInfixOf` err)
    assertBool "warning names --domain" ("--domain" `isInfixOf` err)
    assertBool "warning names the task id" (tid `isInfixOf` err)

{- | @task show@ and @task show --prompt@ read the same three link sets. Pin
both against one fixture so a field added to one branch and forgotten in the
other shows up here.
-}
testShowBranchesAgree :: IO ()
testShowBranchesAgree = withSystemTempDirectory "icarium-test" $ \dir -> do
    let db = dir <> "/icarium.db"
    writeFile (dir <> "/icarium.toml") minimalIcariumToml
    _ <- runIcariumIn dir db ["category", "add", "--axis", "domain", "core"]
    (_, kOut, _) <- runIcariumIn dir db ["ctx", "add", "linked ref", "--body", "xrefbody"]
    -- The human links tree truncates ids to 10 chars; assert on that prefix.
    let kId = take 10 (head (words kOut))
    (_, dOut, _) <- runIcariumIn dir db ["task", "add", "dependency task"]
    let dId = take 10 (head (words dOut))
    (_, aOut, _) <-
        runIcariumIn
            dir
            db
            [ "task"
            , "add"
            , "linked task"
            , "--state"
            , "ready-headless"
            , "--domain"
            , "core"
            , "--depends-on"
            , dId
            , "--references"
            , kId
            ]
    let tid = head (words aOut)

    (hCode, hOut, _) <- runIcariumIn dir db ["task", "show", tid]
    hCode @?= ExitSuccess
    assertBool "human shows ref" (kId `isInfixOf` hOut)
    assertBool "human shows dep" (dId `isInfixOf` hOut)
    assertBool "human shows category" ("core" `isInfixOf` hOut)

    (pCode, pOut, pErr) <- runIcariumIn dir db ["task", "show", tid, "--prompt"]
    pCode @?= ExitSuccess
    assertBool "prompt shows ref" (kId `isInfixOf` pOut)
    assertBool "prompt shows dep" (dId `isInfixOf` pOut)
    -- Cats aren't printed in the prompt; their absence is what warns.
    assertBool "prompt read the categories" (not ("warn:" `isInfixOf` pErr))

{- | @kind@ is not a retrieval axis, so a kind-only task pulls nothing either --
the guard is "no retrieval axis", not "no categories".
-}
testPromptKindOnlyWarns :: IO ()
testPromptKindOnlyWarns = withSystemTempDirectory "icarium-test" $ \dir -> do
    let db = dir <> "/icarium.db"
    writeFile (dir <> "/icarium.toml") minimalIcariumToml
    _ <- runIcariumIn dir db ["category", "add", "--axis", "kind", "bug"]
    (_, aOut, _) <- runIcariumIn dir db ["task", "add", "kind only", "--state", "ready-headless", "--kind", "bug"]
    let tid = head (words aOut)

    (code, _, err) <- runIcariumIn dir db ["task", "show", tid, "--prompt"]
    code @?= ExitSuccess
    assertBool "kind alone still warns" ("warn:" `isInfixOf` err)

-- | One retrieval axis is enough -- the pull just widens. No warning.
testPromptTaggedQuiet :: IO ()
testPromptTaggedQuiet = withSystemTempDirectory "icarium-test" $ \dir -> do
    let db = dir <> "/icarium.db"
    writeFile (dir <> "/icarium.toml") minimalIcariumToml
    _ <- runIcariumIn dir db ["category", "add", "--axis", "domain", "cli"]
    (_, aOut, _) <- runIcariumIn dir db ["task", "add", "tagged", "--state", "ready-headless", "--domain", "cli"]
    let tid = head (words aOut)

    (code, _, err) <- runIcariumIn dir db ["task", "show", tid, "--prompt"]
    code @?= ExitSuccess
    assertBool "no warning when a retrieval axis is present" (not ("warn:" `isInfixOf` err))

{- | Nudge at creation is where fill-rate actually gets fixed -- but it must not
block quick capture, so this is advisory on stderr, not a failure.
-}
testTaskAddUntaggedNudge :: IO ()
testTaskAddUntaggedNudge = withSystemTempDirectory "icarium-test" $ \dir -> do
    let db = dir <> "/icarium.db"
    writeFile (dir <> "/icarium.toml") minimalIcariumToml
    (code, out, err) <- runIcariumIn dir db ["task", "add", "quick capture", "--state", "ready-headless"]
    code @?= ExitSuccess
    let tid = head (words out)
    assertBool "nudge names the fixing command" ("task update" `isInfixOf` err)
    assertBool "nudge names --domain" ("--domain" `isInfixOf` err)
    assertBool "nudge names the task id" (tid `isInfixOf` err)

    _ <- runIcariumIn dir db ["category", "add", "--axis", "domain", "cli"]
    (code2, _, err2) <- runIcariumIn dir db ["task", "add", "tagged capture", "--domain", "cli"]
    code2 @?= ExitSuccess
    assertBool "no nudge when tagged" (not ("--domain" `isInfixOf` err2))
