-- | CLI contract for @icarium task@: CRUD, the ready queue, claims, bodies.
module CliTaskSpec (tests) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (try)
import Control.Monad (forM, forM_, replicateM)
import Data.Aeson.KeyMap qualified as KM
import Data.Char (toLower)
import Data.List (isInfixOf, isPrefixOf, nub, sort, tails)
import System.Directory (doesFileExist)
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
        $ [ testCase "task add/list/show/path/cat round-trip over a body" testTaskRoundtrip
          , testCase "task ids sort into creation order" testTaskIdsSortByCreation
          , testCase "task update --state, start and done move a task through its states" testTaskStateTransitions
          , testCase "task list --limit caps rows" testTaskListLimit
          , testCase "task claim takes each task at most once" testTaskClaimDistinct
          , testCase "task claim: racing processes partition the queue" testTaskClaimConcurrent
          , testCase "task claim records owner; task done clears it" testTaskClaimOwnerLifecycle
          , testCase "task claim --owner empty exits 2, claims nothing" testTaskClaimEmptyOwner
          , testCase "task next/claim serve the interactive queue only" testInteractiveQueueSurfaces
          , testCase "task queue interleaves both ready states; --headless/--interactive narrow" testQueueStatesAndNarrowing
          , testCase "task queue: empty says so, --limit caps, --json spells the stored state" testQueueEmptyLimitJson
          , testCase "task queue --interactive head equals task next" testQueueHeadMatchesNext
          , testCase "--ready and bare --state ready are gone" testRemovedReadySurfaces
          , testCase "task claim TASK_ID takes either ready state, refuses anything else" testTaskClaimNamed
          , testCase "task add --body-stdin with empty stdin exits 2, files nothing" testAddEmptyBodyStdin
          , testCase "dispatch quarantine: blocked upstream drops the dependent from the queue, not from list" testDispatchQuarantine
          , testCase "task show --prompt renders; --prompt --json exits 2" testTaskShowPrompt
          , testCase "task --no-commit: add sets, show displays, --commit-required clears" testTaskNoCommit
          , testCase "task --model/--effort: add, show, update, clear, and reject an unknown effort" testTaskRouting
          , testCase "task exists: found exits 0, not-found exits 1, ambiguous exits 2" testTaskExists
          , testCase "task list/show --json: valid JSON, ids, body_path not body" testTaskJson
          , testCase "task show --prompt: retired ref delivered, stale ref never" testPromptRetiredRefs
          , testCase "task show --prompt: a retrieval axis silences the warning, kind alone does not" testPromptRetrievalAxisGuard
          , testCase "task show: refs, deps and cats reach both branches" testShowBranchesAgree
          , testCase "task add: untagged capture nudges without blocking" testTaskAddUntaggedNudge
          , testCase "task rm removes the body file; an unresolvable id exits 1" testTaskRm
          ]
            <> map addRejectionCase addRejectionCases

{- | The whole read surface over one filed task: what @add@ prints, what the
body file holds, and which of the two @show@ never leaks (the body itself —
it lives on disk, and @cat@ is the way to it).
-}
testTaskRoundtrip :: IO ()
testTaskRoundtrip = withSystemTempDirectory "icarium-test" $ \dir -> do
    let db = dir <> "/icarium.db"
    (addCode, addOut, _) <-
        runIcarium db ["task", "add", "My roundtrip task", "--state", "ready-headless", "--body", "secret body\nline two"]
    addCode @?= ExitSuccess
    let outLines = lines addOut
        tid = head outLines
        bodyPath = outLines !! 1
    assertBool "task id non-empty" (not (null tid))

    contents <- readFile bodyPath
    contents @?= "secret body\nline two"

    (pCode, pathOut, _) <- runIcarium db ["task", "path", tid]
    pCode @?= ExitSuccess
    head (lines pathOut) @?= bodyPath

    (lCode, lOut, _) <- runIcarium db ["task", "list"]
    lCode @?= ExitSuccess
    assertBool "list shows title" ("My roundtrip task" `isInfixOf` lOut)
    assertBool "list shows id prefix" (take 10 tid `isInfixOf` lOut)

    (sCode, sOut, _) <- runIcarium db ["task", "show", take 10 tid]
    sCode @?= ExitSuccess
    assertBool "show contains full id" (tid `isInfixOf` sOut)
    assertBool "show contains title" ("My roundtrip task" `isInfixOf` sOut)
    assertBool "show points at the body file" (bodyPath `isInfixOf` sOut)
    assertBool "show does not inline the body" (not ("secret body" `isInfixOf` sOut))
    assertBool "show has no ## Body header" (not ("## Body" `isInfixOf` sOut))

    (cCode, cOut, _) <- runIcarium db ["task", "cat", tid]
    cCode @?= ExitSuccess
    cOut @?= "secret body\nline two"

    -- A task filed without a body cats to nothing rather than failing.
    (_, bareOut, _) <- runIcarium db ["task", "add", "No body task"]
    (bCode, bOut, _) <- runIcarium db ["task", "cat", head (lines bareOut)]
    bCode @?= ExitSuccess
    bOut @?= ""

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

testTaskStateTransitions :: IO ()
testTaskStateTransitions = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["task", "add", "State change task", "--state", "planned"]
    let tid = head (words addOut)

    (uCode, _, _) <- runIcarium db ["task", "update", tid, "--state", "ready-headless"]
    uCode @?= ExitSuccess
    (_, readyList, _) <- runIcarium db ["task", "list", "--state", "ready-headless"]
    assertBool "updated task appears in ready list" ("State change task" `isInfixOf` readyList)
    (_, plannedList, _) <- runIcarium db ["task", "list", "--state", "planned"]
    assertBool "task no longer in planned list" (not ("State change task" `isInfixOf` plannedList))

    (sCode, sOut, _) <- runIcarium db ["task", "start", tid]
    sCode @?= ExitSuccess
    assertBool "start prints updated" ("updated" `isInfixOf` sOut)
    (_, started, _) <- runIcarium db ["task", "show", tid]
    assertBool "state is in_progress after start" ("in_progress" `isInfixOf` started)

    (dCode, _, _) <- runIcarium db ["task", "done", tid]
    dCode @?= ExitSuccess
    (_, finished, _) <- runIcarium db ["task", "show", tid]
    assertBool "state is done after done" ("done" `isInfixOf` finished)

    -- Flags take the stored spelling as well as the hyphenated CLI one.
    (underscoreCode, _, _) <- runIcarium db ["task", "update", tid, "--state", "in_progress"]
    underscoreCode @?= ExitSuccess

testTaskListLimit :: IO ()
testTaskListLimit = withTempDb $ \db -> do
    mapM_ (\i -> runIcarium db ["task", "add", "Task " ++ show (i :: Int), "--state", "ready-headless"]) [1 .. 5 :: Int]
    (code, out, _) <- runIcarium db ["task", "list", "--limit", "3"]
    code @?= ExitSuccess
    let rows = filter (not . null) (lines out)
    length rows @?= 3

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

-- | A claim must not outlive the work: leaving in_progress drops the stamp.
testTaskClaimOwnerLifecycle :: IO ()
testTaskClaimOwnerLifecycle = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["task", "add", "Claimable", "--state", "ready-interactive"]
    let tid = head (words addOut)

    (code, claimOut, _) <- runIcarium db ["task", "claim", "--owner", "agent-7"]
    code @?= ExitSuccess
    words claimOut @?= [tid]

    (_, claimed, _) <- runIcarium db ["task", "show", tid]
    assertBool "state is in_progress" ("in_progress" `isInfixOf` claimed)
    assertBool "owner shown" ("owner:     agent-7" `isInfixOf` claimed)
    assertBool "claim time shown" ("claimed:   " `isInfixOf` claimed)

    (dCode, _, _) <- runIcarium db ["task", "done", tid]
    dCode @?= ExitSuccess
    (_, released, _) <- runIcarium db ["task", "show", tid]
    assertBool "owner cleared" (not ("agent-7" `isInfixOf` released))
    assertBool "claim time cleared" (not ("claimed:" `isInfixOf` released))

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

{- | The CLI queue serves the human. Headless work sitting in `ready` is
dispatch's to take, and must never be handed to `task next`/`task claim` —
which is also what makes both commands read as empty until interactive work
exists.
-}
testInteractiveQueueSurfaces :: IO ()
testInteractiveQueueSurfaces = withTempDb $ \db -> do
    (_, hOut, _) <- runIcarium db ["task", "add", "Headless", "--state", "ready-headless"]
    let hId = head (words hOut)

    (nCode, _, _) <- runIcarium db ["task", "next"]
    nCode @?= ExitFailure 1
    (cCode, cOut, _) <- runIcarium db ["task", "claim"]
    cCode @?= ExitFailure 1
    cOut @?= ""

    (_, iOut, _) <- runIcarium db ["task", "add", "Interactive", "--state", "ready-interactive"]
    let iId = head (words iOut)
    (nCode2, nOut, _) <- runIcarium db ["task", "next"]
    nCode2 @?= ExitSuccess
    words nOut @?= [iId]

    (cCode2, cOut2, _) <- runIcarium db ["task", "claim"]
    cCode2 @?= ExitSuccess
    words cOut2 @?= [iId]

    -- The headless task is untouched by either surface.
    (_, showOut, _) <- runIcarium db ["task", "show", hId]
    assertBool "headless task still ready" ("ready" `isInfixOf` showOut)
    assertBool "headless task not claimed" (not ("claimed:" `isInfixOf` showOut))

-- | Bare `queue` interleaves both queues in priority order, badged by state.
testQueueStatesAndNarrowing :: IO ()
testQueueStatesAndNarrowing = withTempDb $ \db -> do
    (_, lowOut, _) <- runIcarium db ["task", "add", "Low headless", "--state", "ready-headless", "--priority", "1"]
    (_, hiOut, _) <- runIcarium db ["task", "add", "High interactive", "--state", "ready-interactive", "--priority", "9"]
    let lowId = take 10 (head (words lowOut))
        hiId = take 10 (head (words hiOut))

    (code, out, _) <- runIcarium db ["task", "queue"]
    code @?= ExitSuccess
    assertBool "headless row present" (lowId `isInfixOf` out)
    assertBool "interactive row present" (hiId `isInfixOf` out)
    assertBool "headless badge" ("[ready-headless]" `isInfixOf` out)
    assertBool "interactive badge" ("[ready-interactive]" `isInfixOf` out)
    let idx sub = length (takeWhile (not . isPrefixOf sub) (tails out))
    assertBool "higher priority first" (idx hiId < idx lowId)

    (hCode, hQ, _) <- runIcarium db ["task", "queue", "--headless"]
    hCode @?= ExitSuccess
    assertBool "--headless keeps headless" (lowId `isInfixOf` hQ)
    assertBool "--headless drops interactive" (not (hiId `isInfixOf` hQ))

    (iCode, iQ, _) <- runIcarium db ["task", "queue", "--interactive"]
    iCode @?= ExitSuccess
    assertBool "--interactive keeps interactive" (hiId `isInfixOf` iQ)
    assertBool "--interactive drops headless" (not (lowId `isInfixOf` iQ))

    -- A contradictory request has no honest answer: refuse rather than pick one.
    (xCode, _, xErr) <- runIcarium db ["task", "queue", "--headless", "--interactive"]
    xCode @?= ExitFailure 2
    assertBool "error names both flags" ("--headless" `isInfixOf` xErr && "--interactive" `isInfixOf` xErr)

testQueueEmptyLimitJson :: IO ()
testQueueEmptyLimitJson = withTempDb $ \db -> do
    (eCode, eOut, _) <- runIcarium db ["task", "queue"]
    eCode @?= ExitSuccess
    assertBool "empty queue says so rather than printing nothing" ("no tasks" `isInfixOf` eOut)

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
    (_, pOut, _) <- runIcarium db ["task", "add", "Under-specified", "--state", "planned"]
    let hId = head (words hOut)
        iId = head (words iOut)
        pId = head (words pOut)

    (hCode, hClaim, _) <- runIcarium db ["task", "claim", hId, "--owner", "agent-7"]
    hCode @?= ExitSuccess
    words hClaim @?= [hId]
    (_, hShow, _) <- runIcarium db ["task", "show", hId]
    assertBool "named claim marks in_progress" ("in_progress" `isInfixOf` hShow)
    assertBool "named claim stamps the owner" ("owner:     agent-7" `isInfixOf` hShow)

    (iCode, iClaim, _) <- runIcarium db ["task", "claim", iId]
    iCode @?= ExitSuccess
    words iClaim @?= [iId]

    (pCode, _, pErr) <- runIcarium db ["task", "claim", pId]
    pCode @?= ExitFailure 1
    assertBool "error names the state" ("planned" `isInfixOf` pErr)
    assertBool "error names the fixing command" ("task update" `isInfixOf` pErr)
    (_, pShow, _) <- runIcarium db ["task", "show", pId]
    assertBool "refused task left alone" (not ("in_progress" `isInfixOf` pShow))

{- | Every add-time refusal is exit 2 with the cause named on stderr. These
fail independently of one another, so they get a row each rather than one
case that stops at the first.
-}
addRejectionCases :: [(String, [String], String)]
addRejectionCases =
    [ ("an unresolvable --depends-on", ["task", "add", "Dependent", "--depends-on", "01NONEXISTENT"], "01NONEXISTENT")
    , ("--state blocked", ["task", "add", "Bad state", "--state", "blocked"], "state")
    , ("a whitespace-only --body", ["task", "add", "ws body task", "--body", "  \n "], "empty body")
    , -- ctx add shares the body guard, so it shares the refusal.
      ("an empty --body on ctx add", ["ctx", "add", "ws body ctx", "--body", ""], "empty body")
    ]

addRejectionCase :: (String, [String], String) -> TestTree
addRejectionCase (name, args, expectedErr) =
    testCase ("task add rejects " <> name) $ withTempDb $ \db -> do
        (code, _, err) <- runIcarium db args
        code @?= ExitFailure 2
        assertBool ("stderr names the cause: " <> expectedErr) (expectedErr `isInfixOf` err)

testAddEmptyBodyStdin :: IO ()
testAddEmptyBodyStdin = withTempDb $ \db -> do
    (code, _, err) <- runIcariumStdin db "" ["task", "add", "empty stdin task", "--body-stdin"]
    code @?= ExitFailure 2
    assertBool "error names the empty body" ("empty body" `isInfixOf` err)
    (_, lOut, _) <- runIcarium db ["task", "list"]
    assertBool "nothing was filed" (not ("empty stdin task" `isInfixOf` lOut))

{- | Quarantine contract: a failed dispatch sets its task to 'blocked'.
The ready_tasks view (used by dispatch run and task next) excludes any
task whose depends_on target is not 'done', so the dependent is silently
quarantined until the upstream is resolved. Independent tasks keep
draining normally, and @list@ — a pure filter — still shows the quarantined
one.

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
    (qCode, qOut, _) <- runIcarium db ["task", "queue"]
    qCode @?= ExitSuccess
    assertBool "dependent B absent from ready queue" (not ("Dependent task B" `isInfixOf` qOut))
    assertBool "independent C present in ready queue" ("Independent task C" `isInfixOf` qOut)

    -- task next returns C's full id (the head of what drain would pick), not B's
    (nCode, nOut, _) <- runIcarium db ["task", "next"]
    nCode @?= ExitSuccess
    assertBool "task next picks C, not B" (cId `isInfixOf` nOut)
    assertBool "task next does not pick B" (not (bId `isInfixOf` nOut))

    -- The gate belongs to the queue, not to the state filter.
    (lCode, lOut, _) <- runIcarium db ["task", "list", "--state", "ready-interactive"]
    lCode @?= ExitSuccess
    assertBool "state filter applies no gate" (take 10 bId `isInfixOf` lOut)

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

    -- Two renderings of one task, asked for at once, has no honest answer.
    (xCode, xOut, xErr) <- runIcarium db ["task", "show", tid, "--prompt", "--json"]
    xCode @?= ExitFailure 2
    xOut @?= ""
    assertBool "stderr names both flags" ("--prompt" `isInfixOf` xErr && "--json" `isInfixOf` xErr)

testTaskNoCommit :: IO ()
testTaskNoCommit = withTempDb $ \db -> do
    (addCode, addOut, _) <- runIcarium db ["task", "add", "Side-effect task", "--no-commit"]
    addCode @?= ExitSuccess
    let tid = head (words addOut)
    (_, flagged, _) <- runIcarium db ["task", "show", tid]
    assertBool "no-commit shown in task show" ("no-commit" `isInfixOf` flagged)
    assertBool "no-commit value is yes" ("yes" `isInfixOf` flagged)

    -- The line means "this task is special", so it is absent by default.
    (_, plainOut, _) <- runIcarium db ["task", "add", "Regular task"]
    let plainId = head (words plainOut)
    (_, plain, _) <- runIcarium db ["task", "show", plainId]
    assertBool "no-commit absent for regular task" (not ("no-commit" `isInfixOf` plain))

    (uCode, _, _) <- runIcarium db ["task", "update", plainId, "--no-commit"]
    uCode @?= ExitSuccess
    (_, toggledOn, _) <- runIcarium db ["task", "show", plainId]
    assertBool "no-commit set after --no-commit" ("no-commit" `isInfixOf` toggledOn)

    (rCode, _, _) <- runIcarium db ["task", "update", plainId, "--commit-required"]
    rCode @?= ExitSuccess
    (_, toggledOff, _) <- runIcarium db ["task", "show", plainId]
    assertBool "no-commit cleared after --commit-required" (not ("no-commit" `isInfixOf` toggledOff))

testTaskRouting :: IO ()
testTaskRouting = withTempDb $ \db -> do
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

    -- Overrides mean "this task is special", so an inheriting task shows neither.
    (_, defaultOut, _) <- runIcarium db ["task", "add", "Default task"]
    let defaultId = head (words defaultOut)
    (_, inherited, _) <- runIcarium db ["task", "show", defaultId]
    assertBool "model line absent when unset" (not ("model:" `isInfixOf` inherited))
    assertBool "effort line absent when unset" (not ("effort:" `isInfixOf` inherited))

    (uCode, _, _) <- runIcarium db ["task", "update", defaultId, "--model", "claude-opus-4-8", "--effort", "xhigh"]
    uCode @?= ExitSuccess
    (_, retiered, _) <- runIcarium db ["task", "show", defaultId]
    assertBool "model set" ("claude-opus-4-8" `isInfixOf` retiered)
    assertBool "effort set" ("xhigh" `isInfixOf` retiered)

    -- Empty string clears, matching the category-axis flags.
    (cCode, _, _) <- runIcarium db ["task", "update", defaultId, "--model", "", "--effort", ""]
    cCode @?= ExitSuccess
    (_, cleared, _) <- runIcarium db ["task", "show", defaultId]
    assertBool "model cleared" (not ("model:" `isInfixOf` cleared))
    assertBool "effort cleared" (not ("effort:" `isInfixOf` cleared))

    (badCode, _, badErr) <- runIcarium db ["task", "add", "Bad effort", "--effort", "turbo"]
    badCode @?= ExitFailure 1
    assertBool "names the bad effort" ("turbo" `isInfixOf` badErr)

testTaskExists :: IO ()
testTaskExists = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["task", "add", "Exists task"]
    let tid = head (words addOut)

    -- found: full id → exit 0
    (foundCode, foundOut, _) <- runIcarium db ["task", "exists", tid]
    foundCode @?= ExitSuccess
    foundOut @?= ""

    -- found: prefix → exit 0, and --verbose resolves it to the full id
    (prefCode, _, _) <- runIcarium db ["task", "exists", take 10 tid]
    prefCode @?= ExitSuccess
    (vCode, vOut, _) <- runIcarium db ["task", "exists", "--verbose", take 10 tid]
    vCode @?= ExitSuccess
    assertBool "verbose output contains full id" (tid `isInfixOf` vOut)

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
@kind@ does not count: the guard is "no retrieval axis", not "no categories".
-}
testPromptRetrievalAxisGuard :: IO ()
testPromptRetrievalAxisGuard = withSystemTempDirectory "icarium-test" $ \dir -> do
    let db = dir <> "/icarium.db"
    writeFile (dir <> "/icarium.toml") minimalIcariumToml
    _ <- runIcariumIn dir db ["category", "add", "--axis", "kind", "bug"]
    _ <- runIcariumIn dir db ["category", "add", "--axis", "domain", "cli"]

    (_, bareOut, _) <- runIcariumIn dir db ["task", "add", "untagged task", "--state", "ready-headless"]
    let bareId = head (words bareOut)
    (code, out, err) <- runIcariumIn dir db ["task", "show", bareId, "--prompt"]
    code @?= ExitSuccess
    assertBool "prompt still renders on stdout" ("untagged task" `isInfixOf` out)
    assertBool "warning is on stderr, not stdout" (not ("warn:" `isInfixOf` out))
    assertBool "warns about missing tags" ("warn:" `isInfixOf` err)
    assertBool "warning names the fixing command" ("task update" `isInfixOf` err)
    assertBool "warning names --domain" ("--domain" `isInfixOf` err)
    assertBool "warning names the task id" (bareId `isInfixOf` err)

    (_, kindOut, _) <- runIcariumIn dir db ["task", "add", "kind only", "--state", "ready-headless", "--kind", "bug"]
    (kCode, _, kErr) <- runIcariumIn dir db ["task", "show", head (words kindOut), "--prompt"]
    kCode @?= ExitSuccess
    assertBool "kind alone still warns" ("warn:" `isInfixOf` kErr)

    -- One retrieval axis is enough — the pull just widens.
    (_, domOut, _) <- runIcariumIn dir db ["task", "add", "tagged", "--state", "ready-headless", "--domain", "cli"]
    (dCode, _, dErr) <- runIcariumIn dir db ["task", "show", head (words domOut), "--prompt"]
    dCode @?= ExitSuccess
    assertBool "no warning when a retrieval axis is present" (not ("warn:" `isInfixOf` dErr))

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

{- | rm must take the row and the body file together; a prefix resolves to the
full id, which is what @deleted@ echoes.
-}
testTaskRm :: IO ()
testTaskRm = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["task", "add", "Deletable task", "--body", "task body to delete"]
    let outLines = lines addOut
        tid = head outLines
        bodyPath = outLines !! 1

    before <- doesFileExist bodyPath
    assertBool "body file exists before rm" before

    (code, out, _) <- runIcarium db ["task", "rm", take 10 tid]
    code @?= ExitSuccess
    head (lines out) @?= "deleted " <> tid

    after <- doesFileExist bodyPath
    assertBool "body file gone after rm" (not after)

    (missCode, _, missErr) <- runIcarium db ["task", "rm", "01ZZZZZZZZZZZZZZZZZZZZZZZZ"]
    missCode @?= ExitFailure 1
    assertBool "error names the kind" ("task" `isInfixOf` missErr)
