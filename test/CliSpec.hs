module CliSpec (tests) where

import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.List (isInfixOf)
import System.Directory (makeAbsolute)
import System.Exit (ExitCode (..))
import System.IO.Temp (withSystemTempDirectory)
import System.Process.Typed (proc, readProcess, setWorkingDir)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

bin :: FilePath
bin = "./bin/icarium"

runIcarium :: FilePath -> [String] -> IO (ExitCode, String, String)
runIcarium db args = do
    (code, outBs, errBs) <- readProcess (proc bin (["--db", db] ++ args))
    pure (code, BLC.unpack outBs, BLC.unpack errBs)

{- | Run icarium with a custom working directory so config loading picks
up a test-supplied icarium.toml. Both the binary and db paths are
resolved against the test's cwd before chdir, so they stay valid.
-}
runIcariumIn :: FilePath -> FilePath -> [String] -> IO (ExitCode, String, String)
runIcariumIn workdir db args = do
    absBin <- makeAbsolute bin
    absDb <- makeAbsolute db
    (code, outBs, errBs) <-
        readProcess $
            setWorkingDir workdir $
                proc absBin (["--db", absDb] ++ args)
    pure (code, BLC.unpack outBs, BLC.unpack errBs)

withTempDb :: (FilePath -> IO a) -> IO a
withTempDb k = withSystemTempDirectory "icarium-test" $ \dir ->
    k (dir <> "/icarium.db")

tests :: TestTree
tests =
    testGroup
        "CLI integration"
        [ testCase "task add/list/show roundtrip" testTaskRoundtrip
        , testCase "task update --state changes state" testTaskUpdateState
        , testCase "dispatch list on empty DB exits 0" testDispatchListEmpty
        , testCase "task next exits 1 on empty queue" testTaskNextEmpty
        , testCase "task next prints id on non-empty" testTaskNextNonEmpty
        , testCase "task add --depends-on bad id exits 2" testTaskAddBadDependsOn
        , testCase "task add --state blocked exits 2" testTaskAddStateBlocked
        , testCase "dispatch run drains empty queue without --max" testDispatchRunEmptyQueue
        , testCase "dispatch run ignores stale max_dispatches_per_run" testDispatchRunStaleConfig
        , testCase "know list shows cats and linked count" testKnowledgeListLayout
        , testCase "link --help has corrected argument order example" testLinkHelpExample
        , testCase "link list emits header row when edges exist" testLinkListHeader
        , testCase "know add --help and link add --help cross-reference each other" testHelpCrossRef
        , testCase "task show --body prints only body" testTaskShowBody
        , testCase "task show --prompt works" testTaskShowPrompt
        , testCase "task show --body --prompt exits 2" testTaskShowBodyAndPrompt
        , testCase "know show --body prints only body" testKnowShowBody
        , testCase "task show --body round-trip via update --body-file" testTaskBodyRoundTrip
        , -- bare noun group prints help, not list
          testCase "bare task prints help and exits non-zero" testBareTaskHelp
        , testCase "bare know prints help and exits non-zero" testBareKnowHelp
        , testCase "bare dispatch prints help and exits non-zero" testBareDispatchHelp
        , testCase "bare link prints help and exits non-zero" testBareLinkHelp
        , testCase "bare category prints help and exits non-zero" testBareCategoryHelp
        , -- ls alias
          testCase "task ls and task list produce identical output" testTaskLsAlias
        , testCase "task ls --state ready works" testTaskLsStateFlag
        , -- old bare-with-flag form errors
          testCase "task --state ready errors with usage message" testBareTaskWithFlag
        , -- --help shows list once, not ls
          testCase "task --help shows list but not ls" testTaskHelpNoLs
        , testCase "search: title-match and body-match both surface" testSearchBothKinds
        , testCase "search: title hit on knowledge outranks body hit on task" testSearchCrossKindRank
        , testCase "search: --kind task excludes knowledge results" testSearchKindTask
        , testCase "search: --no-snippet suppresses indented line" testSearchNoSnippet
        , testCase "search: empty result prints (no matches)" testSearchNoMatches
        ]

testTaskRoundtrip :: IO ()
testTaskRoundtrip = withTempDb $ \db -> do
    (addCode, addOut, _) <- runIcarium db ["task", "add", "My roundtrip task", "--state", "ready"]
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

testTaskUpdateState :: IO ()
testTaskUpdateState = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["task", "add", "State change task", "--state", "planned"]
    let tid = head (words addOut)

    (uCode, _, _) <- runIcarium db ["task", "update", tid, "--state", "ready"]
    uCode @?= ExitSuccess

    (lCode, lOut, _) <- runIcarium db ["task", "list", "--state", "ready"]
    lCode @?= ExitSuccess
    assertBool "updated task appears in ready list" ("State change task" `isInfixOf` lOut)

    (lCode2, lOut2, _) <- runIcarium db ["task", "list", "--state", "planned"]
    lCode2 @?= ExitSuccess
    assertBool "task no longer in planned list" (not ("State change task" `isInfixOf` lOut2))

testDispatchListEmpty :: IO ()
testDispatchListEmpty = withTempDb $ \db -> do
    (code, _, err) <- runIcarium db ["dispatch", "list"]
    code @?= ExitSuccess
    assertBool "no error output on empty dispatch list" (null err)

testTaskNextEmpty :: IO ()
testTaskNextEmpty = withTempDb $ \db -> do
    (code, _, _) <- runIcarium db ["task", "next"]
    code @?= ExitFailure 1

testTaskNextNonEmpty :: IO ()
testTaskNextNonEmpty = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["task", "add", "Next task", "--state", "ready"]
    let tid = head (words addOut)

    (code, nextOut, _) <- runIcarium db ["task", "next"]
    code @?= ExitSuccess
    assertBool "next output is the task id" (tid `isInfixOf` nextOut)

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

{- | `dispatch run` with an empty ready queue and no `--max` should exit
0 and report "ready queue empty" — i.e. no built-in run-level cap is
preventing it from reaching that branch.
-}
testDispatchRunEmptyQueue :: IO ()
testDispatchRunEmptyQueue = withSystemTempDirectory "icarium-test" $ \dir -> do
    let db = dir <> "/icarium.db"
    writeFile (dir <> "/icarium.toml") minimalIcariumToml
    (code, _, err) <- runIcariumIn dir db ["dispatch", "run"]
    code @?= ExitSuccess
    assertBool "stderr reports empty queue" ("ready queue empty" `isInfixOf` err)

{- | A config that still carries the now-removed `max_dispatches_per_run`
field must load cleanly. tomland tolerates unknown keys, so the
silent-ignore behavior is contractual.
-}
testDispatchRunStaleConfig :: IO ()
testDispatchRunStaleConfig = withSystemTempDirectory "icarium-test" $ \dir -> do
    let db = dir <> "/icarium.db"
    writeFile (dir <> "/icarium.toml") staleIcariumToml
    (code, _, err) <- runIcariumIn dir db ["dispatch", "run"]
    code @?= ExitSuccess
    assertBool
        "stale field did not break config load"
        ("ready queue empty" `isInfixOf` err)
    assertBool
        "no parse error surfaced"
        (not ("config parse error" `isInfixOf` err))

testKnowledgeListLayout :: IO ()
testKnowledgeListLayout = withTempDb $ \db -> do
    -- k1: no categories, no inbound edges
    (_, _, _) <- runIcarium db ["know", "add", "Plain entry no links"]

    -- k2: no categories, will have one inbound references edge from a task
    (_, k2Out, _) <- runIcarium db ["know", "add", "Entry with one inbound link"]
    let k2Id = head (words k2Out)

    (_, t1Out, _) <- runIcarium db ["task", "add", "Some task", "--state", "ready"]
    let t1Id = head (words t1Out)
    (linkCode, _, _) <- runIcarium db ["link", "add", t1Id, "references", k2Id]
    linkCode @?= ExitSuccess

    (code, out, _) <- runIcarium db ["know", "list"]
    code @?= ExitSuccess
    assertBool "list contains plain entry title" ("Plain entry no links" `isInfixOf` out)
    assertBool "list contains linked entry title" ("Entry with one inbound link" `isInfixOf` out)
    assertBool "[linked:1] badge appears" ("[linked:1]" `isInfixOf` out)
    assertBool "no stale column (yes/no gone)" (not ("  yes  " `isInfixOf` out || "  no  " `isInfixOf` out))
    assertBool "cats column present ([-])" ("[-]" `isInfixOf` out)

testLinkHelpExample :: IO ()
testLinkHelpExample = withTempDb $ \db -> do
    (code, out, _) <- runIcarium db ["link", "--help"]
    code @?= ExitSuccess
    assertBool "old order absent" (not ("depends-on TASK_A TASK_B" `isInfixOf` out))
    assertBool "correct order present" ("TASK_A depends-on TASK_B" `isInfixOf` out)

testLinkListHeader :: IO ()
testLinkListHeader = withTempDb $ \db -> do
    (_, tOut, _) <- runIcarium db ["task", "add", "Src task", "--state", "ready"]
    let tid = head (words tOut)
    (_, kOut, _) <- runIcarium db ["know", "add", "Dst knowledge"]
    let kid = head (words kOut)
    (_, _, _) <- runIcarium db ["link", "add", tid, "references", kid]

    (code, out, _) <- runIcarium db ["link", "list"]
    code @?= ExitSuccess
    assertBool "header row present" ("EDGE_ID" `isInfixOf` out)

testHelpCrossRef :: IO ()
testHelpCrossRef = withTempDb $ \db -> do
    (_, knowOut, _) <- runIcarium db ["know", "add", "--help"]
    assertBool "know add --help mentions link add" ("link add" `isInfixOf` knowOut)

    (_, linkOut, _) <- runIcarium db ["link", "add", "--help"]
    assertBool "link add --help mentions know add --derived-from" ("know add --derived-from" `isInfixOf` linkOut)

testTaskShowBody :: IO ()
testTaskShowBody = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["task", "add", "Body test task", "--body", "hello world body"]
    let tid = head (words addOut)

    (code, out, _) <- runIcarium db ["task", "show", tid, "--body"]
    code @?= ExitSuccess
    out @?= "hello world body"

testTaskShowPrompt :: IO ()
testTaskShowPrompt = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["task", "add", "Prompt test task", "--state", "ready"]
    let tid = head (words addOut)

    (code, out, _) <- runIcarium db ["task", "show", tid, "--prompt"]
    code @?= ExitSuccess
    assertBool "prompt output contains task id" (tid `isInfixOf` out)
    assertBool "prompt output contains title" ("Prompt test task" `isInfixOf` out)

testTaskShowBodyAndPrompt :: IO ()
testTaskShowBodyAndPrompt = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["task", "add", "Conflict test"]
    let tid = head (words addOut)

    (code, _, err) <- runIcarium db ["task", "show", tid, "--body", "--prompt"]
    code @?= ExitFailure 2
    assertBool "error mentions mutual exclusion" ("mutually exclusive" `isInfixOf` err)

testKnowShowBody :: IO ()
testKnowShowBody = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["know", "add", "Body test entry", "--body", "knowledge body text"]
    let kid = head (words addOut)

    (code, out, _) <- runIcarium db ["know", "show", kid, "--body"]
    code @?= ExitSuccess
    out @?= "knowledge body text"

testTaskBodyRoundTrip :: IO ()
testTaskBodyRoundTrip = withSystemTempDirectory "icarium-test" $ \dir -> do
    let db = dir <> "/icarium.db"
        bodyFile = dir <> "/body.txt"

    (_, addOut, _) <- runIcarium db ["task", "add", "Round-trip task", "--body", "original body\nline two"]
    let tid = head (words addOut)

    -- Extract body to file, re-apply via --body-file, confirm no change.
    (_, bodyOut, _) <- runIcarium db ["task", "show", tid, "--body"]
    writeFile bodyFile bodyOut

    (uCode, _, _) <- runIcarium db ["task", "update", tid, "--body-file", bodyFile]
    uCode @?= ExitSuccess

    (_, bodyOut2, _) <- runIcarium db ["task", "show", tid, "--body"]
    bodyOut2 @?= bodyOut

testBareTaskHelp :: IO ()
testBareTaskHelp = withTempDb $ \db -> do
    (code, out, _) <- runIcarium db ["task"]
    code @?= ExitSuccess
    assertBool "bare task shows full help" ("Available commands:" `isInfixOf` out)
    assertBool "bare task help notes ls alias" ("(alias: ls)" `isInfixOf` out)

testBareKnowHelp :: IO ()
testBareKnowHelp = withTempDb $ \db -> do
    (code, out, _) <- runIcarium db ["know"]
    code @?= ExitSuccess
    assertBool "bare know shows full help" ("Available commands:" `isInfixOf` out)
    assertBool "bare know help notes ls alias" ("(alias: ls)" `isInfixOf` out)

testBareDispatchHelp :: IO ()
testBareDispatchHelp = withTempDb $ \db -> do
    (code, out, _) <- runIcarium db ["dispatch"]
    code @?= ExitSuccess
    assertBool "bare dispatch shows full help" ("Available commands:" `isInfixOf` out)
    assertBool "bare dispatch help notes ls alias" ("(alias: ls)" `isInfixOf` out)

testBareLinkHelp :: IO ()
testBareLinkHelp = withTempDb $ \db -> do
    (code, out, _) <- runIcarium db ["link"]
    code @?= ExitSuccess
    assertBool "bare link shows full help" ("Available commands:" `isInfixOf` out)
    assertBool "bare link help notes ls alias" ("(alias: ls)" `isInfixOf` out)

testBareCategoryHelp :: IO ()
testBareCategoryHelp = withTempDb $ \db -> do
    (code, out, _) <- runIcarium db ["category"]
    code @?= ExitSuccess
    assertBool "bare category shows full help" ("Available commands:" `isInfixOf` out)
    assertBool "bare category help notes ls alias" ("(alias: ls)" `isInfixOf` out)

testTaskLsAlias :: IO ()
testTaskLsAlias = withTempDb $ \db -> do
    (_, _, _) <- runIcarium db ["task", "add", "Ls alias task", "--state", "ready"]
    (_, listOut, _) <- runIcarium db ["task", "list"]
    (lsCode, lsOut, _) <- runIcarium db ["task", "ls"]
    lsCode @?= ExitSuccess
    lsOut @?= listOut

testTaskLsStateFlag :: IO ()
testTaskLsStateFlag = withTempDb $ \db -> do
    (_, _, _) <- runIcarium db ["task", "add", "Ready task", "--state", "ready"]
    (_, _, _) <- runIcarium db ["task", "add", "Planned task", "--state", "planned"]
    (code, out, _) <- runIcarium db ["task", "ls", "--state", "ready"]
    code @?= ExitSuccess
    assertBool "ls --state ready shows ready task" ("Ready task" `isInfixOf` out)
    assertBool "ls --state ready hides planned task" (not ("Planned task" `isInfixOf` out))

testBareTaskWithFlag :: IO ()
testBareTaskWithFlag = withTempDb $ \db -> do
    (code, _, err) <- runIcarium db ["task", "--state", "ready"]
    assertBool "bare task --state exits non-zero" (code /= ExitSuccess)
    assertBool "error output non-empty" (not (null err))

testTaskHelpNoLs :: IO ()
testTaskHelpNoLs = withTempDb $ \db -> do
    (code, out, _) <- runIcarium db ["task", "--help"]
    code @?= ExitSuccess
    assertBool "help shows list" ("list" `isInfixOf` out)
    assertBool "help hides ls" (not ("  ls" `isInfixOf` out))

minimalIcariumToml :: String
minimalIcariumToml =
    unlines
        [ "[project]"
        , "integration_branch = \"main\""
        , ""
        , "[commands]"
        , "build = \"true\""
        , "test  = \"true\""
        , ""
        , "[dispatch]"
        , "model  = \"claude-sonnet-4-6\""
        , "effort = \"high\""
        , "tools = []"
        , "allowed_tools = []"
        , "scratch_dir = \".icarium/scratch\""
        , "max_minutes_per_dispatch = 30"
        , "heartbeat_stale_seconds  = 300"
        , "log_retention_runs       = 25"
        , ""
        , "[categories]"
        , "domains     = [\"core\"]"
        , "disciplines = [\"development\"]"
        ]

staleIcariumToml :: String
staleIcariumToml =
    unlines
        [ "[project]"
        , "integration_branch = \"main\""
        , ""
        , "[commands]"
        , "build = \"true\""
        , "test  = \"true\""
        , ""
        , "[dispatch]"
        , "model  = \"claude-sonnet-4-6\""
        , "effort = \"high\""
        , "tools = []"
        , "allowed_tools = []"
        , "scratch_dir = \".icarium/scratch\""
        , "max_minutes_per_dispatch = 30"
        , "max_dispatches_per_run   = 20"
        , "heartbeat_stale_seconds  = 300"
        , "log_retention_runs       = 25"
        , ""
        , "[categories]"
        , "domains     = [\"core\"]"
        , "disciplines = [\"development\"]"
        ]

-- =============================================================
-- search tests
-- =============================================================

testSearchBothKinds :: IO ()
testSearchBothKinds = withTempDb $ \db -> do
    (_, _, _) <- runIcarium db ["task", "add", "mytoken in title task", "--state", "ready"]
    (_, _, _) <- runIcarium db ["know", "add", "unrelated knowledge", "--body", "body contains mytoken here"]
    (code, out, _) <- runIcarium db ["search", "mytoken"]
    code @?= ExitSuccess
    assertBool "title match surfaces" ("mytoken in title task" `isInfixOf` out)
    assertBool "body match surfaces" ("unrelated knowledge" `isInfixOf` out)

testSearchCrossKindRank :: IO ()
testSearchCrossKindRank = withTempDb $ \db -> do
    (_, _, _) <- runIcarium db ["task", "add", "no match here", "--body", "body has xyzzy123", "--state", "ready"]
    (_, kOut, _) <- runIcarium db ["know", "add", "xyzzy123 in title knowledge"]
    let kid = take 10 (head (words kOut))
    (code, out, _) <- runIcarium db ["search", "xyzzy123"]
    code @?= ExitSuccess
    let outLines = lines out
    assertBool "knowledge title hit appears first" (kid `isInfixOf` head outLines)

testSearchKindTask :: IO ()
testSearchKindTask = withTempDb $ \db -> do
    (_, _, _) <- runIcarium db ["task", "add", "needle task", "--state", "ready"]
    (_, _, _) <- runIcarium db ["know", "add", "needle knowledge"]
    (code, out, _) <- runIcarium db ["search", "needle", "--kind", "task"]
    code @?= ExitSuccess
    assertBool "task result present" ("needle task" `isInfixOf` out)
    assertBool "knowledge result absent" (not ("needle knowledge" `isInfixOf` out))

testSearchNoSnippet :: IO ()
testSearchNoSnippet = withTempDb $ \db -> do
    (_, _, _) <- runIcarium db ["know", "add", "some title", "--body", "the body has needle123 inside it"]
    (code, out, _) <- runIcarium db ["search", "needle123", "--no-snippet"]
    code @?= ExitSuccess
    assertBool "title line present" ("some title" `isInfixOf` out)
    assertBool "snippet line absent" (not ("needle123" `isInfixOf` unlines (tail (lines out))))

testSearchNoMatches :: IO ()
testSearchNoMatches = withTempDb $ \db -> do
    (code, out, _) <- runIcarium db ["search", "xyzzy_nothing_matches_this_99"]
    code @?= ExitSuccess
    assertBool "(no matches) printed" ("(no matches)" `isInfixOf` out)
