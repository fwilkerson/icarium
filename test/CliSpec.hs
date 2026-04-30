module CliSpec (tests) where

import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.List                  (isInfixOf)
import           System.Directory           (makeAbsolute)
import           System.Exit                (ExitCode (..))
import           System.IO.Temp             (withSystemTempDirectory)
import           System.Process.Typed       (proc, readProcess, setWorkingDir)
import           Test.Tasty                 (TestTree, testGroup)
import           Test.Tasty.HUnit           (assertBool, testCase, (@?=))

bin :: FilePath
bin = "./bin/icarium"

runIcarium :: FilePath -> [String] -> IO (ExitCode, String, String)
runIcarium db args = do
    (code, outBs, errBs) <- readProcess (proc bin (["--db", db] ++ args))
    pure (code, BLC.unpack outBs, BLC.unpack errBs)

-- | Run icarium with a custom working directory so config loading picks
-- up a test-supplied icarium.toml. Both the binary and db paths are
-- resolved against the test's cwd before chdir, so they stay valid.
runIcariumIn :: FilePath -> FilePath -> [String] -> IO (ExitCode, String, String)
runIcariumIn workdir db args = do
    absBin <- makeAbsolute bin
    absDb  <- makeAbsolute db
    (code, outBs, errBs) <- readProcess
        $ setWorkingDir workdir
        $ proc absBin (["--db", absDb] ++ args)
    pure (code, BLC.unpack outBs, BLC.unpack errBs)

withTempDb :: (FilePath -> IO a) -> IO a
withTempDb k = withSystemTempDirectory "icarium-test" $ \dir ->
    k (dir <> "/icarium.db")

tests :: TestTree
tests = testGroup "CLI integration"
    [ testCase "task add/list/show roundtrip"                      testTaskRoundtrip
    , testCase "task update --state changes state"                 testTaskUpdateState
    , testCase "dispatch list on empty DB exits 0"                 testDispatchListEmpty
    , testCase "task next exits 1 on empty queue"                  testTaskNextEmpty
    , testCase "task next prints id on non-empty"                  testTaskNextNonEmpty
    , testCase "task add --depends-on bad id exits 2"              testTaskAddBadDependsOn
    , testCase "task add --state blocked exits 2"                  testTaskAddStateBlocked
    , testCase "dispatch run drains empty queue without --max"      testDispatchRunEmptyQueue
    , testCase "dispatch run ignores stale max_dispatches_per_run" testDispatchRunStaleConfig
    ]

testTaskRoundtrip :: IO ()
testTaskRoundtrip = withTempDb $ \db -> do
    (addCode, addOut, _) <- runIcarium db ["task", "add", "My roundtrip task", "--state", "ready"]
    addCode @?= ExitSuccess
    let tid = head (words addOut)
    assertBool "task id non-empty" (not (null tid))

    (lCode, lOut, _) <- runIcarium db ["task", "list"]
    lCode @?= ExitSuccess
    assertBool "list shows title"   ("My roundtrip task" `isInfixOf` lOut)
    assertBool "list shows id prefix" (take 10 tid `isInfixOf` lOut)

    (sCode, sOut, _) <- runIcarium db ["task", "show", take 10 tid]
    sCode @?= ExitSuccess
    assertBool "show contains full id"  (tid `isInfixOf` sOut)
    assertBool "show contains title"    ("My roundtrip task" `isInfixOf` sOut)

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

-- | `dispatch run` with an empty ready queue and no `--max` should exit
-- 0 and report "ready queue empty" — i.e. no built-in run-level cap is
-- preventing it from reaching that branch.
testDispatchRunEmptyQueue :: IO ()
testDispatchRunEmptyQueue = withSystemTempDirectory "icarium-test" $ \dir -> do
    let db = dir <> "/icarium.db"
    writeFile (dir <> "/icarium.toml") minimalIcariumToml
    (code, _, err) <- runIcariumIn dir db ["dispatch", "run"]
    code @?= ExitSuccess
    assertBool "stderr reports empty queue" ("ready queue empty" `isInfixOf` err)

-- | A config that still carries the now-removed `max_dispatches_per_run`
-- field must load cleanly. tomland tolerates unknown keys, so the
-- silent-ignore behavior is contractual.
testDispatchRunStaleConfig :: IO ()
testDispatchRunStaleConfig = withSystemTempDirectory "icarium-test" $ \dir -> do
    let db = dir <> "/icarium.db"
    writeFile (dir <> "/icarium.toml") staleIcariumToml
    (code, _, err) <- runIcariumIn dir db ["dispatch", "run"]
    code @?= ExitSuccess
    assertBool "stale field did not break config load"
        ("ready queue empty" `isInfixOf` err)
    assertBool "no parse error surfaced"
        (not ("config parse error" `isInfixOf` err))

minimalIcariumToml :: String
minimalIcariumToml = unlines
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
staleIcariumToml = unlines
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
