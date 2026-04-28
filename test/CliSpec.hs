module CliSpec (tests) where

import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.List                  (isInfixOf)
import           System.Exit                (ExitCode (..))
import           System.IO.Temp             (withSystemTempDirectory)
import           System.Process.Typed       (proc, readProcess)
import           Test.Tasty                 (TestTree, testGroup)
import           Test.Tasty.HUnit           (assertBool, testCase, (@?=))

bin :: FilePath
bin = "./bin/icarium"

runIcarium :: FilePath -> [String] -> IO (ExitCode, String, String)
runIcarium db args = do
    (code, outBs, errBs) <- readProcess (proc bin (["--db", db] ++ args))
    pure (code, BLC.unpack outBs, BLC.unpack errBs)

withTempDb :: (FilePath -> IO a) -> IO a
withTempDb k = withSystemTempDirectory "icarium-test" $ \dir ->
    k (dir <> "/icarium.db")

tests :: TestTree
tests = testGroup "CLI integration"
    [ testCase "task add/list/show roundtrip"          testTaskRoundtrip
    , testCase "task update --state changes state"     testTaskUpdateState
    , testCase "dispatch list on empty DB exits 0"     testDispatchListEmpty
    , testCase "task next exits 1 on empty queue"      testTaskNextEmpty
    , testCase "task next prints id on non-empty"      testTaskNextNonEmpty
    , testCase "task add --depends-on bad id exits 2"  testTaskAddBadDependsOn
    , testCase "task add --state blocked exits 2"      testTaskAddStateBlocked
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
