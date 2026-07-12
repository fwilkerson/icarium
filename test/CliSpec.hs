module CliSpec (tests) where

import Control.Exception (bracket, evaluate)
import Control.Monad (when)
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.Char (isSpace)
import Data.List (dropWhileEnd, isInfixOf, isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))
import Database.SQLite.Simple (Query (..), close, execute, execute_, open)
import Icarium.Schema (execSql, schemaSql)
import System.Directory (createDirectoryIfMissing, doesFileExist, makeAbsolute, setModificationTime)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.IO.Unsafe (unsafePerformIO)
import System.Process.Typed (proc, readProcess, setEnv, setWorkingDir)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import TestHelpers (withTestRepo)

-- Resolved once at load time so concurrent cwd changes in PostClaudeSpec
-- tests (which use withCurrentDirectory) don't corrupt makeAbsolute calls.
{-# NOINLINE absBin #-}
absBin :: FilePath
absBin = unsafePerformIO (makeAbsolute "./bin/icarium")

runIcarium :: FilePath -> [String] -> IO (ExitCode, String, String)
runIcarium db args = do
    (code, outBs, errBs) <- readProcess (proc absBin (["--db", db] ++ args))
    pure (code, BLC.unpack outBs, BLC.unpack errBs)

{- | Run icarium with a custom working directory so config loading picks
up a test-supplied icarium.toml. Both the binary and db paths are
resolved against the test's cwd before chdir, so they stay valid.
-}
runIcariumIn :: FilePath -> FilePath -> [String] -> IO (ExitCode, String, String)
runIcariumIn workdir db args = do
    absDb <- makeAbsolute db
    (code, outBs, errBs) <-
        readProcess $
            setWorkingDir workdir $
                proc absBin (["--db", absDb] ++ args)
    pure (code, BLC.unpack outBs, BLC.unpack errBs)

withTempDb :: (FilePath -> IO a) -> IO a
withTempDb k = withSystemTempDirectory "icarium-test" $ \dir ->
    k (dir <> "/icarium.db")

{- | Run icarium with no --db flag; ICARIUM_DB in the environment resolves
the db path instead. Replaces any inherited ICARIUM_DB so the value passed
here always wins.
-}
runIcariumEnvDb :: FilePath -> [String] -> IO (ExitCode, String, String)
runIcariumEnvDb db args = do
    parentEnv <- getEnvironment
    let env = ("ICARIUM_DB", db) : filter ((/= "ICARIUM_DB") . fst) parentEnv
    (code, outBs, errBs) <- readProcess (setEnv env (proc absBin args))
    pure (code, BLC.unpack outBs, BLC.unpack errBs)

runIcariumBare :: [String] -> IO (ExitCode, String, String)
runIcariumBare args = do
    (code, outBs, errBs) <- readProcess (proc absBin args)
    pure (code, BLC.unpack outBs, BLC.unpack errBs)

tests :: TestTree
tests =
    testGroup
        "CLI integration"
        [ testCase "--version prints icarium <semver> and exits 0" testVersion
        , testCase "-V short form works" testVersionShort
        , testCase "task add/list/show roundtrip" testTaskRoundtrip
        , testCase "task update --state changes state" testTaskUpdateState
        , testCase "task list --limit caps rows" testTaskListLimit
        , testCase "ctx list --limit caps rows" testCtxListLimit
        , testCase "dispatch list on empty DB exits 0" testDispatchListEmpty
        , testCase "dispatch list --limit caps rows" testDispatchListLimit
        , testCase "task next exits 1 on empty queue" testTaskNextEmpty
        , testCase "task next prints id on non-empty" testTaskNextNonEmpty
        , testCase "task add --depends-on bad id exits 2" testTaskAddBadDependsOn
        , testCase "task add --state blocked exits 2" testTaskAddStateBlocked
        , testCase "dispatch run drains empty queue without --max" testDispatchRunEmptyQueue
        , testCase "dispatch run ignores stale max_dispatches_per_run" testDispatchRunStaleConfig
        , testCase "ctx list shows cats and linked count" testContextListLayout
        , testCase "link --help has corrected argument order example" testLinkHelpExample
        , testCase "link list emits header row when edges exist" testLinkListHeader
        , testCase "ctx add --help and link add --help cross-reference each other" testHelpCrossRef
        , testCase "task path → body file contains body" testTaskShowBody
        , testCase "task show --prompt works" testTaskShowPrompt
        , testCase "ctx path → body file contains body" testCtxShowBody
        , testCase "task add prints id and body path; path matches" testTaskBodyRoundTrip
        , -- bare icarium (no args) prints full help
          testCase "bare icarium prints help and exits 0" testBareIcariumHelp
        , -- bare noun group prints help, not list
          testCase "bare task prints help and exits non-zero" testBareTaskHelp
        , testCase "bare ctx prints help and exits non-zero" testBareCtxHelp
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
        , testCase "search: title hit on context outranks body hit on task" testSearchCrossKindRank
        , testCase "search: --kind task excludes context results" testSearchKindTask
        , testCase "search: --no-snippet suppresses indented line" testSearchNoSnippet
        , testCase "search: empty result prints (no matches)" testSearchNoMatches
        , testCase "search: multi-word AND matches both tokens in any order" testSearchAndTokens
        , testCase "search: quoted phrase requires contiguous match" testSearchPhraseSemantics
        , testCase "search: OR returns union of token matches" testSearchOrSemantics
        , testCase "search: space-separated tokens find underscore-joined corpus entry" testSearchSnakeCaseFallback
        , testCase "search: --limit truncation shows footer with total count" testSearchTruncationFooter
        , testCase "search: stale context ranks below live context at same relevance" testSearchStaleLast
        , testCase "search: snippet collapses embedded newlines to single line" testSearchSnippetSingleLine
        , testCase "search: match-source indicator [t] shown for title-only match" testSearchMatchSourceTitle
        , testCase "search: match-source indicator [b] shown for body-only match" testSearchMatchSourceBody
        , testCase "search: match-source indicator [t+b] shown when match in both" testSearchMatchSourceBoth
        , testCase "search: --title-only excludes body-only matches" testSearchTitleOnlyFlag
        , testCase "search: --body-only excludes title-only matches" testSearchBodyOnlyFlag
        , testCase "search: --domain filters to tagged entries" testSearchDomainFlag
        , testCase "search: --exclude-discipline removes tagged entries" testSearchExcludeDisciplineFlag
        , testCase "dispatch show: tokens line present when values set" testDispatchShowTokensPresent
        , testCase "dispatch show: tokens line absent when all NULL" testDispatchShowTokensAbsent
        , testCase "task add --no-commit sets flag; task show displays it" testTaskNoCommitAddShow
        , testCase "task update --no-commit and --commit-required toggle flag" testTaskNoCommitUpdate
        , testCase "dispatch quarantine: blocked upstream excludes dependent from ready queue" testDispatchQuarantine
        , testCase "task show (human) prints body path, not body content" testTaskShowBodyPath
        , testCase "ctx show prints body path, not body content" testCtxShowBodyPath
        , testCase "task cat prints body to stdout" testTaskCat
        , testCase "task cat on no-body task prints empty and exits 0" testTaskCatNoBody
        , testCase "ctx cat prints body to stdout" testCtxCat
        , testCase "ctx cat on no-body entry prints empty and exits 0" testCtxCatNoBody
        , testCase "mtime sweep: external body edit is re-indexed" testMtimeSweepReindex
        , testCase "orphan sweep: stray .md file moved to trash on sync command" testOrphanRemoval
        , testCase "orphan sweep: read-only command leaves orphan body file intact" testReadOnlyCommandPreservesOrphan
        , testCase "reindex: rebuilds FTS from DB after body_fts wipe" testReindexRestoresFts
        , testCase "dispatch dry-run: prompt reads body file even when sweep is blind" testDryRunPromptReadsBodyFile
        , testCase "link add ctx references ctx is accepted" testLinkAddCtxReferencesCtx
        , testCase "ctx children lists direct children by edge kind" testCtxChildren
        , testCase "ctx tree recurses and detects cycles" testCtxTree
        , testCase "task exists: found exits 0, not-found exits 1, ambiguous exits 2" testTaskExists
        , testCase "task exists --verbose prints full id on match" testTaskExistsVerbose
        , testCase "ctx exists: found exits 0, not-found exits 1, ambiguous exits 2" testCtxExists
        , testCase "ctx exists --verbose prints full id on match" testCtxExistsVerbose
        , testCase "ctx list on externally-created DB (user_version=0) exits 0" testCtxListOnExternalDb
        , testCase "doctor: no [commands] section does not FAIL config" testDoctorNoCommandsSection
        , testCase "ICARIUM_DB env resolves db path when --db is not given" testDbEnvFallback
        , testCase "explicit --db wins over ICARIUM_DB" testDbFlagOverridesEnv
        , -- worktree dispatch + merge (stub claude on PATH)
          testCase "dispatch: invoking checkout untouched from dirty feature branch" testDispatchInvokingCheckoutUntouched
        , testCase "dispatch: child icarium hits parent DB, no nested store" testDispatchNoNestedStore
        , testCase "dispatch: success parks the branch (merge_sha NULL, task done)" testDispatchParks
        , testCase "dispatch merge: fast-forward in place when base checked out clean" testMergeFFInPlace
        , testCase "dispatch merge: fast-forward via temp worktree when base checked out elsewhere" testMergeFFTempWorktree
        , testCase "dispatch merge: dirty base checkout is fatal" testMergeDirtyBaseFatal
        , testCase "dispatch merge: base moved rebases and re-runs gates" testMergeRebaseRegate
        , testCase "dispatch merge: rebase conflict stays parked, exit 3" testMergeConflictParked
        , testCase "dispatch: claude failure blocks task, retains branch, removes worktree" testDispatchFailBlocks
        , testCase "dispatch: dirty tree checkpointed as wip on branch" testDispatchDirtyCheckpoint
        , testCase "dispatch: worktree_setup exit 75 stops drain cleanly" testWorktreeSetup75
        , testCase "dispatch: worktree_setup nonzero errors a single run" testWorktreeSetupErr
        , testCase "dispatch: worktree_teardown runs on success and failure" testWorktreeTeardownRuns
        , testCase "dispatch --dry-run previews dontAsk and worktree path" testDispatchDryRun
        , testCase "dispatch recover: orphaned worktree checkpointed and removed" testDispatchRecoverWorktree
        , testCase "dispatch: no-commit task success is not parked, branch deleted" testDispatchNoCommitSuccess
        , testCase "dispatch review: reviewer sees worker's body-file edits" testReviewerSeesBodyFileEdits
        ]

testVersion :: IO ()
testVersion = do
    (code, out, _) <- runIcariumBare ["--version"]
    code @?= ExitSuccess
    assertBool "output starts with 'icarium '" ("icarium " `isPrefixOf` out)

testVersionShort :: IO ()
testVersionShort = do
    (code, out, _) <- runIcariumBare ["-V"]
    code @?= ExitSuccess
    assertBool "short form output starts with 'icarium '" ("icarium " `isPrefixOf` out)

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

testTaskListLimit :: IO ()
testTaskListLimit = withTempDb $ \db -> do
    mapM_ (\i -> runIcarium db ["task", "add", "Task " ++ show (i :: Int), "--state", "ready"]) [1 .. 5 :: Int]
    (code, out, _) <- runIcarium db ["task", "list", "--limit", "3"]
    code @?= ExitSuccess
    let rows = filter (not . null) (lines out)
    length rows @?= 3

testCtxListLimit :: IO ()
testCtxListLimit = withTempDb $ \db -> do
    mapM_ (\i -> runIcarium db ["ctx", "add", "Ctx " ++ show (i :: Int)]) [1 .. 4 :: Int]
    (code, out, _) <- runIcarium db ["ctx", "list", "--limit", "2"]
    code @?= ExitSuccess
    let rows = filter (not . null) (lines out)
    length rows @?= 2

testDispatchListEmpty :: IO ()
testDispatchListEmpty = withTempDb $ \db -> do
    (code, _, err) <- runIcarium db ["dispatch", "list"]
    code @?= ExitSuccess
    assertBool "no error output on empty dispatch list" (null err)

testDispatchListLimit :: IO ()
testDispatchListLimit = withSystemTempDirectory "icarium-test" $ \dir -> do
    let db = dir <> "/icarium.db"
    -- init DB by adding a task first
    (_, addOut, _) <- runIcarium db ["task", "add", "Limit test task", "--state", "ready"]
    let tid = head (words addOut)
    conn <- open db
    -- Insert 5 dispatches directly so we don't need a full claude run.
    -- IDs must be exactly 26 chars (ULID length).
    mapM_
        ( \i ->
            execute
                conn
                ( Query
                    "INSERT INTO dispatches \
                    \(id, task_id, branch, base_branch, base_sha, model, effort, outcome, merge_sha) \
                    \VALUES (?,?,?,?,?,?,?,?,?)"
                )
                ( "01TESTLIMIT0000000000000" ++ show (i :: Int) ++ "X"
                , tid
                , "dispatch/limit-" ++ show i
                , "main" :: String
                , "abc123" :: String
                , "claude-sonnet-4-6" :: String
                , "medium" :: String
                , "success" :: String
                , "def456" :: String
                )
        )
        [1 .. 5 :: Int]
    close conn
    -- Each dispatch row has a [success] badge; count those.
    let countSuccessRows = length . filter ("[success]" `isInfixOf`) . lines
    (code, out, _) <- runIcarium db ["dispatch", "list", "--limit", "3"]
    code @?= ExitSuccess
    assertBool "at most 3 rows with --limit 3" (countSuccessRows out <= 3)
    (code2, out2, _) <- runIcarium db ["dispatch", "list"]
    code2 @?= ExitSuccess
    assertBool "without --limit all 5 rows returned" (countSuccessRows out2 == 5)

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

testContextListLayout :: IO ()
testContextListLayout = withTempDb $ \db -> do
    -- k1: no categories, no inbound edges
    (_, _, _) <- runIcarium db ["ctx", "add", "Plain entry no links"]

    -- k2: no categories, will have one inbound references edge from a task
    (_, k2Out, _) <- runIcarium db ["ctx", "add", "Entry with one inbound link"]
    let k2Id = head (words k2Out)

    (_, t1Out, _) <- runIcarium db ["task", "add", "Some task", "--state", "ready"]
    let t1Id = head (words t1Out)
    (linkCode, _, _) <- runIcarium db ["link", "add", t1Id, "references", k2Id]
    linkCode @?= ExitSuccess

    (code, out, _) <- runIcarium db ["ctx", "list"]
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
    (_, kOut, _) <- runIcarium db ["ctx", "add", "Dst context"]
    let kid = head (words kOut)
    (_, _, _) <- runIcarium db ["link", "add", tid, "references", kid]

    (code, out, _) <- runIcarium db ["link", "list"]
    code @?= ExitSuccess
    assertBool "header row present" ("EDGE_ID" `isInfixOf` out)

testHelpCrossRef :: IO ()
testHelpCrossRef = withTempDb $ \db -> do
    (_, ctxOut, _) <- runIcarium db ["ctx", "add", "--help"]
    assertBool "ctx add --help mentions link add" ("link add" `isInfixOf` ctxOut)

    (_, linkOut, _) <- runIcarium db ["link", "add", "--help"]
    assertBool "link add --help mentions ctx add --derived-from" ("ctx add --derived-from" `isInfixOf` linkOut)

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
    (_, addOut, _) <- runIcarium db ["task", "add", "Prompt test task", "--state", "ready"]
    let tid = head (words addOut)

    (code, out, _) <- runIcarium db ["task", "show", tid, "--prompt"]
    code @?= ExitSuccess
    assertBool "prompt output contains task id" (tid `isInfixOf` out)
    assertBool "prompt output contains title" ("Prompt test task" `isInfixOf` out)

testCtxShowBody :: IO ()
testCtxShowBody = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["ctx", "add", "Body test entry", "--body", "context body text"]
    let kid = head (words addOut)

    (pCode, pathOut, _) <- runIcarium db ["ctx", "path", kid]
    pCode @?= ExitSuccess
    let bodyPath = head (lines pathOut)
    contents <- readFile bodyPath
    contents @?= "context body text"

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

testBareIcariumHelp :: IO ()
testBareIcariumHelp = do
    (code, out, _) <- runIcariumBare []
    code @?= ExitSuccess
    assertBool "bare icarium shows Available commands" ("Available commands:" `isInfixOf` out)
    assertBool "bare icarium lists task subcommand" ("task" `isInfixOf` out)

testBareTaskHelp :: IO ()
testBareTaskHelp = withTempDb $ \db -> do
    (code, out, _) <- runIcarium db ["task"]
    code @?= ExitSuccess
    assertBool "bare task shows full help" ("Available commands:" `isInfixOf` out)
    assertBool "bare task help notes ls alias" ("(alias: ls)" `isInfixOf` out)

testBareCtxHelp :: IO ()
testBareCtxHelp = withTempDb $ \db -> do
    (code, out, _) <- runIcarium db ["ctx"]
    code @?= ExitSuccess
    assertBool "bare ctx shows full help" ("Available commands:" `isInfixOf` out)
    assertBool "bare ctx help notes ls alias" ("(alias: ls)" `isInfixOf` out)

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
    (_, _, _) <- runIcarium db ["ctx", "add", "unrelated context", "--body", "body contains mytoken here"]
    (code, out, _) <- runIcarium db ["search", "mytoken"]
    code @?= ExitSuccess
    assertBool "title match surfaces" ("mytoken in title task" `isInfixOf` out)
    assertBool "body match surfaces" ("unrelated context" `isInfixOf` out)

testSearchCrossKindRank :: IO ()
testSearchCrossKindRank = withTempDb $ \db -> do
    (_, _, _) <- runIcarium db ["task", "add", "no match here", "--body", "body has xyzzy123", "--state", "ready"]
    (_, kOut, _) <- runIcarium db ["ctx", "add", "xyzzy123 in title context"]
    let kid = take 10 (head (words kOut))
    (code, out, _) <- runIcarium db ["search", "xyzzy123"]
    code @?= ExitSuccess
    let outLines = lines out
    assertBool "context title hit appears first" (kid `isInfixOf` head outLines)

testSearchKindTask :: IO ()
testSearchKindTask = withTempDb $ \db -> do
    (_, _, _) <- runIcarium db ["task", "add", "needle task", "--state", "ready"]
    (_, _, _) <- runIcarium db ["ctx", "add", "needle context"]
    (code, out, _) <- runIcarium db ["search", "needle", "--kind", "task"]
    code @?= ExitSuccess
    assertBool "task result present" ("needle task" `isInfixOf` out)
    assertBool "context result absent" (not ("needle context" `isInfixOf` out))

testSearchNoSnippet :: IO ()
testSearchNoSnippet = withTempDb $ \db -> do
    (_, _, _) <- runIcarium db ["ctx", "add", "some title", "--body", "the body has needle123 inside it"]
    (code, out, _) <- runIcarium db ["search", "needle123", "--no-snippet"]
    code @?= ExitSuccess
    assertBool "title line present" ("some title" `isInfixOf` out)
    assertBool "snippet line absent" (not ("needle123" `isInfixOf` unlines (tail (lines out))))

testSearchNoMatches :: IO ()
testSearchNoMatches = withTempDb $ \db -> do
    (code, out, _) <- runIcarium db ["search", "xyzzy_nothing_matches_this_99"]
    code @?= ExitSuccess
    assertBool "(no matches) printed" ("(no matches)" `isInfixOf` out)

testSearchAndTokens :: IO ()
testSearchAndTokens = withTempDb $ \db -> do
    (_, _, _) <- runIcarium db ["ctx", "add", "credentials owned by client"]
    (_, _, _) <- runIcarium db ["ctx", "add", "client only"]
    (code, out, _) <- runIcarium db ["search", "client credentials"]
    code @?= ExitSuccess
    assertBool "both-token entry present" ("credentials owned by client" `isInfixOf` out)
    assertBool "single-token entry absent" (not ("client only" `isInfixOf` out))

testSearchPhraseSemantics :: IO ()
testSearchPhraseSemantics = withTempDb $ \db -> do
    (_, _, _) <- runIcarium db ["ctx", "add", "client credentials flow"]
    (_, _, _) <- runIcarium db ["ctx", "add", "credentials for client"]
    (code, out, _) <- runIcarium db ["search", "\"client credentials\""]
    code @?= ExitSuccess
    assertBool "exact phrase present" ("client credentials flow" `isInfixOf` out)
    assertBool "out-of-order absent" (not ("credentials for client" `isInfixOf` out))

testSearchOrSemantics :: IO ()
testSearchOrSemantics = withTempDb $ \db -> do
    (_, _, _) <- runIcarium db ["ctx", "add", "foo topic"]
    (_, _, _) <- runIcarium db ["ctx", "add", "bar topic"]
    (_, _, _) <- runIcarium db ["ctx", "add", "unrelated"]
    (code, out, _) <- runIcarium db ["search", "foo OR bar"]
    code @?= ExitSuccess
    assertBool "foo entry present" ("foo topic" `isInfixOf` out)
    assertBool "bar entry present" ("bar topic" `isInfixOf` out)
    assertBool "unrelated absent" (not ("unrelated" `isInfixOf` out))

testSearchSnakeCaseFallback :: IO ()
testSearchSnakeCaseFallback = withTempDb $ \db -> do
    (_, _, _) <- runIcarium db ["ctx", "add", "client_credentials"]
    (_, _, _) <- runIcarium db ["ctx", "add", "unrelated"]
    (code, out, _) <- runIcarium db ["search", "client credentials"]
    code @?= ExitSuccess
    assertBool "snake_case entry found" ("client_credentials" `isInfixOf` out)
    assertBool "unrelated absent" (not ("unrelated" `isInfixOf` out))

testSearchTruncationFooter :: IO ()
testSearchTruncationFooter = withTempDb $ \db -> do
    mapM_
        (\i -> runIcarium db ["ctx", "add", "needle entry " ++ show (i :: Int)])
        [1 .. 5]
    -- limit 3 with 5 total matches should produce a footer
    (code, out, _) <- runIcarium db ["search", "needle", "--limit", "3"]
    code @?= ExitSuccess
    assertBool "footer mentions total count" ("of 5 matches" `isInfixOf` out)
    assertBool "footer mentions shown count" ("showing 3" `isInfixOf` out)
    -- when all results fit, no footer
    (code2, out2, _) <- runIcarium db ["search", "needle", "--limit", "10"]
    code2 @?= ExitSuccess
    assertBool "no footer when all results fit" (not ("showing" `isInfixOf` out2))

testSearchStaleLast :: IO ()
testSearchStaleLast = withTempDb $ \db -> do
    (_, liveOut, _) <- runIcarium db ["ctx", "add", "zqtoken live entry"]
    let liveId = take 10 (head (words liveOut))
    (_, staleOut, _) <- runIcarium db ["ctx", "add", "zqtoken stale entry"]
    let staleId = take 10 (head (words staleOut))
    -- mark the stale entry as stale
    (_, _, _) <- runIcarium db ["ctx", "update", staleId, "--stale"]
    (code, out, _) <- runIcarium db ["search", "zqtoken"]
    code @?= ExitSuccess
    let outLines = lines out
        liveIdx = head [i | (i, l) <- zip [0 :: Int ..] outLines, liveId `isInfixOf` l]
        staleIdx = head [i | (i, l) <- zip [0 :: Int ..] outLines, staleId `isInfixOf` l]
    assertBool "live entry ranks above stale entry" (liveIdx < staleIdx)

testSearchSnippetSingleLine :: IO ()
testSearchSnippetSingleLine = withTempDb $ \db -> do
    let multilineBody = "line one\nline two with needleXYZ here\nline three"
    (_, _, _) <- runIcarium db ["ctx", "add", "multiline body entry", "--body", multilineBody]
    (code, out, _) <- runIcarium db ["search", "needleXYZ"]
    code @?= ExitSuccess
    let snippetLines = filter ("needleXYZ" `isInfixOf`) (lines out)
    assertBool "snippet line found" (not (null snippetLines))
    assertBool "snippet is a single line (no embedded newlines)" (length snippetLines == 1)

testSearchMatchSourceTitle :: IO ()
testSearchMatchSourceTitle = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["ctx", "add", "titletoken in title", "--body", "unrelated body"]
    let cid = take 10 (head (words addOut))
    (code, out, _) <- runIcarium db ["search", "titletoken"]
    code @?= ExitSuccess
    let hitLine = head [l | l <- lines out, cid `isInfixOf` l]
    assertBool "row carries [t] indicator" ("[t]" `isInfixOf` hitLine)
    assertBool "row does not carry [b] indicator" (not ("[b]" `isInfixOf` hitLine))

testSearchMatchSourceBody :: IO ()
testSearchMatchSourceBody = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["ctx", "add", "neutral title", "--body", "bodytoken appears here"]
    let cid = take 10 (head (words addOut))
    (code, out, _) <- runIcarium db ["search", "bodytoken"]
    code @?= ExitSuccess
    let hitLine = head [l | l <- lines out, cid `isInfixOf` l]
    assertBool "row carries [b] indicator" ("[b]" `isInfixOf` hitLine)
    assertBool "row does not carry [t+b] indicator" (not ("[t+b]" `isInfixOf` hitLine))

testSearchMatchSourceBoth :: IO ()
testSearchMatchSourceBoth = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["ctx", "add", "bothtoken in title", "--body", "and bothtoken in body"]
    let cid = take 10 (head (words addOut))
    (code, out, _) <- runIcarium db ["search", "bothtoken"]
    code @?= ExitSuccess
    let hitLine = head [l | l <- lines out, cid `isInfixOf` l]
    assertBool "row carries [t+b] indicator" ("[t+b]" `isInfixOf` hitLine)

testSearchTitleOnlyFlag :: IO ()
testSearchTitleOnlyFlag = withTempDb $ \db -> do
    (_, titleAdd, _) <- runIcarium db ["ctx", "add", "scopetoken in title", "--body", "unrelated"]
    let titleId = take 10 (head (words titleAdd))
    (_, bodyAdd, _) <- runIcarium db ["ctx", "add", "unrelated heading", "--body", "scopetoken in body"]
    let bodyId = take 10 (head (words bodyAdd))
    (code, out, _) <- runIcarium db ["search", "scopetoken", "--title-only"]
    code @?= ExitSuccess
    assertBool "title-only hit retained" (titleId `isInfixOf` out)
    assertBool "body-only hit excluded" (not (bodyId `isInfixOf` out))

testSearchBodyOnlyFlag :: IO ()
testSearchBodyOnlyFlag = withTempDb $ \db -> do
    (_, titleAdd, _) <- runIcarium db ["ctx", "add", "btoken in title", "--body", "unrelated"]
    let titleId = take 10 (head (words titleAdd))
    (_, bodyAdd, _) <- runIcarium db ["ctx", "add", "unrelated heading", "--body", "btoken in body"]
    let bodyId = take 10 (head (words bodyAdd))
    (code, out, _) <- runIcarium db ["search", "btoken", "--body-only"]
    code @?= ExitSuccess
    assertBool "body-only hit retained" (bodyId `isInfixOf` out)
    assertBool "title-only hit excluded" (not (titleId `isInfixOf` out))

testSearchDomainFlag :: IO ()
testSearchDomainFlag = withTempDb $ \db -> do
    -- Seed an untagged ctx first so the DB and schema exist, then insert
    -- a `domain=cli` category via raw SQL (CLI tests don't use TestHelpers).
    (_, untaggedAdd, _) <- runIcarium db ["ctx", "add", "domtoken untagged"]
    let untaggedId = take 10 (head (words untaggedAdd))
    seedCategory db "domain" "cli"
    (_, taggedAdd, _) <- runIcarium db ["ctx", "add", "domtoken tagged", "--domain", "cli"]
    let taggedId = take 10 (head (words taggedAdd))
    (code, out, _) <- runIcarium db ["search", "domtoken", "--domain", "cli"]
    code @?= ExitSuccess
    assertBool "tagged entry retained" (taggedId `isInfixOf` out)
    assertBool "untagged entry excluded" (not (untaggedId `isInfixOf` out))

testSearchExcludeDisciplineFlag :: IO ()
testSearchExcludeDisciplineFlag = withTempDb $ \db -> do
    (_, keptAdd, _) <- runIcarium db ["ctx", "add", "exctoken kept"]
    let keptId = take 10 (head (words keptAdd))
    seedCategory db "discipline" "haskell"
    (_, noisyAdd, _) <- runIcarium db ["ctx", "add", "exctoken noisy", "--discipline", "haskell"]
    let noisyId = take 10 (head (words noisyAdd))
    (code, out, _) <- runIcarium db ["search", "exctoken", "--exclude-discipline", "haskell"]
    code @?= ExitSuccess
    assertBool "non-tagged entry retained" (keptId `isInfixOf` out)
    assertBool "haskell-tagged entry excluded" (not (noisyId `isInfixOf` out))

seedCategory :: FilePath -> String -> String -> IO ()
seedCategory db axis name = do
    conn <- open db
    execute
        conn
        (Query "INSERT INTO categories (id, axis, name) VALUES (?, ?, ?)")
        ("01TESTCAT" ++ replicate 14 '0' ++ take 3 (name ++ "XXX"), axis, name)
    close conn

testDispatchShowTokensPresent :: IO ()
testDispatchShowTokensPresent = withSystemTempDirectory "icarium-test" $ \dir -> do
    let db = dir <> "/icarium.db"
    (_, addOut, _) <- runIcarium db ["task", "add", "Token task", "--state", "ready"]
    let tid = head (words addOut)
        did = "01TOKN0000000000000000001T"
    conn <- open db
    execute
        conn
        ( Query
            "INSERT INTO dispatches \
            \(id, task_id, branch, base_branch, base_sha, model, effort, \
            \ tokens_in, tokens_out, tokens_cache_read) \
            \VALUES (?,?,?,?,?,?,?,?,?,?)"
        )
        ( did
        , tid
        , "dispatch/" ++ did
        , "main" :: String
        , "abc123" :: String
        , "claude-sonnet-4-6" :: String
        , "medium" :: String
        , 1234 :: Int
        , 567 :: Int
        , 89 :: Int
        )
    close conn
    (code, out, _) <- runIcarium db ["dispatch", "show", did]
    code @?= ExitSuccess
    assertBool "tokens line present" ("tokens:" `isInfixOf` out)
    assertBool "in count" ("in 1234" `isInfixOf` out)
    assertBool "out count" ("out 567" `isInfixOf` out)
    assertBool "cache_read count" ("cache_read 89" `isInfixOf` out)

testDispatchShowTokensAbsent :: IO ()
testDispatchShowTokensAbsent = withSystemTempDirectory "icarium-test" $ \dir -> do
    let db = dir <> "/icarium.db"
    (_, addOut, _) <- runIcarium db ["task", "add", "No token task", "--state", "ready"]
    let tid = head (words addOut)
        did = "01TOKN0000000000000000002T"
    conn <- open db
    execute
        conn
        ( Query
            "INSERT INTO dispatches \
            \(id, task_id, branch, base_branch, base_sha, model, effort) \
            \VALUES (?,?,?,?,?,?,?)"
        )
        ( did
        , tid
        , "dispatch/" ++ did
        , "main" :: String
        , "abc123" :: String
        , "claude-sonnet-4-6" :: String
        , "medium" :: String
        )
    close conn
    (code, out, _) <- runIcarium db ["dispatch", "show", did]
    code @?= ExitSuccess
    assertBool "tokens line absent" (not ("tokens:" `isInfixOf` out))

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

{- | Quarantine contract: a failed dispatch sets its task to 'blocked'.
The ready_tasks view (used by dispatch run and task next) excludes any
task whose depends_on target is not 'done', so the dependent is silently
quarantined until the upstream is resolved. Independent tasks keep
draining normally.

We simulate a failed dispatch by blocking task A directly; the view
doesn't care how it got there.
-}
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

testCtxShowBodyPath :: IO ()
testCtxShowBodyPath = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["ctx", "add", "Body path test entry", "--body", "secret context body"]
    let outLines = lines addOut
        cxid = head outLines
        bodyPath = outLines !! 1

    (code, out, _) <- runIcarium db ["ctx", "show", cxid]
    code @?= ExitSuccess
    assertBool "show contains body path" (bodyPath `isInfixOf` out)
    assertBool "show does not contain body content" (not ("secret context body" `isInfixOf` out))
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

testCtxCat :: IO ()
testCtxCat = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["ctx", "add", "Cat context", "--body", "ctx body line one\nctx body line two"]
    let cxid = head (lines addOut)

    (code, out, _) <- runIcarium db ["ctx", "cat", cxid]
    code @?= ExitSuccess
    out @?= "ctx body line one\nctx body line two"

testCtxCatNoBody :: IO ()
testCtxCatNoBody = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["ctx", "add", "No body context"]
    let cxid = head (lines addOut)

    (code, out, _) <- runIcarium db ["ctx", "cat", cxid]
    code @?= ExitSuccess
    out @?= ""

-- =============================================================
-- body-files sync tests (d7d13fa)
-- =============================================================

{- | Write new content to a body file and set its mtime to a far-future
time so the sweep condition (file_mtime > updated_at) is guaranteed to
fire on the next sync command.  The tasks_touch trigger resets updated_at on
every UPDATE, so we cannot rewind it via SQL; bumping the file mtime is
the reliable alternative.
-}
testMtimeSweepReindex :: IO ()
testMtimeSweepReindex = withSystemTempDirectory "icarium-test" $ \dir -> do
    let db = dir <> "/icarium.db"
    (_, addOut, _) <- runIcarium db ["task", "add", "sweep-test task", "--body", "original xsweep1"]
    let outLines = lines addOut
        bodyPath = outLines !! 1
    -- external edit with future mtime so sweep always triggers
    writeFile bodyPath "edited xsweep2"
    setModificationTime bodyPath (UTCTime (fromGregorian 2099 1 1) 0)
    -- search triggers withDbSync → mtimeSweep runs and reindexes the edited file
    _ <- runIcarium db ["search", "xsweep1"]
    -- new content must be findable via FTS
    (code, out, _) <- runIcarium db ["search", "xsweep2"]
    code @?= ExitSuccess
    assertBool "edited body content surfaces in search" ("sweep-test task" `isInfixOf` out)

{- | A .md file placed in bodies/tasks/ with no matching DB row is an
orphan.  When a sync command runs, the orphan is moved to .trash/ and a
warn: line is emitted on stderr.  The original path is vacated.
-}
testOrphanRemoval :: IO ()
testOrphanRemoval = withSystemTempDirectory "icarium-test" $ \dir -> do
    let db = dir <> "/icarium.db"
        orphanFile = dir </> "bodies" </> "tasks" </> "01ORPHAN0000000000000000XX.md"
    -- seed the DB so that bodies/tasks/ gets created
    _ <- runIcarium db ["task", "add", "seed task"]
    -- plant a stray file with no DB row
    writeFile orphanFile "orphan content"
    -- search is a sync command; triggers orphanScan
    (_, _, err) <- runIcarium db ["search", "seed"]
    assertBool "warn: emitted for orphan" ("warn:" `isInfixOf` err)
    gone <- not <$> doesFileExist orphanFile
    assertBool "orphan file moved from original location" gone

{- | A read-only command must NOT trigger mtimeSweep, so an orphan body file
placed before the command must still be present afterwards.
-}
testReadOnlyCommandPreservesOrphan :: IO ()
testReadOnlyCommandPreservesOrphan = withSystemTempDirectory "icarium-test" $ \dir -> do
    let db = dir <> "/icarium.db"
        orphanFile = dir </> "bodies" </> "tasks" </> "01ORPHANREAD00000000000000.md"
    -- seed the DB so that bodies/tasks/ gets created
    _ <- runIcarium db ["task", "add", "read-only seed task"]
    -- plant a stray file with no DB row
    writeFile orphanFile "orphan content"
    -- task list is a read-only command; must not run orphanScan
    _ <- runIcarium db ["task", "list"]
    stillThere <- doesFileExist orphanFile
    assertBool "orphan file untouched by read-only command" stillThere

{- | After wiping body_fts, search should miss (we pin updated_at to the
future so mtimeSweep does not auto-repair it).  icarium reindex rebuilds
the index from the body column; search must find the entry afterwards.
-}
testReindexRestoresFts :: IO ()
testReindexRestoresFts = withSystemTempDirectory "icarium-test" $ \dir -> do
    let db = dir <> "/icarium.db"
    _ <- runIcarium db ["ctx", "add", "reindex test entry", "--body", "xreindex999 unique token"]
    -- wipe FTS and pin updated_at to the future so mtimeSweep won't repair it
    conn <- open db
    execute_ conn "DELETE FROM body_fts"
    execute_ conn "UPDATE context SET updated_at = '2099-01-01 00:00:00'"
    close conn
    -- search must miss before reindex
    (_, outBefore, _) <- runIcarium db ["search", "xreindex999"]
    assertBool "search misses before reindex" (not ("reindex test entry" `isInfixOf` outBefore))
    -- reindex rebuilds FTS from DB body column
    (rCode, _, _) <- runIcarium db ["reindex"]
    rCode @?= ExitSuccess
    -- now search must find the entry
    (code, out, _) <- runIcarium db ["search", "xreindex999"]
    code @?= ExitSuccess
    assertBool "entry found after reindex" ("reindex test entry" `isInfixOf` out)

{- | Regression for issue #8: the dispatch prompt must render the body FILE
even when mtimeSweep is blind to it. A PAST mtime guarantees the sweep's
(mtime > updated_at) check cannot fire, so only the dispatch-time refresh
can explain the fresh content. The refresh also writes through to the
column + FTS, hence the search assert.
-}
testDryRunPromptReadsBodyFile :: IO ()
testDryRunPromptReadsBodyFile = withSystemTempDirectory "icarium-test" $ \dir -> do
    let db = dir </> "icarium.db"
    writeFile (dir </> "icarium.toml") minimalIcariumToml
    (_, addOut, _) <- runIcarium db ["task", "add", "stale-body task", "--state", "ready", "--body", "original xstale1"]
    let tid = head (words addOut)
        bodyPath = lines addOut !! 1
    writeFile bodyPath "edited xstale2"
    setModificationTime bodyPath (UTCTime (fromGregorian 2000 1 1) 0)
    (code, out, _) <- runIcariumIn dir db ["dispatch", "run", tid, "--dry-run"]
    code @?= ExitSuccess
    assertBool "prompt shows body-file content" ("edited xstale2" `isInfixOf` out)
    assertBool "stale column content absent from prompt" (not ("original xstale1" `isInfixOf` out))
    (sCode, sOut, _) <- runIcarium db ["search", "xstale2"]
    sCode @?= ExitSuccess
    assertBool "refreshed body searchable via FTS" ("stale-body task" `isInfixOf` sOut)

testDispatchQuarantine :: IO ()
testDispatchQuarantine = withTempDb $ \db -> do
    -- A: will be blocked (simulating a failed dispatch)
    (_, aOut, _) <- runIcarium db ["task", "add", "Upstream task A", "--state", "ready"]
    let aId = head (words aOut)

    -- B: depends on A; must be quarantined when A is blocked
    (_, bOut, _) <- runIcarium db ["task", "add", "Dependent task B", "--state", "ready", "--depends-on", aId]
    let bId = head (words bOut)

    -- C: independent; must still be drainable after A is blocked
    (_, cOut, _) <- runIcarium db ["task", "add", "Independent task C", "--state", "ready"]
    let cId = head (words cOut)

    -- Simulate a dispatch failure: mark A blocked with a reason
    (uCode, _, _) <- runIcarium db ["task", "update", aId, "--state", "blocked", "--block-reason", "simulated dispatch failure"]
    uCode @?= ExitSuccess

    -- ready queue (task list --ready / task next) must exclude B but include C
    (lCode, lOut, _) <- runIcarium db ["task", "list", "--ready"]
    lCode @?= ExitSuccess
    assertBool "dependent B absent from ready queue" (not ("Dependent task B" `isInfixOf` lOut))
    assertBool "independent C present in ready queue" ("Independent task C" `isInfixOf` lOut)

    -- task next returns C's full id (the head of what drain would pick), not B's
    (nCode, nOut, _) <- runIcarium db ["task", "next"]
    nCode @?= ExitSuccess
    assertBool "task next picks C, not B" (cId `isInfixOf` nOut)
    assertBool "task next does not pick B" (not (bId `isInfixOf` nOut))

testLinkAddCtxReferencesCtx :: IO ()
testLinkAddCtxReferencesCtx = withTempDb $ \db -> do
    (_, aOut, _) <- runIcarium db ["ctx", "add", "Umbrella context"]
    let aId = head (words aOut)
    (_, bOut, _) <- runIcarium db ["ctx", "add", "Child context"]
    let bId = head (words bOut)

    (code, out, _) <- runIcarium db ["link", "add", bId, "references", aId]
    code @?= ExitSuccess
    assertBool "link add ctx references ctx returns edge id" (not (null out))

    (lCode, lOut, _) <- runIcarium db ["link", "list", "--to", aId]
    lCode @?= ExitSuccess
    assertBool "link list shows references edge" ("references" `isInfixOf` lOut)

testCtxChildren :: IO ()
testCtxChildren = withTempDb $ \db -> do
    (_, pOut, _) <- runIcarium db ["ctx", "add", "Parent context"]
    let pId = head (words pOut)
    (_, cOut, _) <- runIcarium db ["ctx", "add", "Child context A"]
    let cId = head (words cOut)
    (_, dOut, _) <- runIcarium db ["ctx", "add", "Child context B"]
    let dId = head (words dOut)

    _ <- runIcarium db ["link", "add", cId, "derived-from", pId]
    _ <- runIcarium db ["link", "add", dId, "references", pId]

    (code, out, _) <- runIcarium db ["ctx", "children", pId]
    code @?= ExitSuccess
    assertBool "children shows child A" ("Child context A" `isInfixOf` out)
    assertBool "children shows child B" ("Child context B" `isInfixOf` out)
    assertBool "children shows derived-from kind" ("derived-from" `isInfixOf` out)
    assertBool "children shows references kind" ("references" `isInfixOf` out)

    (fCode, fOut, _) <- runIcarium db ["ctx", "children", pId, "--kind", "derived-from"]
    fCode @?= ExitSuccess
    assertBool "--kind derived-from shows child A" ("Child context A" `isInfixOf` fOut)
    assertBool "--kind derived-from excludes child B" (not ("Child context B" `isInfixOf` fOut))

    -- no children on dId
    _ <- pure dId
    (eCode, eOut, _) <- runIcarium db ["ctx", "children", cId]
    eCode @?= ExitSuccess
    assertBool "leaf node reports no children" ("(no children)" `isInfixOf` eOut)

testCtxTree :: IO ()
testCtxTree = withTempDb $ \db -> do
    (_, rOut, _) <- runIcarium db ["ctx", "add", "Root"]
    let rId = head (words rOut)
    (_, cOut, _) <- runIcarium db ["ctx", "add", "Child"]
    let cId = head (words cOut)
    (_, gOut, _) <- runIcarium db ["ctx", "add", "Grandchild"]
    let gId = head (words gOut)

    _ <- runIcarium db ["link", "add", cId, "derived-from", rId]
    _ <- runIcarium db ["link", "add", gId, "derived-from", cId]

    (code, out, _) <- runIcarium db ["ctx", "tree", rId]
    code @?= ExitSuccess
    assertBool "tree root shows Root" ("Root" `isInfixOf` out)
    assertBool "tree shows Child" ("Child" `isInfixOf` out)
    assertBool "tree shows Grandchild" ("Grandchild" `isInfixOf` out)

    -- cycle detection: link grandchild back to root
    _ <- runIcarium db ["link", "add", rId, "references", gId]
    (cycCode, cycOut, _) <- runIcarium db ["ctx", "tree", rId]
    cycCode @?= ExitSuccess
    assertBool "cycle detected and noted" ("[cycle:" `isInfixOf` cycOut)

    pure ()

-- =============================================================
-- exists tests
-- =============================================================

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

    -- ambiguous: add a second task and use a shared prefix
    (_, addOut2, _) <- runIcarium db ["task", "add", "Exists task 2"]
    let tid2 = head (words addOut2)
    let sharedPrefix = take 5 tid
    -- only proceed with ambiguity test if the two ids actually share the prefix
    when (sharedPrefix == take 5 tid2) $ do
        (ambCode, _, ambErr) <- runIcarium db ["task", "exists", sharedPrefix]
        ambCode @?= ExitFailure 2
        assertBool "stderr mentions ambiguous" ("ambiguous" `isInfixOf` ambErr)

testTaskExistsVerbose :: IO ()
testTaskExistsVerbose = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["task", "add", "Verbose exists task"]
    let tid = head (words addOut)

    (code, out, _) <- runIcarium db ["task", "exists", "--verbose", take 10 tid]
    code @?= ExitSuccess
    assertBool "verbose output contains full id" (tid `isInfixOf` out)

testCtxExists :: IO ()
testCtxExists = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["ctx", "add", "Exists context"]
    let cxid = head (words addOut)

    -- found: full id → exit 0
    (foundCode, foundOut, _) <- runIcarium db ["ctx", "exists", cxid]
    foundCode @?= ExitSuccess
    foundOut @?= ""

    -- found: prefix → exit 0
    (prefCode, _, _) <- runIcarium db ["ctx", "exists", take 10 cxid]
    prefCode @?= ExitSuccess

    -- not found → exit 1
    (missCode, _, _) <- runIcarium db ["ctx", "exists", "01ZZZZZZZZZZZZZZZZZZZZZZZZ"]
    missCode @?= ExitFailure 1

    -- ambiguous: add a second context and use a shared prefix
    (_, addOut2, _) <- runIcarium db ["ctx", "add", "Exists context 2"]
    let cxid2 = head (words addOut2)
    let sharedPrefix = take 5 cxid
    when (sharedPrefix == take 5 cxid2) $ do
        (ambCode, _, ambErr) <- runIcarium db ["ctx", "exists", sharedPrefix]
        ambCode @?= ExitFailure 2
        assertBool "stderr mentions ambiguous" ("ambiguous" `isInfixOf` ambErr)

testCtxExistsVerbose :: IO ()
testCtxExistsVerbose = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["ctx", "add", "Verbose exists context"]
    let cxid = head (words addOut)

    (code, out, _) <- runIcarium db ["ctx", "exists", "--verbose", take 10 cxid]
    code @?= ExitSuccess
    assertBool "verbose output contains full id" (cxid `isInfixOf` out)

-- Simulate an externally-created DB: spec/schema.sql applied directly without
-- setting user_version, so SQLite leaves it at 0. migrateDb must stamp the
-- version instead of re-running CREATE TABLE (which would fail).
testCtxListOnExternalDb :: IO ()
testCtxListOnExternalDb = withSystemTempDirectory "icarium-extdb" $ \dir -> do
    let dbPath = dir </> "external.db"
    bracket (open dbPath) close $ \conn -> execSql conn schemaSql
    (code, _, _) <- runIcarium dbPath ["ctx", "list"]
    code @?= ExitSuccess

noCommandsIcariumToml :: String
noCommandsIcariumToml =
    unlines
        [ "[project]"
        , "integration_branch = \"main\""
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

testDoctorNoCommandsSection :: IO ()
testDoctorNoCommandsSection = withSystemTempDirectory "icarium-test" $ \dir -> do
    let db = dir <> "/icarium.db"
    writeFile (dir <> "/icarium.toml") noCommandsIcariumToml
    (_, out, _) <- runIcariumIn dir db ["doctor"]
    assertBool "config check passes with no [commands] section" (not ("FAIL  config" `isInfixOf` out))

-- =============================================================
-- ICARIUM_DB env fallback tests
-- =============================================================

testDbEnvFallback :: IO ()
testDbEnvFallback = withTempDb $ \db -> do
    (addCode, addOut, _) <- runIcariumEnvDb db ["task", "add", "Env db task", "--state", "ready"]
    addCode @?= ExitSuccess
    let tid = head (words addOut)
    dbExists <- doesFileExist db
    assertBool "db file created at ICARIUM_DB path" dbExists

    (listCode, listOut, _) <- runIcariumEnvDb db ["task", "list"]
    listCode @?= ExitSuccess
    assertBool "task list (via ICARIUM_DB) shows the added task" ("Env db task" `isInfixOf` listOut)
    assertBool "task list (via ICARIUM_DB) shows id prefix" (take 10 tid `isInfixOf` listOut)

testDbFlagOverridesEnv :: IO ()
testDbFlagOverridesEnv = withSystemTempDirectory "icarium-test" $ \dir -> do
    let envDb = dir </> "env.db"
        flagDb = dir </> "flag.db"
    parentEnv <- getEnvironment
    let env = ("ICARIUM_DB", envDb) : filter ((/= "ICARIUM_DB") . fst) parentEnv
    (code, addOut, _) <-
        readProcess (setEnv env (proc absBin ["--db", flagDb, "task", "add", "Flag wins task", "--state", "ready"]))
    code @?= ExitSuccess
    let tid = head (words (BLC.unpack addOut))
    flagDbExists <- doesFileExist flagDb
    envDbExists <- doesFileExist envDb
    assertBool "explicit --db path was used" flagDbExists
    assertBool "ICARIUM_DB path was not touched" (not envDbExists)

    (listCode, listOut, _) <- runIcarium flagDb ["task", "list"]
    listCode @?= ExitSuccess
    assertBool "task shows up under the --db path" (take 10 tid `isInfixOf` listOut)

-- =============================================================
-- worktree dispatch + merge (stub claude on PATH)
--
-- These drive ./bin/icarium as a subprocess against a throwaway git repo,
-- with the committed test/fixtures/claude stub resolved via PATH. The stub's
-- behavior is selected by STUB_CLAUDE_MODE (see the script). Each test gets
-- its own repo + DB, so they stay isolated under parallel execution.
-- =============================================================

-- | The committed stub-`claude` fixtures dir, resolved once at load time.
{-# NOINLINE absFixtures #-}
absFixtures :: FilePath
absFixtures = unsafePerformIO (makeAbsolute "test/fixtures")

-- | Directory holding ./bin/icarium, so a child @icarium@ resolves on PATH.
binDir :: FilePath
binDir = takeDirectory absBin

{- | Run ./bin/icarium inside a test repo with the stub @claude@ (and the
icarium binary itself) prepended to PATH. @STUB_CLAUDE_MODE@ picks the stub's
behavior. typed-process's 'setEnv' replaces the whole environment, so we
build from the parent's, override PATH, and add the mode. --db is absolute
and cwd is the repo, so the binary's @git -C .@ calls resolve there.
-}
runDispatch :: FilePath -> FilePath -> Maybe String -> [String] -> IO (ExitCode, String, String)
runDispatch repo db mMode args = do
    absDb <- makeAbsolute db
    parentEnv <- getEnvironment
    let path0 = fromMaybe "" (lookup "PATH" parentEnv)
        base = filter ((`notElem` ["PATH", "STUB_CLAUDE_MODE"]) . fst) parentEnv
        env =
            ("PATH", absFixtures <> ":" <> binDir <> ":" <> path0)
                : maybe id (\m -> (("STUB_CLAUDE_MODE", m) :)) mMode base
    (code, out, err) <-
        readProcess (setEnv env (setWorkingDir repo (proc absBin (["--db", absDb] <> args))))
    pure (code, BLC.unpack out, BLC.unpack err)

{- | A git repo with a committed .gitignore (so a worktree's .icarium/ is
ignored), a stub-friendly icarium.toml, an empty .icarium/, and one commit
on main. Yields the repo dir and the DB path (created on first command).
-}
withDispatchRepo :: (FilePath -> FilePath -> IO a) -> IO a
withDispatchRepo k =
    withTestRepo $ \dir -> do
        writeFile (dir </> ".gitignore") ".icarium/\nicarium.toml\n"
        _ <- readProcess (setWorkingDir dir (proc "git" ["add", ".gitignore"]))
        _ <- readProcess (setWorkingDir dir (proc "git" ["commit", "-m", "gitignore"]))
        writeFile (dir </> "icarium.toml") stubToml
        createDirectoryIfMissing True (dir </> ".icarium")
        k dir (dir </> ".icarium" </> "icarium.db")

-- | Default stub config: trivial gates, no worktree hooks.
stubToml :: String
stubToml = stubTomlWith "true" Nothing Nothing

{- | icarium.toml driving the stub model, with an overridable test gate and
optional worktree_setup / worktree_teardown commands.
-}
stubTomlWith :: String -> Maybe String -> Maybe String -> String
stubTomlWith testCmd mSetup mTeardown =
    unlines $
        [ "[project]"
        , "integration_branch = \"main\""
        , "[commands]"
        , "build = \"true\""
        , "test  = " <> show testCmd
        , "[dispatch]"
        , "model  = \"stub\""
        , "effort = \"low\""
        , "tools = [\"Bash\"]"
        , "allowed_tools = [\"Bash\"]"
        , "scratch_dir = \".icarium/scratch\""
        , "max_minutes_per_dispatch = 2"
        , "heartbeat_stale_seconds  = 300"
        , "log_retention_runs       = 5"
        ]
            <> maybe [] (\c -> ["worktree_setup = " <> show c]) mSetup
            <> maybe [] (\c -> ["worktree_teardown = " <> show c]) mTeardown
            <> [ "[categories]"
               , "domains     = [\"core\"]"
               , "disciplines = [\"development\"]"
               ]

{- | Count lines in a file, forcing the read fully (a plain lazy
@readFile@ leaves the handle open, so a concurrent appender can leak into
a later forced read). Used to observe gate/teardown side effects.
-}
countLines :: FilePath -> IO Int
countLines p = readFile p >>= evaluate . length . lines

-- | Run git in a repo dir; return stdout stripped of trailing whitespace.
gitOut :: FilePath -> [String] -> IO String
gitOut dir args = do
    (_, out, _) <- readProcess (setWorkingDir dir (proc "git" args))
    pure (dropWhileEnd isSpace (BLC.unpack out))

-- | Number of worktrees registered in the repo (1 = just the main checkout).
worktreeCount :: FilePath -> IO Int
worktreeCount dir = length . filter (not . null) . lines <$> gitOut dir ["worktree", "list"]

-- | Full names of any dispatch/* branches.
dispatchBranches :: FilePath -> IO [String]
dispatchBranches dir =
    filter (not . null) . lines
        <$> gitOut dir ["branch", "--list", "dispatch/*", "--format=%(refname:short)"]

-- | Add a ready task, returning its full id.
addReadyTask :: FilePath -> FilePath -> String -> IO String
addReadyTask dir db title = do
    (_, out, _) <- runDispatch dir db Nothing ["task", "add", title, "--state", "ready"]
    pure (head (words out))

-- | First token of the first non-empty line (a 10-char id prefix from a list).
firstListId :: String -> String
firstListId out = case filter (not . null) (lines out) of
    (l : _) -> head (words l)
    [] -> ""

-- | The parked dispatch id prefix, or "" if none parked.
parkedId :: FilePath -> FilePath -> IO String
parkedId dir db = firstListId . snd3 <$> runDispatch dir db Nothing ["dispatch", "list", "--parked"]
  where
    snd3 (_, b, _) = b

-- Scenario 1: dispatch from a checkout on a different branch with a dirty
-- tree; the invoking checkout's branch, dirtiness, and base ref are untouched,
-- and no worktree is left behind.
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
    mainAfter <- gitOut dir ["rev-parse", "main"]
    mainAfter @?= baseSha

    brs <- dispatchBranches dir
    assertBool "a dispatch branch was created" (not (null brs))
    parent <- gitOut dir ["rev-parse", head brs <> "~1"]
    assertBool "dispatch branch was cut from the base sha" (parent == baseSha)
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
    park <- parkedId dir db
    assertBool "dispatch parked" (not (null park))

-- Scenario 3: a committed success parks — outcome success, merge_sha NULL,
-- [parked] badge, branch present, base unmoved, task done, worktree gone.
testDispatchParks :: IO ()
testDispatchParks = withDispatchRepo $ \dir db -> do
    tid <- addReadyTask dir db "park task"
    baseSha <- gitOut dir ["rev-parse", "main"]
    (code, _, _) <- runDispatch dir db (Just "commit") ["dispatch", "run", tid]
    code @?= ExitSuccess

    (_, parkedOut, _) <- runDispatch dir db Nothing ["dispatch", "list", "--parked"]
    assertBool "[parked] badge present" ("[parked]" `isInfixOf` parkedOut)
    let did = firstListId parkedOut
    (_, showOut, _) <- runDispatch dir db Nothing ["dispatch", "show", did]
    assertBool "outcome success" ("success" `isInfixOf` showOut)
    assertBool "merged: no (parked ...)" ("no (parked; land with `icarium dispatch merge`)" `isInfixOf` showOut)
    assertBool "notes say parked" ("parked" `isInfixOf` showOut)

    (_, taskOut, _) <- runDispatch dir db Nothing ["task", "show", tid]
    assertBool "task moved to done" ("done" `isInfixOf` taskOut)
    brs <- dispatchBranches dir
    assertBool "dispatch branch retained" (not (null brs))
    mainAfter <- gitOut dir ["rev-parse", "main"]
    mainAfter @?= baseSha
    wc <- worktreeCount dir
    wc @?= 1

-- Scenario 4a: base checked out here and clean -> FF in place, HEAD advances.
testMergeFFInPlace :: IO ()
testMergeFFInPlace = withDispatchRepo $ \dir db -> do
    tid <- addReadyTask dir db "ff task"
    _ <- runDispatch dir db (Just "commit") ["dispatch", "run", tid]
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
    _ <- gitOut dir ["checkout", "-b", "feature"]
    _ <- runDispatch dir db (Just "commit") ["dispatch", "run", tid]
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
    _ <- runDispatch dir db (Just "commit") ["dispatch", "run", tid]
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
    _ <- runDispatch dir db (Just "commit") ["dispatch", "run", tid]
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
    _ <- runDispatch dir db (Just "commit") ["dispatch", "run", tid]
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
    (_, addOut, _) <- runDispatch dir db Nothing ["task", "add", "no-commit task", "--state", "ready", "--no-commit"]
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
    (_, addOut, _) <- runDispatch dir db Nothing ["task", "add", "proof task", "--state", "ready", "--body", "criteria: prove it"]
    let tid = head (words addOut)
    (code, _, _) <- runDispatch dir db (Just "proof") ["dispatch", "run", tid]
    code @?= ExitSuccess
    reviewerPrompt <- readFile (dir </> "stub-reviewer-prompt.txt")
    assertBool "reviewer prompt contains the mid-run body edit" ("xproof1" `isInfixOf` reviewerPrompt)
    -- The reviewer-time refresh also wrote through to column + FTS. The
    -- sweep cannot explain this: the Done update bumped updated_at past
    -- the file's mtime.
    (_, sOut, _) <- runDispatch dir db Nothing ["search", "xproof1"]
    assertBool "mid-run edit searchable after dispatch" ("proof task" `isInfixOf` sOut)
