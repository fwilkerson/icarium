{- | CLI contract for the @icarium dispatch@ read surface (list, show, stats)
and the parts of @dispatch run@ that need no worker: an empty queue, config
loading, and the dry-run prompt. The worker-driving scenarios live in
"CliWorktreeSpec".
-}
module CliDispatchSpec (tests) where

import Data.List (isInfixOf)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))
import Database.SQLite.Simple (Query (..), close, execute, open)
import System.Directory (setModificationTime)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import CliHelpers (minimalIcariumToml, runIcarium, runIcariumIn, withTempDb)

tests :: TestTree
tests =
    testGroup
        "dispatch"
        [ testCase "dispatch list on empty DB exits 0" testDispatchListEmpty
        , testCase "dispatch list --limit caps rows" testDispatchListLimit
        , testCase "dispatch show: tokens line present when values set" testDispatchShowTokensPresent
        , testCase "dispatch show: tokens line absent when all NULL" testDispatchShowTokensAbsent
        , testCase "dispatch stats: byte-for-byte summary, --since filters" testDispatchStats
        , testCase "dispatch run drains empty queue without --max" testDispatchRunEmptyQueue
        , testCase "dispatch run ignores stale max_dispatches_per_run" testDispatchRunStaleConfig
        , testCase "dispatch dry-run: prompt reads body file even when sweep is blind" testDryRunPromptReadsBodyFile
        , testCase "dispatch dry-run: task model/effort beat config, flags beat task" testDryRunTaskOverrides
        ]

testDispatchListEmpty :: IO ()
testDispatchListEmpty = withTempDb $ \db -> do
    (code, _, err) <- runIcarium db ["dispatch", "list"]
    code @?= ExitSuccess
    assertBool "no error output on empty dispatch list" (null err)

testDispatchListLimit :: IO ()
testDispatchListLimit = withSystemTempDirectory "icarium-test" $ \dir -> do
    let db = dir <> "/icarium.db"
    -- init DB by adding a task first
    (_, addOut, _) <- runIcarium db ["task", "add", "Limit test task", "--state", "ready-headless"]
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

testDispatchShowTokensPresent :: IO ()
testDispatchShowTokensPresent = withSystemTempDirectory "icarium-test" $ \dir -> do
    let db = dir <> "/icarium.db"
    (_, addOut, _) <- runIcarium db ["task", "add", "Token task", "--state", "ready-headless"]
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
    (_, addOut, _) <- runIcarium db ["task", "add", "No token task", "--state", "ready-headless"]
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

testDispatchStats :: IO ()
testDispatchStats = withSystemTempDirectory "icarium-test" $ \dir -> do
    let db = dir <> "/icarium.db"
    (_, addOut, _) <- runIcarium db ["task", "add", "Stats task", "--state", "ready-headless"]
    let tid = head (words addOut)
    conn <- open db
    let insertRow did startedAt mOutcome mIn mOut mCache = do
            execute
                conn
                ( Query
                    "INSERT INTO dispatches \
                    \(id, task_id, branch, base_branch, base_sha, model, effort, started_at) \
                    \VALUES (?,?,?,?,?,?,?,?)"
                )
                ( did :: String
                , tid
                , "dispatch/" ++ did
                , "main" :: String
                , "abc123" :: String
                , "claude-sonnet-4-6" :: String
                , "medium" :: String
                , startedAt :: String
                )
            execute
                conn
                ( Query
                    "UPDATE dispatches \
                    \SET outcome = ?, tokens_in = ?, tokens_out = ?, tokens_cache_read = ? \
                    \WHERE id = ?"
                )
                ( mOutcome :: Maybe String
                , mIn :: Maybe Int
                , mOut :: Maybe Int
                , mCache :: Maybe Int
                , did :: String
                )
    insertRow "01STATS0000000000000000001" "2026-01-01 00:00:00" (Just "success") (Just 100) (Just 20) (Just 5)
    insertRow "01STATS0000000000000000002" "2026-01-02 00:00:00" (Just "failure") (Just 50) (Just 10) (Just 1)
    insertRow "01STATS0000000000000000003" "2026-01-03 00:00:00" (Just "interrupted") Nothing Nothing Nothing
    insertRow "01STATS0000000000000000004" "2026-01-04 00:00:00" Nothing Nothing Nothing Nothing
    close conn

    (code, out, _) <- runIcarium db ["dispatch", "stats"]
    code @?= ExitSuccess
    out
        @?= unlines
            [ "since:             (all)"
            , "dispatches:        4"
            , "success:           1"
            , "failure:           1"
            , "interrupted:       1"
            , "open:              1"
            , "tokens_in:         150"
            , "tokens_out:        30"
            , "tokens_cache_read: 6"
            , "missing_tokens:    2"
            ]

    (sinceCode, sinceOut, _) <- runIcarium db ["dispatch", "stats", "--since", "2026-01-02 00:00:00"]
    sinceCode @?= ExitSuccess
    assertBool
        "since filters to the 3 dispatches started on/after the cutoff"
        ("dispatches:        3" `isInfixOf` sinceOut)
    assertBool "since echoes back the given timestamp" ("since:             2026-01-02 00:00:00" `isInfixOf` sinceOut)

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

{- | Routing precedence, read off the dry run: @[dispatch]@ config is the
floor (claude-sonnet-4-6 / high in 'minimalIcariumToml'), the task's own
columns override it, and a @dispatch run@ flag overrides both.
-}
testDryRunTaskOverrides :: IO ()
testDryRunTaskOverrides = withSystemTempDirectory "icarium-test" $ \dir -> do
    let db = dir </> "icarium.db"
    writeFile (dir </> "icarium.toml") minimalIcariumToml
    (_, addOut, _) <-
        runIcarium
            db
            [ "task"
            , "add"
            , "routed task"
            , "--state"
            , "ready-headless"
            , "--body"
            , "b"
            , "--model"
            , "claude-haiku-4-5-20251001"
            , "--effort"
            , "low"
            ]
    let tid = head (words addOut)

    (code, out, _) <- runIcariumIn dir db ["dispatch", "run", tid, "--dry-run"]
    code @?= ExitSuccess
    assertBool "task model wins over config" ("model:                   claude-haiku-4-5-20251001" `isInfixOf` out)
    assertBool "task effort wins over config" ("effort:                  low" `isInfixOf` out)

    (fCode, fOut, _) <-
        runIcariumIn dir db ["dispatch", "run", tid, "--dry-run", "--model", "claude-opus-4-8", "--effort", "max"]
    fCode @?= ExitSuccess
    assertBool "flag model wins over task" ("model:                   claude-opus-4-8" `isInfixOf` fOut)
    assertBool "flag effort wins over task" ("effort:                  max" `isInfixOf` fOut)

    -- An untouched task still inherits the config defaults.
    (_, addOut2, _) <- runIcarium db ["task", "add", "plain task", "--state", "ready-headless", "--body", "b"]
    let tid2 = head (words addOut2)
    (_, out2, _) <- runIcariumIn dir db ["dispatch", "run", tid2, "--dry-run"]
    assertBool "config model when task has none" ("model:                   claude-sonnet-4-6" `isInfixOf` out2)
    assertBool "config effort when task has none" ("effort:                  high" `isInfixOf` out2)

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
    (_, addOut, _) <- runIcarium db ["task", "add", "stale-body task", "--state", "ready-headless", "--body", "original xstale1"]
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
