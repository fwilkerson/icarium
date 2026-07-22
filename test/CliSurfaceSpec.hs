{- | The CLI surface itself, independent of any one noun: @--version@, bare
help, the @ls@ alias, store resolution (@--db@ / @ICARIUM_DB@), @init@,
@doctor@, and the category registry.
-}
module CliSurfaceSpec (tests) where

import Control.Exception (bracket)
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.List (isInfixOf, isPrefixOf)
import Database.SQLite.Simple (Only (..), close, open, query_)
import Icarium.Schema (execSql, schemaSql, schemaVersion)
import System.Directory (doesFileExist)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process.Typed (proc, readProcess, setEnv)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import CliHelpers (
    absBin,
    minimalIcariumToml,
    runIcarium,
    runIcariumBare,
    runIcariumEnvDb,
    runIcariumIn,
    withTempDb,
 )

tests :: TestTree
tests =
    testGroup
        "CLI surface"
        [ testCase "--version prints icarium <semver> and exits 0" testVersion
        , testCase "-V short form works" testVersionShort
        , testCase "bare icarium prints help and exits 0" testBareIcariumHelp
        , testCase "bare task prints help and exits non-zero" testBareTaskHelp
        , testCase "bare ctx prints help and exits non-zero" testBareCtxHelp
        , testCase "bare dispatch prints help and exits non-zero" testBareDispatchHelp
        , testCase "bare link prints help and exits non-zero" testBareLinkHelp
        , testCase "bare category prints help and exits non-zero" testBareCategoryHelp
        , testCase "task ls and task list produce identical output" testTaskLsAlias
        , testCase "task ls --state ready works" testTaskLsStateFlag
        , testCase "task --state ready errors with usage message" testBareTaskWithFlag
        , testCase "task --help shows list but not ls" testTaskHelpNoLs
        , testCase "ctx list on externally-created DB (user_version=0) exits 0" testCtxListOnExternalDb
        , testCase "init: creates a DB at the current schema with no pending migrations" testInitCreatesCurrentSchema
        , testCase "doctor: no [commands] section does not FAIL config" testDoctorNoCommandsSection
        , testCase "doctor: missing body file FAILs, ok after Write, abandoned exempt" testDoctorBodyCheck
        , testCase "ICARIUM_DB env resolves db path when --db is not given" testDbEnvFallback
        , testCase "explicit --db wins over ICARIUM_DB" testDbFlagOverridesEnv
        , testCase "category add: registers in toml+DB, idempotent, unlocks --domain" testCategoryAdd
        , testCase "category add: invalid name exits 2" testCategoryAddBadName
        , testCase "kind axis: register, tag, filter, replace; task-only" testKindAxis
        , testCase "kind does not narrow ctx auto-pull" testKindDoesNotNarrowAutoPull
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
    (_, _, _) <- runIcarium db ["task", "add", "Ls alias task", "--state", "ready-headless"]
    (_, listOut, _) <- runIcarium db ["task", "list"]
    (lsCode, lsOut, _) <- runIcarium db ["task", "ls"]
    lsCode @?= ExitSuccess
    lsOut @?= listOut

testTaskLsStateFlag :: IO ()
testTaskLsStateFlag = withTempDb $ \db -> do
    (_, _, _) <- runIcarium db ["task", "add", "Ready task", "--state", "ready-headless"]
    (_, _, _) <- runIcarium db ["task", "add", "Planned task", "--state", "planned"]
    (code, out, _) <- runIcarium db ["task", "ls", "--state", "ready-headless"]
    code @?= ExitSuccess
    assertBool "ls --state ready shows ready task" ("Ready task" `isInfixOf` out)
    assertBool "ls --state ready hides planned task" (not ("Planned task" `isInfixOf` out))

testBareTaskWithFlag :: IO ()
testBareTaskWithFlag = withTempDb $ \db -> do
    (code, _, err) <- runIcarium db ["task", "--state", "ready-headless"]
    assertBool "bare task --state exits non-zero" (code /= ExitSuccess)
    assertBool "error output non-empty" (not (null err))

testTaskHelpNoLs :: IO ()
testTaskHelpNoLs = withTempDb $ \db -> do
    (code, out, _) <- runIcarium db ["task", "--help"]
    code @?= ExitSuccess
    assertBool "help shows list" ("list" `isInfixOf` out)
    assertBool "help hides ls" (not ("  ls" `isInfixOf` out))

-- Simulate an externally-created DB: spec/schema.sql applied directly without
-- setting user_version, so SQLite leaves it at 0. migrateDb must stamp the
-- version instead of re-running CREATE TABLE (which would fail).
testCtxListOnExternalDb :: IO ()
testCtxListOnExternalDb = withSystemTempDirectory "icarium-extdb" $ \dir -> do
    let dbPath = dir </> "external.db"
    bracket (open dbPath) close $ \conn -> execSql conn schemaSql
    (code, _, _) <- runIcarium dbPath ["ctx", "list"]
    code @?= ExitSuccess

-- `icarium init` must ship the current schema: after init, user_version equals
-- the library's schemaVersion, so a fresh DB has no pending migrations. Runs in
-- the temp dir so init's icarium.toml write and config load resolve there.
testInitCreatesCurrentSchema :: IO ()
testInitCreatesCurrentSchema = withSystemTempDirectory "icarium-init" $ \dir -> do
    let db = dir </> "icarium.db"
    (initCode, _, _) <- runIcariumIn dir db ["init"]
    initCode @?= ExitSuccess

    (listCode, _, _) <- runIcariumIn dir db ["ctx", "list"]
    listCode @?= ExitSuccess

    conn <- open db
    [Only v] <- query_ conn "PRAGMA user_version" :: IO [Only Int]
    close conn
    v @?= schemaVersion

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

testDoctorBodyCheck :: IO ()
testDoctorBodyCheck = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["task", "add", "bodyless intermediate"]
    let outLines = lines addOut
        tid = head outLines
        bodyPath = outLines !! 1
    (_, out1, _) <- runIcarium db ["doctor"]
    assertBool "FAIL body line present" ("FAIL  body" `isInfixOf` out1)
    assertBool "failure names the task" (take 10 tid `isInfixOf` out1)
    writeFile bodyPath "now populated"
    (_, out2, _) <- runIcarium db ["doctor"]
    assertBool "bodies check ok after Write" ("ok    bodies" `isInfixOf` out2)
    -- an abandoned bodyless task is exempt (explicit dead end, not a defect)
    (_, addOut2, _) <- runIcarium db ["task", "add", "junk to abandon"]
    let tid2 = head (words addOut2)
    (_, _, _) <- runIcarium db ["task", "update", tid2, "--state", "abandoned"]
    (_, out3, _) <- runIcarium db ["doctor"]
    assertBool "abandoned bodyless task exempt" ("ok    bodies" `isInfixOf` out3)

testDbEnvFallback :: IO ()
testDbEnvFallback = withTempDb $ \db -> do
    (addCode, addOut, _) <- runIcariumEnvDb db ["task", "add", "Env db task", "--state", "ready-headless"]
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
        readProcess (setEnv env (proc absBin ["--db", flagDb, "task", "add", "Flag wins task", "--state", "ready-headless"]))
    code @?= ExitSuccess
    let tid = head (words (BLC.unpack addOut))
    flagDbExists <- doesFileExist flagDb
    envDbExists <- doesFileExist envDb
    assertBool "explicit --db path was used" flagDbExists
    assertBool "ICARIUM_DB path was not touched" (not envDbExists)

    (listCode, listOut, _) <- runIcarium flagDb ["task", "list"]
    listCode @?= ExitSuccess
    assertBool "task shows up under the --db path" (take 10 tid `isInfixOf` listOut)

testCategoryAdd :: IO ()
testCategoryAdd = withSystemTempDirectory "icarium-test" $ \dir -> do
    let db = dir <> "/icarium.db"
    writeFile (dir <> "/icarium.toml") minimalIcariumToml
    -- unregistered domain errors and points at category add
    (badCode, _, badErr) <- runIcariumIn dir db ["task", "add", "tagged task", "--domain", "infra"]
    badCode @?= ExitFailure 2
    assertBool "error suggests category add" ("category add" `isInfixOf` badErr)
    (aCode, aOut, _) <- runIcariumIn dir db ["category", "add", "--axis", "domain", "infra"]
    aCode @?= ExitSuccess
    assertBool "reports registered" ("registered domain:infra" `isInfixOf` aOut)
    (aCode2, aOut2, _) <- runIcariumIn dir db ["category", "add", "--axis", "domain", "infra"]
    aCode2 @?= ExitSuccess
    assertBool "second run is an idempotent no-op" ("already registered" `isInfixOf` aOut2)
    toml <- readFile (dir <> "/icarium.toml")
    assertBool "icarium.toml carries the new name" ("\"infra\"" `isInfixOf` toml)
    -- toml and DB agree: sync sees no orphans
    (sCode, _, sErr) <- runIcariumIn dir db ["category", "sync"]
    sCode @?= ExitSuccess
    assertBool "no orphan after add" (not ("orphan" `isInfixOf` sErr))
    (tCode, _, _) <- runIcariumIn dir db ["task", "add", "tagged task", "--domain", "infra"]
    tCode @?= ExitSuccess

{- | The kind axis end to end: register it into a config that predates the
axis (no `kinds` line), tag a task, filter on it, replace it, and confirm
ctx refuses it.
-}
testKindAxis :: IO ()
testKindAxis = withSystemTempDirectory "icarium-test" $ \dir -> do
    let db = dir <> "/icarium.db"
        icarium = runIcariumIn dir db
    writeFile (dir <> "/icarium.toml") minimalIcariumToml
    (bCode, _, bErr) <- icarium ["task", "add", "untagged", "--kind", "bug"]
    bCode @?= ExitFailure 2
    assertBool "unknown kind points at category add" ("--axis kind bug" `isInfixOf` bErr)
    -- `kinds` is absent from this config; add must create the array.
    (rCode, rOut, _) <- icarium ["category", "add", "--axis", "kind", "bug"]
    rCode @?= ExitSuccess
    assertBool "reports registered" ("registered kind:bug" `isInfixOf` rOut)
    toml <- readFile (dir <> "/icarium.toml")
    assertBool "kinds array created in toml" ("kinds = [\"bug\"]" `isInfixOf` toml)
    (sCode, _, sErr) <- icarium ["category", "sync"]
    sCode @?= ExitSuccess
    assertBool "toml and DB agree" (not ("orphan" `isInfixOf` sErr))
    _ <- icarium ["category", "add", "--axis", "kind", "chore"]

    (aCode, aOut, _) <- icarium ["task", "add", "a bug", "--kind", "bug"]
    aCode @?= ExitSuccess
    let tid = head (words aOut)
    _ <- icarium ["task", "add", "a chore", "--kind", "chore"]
    (_, showOut, _) <- icarium ["task", "show", tid]
    assertBool "task show carries the kind" ("kind:" `isInfixOf` showOut)

    (lCode, lOut, _) <- icarium ["task", "list", "--kind", "bug"]
    lCode @?= ExitSuccess
    assertBool "filter keeps the bug" ("a bug" `isInfixOf` lOut)
    assertBool "filter drops the chore" (not ("a chore" `isInfixOf` lOut))

    -- Replace, then clear.
    _ <- icarium ["task", "update", tid, "--kind", "chore"]
    (_, lOut2, _) <- icarium ["task", "list", "--kind", "bug"]
    assertBool "old kind detached on replace" (not ("a bug" `isInfixOf` lOut2))
    _ <- icarium ["task", "update", tid, "--kind", ""]
    (_, showOut2, _) <- icarium ["task", "show", tid]
    assertBool "empty string clears the axis" (not ("kind:" `isInfixOf` showOut2))

    -- Workflow axis is task-only.
    (cCode, _, cErr) <- icarium ["ctx", "add", "note", "--body", "b", "--kind", "bug"]
    cCode @?= ExitFailure 1
    assertBool "ctx rejects --kind" ("--kind" `isInfixOf` cErr)

{- | Auto-pull is defined on the retrieval axes only. A task tagged with a
kind must pull the same context it would have pulled without one — the
regression that would otherwise go unnoticed until prompts quietly lost
their related-context block.
-}
testKindDoesNotNarrowAutoPull :: IO ()
testKindDoesNotNarrowAutoPull = withSystemTempDirectory "icarium-test" $ \dir -> do
    let db = dir <> "/icarium.db"
        icarium = runIcariumIn dir db
    writeFile (dir <> "/icarium.toml") minimalIcariumToml
    _ <- icarium ["category", "sync"]
    _ <- icarium ["category", "add", "--axis", "kind", "bug"]
    -- Context carries retrieval axes only, as every entry written before
    -- the kind axis existed does.
    _ <-
        icarium
            ["ctx", "add", "pullme", "--body", "xpullbody", "--domain", "core", "--discipline", "development"]
    (aCode, aOut, _) <-
        icarium
            [ "task"
            , "add"
            , "kinded task"
            , "--body"
            , "b"
            , "--domain"
            , "core"
            , "--discipline"
            , "development"
            , "--kind"
            , "bug"
            ]
    aCode @?= ExitSuccess
    let tid = head (words aOut)
    (pCode, pOut, _) <- icarium ["task", "show", tid, "--prompt"]
    pCode @?= ExitSuccess
    assertBool "related context still auto-pulled" ("xpullbody" `isInfixOf` pOut)

testCategoryAddBadName :: IO ()
testCategoryAddBadName = withTempDb $ \db -> do
    (code, _, err) <- runIcarium db ["category", "add", "--axis", "domain", "bad name!"]
    code @?= ExitFailure 2
    assertBool "error names the invalid name" ("invalid category name" `isInfixOf` err)
