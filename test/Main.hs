{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import           Control.Exception      (bracket)
import           Control.Monad          (forM, forM_, void)
import           Data.FileEmbed         (embedFile, makeRelativeToProject)
import           Data.Int               (Int64)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import qualified Data.Text.IO           as TIO
import           Database.SQLite.Simple (Connection, Only (..), Query (..), close,
                                         execute, execute_, open, query_)
import           System.IO.Temp         (withSystemTempFile)
import           System.IO              (hClose)
import           Test.Tasty             (defaultMain, testGroup, TestTree)
import           Test.Tasty.HUnit       (testCase, (@?=), assertBool)

import           Icarium.Config         (loadConfig, defaultConfigText)
import           Icarium.Db             (dbSchemaVersion, migrateDb)
import           Icarium.Dispatch       (postClaudeGuard)
import           Icarium.Id             (newId)
import           Icarium.Render         (renderTaskPrompt)
import qualified Icarium.Repo.Category  as RC
import qualified Icarium.Repo.Knowledge as RK
import           Icarium.Schema         (applySchema, execSql)
import           Icarium.Types

main :: IO ()
main = defaultMain $ testGroup "icarium"
    [ testGroup "TaskState round-trips"    taskStateTests
    , testGroup "EdgeKind round-trips"     edgeKindTests
    , testGroup "Effort round-trips"       effortTests
    , testGroup "CategoryAxis round-trips" categoryAxisTests
    , testCase "loadConfig succeeds on default template"        loadConfigTest
    , testCase "renderTaskPrompt is non-empty for minimal task" renderTest
    , testGroup "postClaudeGuard"
        [ testCase "dirty tree fires dirty-tree note"    testGuardDirtyTree
        , testCase "empty diff fires empty-diff note"    testGuardEmptyDiff
        , testCase "dirty tree takes priority over empty diff" testGuardDirtyFirst
        , testCase "clean tree with new commit passes"   testGuardPasses
        , testCase "revParse error does not fire empty-diff" testGuardRevParseError
        ]
    , testGroup "categoryMatchedKnowledge"
        [ testCase "both-axis match appears under Related knowledge" testBothAxisMatch
        , testCase "stale knowledge excluded from auto-pull"         testStaleExcluded
        , testCase "zero categories yields empty result"             testNoCats
        , testCase "explicit ref deduped from auto-pull"             testDedup
        , testCase "cap at 5, ordered most-recent-first"             testCap
        , testCase "one-axis match excluded when task has both axes" testOneAxisMismatch
        , testCase "stale explicit ref still renders under refs"     testStaleRef
        ]
    , testGroup "migrations"
        [ testCase "v1 DB migrates to v2: user_version stamped"         testMigrateV1ToV2Version
        , testCase "v1 DB migrates to v2: in_progress accepted"         testMigrateV1ToV2Check
        , testCase "v1 DB migrates to v2: existing rows preserved"      testMigrateV1ToV2Data
        , testCase "migrateDb is idempotent on v2 DB"                   testMigrateIdempotent
        ]
    ]

-- =============================================================
-- postClaudeGuard tests
-- =============================================================

baseSha :: Text
baseSha = "aaaa0000"

newSha :: Text
newSha = "bbbb1111"

testGuardDirtyTree :: IO ()
testGuardDirtyTree =
    postClaudeGuard False (Right newSha) baseSha
        @?= Just "agent left uncommitted changes; refusing to merge"

testGuardEmptyDiff :: IO ()
testGuardEmptyDiff =
    postClaudeGuard True (Right baseSha) baseSha
        @?= Just "agent made no commits on dispatch branch"

testGuardDirtyFirst :: IO ()
testGuardDirtyFirst =
    postClaudeGuard False (Right baseSha) baseSha
        @?= Just "agent left uncommitted changes; refusing to merge"

testGuardPasses :: IO ()
testGuardPasses =
    postClaudeGuard True (Right newSha) baseSha @?= Nothing

testGuardRevParseError :: IO ()
testGuardRevParseError =
    postClaudeGuard True (Left ("git error" :: String)) baseSha @?= Nothing

-- =============================================================
-- Round-trip tests
-- =============================================================

roundTrip :: (Show a, Eq a) => (a -> Text) -> (Text -> Maybe a) -> Text -> a -> TestTree
roundTrip toTxt fromTxt label val = testCase (T.unpack label) $ do
    toTxt val @?= label
    fromTxt label @?= Just val

taskStateTests :: [TestTree]
taskStateTests =
    [ roundTrip taskStateText parseTaskState "idea"        Idea
    , roundTrip taskStateText parseTaskState "planned"     Planned
    , roundTrip taskStateText parseTaskState "ready"       Ready
    , roundTrip taskStateText parseTaskState "in_progress" InProgress
    , roundTrip taskStateText parseTaskState "done"        Done
    , roundTrip taskStateText parseTaskState "blocked"     Blocked
    , roundTrip taskStateText parseTaskState "abandoned"   Abandoned
    ]

edgeKindTests :: [TestTree]
edgeKindTests =
    [ roundTrip edgeKindText parseEdgeKind "depends_on"   DependsOn
    , roundTrip edgeKindText parseEdgeKind "references"   References
    , roundTrip edgeKindText parseEdgeKind "derived_from" DerivedFrom
    , roundTrip edgeKindText parseEdgeKind "supersedes"   Supersedes
    ]

effortTests :: [TestTree]
effortTests =
    [ roundTrip effortText parseEffort "low"    Low
    , roundTrip effortText parseEffort "medium" Medium
    , roundTrip effortText parseEffort "high"   High
    ]

categoryAxisTests :: [TestTree]
categoryAxisTests =
    [ roundTrip categoryAxisText parseCategoryAxis "domain"     Domain
    , roundTrip categoryAxisText parseCategoryAxis "discipline" Discipline
    ]

-- =============================================================
-- Config / render smoke tests
-- =============================================================

loadConfigTest :: IO ()
loadConfigTest =
    withSystemTempFile "icarium.toml" $ \fp h -> do
        hClose h
        TIO.writeFile fp defaultConfigText
        result <- loadConfig fp
        case result of
            Left err -> fail ("loadConfig failed: " <> err)
            Right _  -> pure ()

renderTest :: IO ()
renderTest = do
    let out = renderTaskPrompt minTask [] [] []
    assertBool "renderTaskPrompt returned empty" (not (T.null out))

-- =============================================================
-- In-memory DB helpers
-- =============================================================

withTestDb :: (Connection -> IO a) -> IO a
withTestDb act = bracket (open ":memory:") close $ \conn -> do
    applySchema conn
    act conn

mkCat :: Connection -> CategoryAxis -> Text -> IO Category
mkCat c axis name = do
    cid <- RC.insertCategory c axis name
    pure (Category cid axis name)

mkKnowledge :: Connection -> Text -> Text -> IO Text
mkKnowledge c title body =
    RK.insertKnowledge c RK.NewKnowledge { RK.nkTitle = title, RK.nkBody = body }

attachKnowledgeCats :: Connection -> Text -> [Category] -> IO ()
attachKnowledgeCats c kid cats =
    forM_ cats $ \cat -> RC.attachKnowledgeCategory c kid (categoryId cat)

minTask :: Task
minTask = Task
    { taskId          = "01TEST00000000000000000000"
    , taskTitle       = "Test task"
    , taskBody        = "Body text"
    , taskState       = Ready
    , taskPriority    = Nothing
    , taskBlockReason = Nothing
    , taskCreatedAt   = "2026-01-01T00:00:00Z"
    , taskUpdatedAt   = "2026-01-01T00:00:00Z"
    }

-- =============================================================
-- categoryMatchedKnowledge tests
-- =============================================================

-- Task with both axes + matching non-stale knowledge → under Related knowledge.
testBothAxisMatch :: IO ()
testBothAxisMatch = withTestDb $ \c -> do
    domCat  <- mkCat c Domain     "cli"
    discCat <- mkCat c Discipline "haskell"
    kid <- mkKnowledge c "Match K" "match body"
    attachKnowledgeCats c kid [domCat, discCat]
    result <- RK.categoryMatchedKnowledge c [domCat, discCat] 5
    map knowledgeId result @?= [kid]
    let prompt = renderTaskPrompt minTask [] result []
    assertBool "Related knowledge header present"  ("## Related knowledge"     `T.isInfixOf` prompt)
    assertBool "Hedge sentence present"            ("use judgment"             `T.isInfixOf` prompt)
    assertBool "Knowledge title in prompt"         ("Match K"                  `T.isInfixOf` prompt)

-- Stale knowledge that would otherwise match → excluded from auto-pull.
testStaleExcluded :: IO ()
testStaleExcluded = withTestDb $ \c -> do
    domCat <- mkCat c Domain "cli"
    kid <- mkKnowledge c "Stale K" "stale body"
    attachKnowledgeCats c kid [domCat]
    void $ RK.updateKnowledge c kid RK.emptyUpdate { RK.kuStale = Just True }
    result <- RK.categoryMatchedKnowledge c [domCat] 5
    null result @?= True

-- Task with zero categories → no Related knowledge section.
testNoCats :: IO ()
testNoCats = withTestDb $ \c -> do
    result <- RK.categoryMatchedKnowledge c [] 5
    null result @?= True
    let prompt = renderTaskPrompt minTask [] [] []
    assertBool "no Related knowledge header" (not ("## Related knowledge" `T.isInfixOf` prompt))

-- Knowledge that is both an explicit ref AND a category match → only once, under refs.
testDedup :: IO ()
testDedup = withTestDb $ \c -> do
    domCat <- mkCat c Domain "cli"
    kid    <- mkKnowledge c "Shared K" "shared body"
    attachKnowledgeCats c kid [domCat]
    catResult <- RK.categoryMatchedKnowledge c [domCat] 5
    -- catResult contains kid; simulate dedup as buildPrompt does
    case catResult of
        []  -> fail "expected knowledge to auto-pull"
        k:_ -> do
            let refs       = [k]
                refIds     = map knowledgeId refs
                dedupedCat = filter (\x -> knowledgeId x `notElem` refIds) catResult
            assertBool "dedupedCat is empty" (null dedupedCat)
            let prompt = renderTaskPrompt minTask refs dedupedCat []
            assertBool "refs section present"           ("## Referenced knowledge" `T.isInfixOf` prompt)
            assertBool "related section absent (dedup)" (not ("## Related knowledge" `T.isInfixOf` prompt))

-- More than cap matches → exactly 5, ordered most-recent-first.
testCap :: IO ()
testCap = withTestDb $ \c -> do
    domCat <- mkCat c Domain "cli"
    -- Insert 6 knowledge entries with explicit, distinct timestamps (oldest first).
    kids <- forM [1..6 :: Int] $ \i -> do
        kid <- newId
        execute c
            (Query "INSERT INTO knowledge (id, title, body, created_at) VALUES (?,?,?,?)")
            ( kid
            , ("K" <> T.pack (show i) :: Text)
            , ("" :: Text)
            , ("2026-01-0" <> T.pack (show i) <> "T00:00:00" :: Text)
            )
        RC.attachKnowledgeCategory c kid (categoryId domCat)
        pure kid
    result <- RK.categoryMatchedKnowledge c [domCat] 5
    length result @?= 5
    -- Oldest entry (kids !! 0) should be excluded; remaining 5 newest, most-recent first.
    map knowledgeId result @?= reverse (tail kids)

-- One-axis match when task has both axes → excluded (must match both).
testOneAxisMismatch :: IO ()
testOneAxisMismatch = withTestDb $ \c -> do
    domCat  <- mkCat c Domain     "cli"
    discCat <- mkCat c Discipline "haskell"
    kid <- mkKnowledge c "Domain-only K" "body"
    attachKnowledgeCats c kid [domCat]
    -- knowledge only has domain, task has both → should not match
    result <- RK.categoryMatchedKnowledge c [domCat, discCat] 5
    null result @?= True

-- Stale knowledge attached as explicit ref → still renders under refs (regression guard).
testStaleRef :: IO ()
testStaleRef = withTestDb $ \c -> do
    kid <- mkKnowledge c "Stale ref" "stale ref body"
    void $ RK.updateKnowledge c kid RK.emptyUpdate { RK.kuStale = Just True }
    mk <- RK.getKnowledge c kid
    case mk of
        Nothing -> fail "knowledge not found after insert"
        Just k  -> do
            let prompt = renderTaskPrompt minTask [k] [] []
            assertBool "stale ref body in prompt"       ("stale ref body"          `T.isInfixOf` prompt)
            assertBool "Referenced knowledge header"    ("## Referenced knowledge" `T.isInfixOf` prompt)

-- =============================================================
-- Migration tests
-- =============================================================

-- v1 schema fixture, embedded at compile time.
v1SchemaSql :: Text
v1SchemaSql =
    TE.decodeUtf8 $(makeRelativeToProject "test/fixtures/v1_schema.sql" >>= embedFile)

-- Open an in-memory DB with the v1 schema applied and user_version = 1.
withTestDbV1 :: (Connection -> IO a) -> IO a
withTestDbV1 act = bracket (open ":memory:") close $ \conn -> do
    execSql conn v1SchemaSql
    execute_ conn "PRAGMA user_version = 1"
    act conn

testMigrateV1ToV2Version :: IO ()
testMigrateV1ToV2Version = withTestDbV1 $ \conn -> do
    migrateDb conn
    v <- dbSchemaVersion conn
    v @?= 2

testMigrateV1ToV2Check :: IO ()
testMigrateV1ToV2Check = withTestDbV1 $ \conn -> do
    migrateDb conn
    -- 'in_progress' must be accepted by the new CHECK constraint.
    execute conn
        (Query "INSERT INTO tasks (id, title, body, state) VALUES (?,?,?,?)")
        ( "01MTEST0000000000000000001" :: Text
        , "In-progress task" :: Text
        , "" :: Text
        , "in_progress" :: Text
        )
    rows <- query_ conn "SELECT state FROM tasks WHERE state = 'in_progress'"
                :: IO [Only Text]
    length rows @?= 1

testMigrateV1ToV2Data :: IO ()
testMigrateV1ToV2Data = withTestDbV1 $ \conn -> do
    -- Insert a task before migrating; verify it survives.
    execute conn
        (Query "INSERT INTO tasks (id, title, body, state) VALUES (?,?,?,?)")
        ( "01MTEST0000000000000000002" :: Text
        , "Preserved" :: Text
        , "body" :: Text
        , "ready" :: Text
        )
    migrateDb conn
    rows <- query_ conn "SELECT title FROM tasks" :: IO [Only Text]
    map (\(Only t) -> t) rows @?= ["Preserved"]

testMigrateIdempotent :: IO ()
testMigrateIdempotent = withTestDb $ \conn -> do
    -- withTestDb applies current schema (v2); migrateDb should be a no-op.
    v0 <- dbSchemaVersion conn
    migrateDb conn
    v1 <- dbSchemaVersion conn
    (v0, v1) @?= (2 :: Int64, 2 :: Int64)
