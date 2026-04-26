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
import qualified Icarium.Render
import           Icarium.Render         (renderKnowledgeList, renderTaskList, renderTaskPrompt)
import qualified Icarium.Repo.Category  as RC
import qualified Icarium.Repo.Dispatch  as RD
import qualified Icarium.Repo.Knowledge as RK
import qualified Icarium.Repo.Task      as RT
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
        [ testCase "v1 DB migrates to v4: user_version stamped"         testMigrateV1ToV2Version
        , testCase "v1 DB migrates to v4: in_progress accepted"         testMigrateV1ToV2Check
        , testCase "v1 DB migrates to v4: existing rows preserved"      testMigrateV1ToV2Data
        , testCase "v2 DB migrates to v4: user_version stamped"         testMigrateV2ToV3Version
        , testCase "v2 DB migrates to v4: existing rows preserved"      testMigrateV2ToV3Data
        , testCase "v3 DB migrates to v4: user_version stamped"         testMigrateV3ToV4Version
        , testCase "v3 DB migrates to v4: slug column dropped"          testMigrateV3ToV4SlugGone
        , testCase "v3 DB migrates to v4: rows with slug survive"       testMigrateV3ToV4Data
        , testCase "migrateDb is idempotent on v4 DB"                   testMigrateIdempotent
        ]
    , testGroup "resolveDispatchId"
        [ testCase "right on full id"      testResolveDispatchFullId
        , testCase "right on unique prefix" testResolveDispatchPrefix
        , testCase "left on missing"        testResolveDispatchMissing
        , testCase "left on ambiguous"      testResolveDispatchAmbiguous
        ]
    , testGroup "10-char ULID prefix rendering"
        [ testCase "task list id column is 10 chars"      testTaskListIdWidth
        , testCase "knowledge list id column is 10 chars" testKnowledgeListIdWidth
        ]
    , testGroup "renderTaskList grouped view"
        [ testCase "groups in READY/PLANNED/BLOCKED/IDEA order with counts" testGroupedHeaders
        , testCase "single-state filter suppresses group header"            testSingleStateNoHeader
        , testCase "ASCII fallback uses # and ."                            testAsciiBars
        , testCase "blocked row replaces bar with truncated reason"         testBlockedReason
        , testCase "edge counts omitted when both zero, shown otherwise"    testEdgeCountFormat
        , testCase "category formatting handles missing slots"              testCategoryFormatting
        , testCase "NULL priority sorts last within group"                  testNullPrioritySort
        , testCase "90-char title truncated to 72 chars with UTF-8 ellipsis" testTitleTruncatedUtf8
        , testCase "90-char title truncated with ASCII ... in ASCII mode"  testTitleTruncatedAscii
        , testCase "title at exactly 72 chars renders without truncation"  testTitleExactlyAtLimit
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

-- v2 schema fixture, embedded at compile time.
v2SchemaSql :: Text
v2SchemaSql =
    TE.decodeUtf8 $(makeRelativeToProject "test/fixtures/v2_schema.sql" >>= embedFile)

-- v3 schema fixture, embedded at compile time.
v3SchemaSql :: Text
v3SchemaSql =
    TE.decodeUtf8 $(makeRelativeToProject "test/fixtures/v3_schema.sql" >>= embedFile)

-- Open an in-memory DB with the v1 schema applied and user_version = 1.
withTestDbV1 :: (Connection -> IO a) -> IO a
withTestDbV1 act = bracket (open ":memory:") close $ \conn -> do
    execSql conn v1SchemaSql
    execute_ conn "PRAGMA user_version = 1"
    act conn

-- Open an in-memory DB with the v2 schema applied and user_version = 2.
withTestDbV2 :: (Connection -> IO a) -> IO a
withTestDbV2 act = bracket (open ":memory:") close $ \conn -> do
    execSql conn v2SchemaSql
    execute_ conn "PRAGMA user_version = 2"
    act conn

-- Open an in-memory DB with the v3 schema applied and user_version = 3.
withTestDbV3 :: (Connection -> IO a) -> IO a
withTestDbV3 act = bracket (open ":memory:") close $ \conn -> do
    execSql conn v3SchemaSql
    execute_ conn "PRAGMA user_version = 3"
    act conn

testMigrateV1ToV2Version :: IO ()
testMigrateV1ToV2Version = withTestDbV1 $ \conn -> do
    -- migrateDb applies all pending migrations: v1 → v2 → v3 → v4.
    migrateDb conn
    v <- dbSchemaVersion conn
    v @?= 4

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
    -- withTestDb applies current schema (v4); migrateDb should be a no-op.
    v0 <- dbSchemaVersion conn
    migrateDb conn
    v1 <- dbSchemaVersion conn
    (v0, v1) @?= (4 :: Int64, 4 :: Int64)

testMigrateV2ToV3Version :: IO ()
testMigrateV2ToV3Version = withTestDbV2 $ \conn -> do
    migrateDb conn
    v <- dbSchemaVersion conn
    v @?= 4

testMigrateV2ToV3Data :: IO ()
testMigrateV2ToV3Data = withTestDbV2 $ \conn -> do
    -- Insert rows before migrating; verify they survive.
    execute conn
        (Query "INSERT INTO tasks (id, title, body, state) VALUES (?,?,?,?)")
        ( "01MTEST0000000000000000011" :: Text
        , "Legacy task" :: Text
        , "body" :: Text
        , "ready" :: Text
        )
    migrateDb conn
    rows <- query_ conn "SELECT title FROM tasks" :: IO [Only Text]
    map (\(Only t) -> t) rows @?= ["Legacy task"]

testMigrateV3ToV4Version :: IO ()
testMigrateV3ToV4Version = withTestDbV3 $ \conn -> do
    migrateDb conn
    v <- dbSchemaVersion conn
    v @?= 4

testMigrateV3ToV4SlugGone :: IO ()
testMigrateV3ToV4SlugGone = withTestDbV3 $ \conn -> do
    migrateDb conn
    -- After migration, inserting with slug column should fail (column gone).
    rows <- query_ conn "SELECT name FROM pragma_table_info('tasks')" :: IO [Only Text]
    let cols = map (\(Only c) -> c) rows
    assertBool "slug column absent from tasks" ("slug" `notElem` cols)
    rows2 <- query_ conn "SELECT name FROM pragma_table_info('knowledge')" :: IO [Only Text]
    let cols2 = map (\(Only c) -> c) rows2
    assertBool "slug column absent from knowledge" ("slug" `notElem` cols2)

testMigrateV3ToV4Data :: IO ()
testMigrateV3ToV4Data = withTestDbV3 $ \conn -> do
    -- Insert rows with non-NULL slugs before migrating; verify non-slug data survives.
    execute conn
        (Query "INSERT INTO tasks (id, title, body, state, slug) VALUES (?,?,?,?,?)")
        ( "01MTEST0000000000000000020" :: Text
        , "Slug task" :: Text
        , "body" :: Text
        , "ready" :: Text
        , "slug-task" :: Text
        )
    execute conn
        (Query "INSERT INTO knowledge (id, title, body, slug) VALUES (?,?,?,?)")
        ( "01MTEST0000000000000000021" :: Text
        , "Slug knowledge" :: Text
        , "know body" :: Text
        , "slug-knowledge" :: Text
        )
    migrateDb conn
    taskRows <- query_ conn "SELECT title FROM tasks" :: IO [Only Text]
    map (\(Only t) -> t) taskRows @?= ["Slug task"]
    knowRows <- query_ conn "SELECT title FROM knowledge" :: IO [Only Text]
    map (\(Only t) -> t) knowRows @?= ["Slug knowledge"]

-- =============================================================
-- resolveDispatchId tests
-- =============================================================

insertTestDispatch :: Connection -> Text -> Text -> IO ()
insertTestDispatch c did tid =
    execute c
        (Query "INSERT INTO dispatches \
               \(id, task_id, branch, base_branch, base_sha, model, effort) \
               \VALUES (?,?,?,?,?,?,?)")
        ( did
        , tid
        , ("dispatch/" <> did :: Text)
        , ("main" :: Text)
        , ("0000000000000000000000000000000000000000" :: Text)
        , ("claude-sonnet-4-6" :: Text)
        , ("medium" :: Text)
        )

testResolveDispatchFullId :: IO ()
testResolveDispatchFullId = withTestDb $ \c -> do
    tid <- RT.insertTask c RT.NewTask
        { RT.ntTitle = "T", RT.ntBody = "", RT.ntState = Ready, RT.ntPriority = Nothing }
    let did = "01AAAA000000000000000000AA" :: Text
    insertTestDispatch c did tid
    r <- RD.resolveDispatchId c did
    r @?= Right did

testResolveDispatchPrefix :: IO ()
testResolveDispatchPrefix = withTestDb $ \c -> do
    tid <- RT.insertTask c RT.NewTask
        { RT.ntTitle = "T", RT.ntBody = "", RT.ntState = Ready, RT.ntPriority = Nothing }
    let did = "01AAAA000000000000000000AA" :: Text
    insertTestDispatch c did tid
    r <- RD.resolveDispatchId c "01AAAA0000"
    r @?= Right did

testResolveDispatchMissing :: IO ()
testResolveDispatchMissing = withTestDb $ \c -> do
    r <- RD.resolveDispatchId c "01ZZZZZZZZ"
    case r of
        Left msg -> assertBool "error mentions input" ("01ZZZZZZZZ" `T.isInfixOf` T.pack msg)
        Right _  -> fail "expected Left for missing dispatch"

testResolveDispatchAmbiguous :: IO ()
testResolveDispatchAmbiguous = withTestDb $ \c -> do
    tid <- RT.insertTask c RT.NewTask
        { RT.ntTitle = "T", RT.ntBody = "", RT.ntState = Ready, RT.ntPriority = Nothing }
    let did1 = "01BBBB000000000000000000AA" :: Text
        did2 = "01BBBB000000000000000000BB" :: Text
    insertTestDispatch c did1 tid
    insertTestDispatch c did2 tid
    r <- RD.resolveDispatchId c "01BBBB0000"
    case r of
        Left msg -> do
            assertBool "error mentions input"  ("01BBBB0000" `T.isInfixOf` T.pack msg)
            assertBool "error lists first id"  (T.unpack did1 `isInfixOf` msg)
            assertBool "error lists second id" (T.unpack did2 `isInfixOf` msg)
        Right _  -> fail "expected Left for ambiguous dispatch"
  where
    isInfixOf needle haystack = T.isInfixOf (T.pack needle) (T.pack haystack)

-- =============================================================
-- 10-char ULID prefix rendering tests
-- =============================================================

testTaskListIdWidth :: IO ()
testTaskListIdWidth = withTestDb $ \c -> do
    tid <- RT.insertTask c RT.NewTask
        { RT.ntTitle = "Width test", RT.ntBody = "", RT.ntState = Ready, RT.ntPriority = Nothing }
    mt <- RT.getTask c tid
    case mt of
        Nothing -> fail "task not found"
        Just t  -> do
            let row = Icarium.Render.TaskRow { Icarium.Render.trTask = t
                                             , Icarium.Render.trCats = []
                                             , Icarium.Render.trDeps = 0
                                             , Icarium.Render.trRefs = 0 }
                out = renderTaskList True [row] [Ready]
                ls  = filter (not . T.null) (T.lines out)
            assertBool "at least one data row" (not (null ls))
            let dataRow = head ls
                stripped = T.stripStart dataRow
            assertBool "id prefix is 10 chars" (T.take 10 stripped == T.take 10 tid)

testKnowledgeListIdWidth :: IO ()
testKnowledgeListIdWidth = withTestDb $ \c -> do
    kid <- mkKnowledge c "Width K" "body"
    mk  <- RK.getKnowledge c kid
    case mk of
        Nothing -> fail "knowledge not found"
        Just k  -> do
            let out = renderKnowledgeList True [k]
                ls  = T.lines out
            assertBool "at least one data row" (length ls >= 2)
            let dataRow = ls !! 1
                prefix  = T.take 12 dataRow
            assertBool "id prefix is 10 chars padded to 12" (T.length prefix == 12)
            assertBool "id matches knowledge prefix" (T.take 10 kid `T.isPrefixOf` T.stripEnd prefix)

-- =============================================================
-- renderTaskList grouped view tests
-- =============================================================

mkRow :: Text -> Text -> TaskState -> Maybe Int -> [Category] -> Int -> Int -> Maybe Text -> Icarium.Render.TaskRow
mkRow tid title st pri cats deps refs blockReason = Icarium.Render.TaskRow
    { Icarium.Render.trTask = Task
        { taskId          = tid
        , taskTitle       = title
        , taskBody        = ""
        , taskState       = st
        , taskPriority    = pri
        , taskBlockReason = blockReason
        , taskCreatedAt   = "2026-04-26 00:00:00"
        , taskUpdatedAt   = "2026-04-26 00:00:00"
        }
    , Icarium.Render.trCats = cats
    , Icarium.Render.trDeps = deps
    , Icarium.Render.trRefs = refs
    }

mkCatPure :: CategoryAxis -> Text -> Category
mkCatPure ax nm = Category { categoryId = "cat-" <> nm, categoryAxis = ax, categoryName = nm }

testGroupedHeaders :: IO ()
testGroupedHeaders = do
    let rows = [ mkRow "01ABCDEFGH01" "ready task"   Ready   (Just 5) [] 0 0 Nothing
               , mkRow "01ABCDEFGH02" "planned task" Planned (Just 3) [] 0 0 Nothing
               , mkRow "01ABCDEFGH03" "idea task"    Idea    Nothing  [] 0 0 Nothing
               ]
        out = renderTaskList True rows []
    assertBool "READY (1) header"   ("READY  (1)"   `T.isInfixOf` out)
    assertBool "PLANNED (1) header" ("PLANNED  (1)" `T.isInfixOf` out)
    assertBool "BLOCKED (0) header" ("BLOCKED  (0)" `T.isInfixOf` out)
    assertBool "IDEA (1) header"    ("IDEA  (1)"    `T.isInfixOf` out)
    -- Order: READY before PLANNED before BLOCKED before IDEA.
    let Just iReady   = lengthBefore "READY"   out
        Just iPlanned = lengthBefore "PLANNED" out
        Just iBlocked = lengthBefore "BLOCKED" out
        Just iIdea    = lengthBefore "IDEA"    out
    assertBool "READY before PLANNED" (iReady < iPlanned)
    assertBool "PLANNED before BLOCKED" (iPlanned < iBlocked)
    assertBool "BLOCKED before IDEA" (iBlocked < iIdea)
  where
    lengthBefore needle haystack =
        let (pre, suf) = T.breakOn needle haystack
        in if T.null suf then Nothing else Just (T.length pre)

testSingleStateNoHeader :: IO ()
testSingleStateNoHeader = do
    let rows = [mkRow "01ABCDEFGH01" "only ready" Ready (Just 5) [] 0 0 Nothing]
        out  = renderTaskList True rows [Ready]
    assertBool "no READY header" (not ("READY" `T.isInfixOf` out))
    assertBool "still has the row" ("only ready" `T.isInfixOf` out)

testAsciiBars :: IO ()
testAsciiBars = do
    let rows = [mkRow "01ABCDEFGH01" "task" Ready (Just 5) [] 0 0 Nothing]
        out  = renderTaskList False rows [Ready]
    assertBool "uses # for filled" ("#####....." `T.isInfixOf` out)
    assertBool "no unicode bullet" (not ("●" `T.isInfixOf` out))

testBlockedReason :: IO ()
testBlockedReason = do
    let longReason = T.replicate 80 "x"
        rows = [ mkRow "01ABCDEFGH01" "short" Blocked (Just 5) [] 0 0 (Just "nope")
               , mkRow "01ABCDEFGH02" "long"  Blocked (Just 5) [] 0 0 (Just longReason)
               ]
        out = renderTaskList True rows [Blocked]
    assertBool "shows short reason" ("nope" `T.isInfixOf` out)
    assertBool "no priority bar in blocked" (not ("●●●●●·····" `T.isInfixOf` out))
    assertBool "long reason truncated with ellipsis" ((T.replicate 57 "x" <> "...") `T.isInfixOf` out)

testEdgeCountFormat :: IO ()
testEdgeCountFormat = do
    let rows = [ mkRow "01ABCDEFGH01" "no edges"   Ready (Just 5) [] 0 0 Nothing
               , mkRow "01ABCDEFGH02" "deps only"  Ready (Just 5) [] 2 0 Nothing
               , mkRow "01ABCDEFGH03" "refs only"  Ready (Just 5) [] 0 3 Nothing
               , mkRow "01ABCDEFGH04" "both"       Ready (Just 5) [] 1 4 Nothing
               ]
        out  = renderTaskList True rows [Ready]
        line title = head $ filter (T.isInfixOf title) (T.lines out)
    assertBool "no edges → no bracket" (not ("[deps" `T.isInfixOf` line "no edges") && not ("[refs" `T.isInfixOf` line "no edges"))
    assertBool "deps-only" ("[deps:2]" `T.isInfixOf` line "deps only")
    assertBool "refs-only" ("[refs:3]" `T.isInfixOf` line "refs only")
    assertBool "both"      ("[deps:1 refs:4]" `T.isInfixOf` line "both")

testCategoryFormatting :: IO ()
testCategoryFormatting = do
    let dom  = mkCatPure Domain     "cli"
        disc = mkCatPure Discipline "haskell"
        rows = [ mkRow "01ABCDEFGH01" "both"     Ready (Just 5) [dom, disc] 0 0 Nothing
               , mkRow "01ABCDEFGH02" "dom-only" Ready (Just 5) [dom]       0 0 Nothing
               , mkRow "01ABCDEFGH03" "dis-only" Ready (Just 5) [disc]      0 0 Nothing
               , mkRow "01ABCDEFGH04" "none"     Ready (Just 5) []          0 0 Nothing
               ]
        out  = renderTaskList True rows [Ready]
        line title = head $ filter (T.isInfixOf title) (T.lines out)
    assertBool "[cli/haskell]" ("[cli/haskell]" `T.isInfixOf` line "both")
    assertBool "[cli/-]"       ("[cli/-]"       `T.isInfixOf` line "dom-only")
    assertBool "[-/haskell]"   ("[-/haskell]"   `T.isInfixOf` line "dis-only")
    assertBool "[-]"           ("[-]"           `T.isInfixOf` line "none")

testNullPrioritySort :: IO ()
testNullPrioritySort = do
    let rows = [ mkRow "01ABCDEFGH01" "null-pri" Idea Nothing  [] 0 0 Nothing
               , mkRow "01ABCDEFGH02" "low-pri"  Idea (Just 1) [] 0 0 Nothing
               , mkRow "01ABCDEFGH03" "high-pri" Idea (Just 9) [] 0 0 Nothing
               ]
        out  = renderTaskList True rows [Idea]
        ls   = filter (\l -> "pri" `T.isInfixOf` l) (T.lines out)
        titles = map (T.strip . snd . T.breakOn "high" . T.intercalate "|") [ls]  -- guard list non-empty
    assertBool "non-empty rendered rows" (not (null ls))
    -- Expected order: high-pri, low-pri, null-pri (NULL last).
    let idx t = head [i | (i, l) <- zip [0::Int ..] ls, t `T.isInfixOf` l]
    (idx "high-pri" < idx "low-pri") @?= True
    (idx "low-pri"  < idx "null-pri") @?= True
    -- silence unused warning
    seq titles (return ())

testTitleTruncatedUtf8 :: IO ()
testTitleTruncatedUtf8 = do
    let longTitle = T.replicate 90 "x"
        rows = [mkRow "01ABCDEFGH01" longTitle Ready (Just 5) [] 0 0 Nothing]
        out  = renderTaskList True rows [Ready]
        ls   = T.lines out
    assertBool "has a data row" (not (null ls))
    let dataRow = head ls
    -- The title column is at most 72 chars and ends with the UTF-8 ellipsis.
    assertBool "row contains UTF-8 ellipsis" ("…" `T.isInfixOf` dataRow)
    assertBool "row does not contain raw 90-char title" (not (longTitle `T.isInfixOf` dataRow))
    let titlePart = T.take (Icarium.Render.recommendedTitleMax) (T.drop 14 dataRow)
    assertBool "title column is exactly 72 chars" (T.length titlePart == Icarium.Render.recommendedTitleMax)

testTitleTruncatedAscii :: IO ()
testTitleTruncatedAscii = do
    let longTitle = T.replicate 90 "y"
        rows = [mkRow "01ABCDEFGH01" longTitle Ready (Just 5) [] 0 0 Nothing]
        out  = renderTaskList False rows [Ready]
    assertBool "row contains ASCII ellipsis" ("..." `T.isInfixOf` out)
    assertBool "row does not contain raw 90-char title" (not (longTitle `T.isInfixOf` out))
    assertBool "no UTF-8 ellipsis in ASCII mode" (not ("…" `T.isInfixOf` out))

testTitleExactlyAtLimit :: IO ()
testTitleExactlyAtLimit = do
    let exactTitle = T.replicate 72 "z"
        rows = [mkRow "01ABCDEFGH01" exactTitle Ready (Just 5) [] 0 0 Nothing]
        out  = renderTaskList True rows [Ready]
    assertBool "title at limit appears untruncated" (exactTitle `T.isInfixOf` out)
    assertBool "no ellipsis when title fits" (not ("…" `T.isInfixOf` out))
