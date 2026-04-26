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
import           Icarium.Render         (renderKnowledgeList, renderTaskList, renderTaskPrompt)
import qualified Icarium.Repo.Category  as RC
import qualified Icarium.Repo.Dispatch  as RD
import qualified Icarium.Repo.Knowledge as RK
import qualified Icarium.Repo.Task      as RT
import           Icarium.Schema         (applySchema, execSql)
import           Icarium.Slug           (titleToSlug)
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
        , testCase "v2 DB migrates to v3: user_version stamped"         testMigrateV2ToV3Version
        , testCase "v2 DB migrates to v3: slug column exists"           testMigrateV2ToV3SlugCol
        , testCase "v2 DB migrates to v3: existing rows preserved"      testMigrateV2ToV3Data
        , testCase "migrateDb is idempotent on v3 DB"                   testMigrateIdempotent
        ]
    , testGroup "slug"
        [ testCase "titleToSlug: basic kebab conversion"                testSlugBasic
        , testCase "titleToSlug: consecutive separators collapsed"      testSlugCollapse
        , testCase "titleToSlug: truncates at 30 chars"                 testSlugTruncate
        , testCase "titleToSlug: strips trailing dash after truncation" testSlugStripDash
        , testCase "slug roundtrip: insert then resolve by slug"        testSlugResolve
        , testCase "slug collision: appends -2"                         testSlugCollision
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
    , taskSlug        = Nothing
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

testMigrateV1ToV2Version :: IO ()
testMigrateV1ToV2Version = withTestDbV1 $ \conn -> do
    -- migrateDb applies all pending migrations; v1 → v2 → v3.
    migrateDb conn
    v <- dbSchemaVersion conn
    v @?= 3

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
    -- withTestDb applies current schema (v3); migrateDb should be a no-op.
    v0 <- dbSchemaVersion conn
    migrateDb conn
    v1 <- dbSchemaVersion conn
    (v0, v1) @?= (3 :: Int64, 3 :: Int64)

testMigrateV2ToV3Version :: IO ()
testMigrateV2ToV3Version = withTestDbV2 $ \conn -> do
    migrateDb conn
    v <- dbSchemaVersion conn
    v @?= 3

testMigrateV2ToV3SlugCol :: IO ()
testMigrateV2ToV3SlugCol = withTestDbV2 $ \conn -> do
    migrateDb conn
    -- Inserting a task with a slug value must succeed after migration.
    execute conn
        (Query "INSERT INTO tasks (id, title, body, state, slug) VALUES (?,?,?,?,?)")
        ( "01MTEST0000000000000000010" :: Text
        , "Slug test task" :: Text
        , "" :: Text
        , "ready" :: Text
        , "slug-test-task" :: Text
        )
    rows <- query_ conn "SELECT slug FROM tasks WHERE id = '01MTEST0000000000000000010'"
                :: IO [Only Text]
    map (\(Only s) -> s) rows @?= ["slug-test-task"]

testMigrateV2ToV3Data :: IO ()
testMigrateV2ToV3Data = withTestDbV2 $ \conn -> do
    -- Insert rows before migrating; verify they survive with slug = NULL.
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

-- =============================================================
-- Slug tests
-- =============================================================

testSlugBasic :: IO ()
testSlugBasic =
    titleToSlug "Hello World" @?= "hello-world"

testSlugCollapse :: IO ()
testSlugCollapse =
    titleToSlug "foo  --  bar" @?= "foo-bar"

testSlugTruncate :: IO ()
testSlugTruncate = do
    let s = titleToSlug "a very long title with many many words here"
    assertBool "truncated to ≤30 chars" (T.length s <= 30)

testSlugStripDash :: IO ()
testSlugStripDash = do
    -- A title whose norm+truncation lands on a dash should strip it.
    -- Force the issue by making a title where position 30 would be '-'.
    -- "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaX" (30 a's then X) — no dash case.
    -- Instead test that a trailing dash is stripped:
    let s = titleToSlug "foo-bar-baz-qux-quux-corge-grault-garply"
    assertBool "no trailing dash" (not (T.isSuffixOf "-" s))

testSlugResolve :: IO ()
testSlugResolve = withTestDb $ \c -> do
    tid <- RT.insertTask c RT.NewTask
        { RT.ntTitle    = "My Test Task"
        , RT.ntBody     = ""
        , RT.ntState    = Ready
        , RT.ntPriority = Nothing
        }
    -- The auto-generated slug should be "my-test-task".
    mt <- RT.getTask c tid
    case mt of
        Nothing -> fail "task not inserted"
        Just t  -> do
            taskSlug t @?= Just "my-test-task"
            -- Resolve by slug
            r <- RT.resolveTaskId c "my-test-task"
            r @?= Right tid

testSlugCollision :: IO ()
testSlugCollision = withTestDb $ \c -> do
    tid1 <- RT.insertTask c RT.NewTask
        { RT.ntTitle = "Dup Slug", RT.ntBody = "", RT.ntState = Planned, RT.ntPriority = Nothing }
    tid2 <- RT.insertTask c RT.NewTask
        { RT.ntTitle = "Dup Slug", RT.ntBody = "", RT.ntState = Planned, RT.ntPriority = Nothing }
    mt1 <- RT.getTask c tid1
    mt2 <- RT.getTask c tid2
    case (mt1, mt2) of
        (Just t1, Just t2) -> do
            taskSlug t1 @?= Just "dup-slug"
            taskSlug t2 @?= Just "dup-slug-2"
        _ -> fail "one or both tasks missing"

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
            let out = renderTaskList [t]
                ls  = T.lines out
            assertBool "at least one data row" (length ls >= 2)
            let dataRow = ls !! 1
                prefix  = T.take 12 dataRow
            assertBool "id prefix is 10 chars padded to 12" (T.length prefix == 12)
            assertBool "id matches task prefix" (T.take 10 tid `T.isPrefixOf` T.stripEnd prefix)

testKnowledgeListIdWidth :: IO ()
testKnowledgeListIdWidth = withTestDb $ \c -> do
    kid <- mkKnowledge c "Width K" "body"
    mk  <- RK.getKnowledge c kid
    case mk of
        Nothing -> fail "knowledge not found"
        Just k  -> do
            let out = renderKnowledgeList [k]
                ls  = T.lines out
            assertBool "at least one data row" (length ls >= 2)
            let dataRow = ls !! 1
                prefix  = T.take 12 dataRow
            assertBool "id prefix is 10 chars padded to 12" (T.length prefix == 12)
            assertBool "id matches knowledge prefix" (T.take 10 kid `T.isPrefixOf` T.stripEnd prefix)
