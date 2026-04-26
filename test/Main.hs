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
import           Icarium.Render         (renderKnowledge, renderKnowledgeList, renderTaskHuman, renderTaskList, renderTaskPrompt)
import           Icarium.Commands.Dispatch (renderDispatch)
import           Icarium.Commands.Know  (autoDeriveDeps)
import qualified Icarium.Repo.Category  as RC
import qualified Icarium.Repo.Dispatch  as RD
import qualified Icarium.Repo.Edge      as RE
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
    -- PREFIX_RESOLUTION: one group per id-taking CLI surface.
    -- Grep for PREFIX_RESOLUTION to audit coverage when adding new commands.
    , testGroup "resolveTaskId (PREFIX_RESOLUTION: task show/update/rm, task add --depends-on, dispatch run, dispatch list --task)"
        [ testCase "right on unique prefix" testResolveTaskPrefix
        , testCase "left on missing"        testResolveTaskMissing
        , testCase "left on ambiguous"      testResolveTaskAmbiguous
        ]
    , testGroup "resolveKnowledgeId (PREFIX_RESOLUTION: know show/update/rm, know add --supersedes, task add --references)"
        [ testCase "right on unique prefix" testResolveKnowledgePrefix
        , testCase "left on missing"        testResolveKnowledgeMissing
        , testCase "left on ambiguous"      testResolveKnowledgeAmbiguous
        ]
    , testGroup "resolveEdgeId (PREFIX_RESOLUTION: link rm)"
        [ testCase "right on unique prefix" testResolveEdgePrefix
        , testCase "left on missing"        testResolveEdgeMissing
        , testCase "left on ambiguous"      testResolveEdgeAmbiguous
        ]
    , testGroup "resolveNode (PREFIX_RESOLUTION: link add src/dst, link list --from/--to, know add --derived-from)"
        [ testCase "resolves task prefix"      testResolveNodeTask
        , testCase "resolves knowledge prefix" testResolveNodeKnowledge
        , testCase "left on ambiguous cross-type" testResolveNodeAmbiguous
        ]
    , testGroup "10-char ULID prefix rendering"
        [ testCase "task list id column is 10 chars"      testTaskListIdWidth
        , testCase "knowledge list id column is 10 chars" testKnowledgeListIdWidth
        ]
    , testGroup "show views render full ULIDs"
        [ testCase "task show id is 26 chars"      testTaskShowIdFull
        , testCase "knowledge show id is 26 chars" testKnowledgeShowIdFull
        , testCase "dispatch show id is 26 chars"  testDispatchShowIdFull
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
    , testGroup "autoDeriveDeps"
        [ testCase "ICARIUM_TASK_ID set, no explicit → edge inserted"     testAutoDeriveDepsEdgeInserted
        , testCase "explicit --derived-from wins over ICARIUM_TASK_ID"    testAutoDeriveDepsExplicitWins
        , testCase "ICARIUM_TASK_ID unset → no auto edge"                 testAutoDeriveDepsNoEnv
        , testCase "ICARIUM_TASK_ID set but task missing → empty"         testAutoDeriveDepsTaskMissing
        ]
    , testGroup "updateTask block_reason invariant"
        [ testCase "transition Blocked → Done clears block_reason"         testUpdateClearsBlockReasonOnDone
        , testCase "transition Blocked → Ready clears block_reason"        testUpdateClearsBlockReasonOnReady
        , testCase "Blocked → Blocked preserves block_reason"              testUpdateBlockedPreservesReason
        ]
    , testGroup "task show links section"
        [ testCase "no edges renders (none)"                               testLinksNoEdges
        , testCase "only depends-on edges"                                 testLinksOnlyDeps
        , testCase "only references edges"                                 testLinksOnlyRefs
        , testCase "both kinds present, deps before refs"                  testLinksBothKinds
        , testCase "stale knowledge gets [STALE] suffix"                   testLinksStaleKnowledge
        , testCase "done task gets [done] suffix"                          testLinksTaskDone
        , testCase "blocked task gets [blocked] suffix"                    testLinksTaskBlocked
        , testCase "ASCII mode uses +- and \\- glyphs"                     testLinksAscii
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
    let outH = renderTaskHuman True minTask [] [] []
    assertBool "renderTaskHuman returned empty" (not (T.null outH))

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
-- resolveTaskId tests
-- =============================================================

testResolveTaskPrefix :: IO ()
testResolveTaskPrefix = withTestDb $ \c -> do
    tid <- RT.insertTask c RT.NewTask
        { RT.ntTitle = "T", RT.ntBody = "", RT.ntState = Ready, RT.ntPriority = Nothing }
    r <- RT.resolveTaskId c (T.take 10 tid)
    r @?= Right tid

testResolveTaskMissing :: IO ()
testResolveTaskMissing = withTestDb $ \c -> do
    r <- RT.resolveTaskId c "01ZZZZZZZZ"
    case r of
        Left msg -> assertBool "error mentions input" ("01ZZZZZZZZ" `T.isInfixOf` T.pack msg)
        Right _  -> fail "expected Left for missing task"

testResolveTaskAmbiguous :: IO ()
testResolveTaskAmbiguous = withTestDb $ \c -> do
    let tid1 = "01CCCC000000000000000000AA" :: Text
        tid2 = "01CCCC000000000000000000BB" :: Text
    forM_ [tid1, tid2] $ \tid ->
        execute c
            (Query "INSERT INTO tasks (id, title, body, state) VALUES (?,?,?,?)")
            (tid, "T" :: Text, "" :: Text, "ready" :: Text)
    r <- RT.resolveTaskId c "01CCCC0000"
    case r of
        Left msg -> assertBool "error mentions input" ("01CCCC0000" `T.isInfixOf` T.pack msg)
        Right _  -> fail "expected Left for ambiguous task"

-- =============================================================
-- resolveKnowledgeId tests
-- =============================================================

testResolveKnowledgePrefix :: IO ()
testResolveKnowledgePrefix = withTestDb $ \c -> do
    kid <- mkKnowledge c "K" "body"
    r <- RK.resolveKnowledgeId c (T.take 10 kid)
    r @?= Right kid

testResolveKnowledgeMissing :: IO ()
testResolveKnowledgeMissing = withTestDb $ \c -> do
    r <- RK.resolveKnowledgeId c "01ZZZZZZZZ"
    case r of
        Left msg -> assertBool "error mentions input" ("01ZZZZZZZZ" `T.isInfixOf` T.pack msg)
        Right _  -> fail "expected Left for missing knowledge"

testResolveKnowledgeAmbiguous :: IO ()
testResolveKnowledgeAmbiguous = withTestDb $ \c -> do
    let kid1 = "01DDDD000000000000000000AA" :: Text
        kid2 = "01DDDD000000000000000000BB" :: Text
    forM_ [kid1, kid2] $ \kid ->
        execute c
            (Query "INSERT INTO knowledge (id, title, body) VALUES (?,?,?)")
            (kid, "K" :: Text, "" :: Text)
    r <- RK.resolveKnowledgeId c "01DDDD0000"
    case r of
        Left msg -> assertBool "error mentions input" ("01DDDD0000" `T.isInfixOf` T.pack msg)
        Right _  -> fail "expected Left for ambiguous knowledge"

-- =============================================================
-- resolveEdgeId tests
-- =============================================================

-- Insert a depends_on edge between two tasks; return the edge id.
insertTestEdge :: Connection -> Text -> Text -> IO Text
insertTestEdge c src dst =
    RE.insertEdge c DependsOn TaskNode src TaskNode dst

testResolveEdgePrefix :: IO ()
testResolveEdgePrefix = withTestDb $ \c -> do
    t1 <- RT.insertTask c RT.NewTask { RT.ntTitle = "A", RT.ntBody = "", RT.ntState = Ready, RT.ntPriority = Nothing }
    t2 <- RT.insertTask c RT.NewTask { RT.ntTitle = "B", RT.ntBody = "", RT.ntState = Ready, RT.ntPriority = Nothing }
    eid <- insertTestEdge c t1 t2
    r <- RE.resolveEdgeId c (T.take 10 eid)
    r @?= Right eid

testResolveEdgeMissing :: IO ()
testResolveEdgeMissing = withTestDb $ \c -> do
    r <- RE.resolveEdgeId c "01ZZZZZZZZ"
    case r of
        Left msg -> assertBool "error mentions input" ("01ZZZZZZZZ" `T.isInfixOf` T.pack msg)
        Right _  -> fail "expected Left for missing edge"

testResolveEdgeAmbiguous :: IO ()
testResolveEdgeAmbiguous = withTestDb $ \c -> do
    -- Two tasks; two edges between different fixed-id pairs share a prefix.
    t1 <- RT.insertTask c RT.NewTask { RT.ntTitle = "A", RT.ntBody = "", RT.ntState = Ready, RT.ntPriority = Nothing }
    t2 <- RT.insertTask c RT.NewTask { RT.ntTitle = "B", RT.ntBody = "", RT.ntState = Ready, RT.ntPriority = Nothing }
    t3 <- RT.insertTask c RT.NewTask { RT.ntTitle = "C", RT.ntBody = "", RT.ntState = Ready, RT.ntPriority = Nothing }
    let eid1 = "01EEEE000000000000000000AA" :: Text
        eid2 = "01EEEE000000000000000000BB" :: Text
    execute c
        (Query "INSERT INTO edges (id, kind, src_kind, src_id, dst_kind, dst_id) VALUES (?,?,?,?,?,?)")
        (eid1, "depends_on" :: Text, "task" :: Text, t1, "task" :: Text, t2)
    execute c
        (Query "INSERT INTO edges (id, kind, src_kind, src_id, dst_kind, dst_id) VALUES (?,?,?,?,?,?)")
        (eid2, "depends_on" :: Text, "task" :: Text, t1, "task" :: Text, t3)
    r <- RE.resolveEdgeId c "01EEEE0000"
    case r of
        Left msg -> assertBool "error mentions input" ("01EEEE0000" `T.isInfixOf` T.pack msg)
        Right _  -> fail "expected Left for ambiguous edge"

-- =============================================================
-- resolveNode tests (shared between link add and know --derived-from)
-- =============================================================

testResolveNodeTask :: IO ()
testResolveNodeTask = withTestDb $ \c -> do
    tid <- RT.insertTask c RT.NewTask { RT.ntTitle = "T", RT.ntBody = "", RT.ntState = Ready, RT.ntPriority = Nothing }
    ts <- RT.getTasksByPrefix c (T.take 10 tid)
    ks <- RK.getKnowledgesByPrefix c (T.take 10 tid)
    (length ts, length ks) @?= (1, 0)
    taskId (head ts) @?= tid

testResolveNodeKnowledge :: IO ()
testResolveNodeKnowledge = withTestDb $ \c -> do
    kid <- mkKnowledge c "K" "body"
    ts <- RT.getTasksByPrefix c (T.take 10 kid)
    ks <- RK.getKnowledgesByPrefix c (T.take 10 kid)
    (length ts, length ks) @?= (0, 1)
    knowledgeId (head ks) @?= kid

testResolveNodeAmbiguous :: IO ()
testResolveNodeAmbiguous = withTestDb $ \c -> do
    -- Force a task and knowledge to share a prefix by inserting with fixed ids.
    let sharedPrefix = "01FFFF0000"
        tid = sharedPrefix <> "0000000000000000" :: Text
        kid = sharedPrefix <> "1111111111111111" :: Text
    execute c
        (Query "INSERT INTO tasks (id, title, body, state) VALUES (?,?,?,?)")
        (tid, "T" :: Text, "" :: Text, "ready" :: Text)
    execute c
        (Query "INSERT INTO knowledge (id, title, body) VALUES (?,?,?)")
        (kid, "K" :: Text, "" :: Text)
    ts <- RT.getTasksByPrefix c sharedPrefix
    ks <- RK.getKnowledgesByPrefix c sharedPrefix
    assertBool "both task and knowledge match prefix" (length ts == 1 && length ks == 1)

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

-- =============================================================
-- show views render full ULIDs
-- =============================================================

minKnowledge :: Knowledge
minKnowledge = Knowledge
    { knowledgeId        = "01KNOW000000000000000000KK"
    , knowledgeTitle     = "Test knowledge"
    , knowledgeBody      = "Know body"
    , knowledgeStale     = False
    , knowledgeCreatedAt = "2026-01-01T00:00:00Z"
    , knowledgeUpdatedAt = "2026-01-01T00:00:00Z"
    }

minDispatch :: Dispatch
minDispatch = Dispatch
    { dispatchId         = "01DISP000000000000000000DD"
    , dispatchTaskId     = "01TEST00000000000000000000"
    , dispatchBranch     = "dispatch/01DISP000000000000000000DD"
    , dispatchBaseBranch = "main"
    , dispatchBaseSha    = "0000000000000000000000000000000000000000"
    , dispatchPid        = Nothing
    , dispatchModel      = "claude-sonnet-4-6"
    , dispatchEffort     = Medium
    , dispatchStartedAt  = "2026-01-01T00:00:00Z"
    , dispatchHeartbeat  = "2026-01-01T00:00:00Z"
    , dispatchEndedAt    = Nothing
    , dispatchOutcome    = Nothing
    , dispatchMergeSha   = Nothing
    , dispatchLastCommit = Nothing
    , dispatchNotes      = Nothing
    , dispatchLogPath    = Nothing
    }

testTaskShowIdFull :: IO ()
testTaskShowIdFull = do
    let out  = renderTaskHuman True minTask [] [] []
        ls   = T.lines out
        idLine = head $ filter ("id:" `T.isPrefixOf`) ls
        val  = T.strip (T.drop (T.length "id:") idLine)
    T.length val @?= 26

testKnowledgeShowIdFull :: IO ()
testKnowledgeShowIdFull = do
    let out  = renderKnowledge minKnowledge []
        ls   = T.lines out
        idLine = head $ filter ("id:" `T.isPrefixOf`) ls
        val  = T.strip (T.drop (T.length "id:") idLine)
    T.length val @?= 26

testDispatchShowIdFull :: IO ()
testDispatchShowIdFull = do
    let out  = renderDispatch minDispatch (Just minTask) []
        ls   = T.lines out
        fieldVal prefix = T.strip . T.drop (T.length prefix) . head
                        $ filter (prefix `T.isPrefixOf`) ls
        idVal      = fieldVal "id:"
        taskIdVal  = fieldVal "task_id:"
    T.length idVal     @?= 26
    T.length taskIdVal @?= 26

-- =============================================================
-- autoDeriveDeps tests
-- =============================================================

testAutoDeriveDepsEdgeInserted :: IO ()
testAutoDeriveDepsEdgeInserted = withTestDb $ \c -> do
    tid <- RT.insertTask c RT.NewTask
        { RT.ntTitle = "Dispatch task", RT.ntBody = "", RT.ntState = Ready, RT.ntPriority = Nothing }
    result <- autoDeriveDeps c [] (Just (T.unpack tid))
    result @?= [(TaskNode, tid)]

testAutoDeriveDepsExplicitWins :: IO ()
testAutoDeriveDepsExplicitWins = withTestDb $ \c -> do
    tid <- RT.insertTask c RT.NewTask
        { RT.ntTitle = "Dispatch task", RT.ntBody = "", RT.ntState = Ready, RT.ntPriority = Nothing }
    otherTid <- RT.insertTask c RT.NewTask
        { RT.ntTitle = "Other task", RT.ntBody = "", RT.ntState = Ready, RT.ntPriority = Nothing }
    result <- autoDeriveDeps c [otherTid] (Just (T.unpack tid))
    result @?= []

testAutoDeriveDepsNoEnv :: IO ()
testAutoDeriveDepsNoEnv = withTestDb $ \c -> do
    result <- autoDeriveDeps c [] Nothing
    result @?= []

testAutoDeriveDepsTaskMissing :: IO ()
testAutoDeriveDepsTaskMissing = withTestDb $ \c -> do
    result <- autoDeriveDeps c [] (Just "01ZZZZZZZZ0000000000000000")
    result @?= []

-- =============================================================
-- updateTask block_reason invariant tests
-- =============================================================

-- Insert a Blocked task with a block_reason, then update its state and
-- assert what happens to block_reason. The invariant: block_reason is
-- only meaningful for Blocked, so transitioning out should clear it.

insertBlockedTask :: Connection -> Text -> IO Text
insertBlockedTask c reason = do
    tid <- RT.insertTask c RT.NewTask
        { RT.ntTitle = "T", RT.ntBody = "", RT.ntState = Ready, RT.ntPriority = Nothing }
    _ <- RT.updateTask c tid RT.emptyUpdate
        { RT.tuState       = Just Blocked
        , RT.tuBlockReason = Just (Just reason)
        }
    pure tid

testUpdateClearsBlockReasonOnDone :: IO ()
testUpdateClearsBlockReasonOnDone = withTestDb $ \c -> do
    tid <- insertBlockedTask c "old reason"
    _ <- RT.updateTask c tid RT.emptyUpdate { RT.tuState = Just Done }
    Just t <- RT.getTask c tid
    taskBlockReason t @?= Nothing

testUpdateClearsBlockReasonOnReady :: IO ()
testUpdateClearsBlockReasonOnReady = withTestDb $ \c -> do
    tid <- insertBlockedTask c "old reason"
    _ <- RT.updateTask c tid RT.emptyUpdate { RT.tuState = Just Ready }
    Just t <- RT.getTask c tid
    taskBlockReason t @?= Nothing

testUpdateBlockedPreservesReason :: IO ()
testUpdateBlockedPreservesReason = withTestDb $ \c -> do
    tid <- insertBlockedTask c "still blocked"
    _ <- RT.updateTask c tid RT.emptyUpdate { RT.tuPriority = Just (Just 5) }
    Just t <- RT.getTask c tid
    taskBlockReason t @?= Just "still blocked"

-- =============================================================
-- task show links section tests
-- =============================================================

-- Shared fixtures for links section tests.
depTask :: TaskState -> Task
depTask st = minTask
    { taskId    = "01DDDD000000000000000000DD"
    , taskTitle = "Dep task"
    , taskState = st
    , taskBlockReason = Nothing
    }

refKnow :: Bool -> Knowledge
refKnow stale = minKnowledge
    { knowledgeId    = "01RRRR000000000000000000RR"
    , knowledgeTitle = "Ref knowledge"
    , knowledgeStale = stale
    }

testLinksNoEdges :: IO ()
testLinksNoEdges = do
    let out = renderTaskHuman True minTask [] [] []
    assertBool "## Links header"        ("## Links"  `T.isInfixOf` out)
    assertBool "(none) line"            ("(none)"    `T.isInfixOf` out)
    assertBool "no depends-on edge"     (not ("depends-on" `T.isInfixOf` out))
    assertBool "no references edge"     (not ("references" `T.isInfixOf` out))

testLinksOnlyDeps :: IO ()
testLinksOnlyDeps = do
    let dep = depTask Planned
        out = renderTaskHuman True minTask [] [dep] []
    assertBool "## Links header"        ("## Links"   `T.isInfixOf` out)
    assertBool "depends-on edge"        ("depends-on" `T.isInfixOf` out)
    assertBool "dep id prefix"          (T.take 10 (taskId dep) `T.isInfixOf` out)
    assertBool "dep state"              ("[planned]"  `T.isInfixOf` out)
    assertBool "no references"          (not ("references" `T.isInfixOf` out))
    assertBool "last glyph is └─"      ("└─"         `T.isInfixOf` out)

testLinksOnlyRefs :: IO ()
testLinksOnlyRefs = do
    let ref = refKnow False
        out = renderTaskHuman True minTask [ref] [] []
    assertBool "## Links header"        ("## Links"   `T.isInfixOf` out)
    assertBool "references edge"        ("references" `T.isInfixOf` out)
    assertBool "ref id prefix"          (T.take 10 (knowledgeId ref) `T.isInfixOf` out)
    assertBool "no [STALE] suffix"      (not ("[STALE]"    `T.isInfixOf` out))
    assertBool "no depends-on"          (not ("depends-on" `T.isInfixOf` out))
    assertBool "last glyph is └─"      ("└─"         `T.isInfixOf` out)

testLinksBothKinds :: IO ()
testLinksBothKinds = do
    let dep = depTask Ready
        ref = refKnow False
        out = renderTaskHuman True minTask [ref] [dep] []
    assertBool "depends-on edge"        ("depends-on" `T.isInfixOf` out)
    assertBool "references edge"        ("references" `T.isInfixOf` out)
    -- depends-on appears before references in the output
    let Just iDep = posOf "depends-on" out
        Just iRef = posOf "references" out
    assertBool "depends-on before references" (iDep < iRef)
    -- branch glyph on dep (not last), last glyph on ref
    assertBool "branch glyph ├─"       ("├─" `T.isInfixOf` out)
    assertBool "last glyph └─"         ("└─" `T.isInfixOf` out)
  where
    posOf needle h = let (pre, suf) = T.breakOn needle h
                     in if T.null suf then Nothing else Just (T.length pre)

testLinksStaleKnowledge :: IO ()
testLinksStaleKnowledge = do
    let ref = refKnow True
        out = renderTaskHuman True minTask [ref] [] []
    assertBool "[STALE] suffix present" ("[STALE]" `T.isInfixOf` out)

testLinksTaskDone :: IO ()
testLinksTaskDone = do
    let dep = depTask Done
        out = renderTaskHuman True minTask [] [dep] []
    assertBool "[done] suffix" ("[done]" `T.isInfixOf` out)

testLinksTaskBlocked :: IO ()
testLinksTaskBlocked = do
    let dep = (depTask Blocked) { taskBlockReason = Just "waiting" }
        out = renderTaskHuman True minTask [] [dep] []
    assertBool "[blocked] suffix" ("[blocked]" `T.isInfixOf` out)

testLinksAscii :: IO ()
testLinksAscii = do
    let dep = depTask Planned
        ref = refKnow False
        out = renderTaskHuman False minTask [ref] [dep] []
    assertBool "ASCII branch glyph +- present" ("+-" `T.isInfixOf` out)
    assertBool "ASCII last glyph \\- present"   ("\\-" `T.isInfixOf` out)
    assertBool "no UTF-8 branch glyph"          (not ("├─" `T.isInfixOf` out))
    assertBool "no UTF-8 last glyph"            (not ("└─" `T.isInfixOf` out))
