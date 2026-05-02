module RepoSpec (tests) where

import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_, void)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Database.SQLite.Simple (Connection, Only (..), Query (..), execute, query_)
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Icarium.Commands.Category (SyncReport (..), syncCategories)
import Icarium.Commands.Know (autoDeriveDeps)
import Icarium.Config (CategoriesConfig (..), defaultConfigText, loadConfig)
import Icarium.Db (dbSchemaVersion, migrateDb)
import Icarium.Id (newId)
import Icarium.Migrations (Migration (..), mkSqlMigration)
import Icarium.Render (renderTaskPrompt)
import Icarium.Repo.Category qualified as RC
import Icarium.Repo.Dispatch qualified as RD
import Icarium.Repo.Edge qualified as RE
import Icarium.Repo.Knowledge qualified as RK
import Icarium.Repo.Task qualified as RT
import Icarium.Types

import TestHelpers

tests :: TestTree
tests =
    testGroup
        "repo"
        [ testGroup "EdgeKind round-trips" edgeKindTests
        , testCase "parseTaskState CLI form" $ parseTaskState "in-progress" @?= Just InProgress
        , testCase "parseTaskStateDb DB form" $ parseTaskStateDb "in_progress" @?= Just InProgress
        , testCase "loadConfig succeeds on default template" loadConfigTest
        , testGroup
            "categoryMatchedKnowledge"
            [ testCase "both-axis match appears under Related knowledge" testBothAxisMatch
            , testCase "stale knowledge excluded from auto-pull" testStaleExcluded
            , testCase "zero categories yields empty result" testNoCats
            , testCase "explicit ref deduped from auto-pull" testDedup
            , testCase "cap at 5, ordered most-recent-first" testCap
            , testCase "one-axis match excluded when task has both axes" testOneAxisMismatch
            , testCase "stale explicit ref still renders under refs" testStaleRef
            ]
        , testGroup
            "schema"
            [ testCase "applying embedded schema produces user_version = 1 with expected tables" testInitialSchema
            , testCase "deleting a knowledge entry cascades to knowledge_categories rows" testKnowledgeCategoriesCascade
            ]
        , testGroup
            "migrations"
            [ testCase "v1 DB advances to latest version after migrateDb" testMigrateAdvances
            , testCase "bad SQL rolls back; user_version unchanged" testMigrateBadSqlRollback
            ]
        , testGroup
            "resolveDispatchId (PREFIX_RESOLUTION: dispatch show, dispatch logs, dispatch recover)"
            [ testCase "right on full id" testResolveDispatchFullId
            , testCase "right on unique prefix" testResolveDispatchPrefix
            , testCase "left on missing" testResolveDispatchMissing
            , testCase "left on ambiguous" testResolveDispatchAmbiguous
            ]
        , -- PREFIX_RESOLUTION: one group per id-taking CLI surface.
          -- Grep for PREFIX_RESOLUTION to audit coverage when adding new commands.
          testGroup
            "resolveTaskId (PREFIX_RESOLUTION: task show/update/rm, task add --depends-on, dispatch run, dispatch list --task)"
            [ testCase "right on unique prefix" testResolveTaskPrefix
            , testCase "left on missing" testResolveTaskMissing
            , testCase "left on ambiguous" testResolveTaskAmbiguous
            ]
        , testGroup
            "resolveKnowledgeId (PREFIX_RESOLUTION: know show/update/rm, know add --supersedes, task add --references)"
            [ testCase "right on unique prefix" testResolveKnowledgePrefix
            , testCase "left on missing" testResolveKnowledgeMissing
            , testCase "left on ambiguous" testResolveKnowledgeAmbiguous
            ]
        , testGroup
            "resolveEdgeId (PREFIX_RESOLUTION: link rm)"
            [ testCase "right on unique prefix" testResolveEdgePrefix
            , testCase "left on missing" testResolveEdgeMissing
            , testCase "left on ambiguous" testResolveEdgeAmbiguous
            ]
        , testGroup
            "listEdges filtering"
            [ testCase "src + kind filter returns only matching rows" testListEdgesSrcKindFilter
            ]
        , testGroup
            "resolveNode (PREFIX_RESOLUTION: link add src/dst, link list --from/--to, know add --derived-from)"
            [ testCase "resolves task prefix" testResolveNodeTask
            , testCase "resolves knowledge prefix" testResolveNodeKnowledge
            , testCase "left on ambiguous cross-type" testResolveNodeAmbiguous
            ]
        , testGroup
            "category sync"
            [ testCase "inserts toml-only categories" testSyncInserts
            , testCase "reports orphans and exits non-zero (no prune)" testSyncOrphanNoPrune
            , testCase "prunes unused orphans when no blockers" testSyncPrunesUnused
            , testCase "blocks on in-use category; no deletions" testSyncBlocksInUse
            ]
        , testGroup
            "autoDeriveDeps"
            [ testCase "ICARIUM_TASK_ID set, no explicit → edge inserted" testAutoDeriveDepsEdgeInserted
            , testCase "explicit --derived-from wins over ICARIUM_TASK_ID" testAutoDeriveDepsExplicitWins
            , testCase "ICARIUM_TASK_ID unset → no auto edge" testAutoDeriveDepsNoEnv
            , testCase "ICARIUM_TASK_ID set but task missing → empty" testAutoDeriveDepsTaskMissing
            ]
        , testGroup
            "updateTask block_reason invariant"
            [ testCase "transition Blocked → Done clears block_reason" testUpdateClearsBlockReasonOnDone
            , testCase "transition Blocked → Ready clears block_reason" testUpdateClearsBlockReasonOnReady
            , testCase "Blocked → Blocked preserves block_reason" testUpdateBlockedPreservesReason
            ]
        , testGroup
            "category replace semantics"
            [ testCase "task update --domain replaces existing domain" testTaskUpdateDomainReplaces
            , testCase "task update --domain empty string clears domain" testTaskUpdateDomainClears
            , testCase "know update --domain replaces not appends" testKnowUpdateDomainReplaces
            ]
        ]

-- =============================================================
-- EdgeKind round-trips
-- =============================================================

edgeKindTests :: [TestTree]
edgeKindTests =
    [ testCase "all constructors round-trip through display form" $
        mapM_ (\k -> parseEdgeKind (edgeKindDisplay k) @?= Just k) allEdgeKinds
    , testCase "all constructors round-trip through DB form" $
        mapM_ (\k -> parseEdgeKindDb (edgeKindDbText k) @?= Just k) allEdgeKinds
    , testCase "underscore form rejected by parseEdgeKind (hyphens enforced)" $ do
        parseEdgeKind "depends_on" @?= Nothing
        parseEdgeKind "derived_from" @?= Nothing
    ]
  where
    allEdgeKinds = [DependsOn, References, DerivedFrom, Supersedes]

-- =============================================================
-- Config smoke test
-- =============================================================

loadConfigTest :: IO ()
loadConfigTest =
    withSystemTempFile "icarium.toml" $ \fp h -> do
        hClose h
        TIO.writeFile fp defaultConfigText
        result <- loadConfig fp
        case result of
            Left err -> fail ("loadConfig failed: " <> err)
            Right _ -> pure ()

-- =============================================================
-- categoryMatchedKnowledge tests
-- =============================================================

testBothAxisMatch :: IO ()
testBothAxisMatch = withTestDb $ \c -> do
    domCat <- mkCat c Domain "cli"
    discCat <- mkCat c Discipline "haskell"
    kid <- mkKnowledge c "Match K" "match body"
    attachKnowledgeCats c kid [domCat, discCat]
    result <- RK.categoryMatchedKnowledge c [domCat, discCat] 5
    map knowledgeId result @?= [kid]
    let prompt = renderTaskPrompt minTask [] result []
    assertBool "Related knowledge header present" ("## Related knowledge" `T.isInfixOf` prompt)
    assertBool "Hedge sentence present" ("use judgment" `T.isInfixOf` prompt)
    assertBool "Knowledge title in prompt" ("Match K" `T.isInfixOf` prompt)

testStaleExcluded :: IO ()
testStaleExcluded = withTestDb $ \c -> do
    domCat <- mkCat c Domain "cli"
    kid <- mkKnowledge c "Stale K" "stale body"
    attachKnowledgeCats c kid [domCat]
    void $ RK.updateKnowledge c kid RK.emptyUpdate{RK.kuStale = Just True}
    result <- RK.categoryMatchedKnowledge c [domCat] 5
    null result @?= True

testNoCats :: IO ()
testNoCats = withTestDb $ \c -> do
    result <- RK.categoryMatchedKnowledge c [] 5
    null result @?= True
    let prompt = renderTaskPrompt minTask [] [] []
    assertBool "no Related knowledge header" (not ("## Related knowledge" `T.isInfixOf` prompt))

testDedup :: IO ()
testDedup = withTestDb $ \c -> do
    domCat <- mkCat c Domain "cli"
    kid <- mkKnowledge c "Shared K" "shared body"
    attachKnowledgeCats c kid [domCat]
    catResult <- RK.categoryMatchedKnowledge c [domCat] 5
    case catResult of
        [] -> fail "expected knowledge to auto-pull"
        k : _ -> do
            let refs = [k]
                refIds = map knowledgeId refs
                dedupedCat = filter (\x -> knowledgeId x `notElem` refIds) catResult
            assertBool "dedupedCat is empty" (null dedupedCat)
            let prompt = renderTaskPrompt minTask refs dedupedCat []
            assertBool "refs section present" ("## Referenced knowledge" `T.isInfixOf` prompt)
            assertBool "related section absent (dedup)" (not ("## Related knowledge" `T.isInfixOf` prompt))

testCap :: IO ()
testCap = withTestDb $ \c -> do
    domCat <- mkCat c Domain "cli"
    kids <- forM [1 .. 6 :: Int] $ \i -> do
        kid <- newId
        execute
            c
            (Query "INSERT INTO knowledge (id, title, body, created_at) VALUES (?,?,?,?)")
            ( kid
            , "K" <> T.pack (show i) :: Text
            , "" :: Text
            , "2026-01-0" <> T.pack (show i) <> "T00:00:00" :: Text
            )
        RC.attachKnowledgeCategory c kid (categoryId domCat)
        pure kid
    result <- RK.categoryMatchedKnowledge c [domCat] 5
    length result @?= 5
    map knowledgeId result @?= reverse (tail kids)

testOneAxisMismatch :: IO ()
testOneAxisMismatch = withTestDb $ \c -> do
    domCat <- mkCat c Domain "cli"
    discCat <- mkCat c Discipline "haskell"
    kid <- mkKnowledge c "Domain-only K" "body"
    attachKnowledgeCats c kid [domCat]
    result <- RK.categoryMatchedKnowledge c [domCat, discCat] 5
    null result @?= True

testStaleRef :: IO ()
testStaleRef = withTestDb $ \c -> do
    kid <- mkKnowledge c "Stale ref" "stale ref body"
    void $ RK.updateKnowledge c kid RK.emptyUpdate{RK.kuStale = Just True}
    mk <- RK.getKnowledge c kid
    case mk of
        Nothing -> fail "knowledge not found after insert"
        Just k -> do
            let prompt = renderTaskPrompt minTask [k] [] []
            assertBool "stale ref body in prompt" ("stale ref body" `T.isInfixOf` prompt)
            assertBool "Referenced knowledge header" ("## Referenced knowledge" `T.isInfixOf` prompt)

-- =============================================================
-- Schema tests
-- =============================================================

testInitialSchema :: IO ()
testInitialSchema = withTestDb $ \conn -> do
    v <- dbSchemaVersion conn
    v @?= (1 :: Int64)
    tableRows <-
        query_
            conn
            "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name" ::
            IO [Only Text]
    let tables = map (\(Only n) -> n) tableRows
    assertBool "tasks table exists" ("tasks" `elem` tables)
    assertBool "knowledge table exists" ("knowledge" `elem` tables)
    assertBool "categories table exists" ("categories" `elem` tables)
    assertBool "edges table exists" ("edges" `elem` tables)
    assertBool "dispatches table exists" ("dispatches" `elem` tables)
    assertBool "task_categories table exists" ("task_categories" `elem` tables)
    assertBool "knowledge_categories table exists" ("knowledge_categories" `elem` tables)

testKnowledgeCategoriesCascade :: IO ()
testKnowledgeCategoriesCascade = withTestDb $ \conn -> do
    domCat <- mkCat conn Domain "cli"
    kid <- mkKnowledge conn "K" "body"
    RC.attachKnowledgeCategory conn kid (categoryId domCat)
    pre <- query_ conn "SELECT knowledge_id FROM knowledge_categories" :: IO [Only Text]
    length pre @?= 1
    execute conn (Query "DELETE FROM knowledge WHERE id = ?") (Only kid)
    post <- query_ conn "SELECT knowledge_id FROM knowledge_categories" :: IO [Only Text]
    length post @?= 0

-- =============================================================
-- Migration tests
-- =============================================================

testMigrateAdvances :: IO ()
testMigrateAdvances = withTestDb $ \conn -> do
    v0 <- dbSchemaVersion conn
    v0 @?= 1
    migrateDb conn
    v1 <- dbSchemaVersion conn
    assertBool "version advanced past 1 after migrateDb" (v1 > 1)

testMigrateBadSqlRollback :: IO ()
testMigrateBadSqlRollback = withTestDb $ \conn -> do
    let m = mkSqlMigration 99 "THIS IS NOT VALID SQL AT ALL"
    result <- try (migrationUp m conn) :: IO (Either SomeException ())
    assertBool "bad migration should throw" (either (const True) (const False) result)
    v <- dbSchemaVersion conn
    v @?= 1

-- =============================================================
-- resolveDispatchId tests
-- =============================================================

insertTestDispatch :: Connection -> Text -> Text -> IO ()
insertTestDispatch c did tid =
    execute
        c
        ( Query
            "INSERT INTO dispatches \
            \(id, task_id, branch, base_branch, base_sha, model, effort) \
            \VALUES (?,?,?,?,?,?,?)"
        )
        ( did
        , tid
        , "dispatch/" <> did :: Text
        , "main" :: Text
        , "0000000000000000000000000000000000000000" :: Text
        , "claude-sonnet-4-6" :: Text
        , "medium" :: Text
        )

testResolveDispatchFullId :: IO ()
testResolveDispatchFullId = withTestDb $ \c -> do
    tid <-
        RT.insertTask
            c
            RT.NewTask
                { RT.ntTitle = "T"
                , RT.ntBody = ""
                , RT.ntState = Ready
                , RT.ntPriority = Nothing
                }
    let did = "01AAAA000000000000000000AA" :: Text
    insertTestDispatch c did tid
    r <- RD.resolveDispatchId c did
    r @?= Right did

testResolveDispatchPrefix :: IO ()
testResolveDispatchPrefix = withTestDb $ \c -> do
    tid <-
        RT.insertTask
            c
            RT.NewTask
                { RT.ntTitle = "T"
                , RT.ntBody = ""
                , RT.ntState = Ready
                , RT.ntPriority = Nothing
                }
    let did = "01AAAA000000000000000000AA" :: Text
    insertTestDispatch c did tid
    r <- RD.resolveDispatchId c "01AAAA0000"
    r @?= Right did

testResolveDispatchMissing :: IO ()
testResolveDispatchMissing = withTestDb $ \c -> do
    r <- RD.resolveDispatchId c "01ZZZZZZZZ"
    case r of
        Left msg -> assertBool "error mentions input" ("01ZZZZZZZZ" `T.isInfixOf` T.pack msg)
        Right _ -> fail "expected Left for missing dispatch"

testResolveDispatchAmbiguous :: IO ()
testResolveDispatchAmbiguous = withTestDb $ \c -> do
    tid <-
        RT.insertTask
            c
            RT.NewTask
                { RT.ntTitle = "T"
                , RT.ntBody = ""
                , RT.ntState = Ready
                , RT.ntPriority = Nothing
                }
    let did1 = "01BBBB000000000000000000AA" :: Text
        did2 = "01BBBB000000000000000000BB" :: Text
    insertTestDispatch c did1 tid
    insertTestDispatch c did2 tid
    r <- RD.resolveDispatchId c "01BBBB0000"
    case r of
        Left msg -> do
            assertBool "error mentions input" ("01BBBB0000" `T.isInfixOf` T.pack msg)
            assertBool "error lists first id" (T.unpack did1 `isInfixOf` msg)
            assertBool "error lists second id" (T.unpack did2 `isInfixOf` msg)
        Right _ -> fail "expected Left for ambiguous dispatch"
  where
    isInfixOf needle haystack = T.isInfixOf (T.pack needle) (T.pack haystack)

-- =============================================================
-- resolveTaskId tests
-- =============================================================

testResolveTaskPrefix :: IO ()
testResolveTaskPrefix = withTestDb $ \c -> do
    tid <-
        RT.insertTask
            c
            RT.NewTask
                { RT.ntTitle = "T"
                , RT.ntBody = ""
                , RT.ntState = Ready
                , RT.ntPriority = Nothing
                }
    r <- RT.resolveTaskId c (T.take 10 tid)
    r @?= Right tid

testResolveTaskMissing :: IO ()
testResolveTaskMissing = withTestDb $ \c -> do
    r <- RT.resolveTaskId c "01ZZZZZZZZ"
    case r of
        Left msg -> assertBool "error mentions input" ("01ZZZZZZZZ" `T.isInfixOf` T.pack msg)
        Right _ -> fail "expected Left for missing task"

testResolveTaskAmbiguous :: IO ()
testResolveTaskAmbiguous = withTestDb $ \c -> do
    let tid1 = "01CCCC000000000000000000AA" :: Text
        tid2 = "01CCCC000000000000000000BB" :: Text
    forM_ [tid1, tid2] $ \tid ->
        execute
            c
            (Query "INSERT INTO tasks (id, title, body, state) VALUES (?,?,?,?)")
            (tid, "T" :: Text, "" :: Text, "ready" :: Text)
    r <- RT.resolveTaskId c "01CCCC0000"
    case r of
        Left msg -> assertBool "error mentions input" ("01CCCC0000" `T.isInfixOf` T.pack msg)
        Right _ -> fail "expected Left for ambiguous task"

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
        Right _ -> fail "expected Left for missing knowledge"

testResolveKnowledgeAmbiguous :: IO ()
testResolveKnowledgeAmbiguous = withTestDb $ \c -> do
    let kid1 = "01DDDD000000000000000000AA" :: Text
        kid2 = "01DDDD000000000000000000BB" :: Text
    forM_ [kid1, kid2] $ \kid ->
        execute
            c
            (Query "INSERT INTO knowledge (id, title, body) VALUES (?,?,?)")
            (kid, "K" :: Text, "" :: Text)
    r <- RK.resolveKnowledgeId c "01DDDD0000"
    case r of
        Left msg -> assertBool "error mentions input" ("01DDDD0000" `T.isInfixOf` T.pack msg)
        Right _ -> fail "expected Left for ambiguous knowledge"

-- =============================================================
-- resolveEdgeId tests
-- =============================================================

insertTestEdge :: Connection -> Text -> Text -> IO Text
insertTestEdge c src =
    RE.insertEdge c DependsOn TaskNode src TaskNode

testResolveEdgePrefix :: IO ()
testResolveEdgePrefix = withTestDb $ \c -> do
    t1 <- RT.insertTask c RT.NewTask{RT.ntTitle = "A", RT.ntBody = "", RT.ntState = Ready, RT.ntPriority = Nothing}
    t2 <- RT.insertTask c RT.NewTask{RT.ntTitle = "B", RT.ntBody = "", RT.ntState = Ready, RT.ntPriority = Nothing}
    eid <- insertTestEdge c t1 t2
    r <- RE.resolveEdgeId c (T.take 10 eid)
    r @?= Right eid

testResolveEdgeMissing :: IO ()
testResolveEdgeMissing = withTestDb $ \c -> do
    r <- RE.resolveEdgeId c "01ZZZZZZZZ"
    case r of
        Left msg -> assertBool "error mentions input" ("01ZZZZZZZZ" `T.isInfixOf` T.pack msg)
        Right _ -> fail "expected Left for missing edge"

testResolveEdgeAmbiguous :: IO ()
testResolveEdgeAmbiguous = withTestDb $ \c -> do
    t1 <- RT.insertTask c RT.NewTask{RT.ntTitle = "A", RT.ntBody = "", RT.ntState = Ready, RT.ntPriority = Nothing}
    t2 <- RT.insertTask c RT.NewTask{RT.ntTitle = "B", RT.ntBody = "", RT.ntState = Ready, RT.ntPriority = Nothing}
    t3 <- RT.insertTask c RT.NewTask{RT.ntTitle = "C", RT.ntBody = "", RT.ntState = Ready, RT.ntPriority = Nothing}
    let eid1 = "01EEEE000000000000000000AA" :: Text
        eid2 = "01EEEE000000000000000000BB" :: Text
    execute
        c
        (Query "INSERT INTO edges (id, kind, src_kind, src_id, dst_kind, dst_id) VALUES (?,?,?,?,?,?)")
        (eid1, "depends_on" :: Text, "task" :: Text, t1, "task" :: Text, t2)
    execute
        c
        (Query "INSERT INTO edges (id, kind, src_kind, src_id, dst_kind, dst_id) VALUES (?,?,?,?,?,?)")
        (eid2, "depends_on" :: Text, "task" :: Text, t1, "task" :: Text, t3)
    r <- RE.resolveEdgeId c "01EEEE0000"
    case r of
        Left msg -> assertBool "error mentions input" ("01EEEE0000" `T.isInfixOf` T.pack msg)
        Right _ -> fail "expected Left for ambiguous edge"

-- =============================================================
-- listEdges filtering tests
-- =============================================================

testListEdgesSrcKindFilter :: IO ()
testListEdgesSrcKindFilter = withTestDb $ \c -> do
    t1 <- RT.insertTask c RT.NewTask{RT.ntTitle = "A", RT.ntBody = "", RT.ntState = Ready, RT.ntPriority = Nothing}
    t2 <- RT.insertTask c RT.NewTask{RT.ntTitle = "B", RT.ntBody = "", RT.ntState = Ready, RT.ntPriority = Nothing}
    t3 <- RT.insertTask c RT.NewTask{RT.ntTitle = "C", RT.ntBody = "", RT.ntState = Ready, RT.ntPriority = Nothing}
    kid <- RK.insertKnowledge c RK.NewKnowledge{RK.nkTitle = "K", RK.nkBody = ""}
    _ <- RE.insertEdge c DependsOn TaskNode t1 TaskNode t2
    _ <- RE.insertEdge c References TaskNode t1 KnowledgeNode kid
    _ <- RE.insertEdge c DependsOn TaskNode t2 TaskNode t3
    es <- RE.listEdges c (Just t1) Nothing (Just DependsOn)
    length es @?= 1
    edgeSrcId (head es) @?= t1
    edgeKind (head es) @?= DependsOn

-- =============================================================
-- resolveNode tests
-- =============================================================

testResolveNodeTask :: IO ()
testResolveNodeTask = withTestDb $ \c -> do
    tid <- RT.insertTask c RT.NewTask{RT.ntTitle = "T", RT.ntBody = "", RT.ntState = Ready, RT.ntPriority = Nothing}
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
    let sharedPrefix = "01FFFF0000"
        tid = sharedPrefix <> "0000000000000000" :: Text
        kid = sharedPrefix <> "1111111111111111" :: Text
    execute
        c
        (Query "INSERT INTO tasks (id, title, body, state) VALUES (?,?,?,?)")
        (tid, "T" :: Text, "" :: Text, "ready" :: Text)
    execute
        c
        (Query "INSERT INTO knowledge (id, title, body) VALUES (?,?,?)")
        (kid, "K" :: Text, "" :: Text)
    ts <- RT.getTasksByPrefix c sharedPrefix
    ks <- RK.getKnowledgesByPrefix c sharedPrefix
    assertBool "both task and knowledge match prefix" (length ts == 1 && length ks == 1)

-- =============================================================
-- category sync tests
-- =============================================================

testSyncInserts :: IO ()
testSyncInserts = withTestDb $ \conn -> do
    let cfg = CategoriesConfig{catDomains = ["cli"], catDisciplines = ["haskell"]}
    rpt <- syncCategories conn cfg False
    srInserted rpt @?= [(Domain, "cli"), (Discipline, "haskell")]
    null (srOrphans rpt) @?= True
    null (srPruned rpt) @?= True
    null (srBlocking rpt) @?= True
    cats <- RC.listCategories conn Nothing
    length cats @?= 2

testSyncOrphanNoPrune :: IO ()
testSyncOrphanNoPrune = withTestDb $ \conn -> do
    _ <- RC.insertCategory conn Domain "stale-domain"
    let cfg = CategoriesConfig{catDomains = [], catDisciplines = []}
    rpt <- syncCategories conn cfg False
    null (srInserted rpt) @?= True
    length (srOrphans rpt) @?= 1
    null (srPruned rpt) @?= True
    null (srBlocking rpt) @?= True
    cats <- RC.listCategories conn Nothing
    length cats @?= 1

testSyncPrunesUnused :: IO ()
testSyncPrunesUnused = withTestDb $ \conn -> do
    _ <- RC.insertCategory conn Domain "stale-domain"
    let cfg = CategoriesConfig{catDomains = [], catDisciplines = []}
    rpt <- syncCategories conn cfg True
    null (srInserted rpt) @?= True
    null (srOrphans rpt) @?= True
    length (srPruned rpt) @?= 1
    null (srBlocking rpt) @?= True
    cats <- RC.listCategories conn Nothing
    length cats @?= 0

testSyncBlocksInUse :: IO ()
testSyncBlocksInUse = withTestDb $ \conn -> do
    cid <- RC.insertCategory conn Domain "stale-domain"
    tid <-
        RT.insertTask
            conn
            RT.NewTask
                { RT.ntTitle = "T"
                , RT.ntBody = ""
                , RT.ntState = Ready
                , RT.ntPriority = Nothing
                }
    RC.attachTaskCategory conn tid cid
    let cfg = CategoriesConfig{catDomains = [], catDisciplines = []}
    rpt <- syncCategories conn cfg True
    null (srInserted rpt) @?= True
    null (srOrphans rpt) @?= True
    null (srPruned rpt) @?= True
    length (srBlocking rpt) @?= 1
    let (_, nodeIds) = head (srBlocking rpt)
    nodeIds @?= [tid]
    cats <- RC.listCategories conn Nothing
    length cats @?= 1

-- =============================================================
-- autoDeriveDeps tests
-- =============================================================

testAutoDeriveDepsEdgeInserted :: IO ()
testAutoDeriveDepsEdgeInserted = withTestDb $ \c -> do
    tid <-
        RT.insertTask
            c
            RT.NewTask
                { RT.ntTitle = "Dispatch task"
                , RT.ntBody = ""
                , RT.ntState = Ready
                , RT.ntPriority = Nothing
                }
    result <- autoDeriveDeps c [] (Just (T.unpack tid))
    result @?= [(TaskNode, tid)]

testAutoDeriveDepsExplicitWins :: IO ()
testAutoDeriveDepsExplicitWins = withTestDb $ \c -> do
    tid <-
        RT.insertTask
            c
            RT.NewTask
                { RT.ntTitle = "Dispatch task"
                , RT.ntBody = ""
                , RT.ntState = Ready
                , RT.ntPriority = Nothing
                }
    otherTid <-
        RT.insertTask
            c
            RT.NewTask
                { RT.ntTitle = "Other task"
                , RT.ntBody = ""
                , RT.ntState = Ready
                , RT.ntPriority = Nothing
                }
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

insertBlockedTask :: Connection -> Text -> IO Text
insertBlockedTask c reason = do
    tid <-
        RT.insertTask
            c
            RT.NewTask
                { RT.ntTitle = "T"
                , RT.ntBody = ""
                , RT.ntState = Ready
                , RT.ntPriority = Nothing
                }
    _ <-
        RT.updateTask
            c
            tid
            RT.emptyUpdate
                { RT.tuState = Just Blocked
                , RT.tuBlockReason = Just (Just reason)
                }
    pure tid

testUpdateClearsBlockReasonOnDone :: IO ()
testUpdateClearsBlockReasonOnDone = withTestDb $ \c -> do
    tid <- insertBlockedTask c "old reason"
    _ <- RT.updateTask c tid RT.emptyUpdate{RT.tuState = Just Done}
    Just t <- RT.getTask c tid
    taskBlockReason t @?= Nothing

testUpdateClearsBlockReasonOnReady :: IO ()
testUpdateClearsBlockReasonOnReady = withTestDb $ \c -> do
    tid <- insertBlockedTask c "old reason"
    _ <- RT.updateTask c tid RT.emptyUpdate{RT.tuState = Just Ready}
    Just t <- RT.getTask c tid
    taskBlockReason t @?= Nothing

testUpdateBlockedPreservesReason :: IO ()
testUpdateBlockedPreservesReason = withTestDb $ \c -> do
    tid <- insertBlockedTask c "still blocked"
    _ <- RT.updateTask c tid RT.emptyUpdate{RT.tuPriority = Just (Just 5)}
    Just t <- RT.getTask c tid
    taskBlockReason t @?= Just "still blocked"

-- =============================================================
-- category replace semantics tests
-- =============================================================

testTaskUpdateDomainReplaces :: IO ()
testTaskUpdateDomainReplaces = withTestDb $ \c -> do
    domA <- mkCat c Domain "domA"
    domB <- mkCat c Domain "domB"
    tid <-
        RT.insertTask
            c
            RT.NewTask
                { RT.ntTitle = "T"
                , RT.ntBody = ""
                , RT.ntState = Ready
                , RT.ntPriority = Nothing
                }
    RC.attachTaskCategory c tid (categoryId domA)
    RC.detachTaskCategoriesByAxis c tid Domain
    RC.attachTaskCategory c tid (categoryId domB)
    cats <- RC.taskCategoriesFor c tid
    map categoryName (filter (\x -> categoryAxis x == Domain) cats) @?= ["domB"]

testTaskUpdateDomainClears :: IO ()
testTaskUpdateDomainClears = withTestDb $ \c -> do
    dom <- mkCat c Domain "mydom"
    tid <-
        RT.insertTask
            c
            RT.NewTask
                { RT.ntTitle = "T"
                , RT.ntBody = ""
                , RT.ntState = Ready
                , RT.ntPriority = Nothing
                }
    RC.attachTaskCategory c tid (categoryId dom)
    RC.detachTaskCategoriesByAxis c tid Domain
    cats <- RC.taskCategoriesFor c tid
    null cats @?= True

testKnowUpdateDomainReplaces :: IO ()
testKnowUpdateDomainReplaces = withTestDb $ \c -> do
    domA <- mkCat c Domain "domA"
    domB <- mkCat c Domain "domB"
    kid <- mkKnowledge c "K" "body"
    RC.attachKnowledgeCategory c kid (categoryId domA)
    RC.detachKnowledgeCategoriesByAxis c kid Domain
    RC.attachKnowledgeCategory c kid (categoryId domB)
    cats <- RC.knowledgeCategoriesFor c kid
    let doms = filter (\x -> categoryAxis x == Domain) cats
    length doms @?= 1
    map categoryName doms @?= ["domB"]
