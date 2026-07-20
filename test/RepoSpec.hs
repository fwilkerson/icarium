module RepoSpec (tests) where

import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_, void)
import Data.Int (Int64)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Database.SQLite.Simple (Connection, Only (..), Query (..), close, execute, execute_, open, query_)
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Icarium.Commands.Category (SyncReport (..), syncCategories)
import Icarium.Commands.Ctx (autoDeriveDeps)
import Icarium.Config (
    CategoriesConfig (..),
    Config (..),
    DispatchConfig (..),
    ReviewConfig (..),
    defaultConfigText,
    loadConfig,
 )
import Icarium.Db (dbSchemaVersion, migrateDb)
import Icarium.Id (newId)
import Icarium.Migrations (Migration (..), migrations, mkSqlMigration)
import Icarium.Render (renderTaskPrompt)
import Icarium.Repo.Category qualified as RC
import Icarium.Repo.Context qualified as RK
import Icarium.Repo.Curation qualified as RCur
import Icarium.Repo.Dispatch qualified as RD
import Icarium.Repo.Edge qualified as RE
import Icarium.Repo.Search (ParsedQuery (..), Term (..), parseQuery)
import Icarium.Repo.Search qualified as RS
import Icarium.Repo.Task qualified as RT
import Icarium.Schema (execSql, schemaSql, schemaVersion)
import Icarium.Types

import TestHelpers

tests :: TestTree
tests =
    testGroup
        "repo"
        [ testGroup "EdgeKind round-trips" edgeKindTests
        , testCase "parseTaskState CLI form" $ parseTaskState "in-progress" @?= Just InProgress
        , testCase "parseTaskStateDb DB form" $ parseTaskStateDb "in_progress" @?= Just InProgress
        , testCase "parseTaskState accepts the hyphenated interactive form" $
            parseTaskState "ready-interactive" @?= Just ReadyInteractive
        , testCase "parseTaskStateDb accepts the stored interactive form" $
            parseTaskStateDb "ready_interactive" @?= Just ReadyInteractive
        , testCase "loadConfig succeeds on default template" loadConfigTest
        , testGroup
            "categoryMatchedContexts"
            [ testCase "both-axis match appears under Related context" testBothAxisMatch
            , testCase "retired context excluded from auto-pull" testRetiredExcluded
            , testCase "keep-curated context still auto-pulls" testKeepStillCurrent
            , testCase "zero categories yields empty result" testNoCats
            , testCase "explicit ref deduped from auto-pull" testDedup
            , testCase "cap at 5, ordered most-recent-first" testCap
            , testCase "one-axis match excluded when task has both axes" testOneAxisMismatch
            , testCase "workflow axis (kind) does not narrow the pull" testKindIgnored
            , testCase "kind alone matches nothing" testKindOnlyMatchesNothing
            ]
        , testGroup
            "curation (ADR 0001: derived retirement)"
            [ testCase "latest event wins: stale then keep revives the entry" testLatestEventWins
            , testCase "refactor-retired explicit ref still delivered" testRetiredRefDelivered
            , testCase "stale explicit ref never delivered" testStaleRefNeverDelivered
            , testCase "deleting an entry cascades to its curation events" testCurationCascade
            , testCase "migration 13 backfills stale rows, drops the column" testMigration13Backfill
            ]
        , testGroup
            "schema"
            [ testCase "applying embedded schema produces user_version = schemaVersion with expected tables" testInitialSchema
            , testCase "deleting a context entry cascades to context_categories rows" testContextCategoriesCascade
            ]
        , testGroup
            "migrations"
            [ testCase "base schema is already at schemaVersion; migrateDb is idempotent" testMigrateAdvances
            , testCase "bad SQL rolls back; user_version unchanged" testMigrateBadSqlRollback
            , testCase "user_version=0 with existing tables: stamps schemaVersion, no DDL re-run" testMigrateVersionZeroWithSchema
            , testCase "full migration chain runs from an empty DB to schemaVersion" testMigrateChainFromEmpty
            , testCase "migration 14 widens the axis CHECK, keeping rows and FKs" testMigration14KindAxis
            ]
        , testGroup
            "dispatch token columns"
            [ testCase "FromRow reads populated token columns" testDispatchTokensPopulated
            , testCase "FromRow reads NULL token columns as Nothing" testDispatchTokensNull
            ]
        , testGroup
            "dispatch stats"
            [ testCase "empty DB: all zeros" testDispatchStatsEmpty
            , testCase "counts by outcome, sums tokens, counts NULL-token rows" testDispatchStatsAggregates
            , testCase "--since filters to dispatches started at/after the timestamp" testDispatchStatsSince
            ]
        , testGroup
            "dispatch merge tracking"
            [ testCase "migration 9 adds merged_at column to a pre-migration dispatches table" testMigration9AddsMergedAt
            , testCase "setMerged round-trips merge_sha and a non-null merged_at" testSetMergedRoundTrips
            , testCase "listParkedDispatches includes success+unmerged, excludes after setMerged" testListParkedDispatches
            ]
        , testGroup
            "dependency gating on merged"
            [ testCase "parked dependency blocks dependent" testParkedDepBlocks
            , testCase "merged dependency unblocks dependent" testMergedDepUnblocks
            , testCase "manually-done dependency with no dispatches unblocks" testManualDoneDepUnblocks
            , testCase "migration 10 replaces the pre-existing ready_tasks view" testMigration10ReplacesView
            ]
        , testGroup
            "ready queue split (headless vs interactive)"
            [ testCase "ready_tasks spans both ready states; callers filter" testReadyTasksSpansBothStates
            , testCase "the deps gate applies to ready_interactive too" testInteractiveDepsGated
            , testCase "claimNextTask honours the state filter" testClaimNextTaskStateFiltered
            , testCase "claimReadyTask claims a named task in either ready state" testClaimReadyTaskNamed
            , testCase "claimReadyTask refuses a task that is not ready-ish" testClaimReadyTaskRefusesNonReady
            , testCase "migration 15 widens the state CHECK and the view" testMigration15ReadyInteractive
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
            "resolveContextId (PREFIX_RESOLUTION: ctx show/update/rm, ctx add --supersedes, task add --references)"
            [ testCase "right on unique prefix" testResolveContextPrefix
            , testCase "left on missing" testResolveContextMissing
            , testCase "left on ambiguous" testResolveContextAmbiguous
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
            "dependencyTasks"
            [ testCase "selects all Task columns (regression: missing no_commit)" testDependencyTasksReturnsAllColumns
            ]
        , testGroup
            "resolveNode (PREFIX_RESOLUTION: link add src/dst, link list --from/--to, ctx add --derived-from)"
            [ testCase "resolves task prefix" testResolveNodeTask
            , testCase "resolves context prefix" testResolveNodeContext
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
            "no_commit column"
            [ testCase "insertTask with ntNoCommit=True round-trips through getTask" testNoCommitInsert
            , testCase "tuNoCommit=Just True sets no_commit; Just False clears it" testNoCommitUpdate
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
            , testCase "ctx update --domain replaces not appends" testCtxUpdateDomainReplaces
            ]
        , testGroup
            "searchEntries"
            [ testCase "whitespace-only query returns (0, [])" testSearchWhitespaceOnly
            , testCase "title hit ranks before body hit" testSearchTitleBeforeBody
            , testCase "title hit from context outranks body hit from task" testSearchCrossKindRank
            , testCase "escapeLike: query containing % matches literally" testSearchEscapePercent
            , testCase "escapeLike: query containing _ matches literally" testSearchEscapeUnderscore
            , testCase "--kind task excludes context hits" testSearchKindTask
            , testCase "--kind ctx excludes task hits" testSearchKindCtx
            , testCase "limit caps result count" testSearchLimit
            , testCase "no match returns empty list" testSearchNoMatch
            , testCase "AND: multi-word matches tokens in any order" testSearchAndTokens
            , testCase "AND: entry missing one token excluded" testSearchAndExcludes
            , testCase "phrase: exact substring required" testSearchPhrase
            , testCase "OR: union of token matches" testSearchOrTokens
            , testCase "snake_case: space-separated tokens match underscore-joined form" testSearchSnakeCase
            , testCase "--domain filter includes only domain-tagged entries" testSearchDomainFilter
            , testCase "--discipline filter includes only discipline-tagged entries" testSearchDisciplineFilter
            , testCase "multiple --domain values are OR'd" testSearchMultiDomainOr
            , testCase "--exclude-domain removes tagged entries" testSearchExcludeDomain
            , testCase "--title-only scopes FTS to title column" testSearchTitleOnly
            , testCase "--body-only scopes FTS to body column" testSearchBodyOnly
            , testCase "hitBodyMatch set for body matches" testSearchHitBodyMatch
            ]
        , testGroup
            "parseQuery"
            [ testCase "single word → AndQuery [Word]" testParseQueryWord
            , testCase "two words → AndQuery [Word, Word]" testParseQueryAnd
            , testCase "quoted phrase → AndQuery [Phrase]" testParseQueryPhrase
            , testCase "explicit OR → OrQuery" testParseQueryOr
            , testCase "OR is case-sensitive (lowercase not treated as OR)" testParseQueryOrCase
            , testCase "whitespace-only → AndQuery []" testParseQueryWhitespace
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
            Right c -> do
                -- Shipped defaults are a contract (ADR 0005).
                dcModel (cfgDispatch c) @?= "claude-opus-4-8"
                dcEffort (cfgDispatch c) @?= Medium
                fmap rcEnabled (cfgReview c) @?= Just True
                fmap rcModel (cfgReview c) @?= Just (Just "claude-sonnet-5")
                fmap rcMaxAttempts (cfgReview c) @?= Just 2

-- =============================================================
-- categoryMatchedContexts tests
-- =============================================================

testBothAxisMatch :: IO ()
testBothAxisMatch = withTestDb $ \c -> do
    domCat <- mkCat c Domain "cli"
    discCat <- mkCat c Discipline "haskell"
    kid <- mkContext c "Match K" "match body"
    attachContextCats c kid [domCat, discCat]
    result <- RK.categoryMatchedContexts c [domCat, discCat] 5
    map contextId result @?= [kid]
    let prompt = renderTaskPrompt minTask [] result []
    assertBool "Related context header present" ("## Related context" `T.isInfixOf` prompt)
    assertBool "Hedge sentence present" ("use judgment" `T.isInfixOf` prompt)
    assertBool "Context title in prompt" ("Match K" `T.isInfixOf` prompt)

testRetiredExcluded :: IO ()
testRetiredExcluded = withTestDb $ \c -> do
    domCat <- mkCat c Domain "cli"
    kid <- mkContext c "Retired K" "retired body"
    attachContextCats c kid [domCat]
    void $ RCur.insertCuration c kid Guidance (Just "docs/foo.md") Nothing
    result <- RK.categoryMatchedContexts c [domCat] 5
    null result @?= True

testKeepStillCurrent :: IO ()
testKeepStillCurrent = withTestDb $ \c -> do
    domCat <- mkCat c Domain "cli"
    kid <- mkContext c "Kept K" "kept body"
    attachContextCats c kid [domCat]
    void $ RCur.insertCuration c kid Keep Nothing Nothing
    result <- RK.categoryMatchedContexts c [domCat] 5
    map contextId result @?= [kid]

testNoCats :: IO ()
testNoCats = withTestDb $ \c -> do
    result <- RK.categoryMatchedContexts c [] 5
    null result @?= True
    let prompt = renderTaskPrompt minTask [] [] []
    assertBool "no Related context header" (not ("## Related context" `T.isInfixOf` prompt))

testDedup :: IO ()
testDedup = withTestDb $ \c -> do
    domCat <- mkCat c Domain "cli"
    kid <- mkContext c "Shared K" "shared body"
    attachContextCats c kid [domCat]
    catResult <- RK.categoryMatchedContexts c [domCat] 5
    case catResult of
        [] -> fail "expected context to auto-pull"
        k : _ -> do
            let refs = [k]
                refIds = map contextId refs
                dedupedCat = filter (\x -> contextId x `notElem` refIds) catResult
            assertBool "dedupedCat is empty" (null dedupedCat)
            let prompt = renderTaskPrompt minTask refs dedupedCat []
            assertBool "refs section present" ("## Referenced context" `T.isInfixOf` prompt)
            assertBool "related section absent (dedup)" (not ("## Related context" `T.isInfixOf` prompt))

testCap :: IO ()
testCap = withTestDb $ \c -> do
    domCat <- mkCat c Domain "cli"
    kids <- forM [1 .. 6 :: Int] $ \i -> do
        kid <- newId
        execute
            c
            (Query "INSERT INTO context (id, title, body, created_at) VALUES (?,?,?,?)")
            ( kid
            , "K" <> T.pack (show i) :: Text
            , "" :: Text
            , "2026-01-0" <> T.pack (show i) <> "T00:00:00" :: Text
            )
        RC.attachContextCategory c kid (categoryId domCat)
        pure kid
    result <- RK.categoryMatchedContexts c [domCat] 5
    length result @?= 5
    map contextId result @?= reverse (tail kids)

{- | The load-bearing property of the workflow axis: adding a @kind@ to a
task must pull exactly the context it pulled without one. If @kind@ ever
becomes a retrieval axis, every clause ANDs — and no context carries a
kind, so auto-pull would silently return nothing.
-}
testKindIgnored :: IO ()
testKindIgnored = withTestDb $ \c -> do
    domCat <- mkCat c Domain "cli"
    discCat <- mkCat c Discipline "haskell"
    kindCat <- mkCat c Kind "bug"
    kid <- mkContext c "Retrieval-tagged K" "body"
    attachContextCats c kid [domCat, discCat]
    without <- RK.categoryMatchedContexts c [domCat, discCat] 5
    with <- RK.categoryMatchedContexts c [domCat, discCat, kindCat] 5
    map contextId with @?= map contextId without
    map contextId with @?= [kid]

-- | A kind on its own is not a retrieval axis, so it matches nothing.
testKindOnlyMatchesNothing :: IO ()
testKindOnlyMatchesNothing = withTestDb $ \c -> do
    kindCat <- mkCat c Kind "bug"
    kid <- mkContext c "Untagged K" "body"
    -- Even a context wrongly carrying the kind must not auto-pull on it.
    attachContextCats c kid [kindCat]
    result <- RK.categoryMatchedContexts c [kindCat] 5
    null result @?= True

testOneAxisMismatch :: IO ()
testOneAxisMismatch = withTestDb $ \c -> do
    domCat <- mkCat c Domain "cli"
    discCat <- mkCat c Discipline "haskell"
    kid <- mkContext c "Domain-only K" "body"
    attachContextCats c kid [domCat]
    result <- RK.categoryMatchedContexts c [domCat, discCat] 5
    null result @?= True

-- =============================================================
-- Curation tests (ADR 0001)
-- =============================================================

{- | Same-second events tie-break on id; pin distinct created_at values so
the test exercises the ordering contract, not ULID luck.
-}
insertCurationAt :: Connection -> Text -> Text -> Disposition -> Text -> IO ()
insertCurationAt c eid kid disp at =
    execute
        c
        (Query "INSERT INTO context_curation (id, context_id, disposition, created_at) VALUES (?,?,?,?)")
        (eid, kid, disp, at)

testLatestEventWins :: IO ()
testLatestEventWins = withTestDb $ \c -> do
    kid <- mkContext c "Revived K" "body"
    insertCurationAt c "01EV0000000000000000000001" kid Stale "2026-01-01 00:00:00"
    retired1 <- RCur.retiredContextIds c [kid]
    retired1 @?= [kid]
    insertCurationAt c "01EV0000000000000000000002" kid Keep "2026-01-02 00:00:00"
    retired2 <- RCur.retiredContextIds c [kid]
    retired2 @?= []
    mEvent <- RCur.latestCuration c kid
    fmap curationDisposition mEvent @?= Just Keep

mkTaskWithRef :: Connection -> Text -> IO (Text, Text)
mkTaskWithRef c title = do
    tid <-
        RT.insertTask
            c
            RT.NewTask
                { RT.ntTitle = title
                , RT.ntBody = ""
                , RT.ntState = Ready
                , RT.ntPriority = Nothing
                , RT.ntNoCommit = False
                }
    kid <- mkContext c (title <> " ref") (title <> " ref body")
    void $ RE.insertEdge c References TaskNode tid ContextNode kid
    pure (tid, kid)

testRetiredRefDelivered :: IO ()
testRetiredRefDelivered = withTestDb $ \c -> do
    (tid, kid) <- mkTaskWithRef c "Refactored"
    void $ RCur.insertCuration c kid Refactor (Just tid) Nothing
    refs <- RE.referencedContexts c tid
    map contextId refs @?= [kid]
    let prompt = renderTaskPrompt minTask refs [] []
    assertBool "retired ref body in prompt" ("Refactored ref body" `T.isInfixOf` prompt)

testStaleRefNeverDelivered :: IO ()
testStaleRefNeverDelivered = withTestDb $ \c -> do
    (tid, kid) <- mkTaskWithRef c "Staled"
    void $ RCur.insertCuration c kid Stale Nothing Nothing
    refs <- RE.referencedContexts c tid
    refs @?= []

testCurationCascade :: IO ()
testCurationCascade = withTestDb $ \c -> do
    kid <- mkContext c "Doomed K" "body"
    void $ RCur.insertCuration c kid Keep Nothing Nothing
    void $ RK.deleteContext c kid
    evs <- query_ c "SELECT id FROM context_curation" :: IO [Only Text]
    evs @?= []

{- | applySchema always stamps the latest schemaVersion, so a v12 DB can't
be built that way; hand-write the pre-migration-13 context table (with the
stale column and view) instead.
-}
testMigration13Backfill :: IO ()
testMigration13Backfill = do
    conn <- open ":memory:"
    execSql
        conn
        "CREATE TABLE context (\
        \id TEXT PRIMARY KEY, title TEXT NOT NULL, body TEXT NOT NULL DEFAULT '', \
        \stale INTEGER NOT NULL DEFAULT 0 CHECK (stale IN (0,1)), \
        \created_at TEXT NOT NULL DEFAULT (datetime('now')), \
        \updated_at TEXT NOT NULL DEFAULT (datetime('now'))); \
        \CREATE VIEW stale_context AS SELECT k.* FROM context k WHERE k.stale = 1;"
    execute_
        conn
        "INSERT INTO context (id, title, stale, updated_at) VALUES \
        \('01MIGSTALE0000000000000001', 'Old stale', 1, '2026-01-01 00:00:00'), \
        \('01MIGLIVE00000000000000002', 'Live', 0, '2026-01-01 00:00:00')"
    let m13 = case filter ((== 13) . migrationVersion) migrations of
            (m : _) -> m
            [] -> error "migration 13 not registered"
    migrationUp m13 conn
    cols <- query_ conn "SELECT name FROM pragma_table_info('context')" :: IO [Only Text]
    assertBool "stale column dropped" (Only ("stale" :: Text) `notElem` cols)
    evs <- query_ conn "SELECT id, context_id, disposition FROM context_curation" :: IO [(Text, Text, Text)]
    evs @?= [("backfill-01MIGSTALE0000000000000001", "01MIGSTALE0000000000000001", "stale")]
    retired <- query_ conn "SELECT context_id FROM retired_context" :: IO [Only Text]
    retired @?= [Only "01MIGSTALE0000000000000001"]
    close conn

-- =============================================================
-- Schema tests
-- =============================================================

testInitialSchema :: IO ()
testInitialSchema = withBaseTestDb $ \conn -> do
    v <- dbSchemaVersion conn
    v @?= fromIntegral schemaVersion
    tableRows <-
        query_
            conn
            "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name" ::
            IO [Only Text]
    let tables = map (\(Only n) -> n) tableRows
    assertBool "tasks table exists" ("tasks" `elem` tables)
    assertBool "context table exists" ("context" `elem` tables)
    assertBool "categories table exists" ("categories" `elem` tables)
    assertBool "edges table exists" ("edges" `elem` tables)
    assertBool "dispatches table exists" ("dispatches" `elem` tables)
    assertBool "task_categories table exists" ("task_categories" `elem` tables)
    assertBool "context_categories table exists" ("context_categories" `elem` tables)

testContextCategoriesCascade :: IO ()
testContextCategoriesCascade = withTestDb $ \conn -> do
    domCat <- mkCat conn Domain "cli"
    kid <- mkContext conn "K" "body"
    RC.attachContextCategory conn kid (categoryId domCat)
    pre <- query_ conn "SELECT context_id FROM context_categories" :: IO [Only Text]
    length pre @?= 1
    execute conn (Query "DELETE FROM context WHERE id = ?") (Only kid)
    post <- query_ conn "SELECT context_id FROM context_categories" :: IO [Only Text]
    length post @?= 0

-- =============================================================
-- Migration tests
-- =============================================================

testMigrateAdvances :: IO ()
testMigrateAdvances = withBaseTestDb $ \conn -> do
    v0 <- dbSchemaVersion conn
    v0 @?= fromIntegral schemaVersion
    migrateDb conn
    v1 <- dbSchemaVersion conn
    v1 @?= fromIntegral schemaVersion

testMigrateBadSqlRollback :: IO ()
testMigrateBadSqlRollback = withBaseTestDb $ \conn -> do
    v0 <- dbSchemaVersion conn
    let m = mkSqlMigration 99 "THIS IS NOT VALID SQL AT ALL"
    result <- try (migrationUp m conn) :: IO (Either SomeException ())
    assertBool "bad migration should throw" (either (const True) (const False) result)
    v1 <- dbSchemaVersion conn
    v1 @?= v0

-- Simulates a DB created by an external restore script: schema applied but
-- user_version left at 0 (SQLite default). migrateDb should stamp the version
-- without re-running CREATE TABLE (which would fail with "table already exists").
testMigrateVersionZeroWithSchema :: IO ()
testMigrateVersionZeroWithSchema = do
    conn <- open ":memory:"
    execSql conn schemaSql
    v0 <- dbSchemaVersion conn
    v0 @?= 0
    migrateDb conn
    v1 <- dbSchemaVersion conn
    v1 @?= fromIntegral schemaVersion
    close conn

-- Fresh file with no schema applied: migrateDb must run the whole chain from
-- migration 1, landing at schemaVersion with a working task surface.
testMigrateChainFromEmpty :: IO ()
testMigrateChainFromEmpty =
    withSystemTempFile "icarium-chain.db" $ \fp h -> do
        hClose h
        conn <- open fp
        migrateDb conn
        v <- dbSchemaVersion conn
        v @?= fromIntegral schemaVersion
        tid <-
            RT.insertTask
                conn
                RT.NewTask
                    { RT.ntTitle = "Chain task"
                    , RT.ntBody = ""
                    , RT.ntState = Ready
                    , RT.ntPriority = Nothing
                    , RT.ntNoCommit = False
                    }
        Just t <- RT.getTask conn tid
        taskId t @?= tid
        close conn

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
                , RT.ntNoCommit = False
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
                , RT.ntNoCommit = False
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
                , RT.ntNoCommit = False
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
                , RT.ntNoCommit = False
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
-- resolveContextId tests
-- =============================================================

testResolveContextPrefix :: IO ()
testResolveContextPrefix = withTestDb $ \c -> do
    kid <- mkContext c "K" "body"
    r <- RK.resolveContextId c (T.take 10 kid)
    r @?= Right kid

testResolveContextMissing :: IO ()
testResolveContextMissing = withTestDb $ \c -> do
    r <- RK.resolveContextId c "01ZZZZZZZZ"
    case r of
        Left msg -> assertBool "error mentions input" ("01ZZZZZZZZ" `T.isInfixOf` T.pack msg)
        Right _ -> fail "expected Left for missing context"

testResolveContextAmbiguous :: IO ()
testResolveContextAmbiguous = withTestDb $ \c -> do
    let kid1 = "01DDDD000000000000000000AA" :: Text
        kid2 = "01DDDD000000000000000000BB" :: Text
    forM_ [kid1, kid2] $ \kid ->
        execute
            c
            (Query "INSERT INTO context (id, title, body) VALUES (?,?,?)")
            (kid, "K" :: Text, "" :: Text)
    r <- RK.resolveContextId c "01DDDD0000"
    case r of
        Left msg -> assertBool "error mentions input" ("01DDDD0000" `T.isInfixOf` T.pack msg)
        Right _ -> fail "expected Left for ambiguous context"

-- =============================================================
-- resolveEdgeId tests
-- =============================================================

insertTestEdge :: Connection -> Text -> Text -> IO Text
insertTestEdge c src =
    RE.insertEdge c DependsOn TaskNode src TaskNode

testResolveEdgePrefix :: IO ()
testResolveEdgePrefix = withTestDb $ \c -> do
    t1 <- RT.insertTask c RT.NewTask{RT.ntTitle = "A", RT.ntBody = "", RT.ntState = Ready, RT.ntPriority = Nothing, RT.ntNoCommit = False}
    t2 <- RT.insertTask c RT.NewTask{RT.ntTitle = "B", RT.ntBody = "", RT.ntState = Ready, RT.ntPriority = Nothing, RT.ntNoCommit = False}
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
    t1 <- RT.insertTask c RT.NewTask{RT.ntTitle = "A", RT.ntBody = "", RT.ntState = Ready, RT.ntPriority = Nothing, RT.ntNoCommit = False}
    t2 <- RT.insertTask c RT.NewTask{RT.ntTitle = "B", RT.ntBody = "", RT.ntState = Ready, RT.ntPriority = Nothing, RT.ntNoCommit = False}
    t3 <- RT.insertTask c RT.NewTask{RT.ntTitle = "C", RT.ntBody = "", RT.ntState = Ready, RT.ntPriority = Nothing, RT.ntNoCommit = False}
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
    t1 <- RT.insertTask c RT.NewTask{RT.ntTitle = "A", RT.ntBody = "", RT.ntState = Ready, RT.ntPriority = Nothing, RT.ntNoCommit = False}
    t2 <- RT.insertTask c RT.NewTask{RT.ntTitle = "B", RT.ntBody = "", RT.ntState = Ready, RT.ntPriority = Nothing, RT.ntNoCommit = False}
    t3 <- RT.insertTask c RT.NewTask{RT.ntTitle = "C", RT.ntBody = "", RT.ntState = Ready, RT.ntPriority = Nothing, RT.ntNoCommit = False}
    kid <- RK.insertContext c RK.NewContext{RK.ncTitle = "K", RK.ncBody = ""}
    _ <- RE.insertEdge c DependsOn TaskNode t1 TaskNode t2
    _ <- RE.insertEdge c References TaskNode t1 ContextNode kid
    _ <- RE.insertEdge c DependsOn TaskNode t2 TaskNode t3
    es <- RE.listEdges c (Just t1) Nothing (Just DependsOn)
    length es @?= 1
    edgeSrcId (head es) @?= t1
    edgeKind (head es) @?= DependsOn

testDependencyTasksReturnsAllColumns :: IO ()
testDependencyTasksReturnsAllColumns = withTestDb $ \c -> do
    t1 <- RT.insertTask c RT.NewTask{RT.ntTitle = "A", RT.ntBody = "", RT.ntState = Ready, RT.ntPriority = Nothing, RT.ntNoCommit = False}
    t2 <- RT.insertTask c RT.NewTask{RT.ntTitle = "B", RT.ntBody = "", RT.ntState = Ready, RT.ntPriority = Nothing, RT.ntNoCommit = True}
    _ <- RE.insertEdge c DependsOn TaskNode t1 TaskNode t2
    deps <- RE.dependencyTasks c t1
    map taskNoCommit deps @?= [True]

-- =============================================================
-- resolveNode tests
-- =============================================================

testResolveNodeTask :: IO ()
testResolveNodeTask = withTestDb $ \c -> do
    tid <- RT.insertTask c RT.NewTask{RT.ntTitle = "T", RT.ntBody = "", RT.ntState = Ready, RT.ntPriority = Nothing, RT.ntNoCommit = False}
    ts <- RT.getTasksByPrefix c (T.take 10 tid)
    ks <- RK.getContextsByPrefix c (T.take 10 tid)
    (length ts, length ks) @?= (1, 0)
    taskId (head ts) @?= tid

testResolveNodeContext :: IO ()
testResolveNodeContext = withTestDb $ \c -> do
    kid <- mkContext c "K" "body"
    ts <- RT.getTasksByPrefix c (T.take 10 kid)
    ks <- RK.getContextsByPrefix c (T.take 10 kid)
    (length ts, length ks) @?= (0, 1)
    contextId (head ks) @?= kid

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
        (Query "INSERT INTO context (id, title, body) VALUES (?,?,?)")
        (kid, "K" :: Text, "" :: Text)
    ts <- RT.getTasksByPrefix c sharedPrefix
    ks <- RK.getContextsByPrefix c sharedPrefix
    assertBool "both task and context match prefix" (length ts == 1 && length ks == 1)

-- =============================================================
-- category sync tests
-- =============================================================

testSyncInserts :: IO ()
testSyncInserts = withTestDb $ \conn -> do
    let cfg = CategoriesConfig{catDomains = ["cli"], catDisciplines = ["haskell"], catKinds = []}
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
    let cfg = CategoriesConfig{catDomains = [], catDisciplines = [], catKinds = []}
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
    let cfg = CategoriesConfig{catDomains = [], catDisciplines = [], catKinds = []}
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
                , RT.ntNoCommit = False
                }
    RC.attachTaskCategory conn tid cid
    let cfg = CategoriesConfig{catDomains = [], catDisciplines = [], catKinds = []}
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
                , RT.ntNoCommit = False
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
                , RT.ntNoCommit = False
                }
    otherTid <-
        RT.insertTask
            c
            RT.NewTask
                { RT.ntTitle = "Other task"
                , RT.ntBody = ""
                , RT.ntState = Ready
                , RT.ntPriority = Nothing
                , RT.ntNoCommit = False
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
-- no_commit column tests
-- =============================================================

testNoCommitInsert :: IO ()
testNoCommitInsert = withTestDb $ \c -> do
    tid <-
        RT.insertTask
            c
            RT.NewTask
                { RT.ntTitle = "Side-effect task"
                , RT.ntBody = ""
                , RT.ntState = Ready
                , RT.ntPriority = Nothing
                , RT.ntNoCommit = True
                }
    Just t <- RT.getTask c tid
    taskNoCommit t @?= True

testNoCommitUpdate :: IO ()
testNoCommitUpdate = withTestDb $ \c -> do
    tid <-
        RT.insertTask
            c
            RT.NewTask
                { RT.ntTitle = "Commit task"
                , RT.ntBody = ""
                , RT.ntState = Ready
                , RT.ntPriority = Nothing
                , RT.ntNoCommit = False
                }
    _ <- RT.updateTask c tid RT.emptyUpdate{RT.tuNoCommit = Just True}
    Just t1 <- RT.getTask c tid
    taskNoCommit t1 @?= True
    _ <- RT.updateTask c tid RT.emptyUpdate{RT.tuNoCommit = Just False}
    Just t2 <- RT.getTask c tid
    taskNoCommit t2 @?= False

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
                , RT.ntNoCommit = False
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
                , RT.ntNoCommit = False
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
                , RT.ntNoCommit = False
                }
    RC.attachTaskCategory c tid (categoryId dom)
    RC.detachTaskCategoriesByAxis c tid Domain
    cats <- RC.taskCategoriesFor c tid
    null cats @?= True

testCtxUpdateDomainReplaces :: IO ()
testCtxUpdateDomainReplaces = withTestDb $ \c -> do
    domA <- mkCat c Domain "domA"
    domB <- mkCat c Domain "domB"
    kid <- mkContext c "K" "body"
    RC.attachContextCategory c kid (categoryId domA)
    RC.detachContextCategoriesByAxis c kid Domain
    RC.attachContextCategory c kid (categoryId domB)
    cats <- RC.contextCategoriesFor c kid
    let doms = filter (\x -> categoryAxis x == Domain) cats
    length doms @?= 1
    map categoryName doms @?= ["domB"]

-- =============================================================
-- searchEntries tests
-- =============================================================

testSearchWhitespaceOnly :: IO ()
testSearchWhitespaceOnly = withTestDb $ \c -> do
    _ <- mkContext c "some title" "some body"
    (total, results) <- RS.searchEntries c "   " RS.noFilters 10
    total @?= 0
    null results @?= True

testSearchTitleBeforeBody :: IO ()
testSearchTitleBeforeBody = withTestDb $ \c -> do
    tid <- RT.insertTask c RT.NewTask{RT.ntTitle = "fts needle title", RT.ntBody = "", RT.ntState = Ready, RT.ntPriority = Nothing, RT.ntNoCommit = False}
    kid <- mkContext c "unrelated title" "body contains needle here"
    (_, results) <- RS.searchEntries c "needle" RS.noFilters 10
    assertBool "two results returned" (length results == 2)
    RS.hitId (head results) @?= tid
    RS.hitTitleMatch (head results) @?= True
    RS.hitId (results !! 1) @?= kid
    RS.hitTitleMatch (results !! 1) @?= False

testSearchCrossKindRank :: IO ()
testSearchCrossKindRank = withTestDb $ \c -> do
    _ <- RT.insertTask c RT.NewTask{RT.ntTitle = "no match title", RT.ntBody = "body has xyzzy", RT.ntState = Ready, RT.ntPriority = Nothing, RT.ntNoCommit = False}
    kid <- mkContext c "title has xyzzy" "body"
    (_, results) <- RS.searchEntries c "xyzzy" RS.noFilters 10
    assertBool "context title hit before task body hit" (RS.hitId (head results) == kid)
    RS.hitTitleMatch (head results) @?= True

testSearchEscapePercent :: IO ()
testSearchEscapePercent = withTestDb $ \c -> do
    kid <- mkContext c "100% correct" "body"
    _ <- mkContext c "unrelated" "body"
    (_, results) <- RS.searchEntries c "100%" RS.noFilters 10
    length results @?= 1
    RS.hitId (head results) @?= kid

testSearchEscapeUnderscore :: IO ()
testSearchEscapeUnderscore = withTestDb $ \c -> do
    kid <- mkContext c "snake_case naming" "body"
    _ <- mkContext c "unrelated" "body"
    (_, results) <- RS.searchEntries c "snake_case" RS.noFilters 10
    length results @?= 1
    RS.hitId (head results) @?= kid

testSearchKindTask :: IO ()
testSearchKindTask = withTestDb $ \c -> do
    tid <- RT.insertTask c RT.NewTask{RT.ntTitle = "needle task", RT.ntBody = "", RT.ntState = Ready, RT.ntPriority = Nothing, RT.ntNoCommit = False}
    _ <- mkContext c "needle context" "body"
    (_, results) <- RS.searchEntries c "needle" RS.noFilters{RS.sfKind = Just TaskNode} 10
    length results @?= 1
    RS.hitId (head results) @?= tid

testSearchKindCtx :: IO ()
testSearchKindCtx = withTestDb $ \c -> do
    _ <- RT.insertTask c RT.NewTask{RT.ntTitle = "needle task", RT.ntBody = "", RT.ntState = Ready, RT.ntPriority = Nothing, RT.ntNoCommit = False}
    kid <- mkContext c "needle context" "body"
    (_, results) <- RS.searchEntries c "needle" RS.noFilters{RS.sfKind = Just ContextNode} 10
    length results @?= 1
    RS.hitId (head results) @?= kid

testSearchLimit :: IO ()
testSearchLimit = withTestDb $ \c -> do
    forM_ [(1 :: Int) .. 5] $ \i ->
        void $ mkContext c ("needle entry " <> T.pack (show i)) "body"
    (_, results) <- RS.searchEntries c "needle" RS.noFilters 3
    length results @?= 3

testSearchNoMatch :: IO ()
testSearchNoMatch = withTestDb $ \c -> do
    _ <- mkContext c "some title" "some body"
    (_, results) <- RS.searchEntries c "xyzzy_no_match" RS.noFilters 10
    null results @?= True

testSearchAndTokens :: IO ()
testSearchAndTokens = withTestDb $ \c -> do
    kid <- mkContext c "credentials owned by client" "body"
    _ <- mkContext c "client only" "body"
    (_, results) <- RS.searchEntries c "client credentials" RS.noFilters 10
    length results @?= 1
    RS.hitId (head results) @?= kid

testSearchAndExcludes :: IO ()
testSearchAndExcludes = withTestDb $ \c -> do
    _ <- mkContext c "only alpha here" "body"
    (_, results) <- RS.searchEntries c "alpha beta" RS.noFilters 10
    null results @?= True

testSearchPhrase :: IO ()
testSearchPhrase = withTestDb $ \c -> do
    kid <- mkContext c "client credentials flow" "body"
    _ <- mkContext c "credentials for client" "body"
    (_, results) <- RS.searchEntries c "\"client credentials\"" RS.noFilters 10
    length results @?= 1
    RS.hitId (head results) @?= kid

testSearchOrTokens :: IO ()
testSearchOrTokens = withTestDb $ \c -> do
    _ <- mkContext c "foo topic" "body"
    _ <- mkContext c "bar topic" "body"
    _ <- mkContext c "unrelated" "body"
    (_, results) <- RS.searchEntries c "foo OR bar" RS.noFilters 10
    length results @?= 2

testSearchSnakeCase :: IO ()
testSearchSnakeCase = withTestDb $ \c -> do
    kid <- mkContext c "client_credentials" "body"
    _ <- mkContext c "unrelated" "body"
    (_, results) <- RS.searchEntries c "client credentials" RS.noFilters 10
    length results @?= 1
    RS.hitId (head results) @?= kid

testSearchDomainFilter :: IO ()
testSearchDomainFilter = withTestDb $ \c -> do
    domCat <- mkCat c Domain "mydom"
    kid <- mkContext c "needle in domain entry" "body"
    attachContextCats c kid [domCat]
    _ <- mkContext c "needle no domain" "body"
    (_, results) <- RS.searchEntries c "needle" RS.noFilters{RS.sfDomains = ["mydom"]} 10
    length results @?= 1
    RS.hitId (head results) @?= kid

testSearchDisciplineFilter :: IO ()
testSearchDisciplineFilter = withTestDb $ \c -> do
    discCat <- mkCat c Discipline "mydisc"
    kid <- mkContext c "needle in discipline entry" "body"
    attachContextCats c kid [discCat]
    _ <- mkContext c "needle no discipline" "body"
    (_, results) <- RS.searchEntries c "needle" RS.noFilters{RS.sfDisciplines = ["mydisc"]} 10
    length results @?= 1
    RS.hitId (head results) @?= kid

testSearchMultiDomainOr :: IO ()
testSearchMultiDomainOr = withTestDb $ \c -> do
    domA <- mkCat c Domain "domA"
    domB <- mkCat c Domain "domB"
    kidA <- mkContext c "needle entry A" "body"
    attachContextCats c kidA [domA]
    kidB <- mkContext c "needle entry B" "body"
    attachContextCats c kidB [domB]
    _ <- mkContext c "needle entry C untagged" "body"
    (_, results) <- RS.searchEntries c "needle" RS.noFilters{RS.sfDomains = ["domA", "domB"]} 10
    length results @?= 2
    assertBool "domA entry present" (any (\h -> RS.hitId h == kidA) results)
    assertBool "domB entry present" (any (\h -> RS.hitId h == kidB) results)

testSearchExcludeDomain :: IO ()
testSearchExcludeDomain = withTestDb $ \c -> do
    domCat <- mkCat c Domain "noisydom"
    kidExcluded <- mkContext c "needle noise entry" "body"
    attachContextCats c kidExcluded [domCat]
    kidKept <- mkContext c "needle good entry" "body"
    (_, results) <-
        RS.searchEntries c "needle" RS.noFilters{RS.sfExcludeDomains = ["noisydom"]} 10
    length results @?= 1
    RS.hitId (head results) @?= kidKept

testSearchTitleOnly :: IO ()
testSearchTitleOnly = withTestDb $ \c -> do
    kidTitle <- mkContext c "scopetoken in title" "body content only"
    _ <- mkContext c "unrelated title" "scopetoken in body only"
    (_, results) <- RS.searchEntries c "scopetoken" RS.noFilters{RS.sfScope = RS.ScopeTitle} 10
    length results @?= 1
    RS.hitId (head results) @?= kidTitle

testSearchBodyOnly :: IO ()
testSearchBodyOnly = withTestDb $ \c -> do
    _ <- mkContext c "bodytoken in title" "body content only"
    kidBody <- mkContext c "unrelated title" "bodytoken in body only"
    (_, results) <- RS.searchEntries c "bodytoken" RS.noFilters{RS.sfScope = RS.ScopeBody} 10
    length results @?= 1
    RS.hitId (head results) @?= kidBody

testSearchHitBodyMatch :: IO ()
testSearchHitBodyMatch = withTestDb $ \c -> do
    kidBodyOnly <- mkContext c "unrelated title" "bmatch_token lives here"
    kidBoth <- mkContext c "bmatch_token in title too" "bmatch_token in body"
    (_, results) <- RS.searchEntries c "bmatch_token" RS.noFilters 10
    let findHit i = head [h | h <- results, RS.hitId h == i]
        hBody = findHit kidBodyOnly
        hBoth = findHit kidBoth
    RS.hitTitleMatch hBody @?= False
    RS.hitBodyMatch hBody @?= True
    RS.hitTitleMatch hBoth @?= True
    RS.hitBodyMatch hBoth @?= True

-- =============================================================
-- parseQuery tests
-- =============================================================

testParseQueryWord :: IO ()
testParseQueryWord =
    parseQuery "needle" @?= AndQuery [Word "needle"]

testParseQueryAnd :: IO ()
testParseQueryAnd =
    parseQuery "client credentials" @?= AndQuery [Word "client", Word "credentials"]

testParseQueryPhrase :: IO ()
testParseQueryPhrase =
    parseQuery "\"client credentials\"" @?= AndQuery [Phrase "client credentials"]

testParseQueryOr :: IO ()
testParseQueryOr =
    parseQuery "foo OR bar" @?= OrQuery [Word "foo", Word "bar"]

testParseQueryOrCase :: IO ()
testParseQueryOrCase =
    parseQuery "foo or bar" @?= AndQuery [Word "foo", Word "or", Word "bar"]

testParseQueryWhitespace :: IO ()
testParseQueryWhitespace =
    parseQuery "   " @?= AndQuery []

-- =============================================================
-- dispatch token column tests
-- =============================================================

insertDispatchWithTokens :: Connection -> Text -> Text -> Maybe Int -> Maybe Int -> Maybe Int -> IO ()
insertDispatchWithTokens c did tid mIn mOut mCache =
    execute
        c
        ( Query
            "INSERT INTO dispatches \
            \(id, task_id, branch, base_branch, base_sha, model, effort, \
            \ tokens_in, tokens_out, tokens_cache_read) \
            \VALUES (?,?,?,?,?,?,?,?,?,?)"
        )
        ( did
        , tid
        , "dispatch/" <> did
        , "main" :: Text
        , "0000000000000000000000000000000000000000" :: Text
        , "claude-sonnet-4-6" :: Text
        , "medium" :: Text
        , mIn
        , mOut
        , mCache
        )

testDispatchTokensPopulated :: IO ()
testDispatchTokensPopulated = withTestDb $ \c -> do
    tid <-
        RT.insertTask
            c
            RT.NewTask
                { RT.ntTitle = "Token task"
                , RT.ntBody = ""
                , RT.ntState = Ready
                , RT.ntPriority = Nothing
                , RT.ntNoCommit = False
                }
    let did = "01DISP0000000000000000001D" :: Text
    insertDispatchWithTokens c did tid (Just 1234) (Just 567) (Just 89)
    Just d <- RD.getDispatch c did
    dispatchTokensIn d @?= Just 1234
    dispatchTokensOut d @?= Just 567
    dispatchTokensCacheRead d @?= Just 89

testDispatchTokensNull :: IO ()
testDispatchTokensNull = withTestDb $ \c -> do
    tid <-
        RT.insertTask
            c
            RT.NewTask
                { RT.ntTitle = "No token task"
                , RT.ntBody = ""
                , RT.ntState = Ready
                , RT.ntPriority = Nothing
                , RT.ntNoCommit = False
                }
    let did = "01DISP0000000000000000002D" :: Text
    insertDispatchWithTokens c did tid Nothing Nothing Nothing
    Just d <- RD.getDispatch c did
    dispatchTokensIn d @?= Nothing
    dispatchTokensOut d @?= Nothing
    dispatchTokensCacheRead d @?= Nothing

-- =============================================================
-- dispatch stats tests
-- =============================================================

insertDispatchFull ::
    Connection -> Text -> Text -> Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Int -> IO ()
insertDispatchFull c did tid startedAt mOutcome mIn mOut mCache = do
    execute
        c
        ( Query
            "INSERT INTO dispatches \
            \(id, task_id, branch, base_branch, base_sha, model, effort, started_at) \
            \VALUES (?,?,?,?,?,?,?,?)"
        )
        ( did
        , tid
        , "dispatch/" <> did
        , "main" :: Text
        , "0000000000000000000000000000000000000000" :: Text
        , "claude-sonnet-4-6" :: Text
        , "medium" :: Text
        , startedAt
        )
    execute
        c
        ( Query
            "UPDATE dispatches \
            \SET outcome = ?, tokens_in = ?, tokens_out = ?, tokens_cache_read = ? \
            \WHERE id = ?"
        )
        (mOutcome, mIn, mOut, mCache, did)

testDispatchStatsEmpty :: IO ()
testDispatchStatsEmpty = withTestDb $ \c -> do
    s <- RD.getDispatchStats c Nothing
    s
        @?= RD.DispatchStats
            { RD.dsTotal = 0
            , RD.dsSuccess = 0
            , RD.dsFailure = 0
            , RD.dsInterrupted = 0
            , RD.dsOpen = 0
            , RD.dsTokensIn = 0
            , RD.dsTokensOut = 0
            , RD.dsTokensCacheRead = 0
            , RD.dsMissingTokens = 0
            }

testDispatchStatsAggregates :: IO ()
testDispatchStatsAggregates = withTestDb $ \c -> do
    tid <-
        RT.insertTask
            c
            RT.NewTask
                { RT.ntTitle = "Stats task"
                , RT.ntBody = ""
                , RT.ntState = Ready
                , RT.ntPriority = Nothing
                , RT.ntNoCommit = False
                }
    insertDispatchFull c "01STATS000000000000000001S" tid "2026-01-01 00:00:00" (Just "success") (Just 100) (Just 20) (Just 5)
    insertDispatchFull c "01STATS000000000000000002S" tid "2026-01-01 00:00:00" (Just "failure") (Just 50) (Just 10) (Just 1)
    insertDispatchFull c "01STATS000000000000000003S" tid "2026-01-01 00:00:00" (Just "interrupted") Nothing Nothing Nothing
    insertDispatchFull c "01STATS000000000000000004S" tid "2026-01-01 00:00:00" Nothing Nothing Nothing Nothing
    s <- RD.getDispatchStats c Nothing
    RD.dsTotal s @?= 4
    RD.dsSuccess s @?= 1
    RD.dsFailure s @?= 1
    RD.dsInterrupted s @?= 1
    RD.dsOpen s @?= 1
    RD.dsTokensIn s @?= 150
    RD.dsTokensOut s @?= 30
    RD.dsTokensCacheRead s @?= 6
    RD.dsMissingTokens s @?= 2

testDispatchStatsSince :: IO ()
testDispatchStatsSince = withTestDb $ \c -> do
    tid <-
        RT.insertTask
            c
            RT.NewTask
                { RT.ntTitle = "Stats since task"
                , RT.ntBody = ""
                , RT.ntState = Ready
                , RT.ntPriority = Nothing
                , RT.ntNoCommit = False
                }
    insertDispatchFull c "01SINCE000000000000000001S" tid "2026-01-01 00:00:00" (Just "success") (Just 10) (Just 1) (Just 0)
    insertDispatchFull c "01SINCE000000000000000002S" tid "2026-01-03 00:00:00" (Just "success") (Just 20) (Just 2) (Just 0)
    s <- RD.getDispatchStats c (Just "2026-01-02 00:00:00")
    RD.dsTotal s @?= 1
    RD.dsTokensIn s @?= 20

-- =============================================================
-- dispatch merge tracking tests
-- =============================================================

{- | applySchema always stamps the latest schemaVersion (Schema.hs), so a
"v8" DB can't be built that way; hand-write the pre-migration-9 table
(no merged_at) instead.
-}
testMigration9AddsMergedAt :: IO ()
testMigration9AddsMergedAt = do
    conn <- open ":memory:"
    execSql
        conn
        "CREATE TABLE dispatches (\
        \id TEXT PRIMARY KEY, task_id TEXT NOT NULL, branch TEXT NOT NULL, \
        \base_branch TEXT NOT NULL, base_sha TEXT NOT NULL, pid INTEGER, \
        \model TEXT NOT NULL, effort TEXT NOT NULL, \
        \started_at TEXT NOT NULL DEFAULT (datetime('now')), \
        \heartbeat_at TEXT NOT NULL DEFAULT (datetime('now')), \
        \ended_at TEXT, outcome TEXT, merge_sha TEXT, last_commit TEXT, \
        \notes TEXT, log_path TEXT, tokens_in INTEGER, tokens_out INTEGER, \
        \tokens_cache_read INTEGER, review_verdict TEXT, reviewer_log_path TEXT)"
    let m9 = case filter ((== 9) . migrationVersion) migrations of
            (m : _) -> m
            [] -> error "migration 9 not registered"
    migrationUp m9 conn
    cols <- query_ conn "SELECT name FROM pragma_table_info('dispatches')" :: IO [Only Text]
    assertBool "merged_at column present after migration 9" (Only ("merged_at" :: Text) `elem` cols)
    close conn

testSetMergedRoundTrips :: IO ()
testSetMergedRoundTrips = withTestDb $ \c -> do
    tid <-
        RT.insertTask
            c
            RT.NewTask
                { RT.ntTitle = "Merge tracking task"
                , RT.ntBody = ""
                , RT.ntState = Ready
                , RT.ntPriority = Nothing
                , RT.ntNoCommit = False
                }
    let did = "01MERGE000000000000000001M" :: Text
    insertTestDispatch c did tid
    RD.finishDispatch c did OSuccess Nothing Nothing
    Just before <- RD.getDispatch c did
    dispatchMergeSha before @?= Nothing
    dispatchMergedAt before @?= Nothing

    RD.setMerged c did "deadbeef"
    Just after <- RD.getDispatch c did
    dispatchMergeSha after @?= Just "deadbeef"
    assertBool "merged_at set" (isJust (dispatchMergedAt after))

{- | Insert a done dependency and a ready dependent linked by depends_on;
returns (depId, dependentId). Callers vary the dependency's dispatch state.
-}
mkDepPair :: Connection -> IO (Text, Text)
mkDepPair c = do
    dep <-
        RT.insertTask
            c
            RT.NewTask
                { RT.ntTitle = "Dependency task"
                , RT.ntBody = ""
                , RT.ntState = Ready
                , RT.ntPriority = Nothing
                , RT.ntNoCommit = False
                }
    dependent <-
        RT.insertTask
            c
            RT.NewTask
                { RT.ntTitle = "Dependent task"
                , RT.ntBody = ""
                , RT.ntState = Ready
                , RT.ntPriority = Nothing
                , RT.ntNoCommit = False
                }
    _ <- RE.insertEdge c DependsOn TaskNode dependent TaskNode dep
    void $ RT.updateTask c dep RT.emptyUpdate{RT.tuState = Just Done}
    pure (dep, dependent)

readyIds :: Connection -> IO [Text]
readyIds c = map taskId <$> RT.listTasks c [] True []

testParkedDepBlocks :: IO ()
testParkedDepBlocks = withTestDb $ \c -> do
    (dep, dependent) <- mkDepPair c
    let did = "01DEPGATE00000000000000001" :: Text
    insertTestDispatch c did dep
    RD.finishDispatch c did OSuccess Nothing Nothing
    ready <- readyIds c
    assertBool "dependent held while dependency is parked" (dependent `notElem` ready)

testMergedDepUnblocks :: IO ()
testMergedDepUnblocks = withTestDb $ \c -> do
    (dep, dependent) <- mkDepPair c
    let did = "01DEPGATE00000000000000002" :: Text
    insertTestDispatch c did dep
    RD.finishDispatch c did OSuccess Nothing Nothing
    RD.setMerged c did "deadbeef"
    ready <- readyIds c
    assertBool "dependent eligible once dependency merged" (dependent `elem` ready)

testManualDoneDepUnblocks :: IO ()
testManualDoneDepUnblocks = withTestDb $ \c -> do
    (_, dependent) <- mkDepPair c
    ready <- readyIds c
    assertBool "dependent eligible behind manually-done dependency" (dependent `elem` ready)

{- | An upgrading DB carries the old view; migration 10 must drop and
recreate it. The stand-in view's shape is irrelevant — only its existence
matters for the DROP.
-}
testMigration10ReplacesView :: IO ()
testMigration10ReplacesView = withBaseTestDb $ \conn -> do
    execute_ conn "DROP VIEW ready_tasks"
    execute_ conn "CREATE VIEW ready_tasks AS SELECT * FROM tasks WHERE 0"
    let m10 = case filter ((== 10) . migrationVersion) migrations of
            (m : _) -> m
            [] -> error "migration 10 not registered"
    migrationUp m10 conn
    sql <- query_ conn "SELECT sql FROM sqlite_master WHERE type='view' AND name='ready_tasks'" :: IO [Only Text]
    assertBool
        "recreated view gates on unmerged dispatches"
        (any (\(Only s) -> "merge_sha IS NULL" `T.isInfixOf` s) sql)

-- =============================================================
-- ready queue split (headless vs interactive)
-- =============================================================

mkTask :: Connection -> Text -> TaskState -> IO Text
mkTask c title st =
    RT.insertTask
        c
        RT.NewTask
            { RT.ntTitle = title
            , RT.ntBody = ""
            , RT.ntState = st
            , RT.ntPriority = Nothing
            , RT.ntNoCommit = False
            }

{- | The view is the one place the deps-satisfaction gate lives; it must
admit both ready states so callers can pick their queue by state filter.
-}
testReadyTasksSpansBothStates :: IO ()
testReadyTasksSpansBothStates = withTestDb $ \c -> do
    headless <- mkTask c "Headless work" Ready
    interactive <- mkTask c "Interactive work" ReadyInteractive
    _ <- mkTask c "Not ready" Planned

    both <- map taskId <$> RT.listTasks c [] True []
    assertBool "view carries the headless task" (headless `elem` both)
    assertBool "view carries the interactive task" (interactive `elem` both)
    length both @?= 2

    onlyHeadless <- map taskId <$> RT.listTasks c [Ready] True []
    onlyHeadless @?= [headless]
    onlyInteractive <- map taskId <$> RT.listTasks c [ReadyInteractive] True []
    onlyInteractive @?= [interactive]

-- | Generalizing the view must not lose the gate for the new state.
testInteractiveDepsGated :: IO ()
testInteractiveDepsGated = withTestDb $ \c -> do
    dep <- mkTask c "Unfinished dependency" Ready
    dependent <- mkTask c "Interactive dependent" ReadyInteractive
    _ <- RE.insertEdge c DependsOn TaskNode dependent TaskNode dep

    ready <- map taskId <$> RT.listTasks c [ReadyInteractive] True []
    assertBool "interactive dependent held behind an open dependency" (dependent `notElem` ready)

    void $ RT.updateTask c dep RT.emptyUpdate{RT.tuState = Just Done}
    ready' <- map taskId <$> RT.listTasks c [ReadyInteractive] True []
    ready' @?= [dependent]

{- | Dispatch and the interactive CLI share one claim path and differ only
in which states they will take.
-}
testClaimNextTaskStateFiltered :: IO ()
testClaimNextTaskStateFiltered = withTestDb $ \c -> do
    interactive <- mkTask c "Interactive work" ReadyInteractive

    headlessClaim <- RT.claimNextTask c [Ready] "dispatch"
    assertBool "headless queue does not see interactive work" (isNothing headlessClaim)

    claimed <- RT.claimNextTask c [ReadyInteractive] "human"
    fmap taskId claimed @?= Just interactive
    fmap taskState claimed @?= Just InProgress

testClaimReadyTaskNamed :: IO ()
testClaimReadyTaskNamed = withTestDb $ \c -> do
    headless <- mkTask c "Headless work" Ready
    interactive <- mkTask c "Interactive work" ReadyInteractive

    h <- RT.claimReadyTask c headless "human"
    fmap taskState h @?= Just InProgress
    fmap taskClaimedBy h @?= Just (Just "human")

    i <- RT.claimReadyTask c interactive "human"
    fmap taskState i @?= Just InProgress

-- | Naming a task selects it; it does not license claiming unready work.
testClaimReadyTaskRefusesNonReady :: IO ()
testClaimReadyTaskRefusesNonReady = withTestDb $ \c -> do
    planned <- mkTask c "Under-specified" Planned
    r <- RT.claimReadyTask c planned "human"
    assertBool "planned task refused" (isNothing r)
    still <- RT.getTask c planned
    fmap taskState still @?= Just Planned

{- | Migration 15 rebuilds `tasks` to widen its state CHECK. Existing rows
and the child FKs must survive the rebuild, and the recreated view must
span both ready states.
-}
testMigration15ReadyInteractive :: IO ()
testMigration15ReadyInteractive = withBaseTestDb $ \conn -> do
    -- Upgrading connections do not enable FK enforcement (spec/schema.sql);
    -- applySchema turned it on for this one, so match production before the
    -- rebuild drops `tasks`.
    execute_ conn "PRAGMA foreign_keys = OFF"
    -- Restore the v14 shape: state CHECK without 'ready_interactive'.
    execSql
        conn
        "DROP VIEW ready_tasks;\n\
        \DROP TABLE tasks;\n\
        \CREATE TABLE tasks (\n\
        \  id TEXT PRIMARY KEY,\n\
        \  title TEXT NOT NULL,\n\
        \  body TEXT NOT NULL DEFAULT '',\n\
        \  state TEXT NOT NULL DEFAULT 'planned'\n\
        \    CHECK (state IN ('idea','planned','ready','in_progress','done',\n\
        \                     'blocked','abandoned')),\n\
        \  priority INTEGER,\n\
        \  block_reason TEXT,\n\
        \  no_commit INTEGER NOT NULL DEFAULT 0 CHECK (no_commit IN (0,1)),\n\
        \  claimed_by TEXT,\n\
        \  claimed_at TEXT,\n\
        \  created_at TEXT NOT NULL DEFAULT (datetime('now')),\n\
        \  updated_at TEXT NOT NULL DEFAULT (datetime('now')));\n\
        \CREATE INDEX tasks_state_idx ON tasks(state);\n\
        \CREATE VIEW ready_tasks AS SELECT * FROM tasks WHERE 0;\n\
        \INSERT INTO tasks (id,title,state) VALUES ('01AAA','Dependency','ready');\n\
        \INSERT INTO tasks (id,title,state) VALUES ('01BBB','Dependent','ready');\n\
        \INSERT INTO edges (id,kind,src_kind,src_id,dst_kind,dst_id)\n\
        \  VALUES ('01EDGE','depends_on','task','01BBB','task','01AAA');"

    let m15 = case filter ((== 15) . migrationVersion) migrations of
            (m : _) -> m
            [] -> error "migration 15 not registered"
    migrationUp m15 conn

    carried <- query_ conn "SELECT id FROM tasks ORDER BY id" :: IO [Only Text]
    map fromOnly carried @?= ["01AAA", "01BBB"]
    edges <- query_ conn "SELECT id FROM edges" :: IO [Only Text]
    map fromOnly edges @?= ["01EDGE"]

    -- The new state is accepted, and the recreated view keeps the deps gate.
    execute_ conn "INSERT INTO tasks (id,title,state) VALUES ('01CCC','Interactive','ready_interactive')"
    inView <- query_ conn "SELECT id FROM ready_tasks ORDER BY id" :: IO [Only Text]
    map fromOnly inView @?= ["01AAA", "01CCC"]

    -- The edge-cascade trigger went with the old table and must be back.
    execute_ conn "DELETE FROM tasks WHERE id='01AAA'"
    afterCascade <- query_ conn "SELECT id FROM edges" :: IO [Only Text]
    map fromOnly afterCascade @?= []

{- | Migration 14 rebuilds `categories` to widen its axis CHECK. The rebuild
must keep existing rows, keep the child link tables pointing at the new
table (FK cascade still fires), and accept the new axis.
-}
testMigration14KindAxis :: IO ()
testMigration14KindAxis = withBaseTestDb $ \conn -> do
    -- Restore the v13 shape verbatim: CHECK without 'kind'. Rebuilt from
    -- scratch (children first) so no ALTER-rename semantics are involved in
    -- the fixture itself.
    execSql
        conn
        "DROP TABLE task_categories;\n\
        \DROP TABLE context_categories;\n\
        \DROP TABLE categories;\n\
        \CREATE TABLE categories (\n\
        \  id   TEXT PRIMARY KEY,\n\
        \  axis TEXT NOT NULL CHECK (axis IN ('domain','discipline')),\n\
        \  name TEXT NOT NULL,\n\
        \  UNIQUE (axis, name));\n\
        \CREATE TABLE task_categories (\n\
        \  task_id     TEXT NOT NULL REFERENCES tasks(id) ON DELETE CASCADE,\n\
        \  category_id TEXT NOT NULL REFERENCES categories(id) ON DELETE CASCADE,\n\
        \  PRIMARY KEY (task_id, category_id));\n\
        \CREATE TABLE context_categories (\n\
        \  context_id  TEXT NOT NULL REFERENCES context(id) ON DELETE CASCADE,\n\
        \  category_id TEXT NOT NULL REFERENCES categories(id) ON DELETE CASCADE,\n\
        \  PRIMARY KEY (context_id, category_id));"
    tid <-
        RT.insertTask
            conn
            RT.NewTask
                { RT.ntTitle = "Tagged"
                , RT.ntBody = ""
                , RT.ntState = Ready
                , RT.ntPriority = Nothing
                , RT.ntNoCommit = False
                }
    cid <- RC.insertCategory conn Domain "cli"
    RC.attachTaskCategory conn tid cid

    let m14 = case filter ((== 14) . migrationVersion) migrations of
            (m : _) -> m
            [] -> error "migration 14 not registered"
    migrationUp m14 conn

    survivors <- RC.listCategories conn Nothing
    map (\c -> (categoryAxis c, categoryName c)) survivors @?= [(Domain, "cli")]
    attached <- RC.taskCategoriesFor conn tid
    map categoryName attached @?= ["cli"]

    -- The widened CHECK accepts the workflow axis.
    kid <- RC.insertCategory conn Kind "bug"
    RC.attachTaskCategory conn tid kid
    both <- RC.taskCategoriesFor conn tid
    length both @?= 2

    -- Child FK still resolves to the rebuilt table: deleting the task
    -- cascades its attachments away.
    void $ RT.deleteTask conn tid
    orphaned <- RC.taskCategoriesFor conn tid
    null orphaned @?= True

testListParkedDispatches :: IO ()
testListParkedDispatches = withTestDb $ \c -> do
    tid <-
        RT.insertTask
            c
            RT.NewTask
                { RT.ntTitle = "Parked task"
                , RT.ntBody = ""
                , RT.ntState = Ready
                , RT.ntPriority = Nothing
                , RT.ntNoCommit = False
                }
    let did = "01PARK0000000000000000001P" :: Text
    insertTestDispatch c did tid
    RD.finishDispatch c did OSuccess Nothing Nothing

    parked <- RD.listParkedDispatches c
    map dispatchId parked @?= [did]

    RD.setMerged c did "cafebabe"
    parked' <- RD.listParkedDispatches c
    null parked' @?= True
