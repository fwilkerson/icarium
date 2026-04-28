module Main (main) where

import           CliSpec                   (tests)
import           Control.Exception         (bracket, try)
import           Control.Monad             (forM, forM_, void)
import qualified Data.ByteString.Char8     as BC
import           Data.Int                  (Int64)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import           Database.SQLite.Simple    (Connection, Only (..), Query (..), close, execute, open,
                                            query_)
import           System.Exit               (ExitCode (..))
import           System.IO                 (hClose)
import           System.IO.Temp            (withSystemTempFile)
import           Test.Tasty                (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit          (assertBool, assertFailure, testCase, (@?=))

import           Icarium.Commands.Category (SyncReport (..), syncCategories)
import           Icarium.Commands.Dispatch (renderDispatch)
import           Icarium.Commands.Know     (autoDeriveDeps)
import           Icarium.Commands.Util     (requireCategory)
import           Icarium.Config            (CategoriesConfig (..), defaultConfigText, loadConfig)
import           Icarium.Db                (dbSchemaVersion)
import           Icarium.Dispatch          (postClaudeGuard)
import           Icarium.Dispatch.Tick     (TickState, emptyTickState, summariseTick)
import           Icarium.Id                (newId)
import           Icarium.Render            (renderKnowledge, renderKnowledgeList, renderTaskHuman,
                                            renderTaskList, renderTaskPrompt)
import qualified Icarium.Render
import qualified Icarium.Repo.Category     as RC
import qualified Icarium.Repo.Dispatch     as RD
import qualified Icarium.Repo.Edge         as RE
import qualified Icarium.Repo.Knowledge    as RK
import qualified Icarium.Repo.Task         as RT
import           Icarium.Schema            (applySchema)
import           Icarium.Types

main :: IO ()
main = defaultMain $ testGroup "icarium"
    [ tests
    , testGroup "TaskState round-trips"    taskStateTests
    , testGroup "EdgeKind round-trips"     edgeKindTests
    , testGroup "Effort round-trips"       effortTests
    , testGroup "CategoryAxis round-trips" categoryAxisTests
    , testCase "loadConfig succeeds on default template"        loadConfigTest
    , testCase "renderTaskPrompt is non-empty for minimal task" renderTest
    , testGroup "summariseTick"
        [ testCase "system event emits model= session= line"              testTickSystem
        , testCase "assistant tool_use emits * tool line with name"       testTickAssistantToolUse
        , testCase "assistant text emits > assistant line"                testTickAssistantText
        , testCase "user is_error tool_result emits x tool_result line"   testTickUserError
        , testCase "user tool_result without is_error emits nothing"      testTickUserNoError
        , testCase "result event emits + result and = usage lines"        testTickResult
        , testCase "20th assistant event fires periodic = usage line"     testTickPeriodicUsage
        , testCase "malformed JSON emits ? unknown line and does not crash" testTickMalformed
        ]
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
    , testGroup "schema"
        [ testCase "applying embedded schema produces user_version = 1 with expected tables" testInitialSchema
        , testCase "deleting a knowledge entry cascades to knowledge_categories rows"        testKnowledgeCategoriesCascade
        ]
    , testGroup "resolveDispatchId (PREFIX_RESOLUTION: dispatch show, dispatch logs, dispatch recover)"
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
    , testGroup "mkBar 5-cell Unicode bar" testMkBar
    , testGroup "renderTaskList grouped view"
        [ testCase "groups in READY/PLANNED/BLOCKED/IDEA order with counts" testGroupedHeaders
        , testCase "single-state filter suppresses group header"            testSingleStateNoHeader
        , testCase "blocked row replaces bar with truncated reason"         testBlockedReason
        , testCase "edge counts omitted when both zero, shown otherwise"    testEdgeCountFormat
        , testCase "category formatting handles missing slots"              testCategoryFormatting
        , testCase "NULL priority sorts last within group"                  testNullPrioritySort
        , testCase "90-char title truncated to 72 chars with UTF-8 ellipsis" testTitleTruncatedUtf8
        , testCase "90-char title truncated with ASCII ... in ASCII mode"  testTitleTruncatedAscii
        , testCase "title at exactly 72 chars renders without truncation"  testTitleExactlyAtLimit
        ]
    , testGroup "category sync"
        [ testCase "inserts toml-only categories"                       testSyncInserts
        , testCase "reports orphans and exits non-zero (no prune)"      testSyncOrphanNoPrune
        , testCase "prunes unused orphans when no blockers"             testSyncPrunesUnused
        , testCase "blocks on in-use category; no deletions"            testSyncBlocksInUse
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
    , testGroup "category replace semantics"
        [ testCase "task update --domain replaces existing domain"         testTaskUpdateDomainReplaces
        , testCase "task update --domain empty string clears domain"       testTaskUpdateDomainClears
        , testCase "know update --domain replaces not appends"             testKnowUpdateDomainReplaces
        , testCase "requireCategory exits ExitFailure 2 for unknown name"  testRequireCategoryUnknown
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
-- summariseTick tests
-- =============================================================

tickTs :: String
tickTs = "12:00:00"

tick :: BC.ByteString -> ([String], TickState)
tick bytes = summariseTick tickTs bytes emptyTickState

tickWith :: BC.ByteString -> TickState -> ([String], TickState)
tickWith = summariseTick tickTs

strIn :: String -> String -> Bool
strIn needle = T.isInfixOf (T.pack needle) . T.pack

mustJust :: String -> Maybe a -> IO a
mustJust msg = maybe (assertFailure msg) pure

testTickSystem :: IO ()
testTickSystem = do
    let line = "{\"type\":\"system\",\"model\":\"claude-x\",\"session_id\":\"abcd1234xyz\"}"
        (out, _) = tick line
    length out @?= 1
    assertBool ". system glyph"   (". system"       `strIn` head out)
    assertBool "model= present"   ("model=claude-x" `strIn` head out)
    assertBool "session= present" ("session=abcd1234" `strIn` head out)

testTickAssistantToolUse :: IO ()
testTickAssistantToolUse = do
    let line = "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"tool_use\",\"name\":\"Bash\",\"input\":{\"command\":\"ls\"}}]}}"
        (out, _) = tick line
    assertBool "non-empty output"  (not (null out))
    assertBool "* tool glyph"      ("* tool" `strIn` head out)
    assertBool "tool name in body" ("Bash"   `strIn` head out)

testTickAssistantText :: IO ()
testTickAssistantText = do
    let line = "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"text\",\"text\":\"hello world\"}]}}"
        (out, _) = tick line
    assertBool "non-empty output"   (not (null out))
    assertBool "> assistant glyph" ("> assistant" `strIn` head out)

testTickUserError :: IO ()
testTickUserError = do
    let line = "{\"type\":\"user\",\"message\":{\"content\":[{\"type\":\"tool_result\",\"is_error\":true,\"content\":\"something failed\"}]}}"
        (out, _) = tick line
    length out @?= 1
    assertBool "x tool_result glyph" ("x tool_result" `strIn` head out)

testTickUserNoError :: IO ()
testTickUserNoError = do
    let line = "{\"type\":\"user\",\"message\":{\"content\":[{\"type\":\"tool_result\",\"content\":\"ok\"}]}}"
        (out, _) = tick line
    out @?= []

testTickResult :: IO ()
testTickResult = do
    let line = "{\"type\":\"result\",\"subtype\":\"success\",\"result\":\"done\",\"usage\":{\"input_tokens\":100,\"output_tokens\":50,\"cache_read_input_tokens\":20}}"
        (out, _) = tick line
    length out @?= 2
    assertBool "+ result line" ("+ result" `strIn` head out)
    assertBool "= usage line"  ("= usage"  `strIn` (out !! 1))
    assertBool "in 100"        ("in 100"   `strIn` (out !! 1))

testTickPeriodicUsage :: IO ()
testTickPeriodicUsage = do
    let assistantLine = "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"text\",\"text\":\"x\"}],\"usage\":{\"input_tokens\":5,\"output_tokens\":3,\"cache_read_input_tokens\":1}}}"
        st19 = iterate (snd . tickWith assistantLine) emptyTickState !! 19
        (out20, st20) = tickWith assistantLine st19
    assertBool "periodic = usage fires on 20th" (any ("= usage" `strIn`) out20)
    let (out21, _) = tickWith assistantLine st20
    assertBool "no periodic usage on 21st"      (not (any ("= usage" `strIn`) out21))

testTickMalformed :: IO ()
testTickMalformed = do
    let line = "not valid json at all { }"
        (out, _) = tick line
    length out @?= 1
    assertBool "? unknown glyph" ("? unknown" `strIn` head out)

-- =============================================================
-- postClaudeGuard tests
-- =============================================================

baseSha :: Text
baseSha = "aaaa0000"

newSha :: Text
newSha = "bbbb1111"

testGuardDirtyTree :: IO ()
testGuardDirtyTree =
    postClaudeGuard "?? snapshot-test.json\n M src/Foo.hs" (Right newSha) baseSha
        @?= Just "agent left uncommitted changes; refusing to merge\n\
                 \uncommitted:\n\
                 \  ?? snapshot-test.json\n\
                 \   M src/Foo.hs"

testGuardEmptyDiff :: IO ()
testGuardEmptyDiff =
    postClaudeGuard "" (Right baseSha) baseSha
        @?= Just "agent made no commits on dispatch branch"

testGuardDirtyFirst :: IO ()
testGuardDirtyFirst =
    postClaudeGuard "?? leftover.txt" (Right baseSha) baseSha
        @?= Just "agent left uncommitted changes; refusing to merge\n\
                 \uncommitted:\n\
                 \  ?? leftover.txt"

testGuardPasses :: IO ()
testGuardPasses =
    postClaudeGuard "" (Right newSha) baseSha @?= Nothing

testGuardRevParseError :: IO ()
testGuardRevParseError =
    postClaudeGuard "" (Left ("git error" :: String)) baseSha @?= Nothing

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
    -- parseEdgeKind accepts hyphens (CLI form)
    -- edgeKindDbText produces underscores (DB storage form)
    -- edgeKindDisplay produces hyphens (human/CLI display)
    [ testCase "depends-on parses"    $ parseEdgeKind "depends-on"   @?= Just DependsOn
    , testCase "references parses"    $ parseEdgeKind "references"   @?= Just References
    , testCase "derived-from parses"  $ parseEdgeKind "derived-from" @?= Just DerivedFrom
    , testCase "supersedes parses"    $ parseEdgeKind "supersedes"   @?= Just Supersedes
    , testCase "depends_on rejected"  $ parseEdgeKind "depends_on"   @?= Nothing
    , testCase "derived_from rejected"$ parseEdgeKind "derived_from" @?= Nothing
    , testCase "edgeKindDbText DependsOn"   $ edgeKindDbText DependsOn   @?= "depends_on"
    , testCase "edgeKindDbText References"  $ edgeKindDbText References  @?= "references"
    , testCase "edgeKindDbText DerivedFrom" $ edgeKindDbText DerivedFrom @?= "derived_from"
    , testCase "edgeKindDbText Supersedes"  $ edgeKindDbText Supersedes  @?= "supersedes"
    , testCase "edgeKindDisplay DependsOn"   $ edgeKindDisplay DependsOn   @?= "depends-on"
    , testCase "edgeKindDisplay References"  $ edgeKindDisplay References  @?= "references"
    , testCase "edgeKindDisplay DerivedFrom" $ edgeKindDisplay DerivedFrom @?= "derived-from"
    , testCase "edgeKindDisplay Supersedes"  $ edgeKindDisplay Supersedes  @?= "supersedes"
    ]

effortTests :: [TestTree]
effortTests =
    [ roundTrip effortText parseEffort "low"    Low
    , roundTrip effortText parseEffort "medium" Medium
    , roundTrip effortText parseEffort "high"   High
    , roundTrip effortText parseEffort "xhigh"  XHigh
    , roundTrip effortText parseEffort "max"    Max
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
            , "K" <> T.pack (show i) :: Text
            , "" :: Text
            , "2026-01-0" <> T.pack (show i) <> "T00:00:00" :: Text
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
-- Schema tests
-- =============================================================

testInitialSchema :: IO ()
testInitialSchema = withTestDb $ \conn -> do
    v <- dbSchemaVersion conn
    v @?= (1 :: Int64)
    tableRows <- query_ conn
        "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name"
        :: IO [Only Text]
    let tables = map (\(Only n) -> n) tableRows
    assertBool "tasks table exists"               ("tasks"               `elem` tables)
    assertBool "knowledge table exists"           ("knowledge"           `elem` tables)
    assertBool "categories table exists"          ("categories"          `elem` tables)
    assertBool "edges table exists"               ("edges"               `elem` tables)
    assertBool "dispatches table exists"          ("dispatches"          `elem` tables)
    assertBool "task_categories table exists"     ("task_categories"     `elem` tables)
    assertBool "knowledge_categories table exists" ("knowledge_categories" `elem` tables)

testKnowledgeCategoriesCascade :: IO ()
testKnowledgeCategoriesCascade = withTestDb $ \conn -> do
    domCat <- mkCat conn Domain "cli"
    kid    <- mkKnowledge conn "K" "body"
    RC.attachKnowledgeCategory conn kid (categoryId domCat)
    pre <- query_ conn "SELECT knowledge_id FROM knowledge_categories" :: IO [Only Text]
    length pre @?= 1
    execute conn (Query "DELETE FROM knowledge WHERE id = ?") (Only kid)
    post <- query_ conn "SELECT knowledge_id FROM knowledge_categories" :: IO [Only Text]
    length post @?= 0

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
        , "dispatch/" <> did :: Text
        , "main" :: Text
        , "0000000000000000000000000000000000000000" :: Text
        , "claude-sonnet-4-6" :: Text
        , "medium" :: Text
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
insertTestEdge c src =
    RE.insertEdge c DependsOn TaskNode src TaskNode

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

-- =============================================================
-- mkBar tests
-- =============================================================

testMkBar :: [TestTree]
testMkBar =
    [ testCase "Nothing" $ Icarium.Render.mkBar Nothing   @?= "□ □ □ □ □"
    , testCase "0"       $ Icarium.Render.mkBar (Just 0)  @?= "□ □ □ □ □"
    , testCase "1"       $ Icarium.Render.mkBar (Just 1)  @?= "◧ □ □ □ □"
    , testCase "2"       $ Icarium.Render.mkBar (Just 2)  @?= "■ □ □ □ □"
    , testCase "3"       $ Icarium.Render.mkBar (Just 3)  @?= "■ ◧ □ □ □"
    , testCase "4"       $ Icarium.Render.mkBar (Just 4)  @?= "■ ■ □ □ □"
    , testCase "5"       $ Icarium.Render.mkBar (Just 5)  @?= "■ ■ ◧ □ □"
    , testCase "6"       $ Icarium.Render.mkBar (Just 6)  @?= "■ ■ ■ □ □"
    , testCase "7"       $ Icarium.Render.mkBar (Just 7)  @?= "■ ■ ■ ◧ □"
    , testCase "8"       $ Icarium.Render.mkBar (Just 8)  @?= "■ ■ ■ ■ □"
    , testCase "9"       $ Icarium.Render.mkBar (Just 9)  @?= "■ ■ ■ ■ ◧"
    , testCase "10"      $ Icarium.Render.mkBar (Just 10) @?= "■ ■ ■ ■ ■"
    ]

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
    iReady   <- mustJust "READY position"   (lengthBefore "READY"   out)
    iPlanned <- mustJust "PLANNED position" (lengthBefore "PLANNED" out)
    iBlocked <- mustJust "BLOCKED position" (lengthBefore "BLOCKED" out)
    iIdea    <- mustJust "IDEA position"    (lengthBefore "IDEA"    out)
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

testBlockedReason :: IO ()
testBlockedReason = do
    let longReason = T.replicate 80 "x"
        rows = [ mkRow "01ABCDEFGH01" "short" Blocked (Just 5) [] 0 0 (Just "nope")
               , mkRow "01ABCDEFGH02" "long"  Blocked (Just 5) [] 0 0 (Just longReason)
               ]
        out = renderTaskList True rows [Blocked]
    assertBool "shows short reason" ("nope" `T.isInfixOf` out)
    assertBool "no priority bar in blocked" (not ("■ ■ ◧ □ □" `T.isInfixOf` out))
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
    assertBool "non-empty rendered rows" (not (null ls))
    -- Expected order: high-pri, low-pri, null-pri (NULL last).
    let idx t = head [i | (i, l) <- zip [0::Int ..] ls, t `T.isInfixOf` l]
    (idx "high-pri" < idx "low-pri") @?= True
    (idx "low-pri"  < idx "null-pri") @?= True

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
    let titlePart = T.take Icarium.Render.recommendedTitleMax (T.drop 14 dataRow)
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
-- category sync tests
-- =============================================================

testSyncInserts :: IO ()
testSyncInserts = withTestDb $ \conn -> do
    let cfg = CategoriesConfig { catDomains = ["cli"], catDisciplines = ["haskell"] }
    rpt <- syncCategories conn cfg False
    srInserted rpt @?= [(Domain, "cli"), (Discipline, "haskell")]
    null (srOrphans rpt)  @?= True
    null (srPruned rpt)   @?= True
    null (srBlocking rpt) @?= True
    cats <- RC.listCategories conn Nothing
    length cats @?= 2

testSyncOrphanNoPrune :: IO ()
testSyncOrphanNoPrune = withTestDb $ \conn -> do
    _ <- RC.insertCategory conn Domain "stale-domain"
    let cfg = CategoriesConfig { catDomains = [], catDisciplines = [] }
    rpt <- syncCategories conn cfg False
    null (srInserted rpt) @?= True
    length (srOrphans rpt) @?= 1
    null (srPruned rpt)   @?= True
    null (srBlocking rpt) @?= True
    -- category still present: sync without --prune must not delete
    cats <- RC.listCategories conn Nothing
    length cats @?= 1

testSyncPrunesUnused :: IO ()
testSyncPrunesUnused = withTestDb $ \conn -> do
    _ <- RC.insertCategory conn Domain "stale-domain"
    let cfg = CategoriesConfig { catDomains = [], catDisciplines = [] }
    rpt <- syncCategories conn cfg True
    null (srInserted rpt)  @?= True
    null (srOrphans rpt)   @?= True
    length (srPruned rpt)  @?= 1
    null (srBlocking rpt)  @?= True
    cats <- RC.listCategories conn Nothing
    length cats @?= 0

testSyncBlocksInUse :: IO ()
testSyncBlocksInUse = withTestDb $ \conn -> do
    cid <- RC.insertCategory conn Domain "stale-domain"
    tid <- RT.insertTask conn RT.NewTask
        { RT.ntTitle = "T", RT.ntBody = "", RT.ntState = Ready, RT.ntPriority = Nothing }
    RC.attachTaskCategory conn tid cid
    let cfg = CategoriesConfig { catDomains = [], catDisciplines = [] }
    rpt <- syncCategories conn cfg True
    null (srInserted rpt)  @?= True
    null (srOrphans rpt)   @?= True
    null (srPruned rpt)    @?= True
    length (srBlocking rpt) @?= 1
    -- blocking report contains the task id
    let (_, nodeIds) = head (srBlocking rpt)
    nodeIds @?= [tid]
    -- category still present: in-use category must not be deleted
    cats <- RC.listCategories conn Nothing
    length cats @?= 1

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
    iDep <- mustJust "depends-on position" (posOf "depends-on" out)
    iRef <- mustJust "references position" (posOf "references" out)
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

-- =============================================================
-- category replace semantics tests
-- =============================================================

testTaskUpdateDomainReplaces :: IO ()
testTaskUpdateDomainReplaces = withTestDb $ \c -> do
    domA <- mkCat c Domain "domA"
    domB <- mkCat c Domain "domB"
    tid  <- RT.insertTask c RT.NewTask
        { RT.ntTitle = "T", RT.ntBody = "", RT.ntState = Ready, RT.ntPriority = Nothing }
    RC.attachTaskCategory c tid (categoryId domA)
    -- Simulate: task update tid --domain domB (replace semantics)
    RC.detachTaskCategoriesByAxis c tid Domain
    RC.attachTaskCategory c tid (categoryId domB)
    cats <- RC.taskCategoriesFor c tid
    map categoryName (filter (\x -> categoryAxis x == Domain) cats) @?= ["domB"]

testTaskUpdateDomainClears :: IO ()
testTaskUpdateDomainClears = withTestDb $ \c -> do
    dom <- mkCat c Domain "mydom"
    tid <- RT.insertTask c RT.NewTask
        { RT.ntTitle = "T", RT.ntBody = "", RT.ntState = Ready, RT.ntPriority = Nothing }
    RC.attachTaskCategory c tid (categoryId dom)
    -- Simulate: task update tid --domain '' (clear)
    RC.detachTaskCategoriesByAxis c tid Domain
    cats <- RC.taskCategoriesFor c tid
    null cats @?= True

testKnowUpdateDomainReplaces :: IO ()
testKnowUpdateDomainReplaces = withTestDb $ \c -> do
    domA <- mkCat c Domain "domA"
    domB <- mkCat c Domain "domB"
    kid  <- mkKnowledge c "K" "body"
    RC.attachKnowledgeCategory c kid (categoryId domA)
    -- Old additive behaviour would leave both domA and domB; replace leaves only domB.
    RC.detachKnowledgeCategoriesByAxis c kid Domain
    RC.attachKnowledgeCategory c kid (categoryId domB)
    cats <- RC.knowledgeCategoriesFor c kid
    let doms = filter (\x -> categoryAxis x == Domain) cats
    length doms @?= 1
    map categoryName doms @?= ["domB"]

testRequireCategoryUnknown :: IO ()
testRequireCategoryUnknown = withTestDb $ \c -> do
    result <- try (requireCategory c Domain "no-such-cat") :: IO (Either ExitCode Category)
    case result of
        Left (ExitFailure 2) -> pure ()
        Left e               -> fail ("unexpected exit code: " <> show e)
        Right _              -> fail "expected failure for unknown category"
