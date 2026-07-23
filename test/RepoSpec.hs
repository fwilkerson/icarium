{- | Repo CRUD and the enum spellings it stores. Larger subjects have their
own siblings: 'SchemaSpec', 'ResolverSpec', 'SearchSpec', 'ContextSpec',
'QueueSpec', 'DispatchRepoSpec'.
-}
module RepoSpec (tests) where

import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Database.SQLite.Simple (Connection)
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Icarium.Config (
    Config (..),
    DispatchConfig (..),
    ReviewConfig (..),
    defaultConfigText,
    loadConfig,
 )
import Icarium.Repo.Category qualified as RC
import Icarium.Repo.Edge qualified as RE
import Icarium.Repo.Internal (inClause)
import Icarium.Repo.Task qualified as RT
import Icarium.Types

import TestHelpers

tests :: TestTree
tests =
    testGroup
        "repo"
        [ testGroup "EdgeKind round-trips" edgeKindTests
        , testCase "every state round-trips through the CLI spelling" $
            mapM_ (\st -> parseTaskState (taskStateCli st) @?= Just st) allTaskStates
        , testCase "every state round-trips through the stored spelling" $
            mapM_ (\st -> parseTaskStateDb (taskStateText st) @?= Just st) allTaskStates
        , testCase "the stored spelling is also accepted on the CLI" $
            mapM_ (\st -> parseTaskState (taskStateText st) @?= Just st) allTaskStates
        , testCase "bare ready parses as nothing, on either axis" $ do
            parseTaskState "ready" @?= Nothing
            parseTaskStateDb "ready" @?= Nothing
        , testCase "loadConfig succeeds on default template" loadConfigTest
        , testCase "inClause brings its own parens" $ do
            inClause [() | _ <- [1 :: Int .. 3]] @?= "(?,?,?)"
            inClause [()] @?= "(?)"
        , testGroup
            "listEdges filtering"
            [ testCase "src + kind filter returns only matching rows" testListEdgesSrcKindFilter
            ]
        , testGroup
            "dependencyTasks"
            [ testCase "selects all Task columns (regression: missing no_commit)" testDependencyTasksReturnsAllColumns
            ]
        , testGroup
            "no_commit column"
            [ testCase "insertTask with ntNoCommit=True round-trips through getTask" testNoCommitInsert
            , testCase "tuNoCommit=Just True sets no_commit; Just False clears it" testNoCommitUpdate
            ]
        , testGroup
            "updateTask block_reason invariant"
            [ testCase "transition Blocked → Done clears block_reason" testUpdateClearsBlockReasonOnDone
            , testCase "transition Blocked → ReadyHeadless clears block_reason" testUpdateClearsBlockReasonOnReady
            , testCase "Blocked → Blocked preserves block_reason" testUpdateBlockedPreservesReason
            ]
        , testGroup
            "category replace semantics"
            [ testCase "task update --domain replaces existing domain" testTaskUpdateDomainReplaces
            , testCase "task update --domain empty string clears domain" testTaskUpdateDomainClears
            , testCase "ctx update --domain replaces not appends" testCtxUpdateDomainReplaces
            , testCase "attaching a kind category to a context entry is a no-op" testContextCategoryKindBlocked
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
-- Edges
-- =============================================================

testListEdgesSrcKindFilter :: IO ()
testListEdgesSrcKindFilter = withTestDb $ \c -> do
    t1 <- mkTaskRow c "A"
    t2 <- mkTaskRow c "B"
    t3 <- mkTaskRow c "C"
    kid <- mkContext c "K" ""
    _ <- RE.insertEdge c DependsOn TaskNode t1 TaskNode t2
    _ <- RE.insertEdge c References TaskNode t1 ContextNode kid
    _ <- RE.insertEdge c DependsOn TaskNode t2 TaskNode t3
    es <- RE.listEdges c (Just t1) Nothing (Just DependsOn)
    length es @?= 1
    edgeSrcId (head es) @?= t1
    edgeKind (head es) @?= DependsOn

testDependencyTasksReturnsAllColumns :: IO ()
testDependencyTasksReturnsAllColumns = withTestDb $ \c -> do
    t1 <- mkTaskRow c "A"
    t2 <- mkNoCommitTask c "B"
    _ <- RE.insertEdge c DependsOn TaskNode t1 TaskNode t2
    deps <- RE.dependencyTasks c t1
    map taskNoCommit deps @?= [True]

-- =============================================================
-- no_commit column
-- =============================================================

-- | A task whose changes dispatch must not commit.
mkNoCommitTask :: Connection -> Text -> IO Text
mkNoCommitTask c title =
    RT.insertTask
        c
        RT.NewTask
            { RT.ntTitle = title
            , RT.ntBody = ""
            , RT.ntState = ReadyHeadless
            , RT.ntPriority = Nothing
            , RT.ntNoCommit = True
            , RT.ntRouting = mempty
            }

testNoCommitInsert :: IO ()
testNoCommitInsert = withTestDb $ \c -> do
    tid <- mkNoCommitTask c "Side-effect task"
    Just t <- RT.getTask c tid
    taskNoCommit t @?= True

testNoCommitUpdate :: IO ()
testNoCommitUpdate = withTestDb $ \c -> do
    tid <- mkTaskRow c "Commit task"
    _ <- RT.updateTask c tid RT.emptyUpdate{RT.tuNoCommit = Just True}
    Just t1 <- RT.getTask c tid
    taskNoCommit t1 @?= True
    _ <- RT.updateTask c tid RT.emptyUpdate{RT.tuNoCommit = Just False}
    Just t2 <- RT.getTask c tid
    taskNoCommit t2 @?= False

-- =============================================================
-- updateTask block_reason invariant
-- =============================================================

insertBlockedTask :: Connection -> Text -> IO Text
insertBlockedTask c reason = do
    tid <- mkTaskRow c "T"
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
    _ <- RT.updateTask c tid RT.emptyUpdate{RT.tuState = Just ReadyHeadless}
    Just t <- RT.getTask c tid
    taskBlockReason t @?= Nothing

testUpdateBlockedPreservesReason :: IO ()
testUpdateBlockedPreservesReason = withTestDb $ \c -> do
    tid <- insertBlockedTask c "still blocked"
    _ <- RT.updateTask c tid RT.emptyUpdate{RT.tuPriority = Just (Just 5)}
    Just t <- RT.getTask c tid
    taskBlockReason t @?= Just "still blocked"

-- =============================================================
-- category replace semantics
-- =============================================================

testTaskUpdateDomainReplaces :: IO ()
testTaskUpdateDomainReplaces = withTestDb $ \c -> do
    domA <- mkCat c Domain "domA"
    domB <- mkCat c Domain "domB"
    tid <- mkTaskRow c "T"
    RC.attachTaskCategory c tid (categoryId domA)
    RC.detachTaskCategoriesByAxis c tid Domain
    RC.attachTaskCategory c tid (categoryId domB)
    cats <- RC.taskCategoriesFor c tid
    map categoryName (filter (\x -> categoryAxis x == Domain) cats) @?= ["domB"]

testTaskUpdateDomainClears :: IO ()
testTaskUpdateDomainClears = withTestDb $ \c -> do
    dom <- mkCat c Domain "mydom"
    tid <- mkTaskRow c "T"
    RC.attachTaskCategory c tid (categoryId dom)
    RC.detachTaskCategoriesByAxis c tid Domain
    cats <- RC.taskCategoriesFor c tid
    null cats @?= True

testCtxUpdateDomainReplaces :: IO ()
testCtxUpdateDomainReplaces = withTestDb $ \c -> do
    domA <- mkCat c Domain "domA"
    domB <- mkCat c Domain "domB"
    kid <- mkContext c "K" "body"
    RC.attachContextCategory c kid domA
    RC.detachContextCategoriesByAxis c kid Domain
    RC.attachContextCategory c kid domB
    cats <- RC.contextCategoriesFor c kid
    let doms = filter (\x -> categoryAxis x == Domain) cats
    length doms @?= 1
    map categoryName doms @?= ["domB"]

testContextCategoryKindBlocked :: IO ()
testContextCategoryKindBlocked = withTestDb $ \conn -> do
    domCat <- mkCat conn Domain "cli"
    kindCat <- mkCat conn Kind "bug"
    kid <- mkContext conn "K" "body"
    RC.attachContextCategory conn kid domCat
    RC.attachContextCategory conn kid kindCat
    cats <- RC.contextCategoriesFor conn kid
    map categoryAxis cats @?= [Domain]
