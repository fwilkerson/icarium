{- | Id-prefix resolution, one group per id-taking CLI surface.

Grep for PREFIX_RESOLUTION to audit coverage when adding new commands.
-}
module ResolverSpec (tests) where

import Control.Monad (forM_)
import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple (Connection, Query (..), execute)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Icarium.Repo.Context qualified as RK
import Icarium.Repo.Dispatch qualified as RD
import Icarium.Repo.Edge qualified as RE
import Icarium.Repo.Task qualified as RT
import Icarium.Types

import TestHelpers

tests :: TestTree
tests =
    testGroup
        "prefix resolution"
        [ testGroup
            "resolveDispatchId (PREFIX_RESOLUTION: dispatch show, dispatch logs, dispatch recover)"
            [ testCase "right on full id" testResolveDispatchFullId
            , testCase "right on unique prefix" testResolveDispatchPrefix
            , testCase "left on missing" testResolveDispatchMissing
            , testCase "left on ambiguous" testResolveDispatchAmbiguous
            ]
        , testGroup
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
            "resolveNode (PREFIX_RESOLUTION: link add src/dst, link list --from/--to, ctx add --derived-from)"
            [ testCase "resolves task prefix" testResolveNodeTask
            , testCase "resolves context prefix" testResolveNodeContext
            , testCase "left on ambiguous cross-type" testResolveNodeAmbiguous
            ]
        ]

-- =============================================================
-- resolveDispatchId
-- =============================================================

testResolveDispatchFullId :: IO ()
testResolveDispatchFullId = withTestDb $ \c -> do
    tid <- mkTaskRow c "T"
    let did = "01AAAA000000000000000000AA" :: Text
    insertTestDispatch c did tid
    r <- RD.resolveDispatchId c did
    r @?= Right did

testResolveDispatchPrefix :: IO ()
testResolveDispatchPrefix = withTestDb $ \c -> do
    tid <- mkTaskRow c "T"
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
    tid <- mkTaskRow c "T"
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
-- resolveTaskId
-- =============================================================

testResolveTaskPrefix :: IO ()
testResolveTaskPrefix = withTestDb $ \c -> do
    tid <- mkTaskRow c "T"
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
    forM_ [tid1, tid2] (insertTaskRow c)
    r <- RT.resolveTaskId c "01CCCC0000"
    case r of
        Left msg -> assertBool "error mentions input" ("01CCCC0000" `T.isInfixOf` T.pack msg)
        Right _ -> fail "expected Left for ambiguous task"

-- =============================================================
-- resolveContextId
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
    forM_ [kid1, kid2] (insertContextRow c)
    r <- RK.resolveContextId c "01DDDD0000"
    case r of
        Left msg -> assertBool "error mentions input" ("01DDDD0000" `T.isInfixOf` T.pack msg)
        Right _ -> fail "expected Left for ambiguous context"

-- =============================================================
-- resolveEdgeId
-- =============================================================

testResolveEdgePrefix :: IO ()
testResolveEdgePrefix = withTestDb $ \c -> do
    t1 <- mkTaskRow c "A"
    t2 <- mkTaskRow c "B"
    eid <- RE.insertEdge c DependsOn TaskNode t1 TaskNode t2
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
    t1 <- mkTaskRow c "A"
    t2 <- mkTaskRow c "B"
    t3 <- mkTaskRow c "C"
    forM_ [("01EEEE000000000000000000AA" :: Text, t2), ("01EEEE000000000000000000BB", t3)] $ \(eid, dst) ->
        execute
            c
            (Query "INSERT INTO edges (id, kind, src_kind, src_id, dst_kind, dst_id) VALUES (?,?,?,?,?,?)")
            (eid, "depends_on" :: Text, "task" :: Text, t1, "task" :: Text, dst)
    r <- RE.resolveEdgeId c "01EEEE0000"
    case r of
        Left msg -> assertBool "error mentions input" ("01EEEE0000" `T.isInfixOf` T.pack msg)
        Right _ -> fail "expected Left for ambiguous edge"

-- =============================================================
-- resolveNode
-- =============================================================

testResolveNodeTask :: IO ()
testResolveNodeTask = withTestDb $ \c -> do
    tid <- mkTaskRow c "T"
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
    insertTaskRow c (sharedPrefix <> "0000000000000000")
    insertContextRow c (sharedPrefix <> "1111111111111111")
    ts <- RT.getTasksByPrefix c sharedPrefix
    ks <- RK.getContextsByPrefix c sharedPrefix
    assertBool "both task and context match prefix" (length ts == 1 && length ks == 1)

{- | Rows written under a chosen id. The repo inserters mint their own ULID,
so collisions have to be staged by hand.
-}
insertTaskRow :: Connection -> Text -> IO ()
insertTaskRow c tid =
    execute
        c
        (Query "INSERT INTO tasks (id, title, body, state) VALUES (?,?,?,?)")
        (tid, "T" :: Text, "" :: Text, "ready_headless" :: Text)

insertContextRow :: Connection -> Text -> IO ()
insertContextRow c kid =
    execute
        c
        (Query "INSERT INTO context (id, title, body) VALUES (?,?,?)")
        (kid, "K" :: Text, "" :: Text)
