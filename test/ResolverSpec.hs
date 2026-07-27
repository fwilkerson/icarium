{- | Id-prefix resolution. Every @resolve*Id@ has the same three-way contract,
so they share one table; only the seeding differs.

Grep for PREFIX_RESOLUTION to audit coverage when adding new commands.
-}
module ResolverSpec (tests) where

import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple (Connection, Query (..), execute)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

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
        [ testGroup "resolvers" (map resolverGroup resolvers)
        , testGroup
            "resolveNode (PREFIX_RESOLUTION: link add src/dst, link list --from/--to, ctx add --derived-from)"
            [testCase "a prefix resolves within its own kind; a shared one hits both" testResolveNode]
        ]

-- | Ids sharing a 10-char prefix, and one that shares with nothing.
uniqueId, ambiguousA, ambiguousB :: Text
uniqueId = "01UNIQUE00CCCCCCCCCCCCCCCC"
ambiguousA = "01AMBIG000AAAAAAAAAAAAAAAA"
ambiguousB = "01AMBIG000BBBBBBBBBBBBBBBB"

prefixOf :: Text -> Text
prefixOf = T.take 10

{- | One resolver: how to write a row under a chosen id, and how to look one
up. The repo inserters mint their own ULID, so collisions are staged by hand.
-}
data Resolver = Resolver
    { rName :: String
    , rSeed :: Connection -> Text -> IO ()
    , rResolve :: Connection -> Text -> IO (Either String Text)
    }

resolvers :: [Resolver]
resolvers =
    [ Resolver
        "resolveTaskId (PREFIX_RESOLUTION: task show/update/rm, task add --depends-on, dispatch run, dispatch list --task)"
        insertTaskRow
        RT.resolveTaskId
    , Resolver
        "resolveContextId (PREFIX_RESOLUTION: ctx show/update/rm, ctx add --supersedes, task add --references)"
        insertContextRow
        RK.resolveContextId
    , Resolver
        "resolveDispatchId (PREFIX_RESOLUTION: dispatch show, dispatch logs, dispatch recover)"
        (\c did -> mkTaskRow c "T" >>= insertTestDispatch c did)
        RD.resolveDispatchId
    , Resolver
        "resolveEdgeId (PREFIX_RESOLUTION: link rm)"
        insertEdgeRow
        RE.resolveEdgeId
    ]

resolverGroup :: Resolver -> TestTree
resolverGroup Resolver{..} =
    testGroup
        rName
        [ testCase "right on a unique prefix" $ withTestDb $ \c -> do
            rSeed c uniqueId
            r <- rResolve c (prefixOf uniqueId)
            r @?= Right uniqueId
        , testCase "left on a missing id, naming the input" $ withTestDb $ \c -> do
            r <- rResolve c "01ZZZZZZZZ"
            msg <- expectLeft r
            assertBool "error names the input" ("01ZZZZZZZZ" `isIn` msg)
        , testCase "left on an ambiguous prefix, naming the input and the candidates" $ withTestDb $ \c -> do
            rSeed c ambiguousA
            rSeed c ambiguousB
            r <- rResolve c (prefixOf ambiguousA)
            msg <- expectLeft r
            assertBool "error names the input" (prefixOf ambiguousA `isIn` msg)
            assertBool "error lists the first candidate" (ambiguousA `isIn` msg)
            assertBool "error lists the second candidate" (ambiguousB `isIn` msg)
        ]
  where
    isIn needle haystack = needle `T.isInfixOf` T.pack haystack
    expectLeft = either pure (\v -> assertFailure ("expected Left, got " <> show v))

{- | The node resolvers do not disambiguate — they report what each kind
matched, and the caller decides. A prefix shared across kinds matches both,
which is what makes it ambiguous.
-}
testResolveNode :: IO ()
testResolveNode = withTestDb $ \c -> do
    tid <- mkTaskRow c "T"
    kid <- mkContext c "K" "body"

    taskHits <- RT.getTasksByPrefix c (prefixOf tid)
    taskMisses <- RK.getContextsByPrefix c (prefixOf tid)
    (map taskId taskHits, length taskMisses) @?= ([tid], 0)

    ctxHits <- RK.getContextsByPrefix c (prefixOf kid)
    ctxMisses <- RT.getTasksByPrefix c (prefixOf kid)
    (map contextId ctxHits, length ctxMisses) @?= ([kid], 0)

    insertTaskRow c ambiguousA
    insertContextRow c ambiguousB
    ts <- RT.getTasksByPrefix c (prefixOf ambiguousA)
    ks <- RK.getContextsByPrefix c (prefixOf ambiguousA)
    (length ts, length ks) @?= (1, 1)

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

insertEdgeRow :: Connection -> Text -> IO ()
insertEdgeRow c eid = do
    src <- mkTaskRow c "A"
    dst <- mkTaskRow c "B"
    execute
        c
        (Query "INSERT INTO edges (id, kind, src_kind, src_id, dst_kind, dst_id) VALUES (?,?,?,?,?,?)")
        (eid, "depends_on" :: Text, "task" :: Text, src, "task" :: Text, dst)
