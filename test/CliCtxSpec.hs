-- | CLI contract for @icarium ctx@ and @icarium link@: CRUD, edges, curation.
module CliCtxSpec (tests) where

import Control.Monad (when)
import Data.Aeson (Key, Object, Value (..), decode)
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.Foldable (toList)
import Data.List (isInfixOf)
import System.Exit (ExitCode (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import CliHelpers (decodeOut, expectField, expectObject, jsonIds, runIcarium, withTempDb)

tests :: TestTree
tests =
    testGroup
        "ctx and link"
        [ testCase "ctx list --limit caps rows" testCtxListLimit
        , testCase "ctx list shows cats and linked count" testContextListLayout
        , testCase "link --help has corrected argument order example" testLinkHelpExample
        , testCase "link list emits header row when edges exist" testLinkListHeader
        , testCase "ctx add --help and link add --help cross-reference each other" testHelpCrossRef
        , testCase "ctx path → body file contains body" testCtxShowBody
        , testCase "ctx show prints body path, not body content" testCtxShowBodyPath
        , testCase "ctx cat prints body to stdout" testCtxCat
        , testCase "ctx cat on no-body entry prints empty and exits 0" testCtxCatNoBody
        , testCase "link add ctx references ctx is accepted" testLinkAddCtxReferencesCtx
        , testCase "link add task derived-from task records a follow-up" testLinkAddTaskDerivedFromTask
        , testCase "ctx children lists direct children by edge kind" testCtxChildren
        , testCase "ctx tree recurses and detects cycles" testCtxTree
        , testCase "ctx children --json: kind, id, title per row" testCtxChildrenJson
        , testCase "ctx tree --json: nested children, cycle flag" testCtxTreeJson
        , testCase "ctx exists: found exits 0, not-found exits 1, ambiguous exits 2" testCtxExists
        , testCase "ctx exists --verbose prints full id on match" testCtxExistsVerbose
        , testCase "ctx list/show --json: valid JSON, ids, body_path not body" testCtxJson
        , testCase "ctx curate records events; show renders latest; list hides retired" testCtxCurateLifecycle
        , testCase "ctx curate validation: artifact rules per disposition" testCtxCurateValidation
        , testCase "ctx curate queue: never-curated, --older-than, --json" testCtxCurateQueue
        ]

testCtxListLimit :: IO ()
testCtxListLimit = withTempDb $ \db -> do
    mapM_ (\i -> runIcarium db ["ctx", "add", "Ctx " ++ show (i :: Int)]) [1 .. 4 :: Int]
    (code, out, _) <- runIcarium db ["ctx", "list", "--limit", "2"]
    code @?= ExitSuccess
    let rows = filter (not . null) (lines out)
    length rows @?= 2

testContextListLayout :: IO ()
testContextListLayout = withTempDb $ \db -> do
    -- k1: no categories, no inbound edges
    (_, _, _) <- runIcarium db ["ctx", "add", "Plain entry no links"]

    -- k2: no categories, will have one inbound references edge from a task
    (_, k2Out, _) <- runIcarium db ["ctx", "add", "Entry with one inbound link"]
    let k2Id = head (words k2Out)

    (_, t1Out, _) <- runIcarium db ["task", "add", "Some task", "--state", "ready-headless"]
    let t1Id = head (words t1Out)
    (linkCode, _, _) <- runIcarium db ["link", "add", t1Id, "references", k2Id]
    linkCode @?= ExitSuccess

    (code, out, _) <- runIcarium db ["ctx", "list"]
    code @?= ExitSuccess
    assertBool "list contains plain entry title" ("Plain entry no links" `isInfixOf` out)
    assertBool "list contains linked entry title" ("Entry with one inbound link" `isInfixOf` out)
    assertBool "[linked:1] badge appears" ("[linked:1]" `isInfixOf` out)
    assertBool "no stale column (yes/no gone)" (not ("  yes  " `isInfixOf` out || "  no  " `isInfixOf` out))
    assertBool "cats column present ([-])" ("[-]" `isInfixOf` out)

testLinkHelpExample :: IO ()
testLinkHelpExample = withTempDb $ \db -> do
    (code, out, _) <- runIcarium db ["link", "--help"]
    code @?= ExitSuccess
    assertBool "old order absent" (not ("depends-on TASK_A TASK_B" `isInfixOf` out))
    assertBool "correct order present" ("TASK_A depends-on TASK_B" `isInfixOf` out)

testLinkListHeader :: IO ()
testLinkListHeader = withTempDb $ \db -> do
    (_, tOut, _) <- runIcarium db ["task", "add", "Src task", "--state", "ready-headless"]
    let tid = head (words tOut)
    (_, kOut, _) <- runIcarium db ["ctx", "add", "Dst context"]
    let kid = head (words kOut)
    (_, _, _) <- runIcarium db ["link", "add", tid, "references", kid]

    (code, out, _) <- runIcarium db ["link", "list"]
    code @?= ExitSuccess
    assertBool "header row present" ("EDGE_ID" `isInfixOf` out)

testHelpCrossRef :: IO ()
testHelpCrossRef = withTempDb $ \db -> do
    (_, ctxOut, _) <- runIcarium db ["ctx", "add", "--help"]
    assertBool "ctx add --help mentions link add" ("link add" `isInfixOf` ctxOut)

    (_, linkOut, _) <- runIcarium db ["link", "add", "--help"]
    assertBool "link add --help mentions ctx add --derived-from" ("ctx add --derived-from" `isInfixOf` linkOut)

testCtxShowBody :: IO ()
testCtxShowBody = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["ctx", "add", "Body test entry", "--body", "context body text"]
    let kid = head (words addOut)

    (pCode, pathOut, _) <- runIcarium db ["ctx", "path", kid]
    pCode @?= ExitSuccess
    let bodyPath = head (lines pathOut)
    contents <- readFile bodyPath
    contents @?= "context body text"

testCtxShowBodyPath :: IO ()
testCtxShowBodyPath = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["ctx", "add", "Body path test entry", "--body", "secret context body"]
    let outLines = lines addOut
        cxid = head outLines
        bodyPath = outLines !! 1

    (code, out, _) <- runIcarium db ["ctx", "show", cxid]
    code @?= ExitSuccess
    assertBool "show contains body path" (bodyPath `isInfixOf` out)
    assertBool "show does not contain body content" (not ("secret context body" `isInfixOf` out))
    assertBool "show does not have ## Body header" (not ("## Body" `isInfixOf` out))

testCtxCat :: IO ()
testCtxCat = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["ctx", "add", "Cat context", "--body", "ctx body line one\nctx body line two"]
    let cxid = head (lines addOut)

    (code, out, _) <- runIcarium db ["ctx", "cat", cxid]
    code @?= ExitSuccess
    out @?= "ctx body line one\nctx body line two"

testCtxCatNoBody :: IO ()
testCtxCatNoBody = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["ctx", "add", "No body context"]
    let cxid = head (lines addOut)

    (code, out, _) <- runIcarium db ["ctx", "cat", cxid]
    code @?= ExitSuccess
    out @?= ""

testLinkAddCtxReferencesCtx :: IO ()
testLinkAddCtxReferencesCtx = withTempDb $ \db -> do
    (_, aOut, _) <- runIcarium db ["ctx", "add", "Umbrella context"]
    let aId = head (words aOut)
    (_, bOut, _) <- runIcarium db ["ctx", "add", "Child context"]
    let bId = head (words bOut)

    (code, out, _) <- runIcarium db ["link", "add", bId, "references", aId]
    code @?= ExitSuccess
    assertBool "link add ctx references ctx returns edge id" (not (null out))

    (lCode, lOut, _) <- runIcarium db ["link", "list", "--to", aId]
    lCode @?= ExitSuccess
    assertBool "link list shows references edge" ("references" `isInfixOf` lOut)

{- | Scenario: a follow-up task discovered while working another one. Filing it
unlinked was the old outcome — `references` is task→context and `depends-on`
claims the child is blocked on its parent, which it is not.
-}
testLinkAddTaskDerivedFromTask :: IO ()
testLinkAddTaskDerivedFromTask = withTempDb $ \db -> do
    (_, pOut, _) <- runIcarium db ["task", "add", "Parent work"]
    let pId = head (words pOut)
    (_, cOut, _) <- runIcarium db ["task", "add", "Follow-up found while working it"]
    let cId = head (words cOut)

    (code, out, _) <- runIcarium db ["link", "add", cId, "derived-from", pId]
    code @?= ExitSuccess
    assertBool "link add task derived-from task returns edge id" (not (null out))

    (lCode, lOut, _) <- runIcarium db ["link", "list", "--to", pId]
    lCode @?= ExitSuccess
    assertBool "link list shows the derived-from edge" ("derived-from" `isInfixOf` lOut)

    -- Widening one kind must not widen the table: `supersedes` still has no
    -- task→task shape, and the error has to name the shapes that are allowed.
    (bad, _, bErr) <- runIcarium db ["link", "add", cId, "supersedes", pId]
    bad @?= ExitFailure 2
    assertBool "error names the expected shape" ("context -> context" `isInfixOf` bErr)

testCtxChildren :: IO ()
testCtxChildren = withTempDb $ \db -> do
    (_, pOut, _) <- runIcarium db ["ctx", "add", "Parent context"]
    let pId = head (words pOut)
    (_, cOut, _) <- runIcarium db ["ctx", "add", "Child context A"]
    let cId = head (words cOut)
    (_, dOut, _) <- runIcarium db ["ctx", "add", "Child context B"]
    let dId = head (words dOut)

    _ <- runIcarium db ["link", "add", cId, "derived-from", pId]
    _ <- runIcarium db ["link", "add", dId, "references", pId]

    (code, out, _) <- runIcarium db ["ctx", "children", pId]
    code @?= ExitSuccess
    assertBool "children shows child A" ("Child context A" `isInfixOf` out)
    assertBool "children shows child B" ("Child context B" `isInfixOf` out)
    assertBool "children shows derived-from kind" ("derived-from" `isInfixOf` out)
    assertBool "children shows references kind" ("references" `isInfixOf` out)

    (fCode, fOut, _) <- runIcarium db ["ctx", "children", pId, "--kind", "derived-from"]
    fCode @?= ExitSuccess
    assertBool "--kind derived-from shows child A" ("Child context A" `isInfixOf` fOut)
    assertBool "--kind derived-from excludes child B" (not ("Child context B" `isInfixOf` fOut))

    -- no children on dId
    _ <- pure dId
    (eCode, eOut, _) <- runIcarium db ["ctx", "children", cId]
    eCode @?= ExitSuccess
    assertBool "leaf node reports no children" ("(no children)" `isInfixOf` eOut)

testCtxTree :: IO ()
testCtxTree = withTempDb $ \db -> do
    (_, rOut, _) <- runIcarium db ["ctx", "add", "Root"]
    let rId = head (words rOut)
    (_, cOut, _) <- runIcarium db ["ctx", "add", "Child"]
    let cId = head (words cOut)
    (_, gOut, _) <- runIcarium db ["ctx", "add", "Grandchild"]
    let gId = head (words gOut)

    _ <- runIcarium db ["link", "add", cId, "derived-from", rId]
    _ <- runIcarium db ["link", "add", gId, "derived-from", cId]

    (code, out, _) <- runIcarium db ["ctx", "tree", rId]
    code @?= ExitSuccess
    assertBool "tree root shows Root" ("Root" `isInfixOf` out)
    assertBool "tree shows Child" ("Child" `isInfixOf` out)
    assertBool "tree shows Grandchild" ("Grandchild" `isInfixOf` out)

    -- cycle detection: link grandchild back to root
    _ <- runIcarium db ["link", "add", rId, "references", gId]
    (cycCode, cycOut, _) <- runIcarium db ["ctx", "tree", rId]
    cycCode @?= ExitSuccess
    assertBool "cycle detected and noted" ("[cycle:" `isInfixOf` cycOut)

    pure ()

testCtxChildrenJson :: IO ()
testCtxChildrenJson = withTempDb $ \db -> do
    (_, pOut, _) <- runIcarium db ["ctx", "add", "Parent context"]
    let pId = head (words pOut)
    (_, cOut, _) <- runIcarium db ["ctx", "add", "Child context A"]
    let cId = head (words cOut)
    (_, dOut, _) <- runIcarium db ["ctx", "add", "Child context B"]
    let dId = head (words dOut)

    _ <- runIcarium db ["link", "add", cId, "derived-from", pId]
    _ <- runIcarium db ["link", "add", dId, "references", pId]

    (code, out, _) <- runIcarium db ["ctx", "children", pId, "--json"]
    code @?= ExitSuccess
    jsonIds out @?= [cId, dId]
    let rows = map expectObject (jsonArray out)
    map (expectField "kind") rows @?= ["derived-from", "references"]
    map (expectField "title") rows @?= ["Child context A", "Child context B"]

    (fCode, fOut, _) <- runIcarium db ["ctx", "children", pId, "--kind", "references", "--json"]
    fCode @?= ExitSuccess
    jsonIds fOut @?= [dId]

    -- a leaf is an empty array, not prose
    (eCode, eOut, _) <- runIcarium db ["ctx", "children", cId, "--json"]
    eCode @?= ExitSuccess
    jsonIds eOut @?= []

testCtxTreeJson :: IO ()
testCtxTreeJson = withTempDb $ \db -> do
    (_, rOut, _) <- runIcarium db ["ctx", "add", "Root"]
    let rId = head (words rOut)
    (_, cOut, _) <- runIcarium db ["ctx", "add", "Child"]
    let cId = head (words cOut)
    (_, gOut, _) <- runIcarium db ["ctx", "add", "Grandchild"]
    let gId = head (words gOut)

    _ <- runIcarium db ["link", "add", cId, "derived-from", rId]
    _ <- runIcarium db ["link", "add", gId, "derived-from", cId]

    (code, out, _) <- runIcarium db ["ctx", "tree", rId, "--json"]
    code @?= ExitSuccess
    let root = expectObject (decodeOut out)
    expectField "id" root @?= rId
    expectField "title" root @?= "Root"
    let [child] = map expectObject (childrenOf root)
    expectField "id" child @?= cId
    expectField "kind" child @?= "derived-from"
    let [grandchild] = map expectObject (childrenOf child)
    expectField "id" grandchild @?= gId
    childrenOf grandchild @?= []
    boolField "cycle" grandchild @?= False

    -- cycle: grandchild references root, so root reappears and stops there
    _ <- runIcarium db ["link", "add", rId, "references", gId]
    (cycCode, cycOut, _) <- runIcarium db ["ctx", "tree", rId, "--json"]
    cycCode @?= ExitSuccess
    -- root → Child → Grandchild → root again, where the walk stops
    let firstChild = expectObject . head . childrenOf
        repeated = firstChild (firstChild (firstChild (expectObject (decodeOut cycOut))))
    expectField "id" repeated @?= rId
    boolField "cycle" repeated @?= True
    childrenOf repeated @?= []

jsonArray :: String -> [Value]
jsonArray out = case decodeOut out of
    Array vs -> toList vs
    v -> error ("expected a JSON array, got: " <> show v)

childrenOf :: Object -> [Value]
childrenOf o = case KM.lookup "children" o of
    Just (Array vs) -> toList vs
    other -> error ("expected a children array, got: " <> show other)

boolField :: Key -> Object -> Bool
boolField k o = case KM.lookup k o of
    Just (Bool b) -> b
    other -> error ("expected bool field " <> show k <> ", got: " <> show other)

testCtxExists :: IO ()
testCtxExists = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["ctx", "add", "Exists context"]
    let cxid = head (words addOut)

    -- found: full id → exit 0
    (foundCode, foundOut, _) <- runIcarium db ["ctx", "exists", cxid]
    foundCode @?= ExitSuccess
    foundOut @?= ""

    -- found: prefix → exit 0
    (prefCode, _, _) <- runIcarium db ["ctx", "exists", take 10 cxid]
    prefCode @?= ExitSuccess

    -- not found → exit 1
    (missCode, _, _) <- runIcarium db ["ctx", "exists", "01ZZZZZZZZZZZZZZZZZZZZZZZZ"]
    missCode @?= ExitFailure 1

    -- ambiguous: add a second context and use a shared prefix
    (_, addOut2, _) <- runIcarium db ["ctx", "add", "Exists context 2"]
    let cxid2 = head (words addOut2)
    let sharedPrefix = take 5 cxid
    when (sharedPrefix == take 5 cxid2) $ do
        (ambCode, _, ambErr) <- runIcarium db ["ctx", "exists", sharedPrefix]
        ambCode @?= ExitFailure 2
        assertBool "stderr mentions ambiguous" ("ambiguous" `isInfixOf` ambErr)

testCtxExistsVerbose :: IO ()
testCtxExistsVerbose = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["ctx", "add", "Verbose exists context"]
    let cxid = head (words addOut)

    (code, out, _) <- runIcarium db ["ctx", "exists", "--verbose", take 10 cxid]
    code @?= ExitSuccess
    assertBool "verbose output contains full id" (cxid `isInfixOf` out)

testCtxJson :: IO ()
testCtxJson = withTempDb $ \db -> do
    (emptyCode, emptyOut, _) <- runIcarium db ["ctx", "list", "--json"]
    emptyCode @?= ExitSuccess
    jsonIds emptyOut @?= []

    (_, addOut, _) <- runIcarium db ["ctx", "add", "Json context", "--body", "unmistakable body prose"]
    let cxid = head (words addOut)

    (lCode, lOut, _) <- runIcarium db ["ctx", "list", "--json"]
    lCode @?= ExitSuccess
    jsonIds lOut @?= [cxid]

    (sCode, sOut, _) <- runIcarium db ["ctx", "show", take 10 cxid, "--json"]
    sCode @?= ExitSuccess
    let o = expectObject (decodeOut sOut)
    expectField "id" o @?= cxid
    assertBool "show carries body_path" (KM.member "body_path" o)
    assertBool "show omits body content" (not ("unmistakable body prose" `isInfixOf` sOut))

testCtxCurateLifecycle :: IO ()
testCtxCurateLifecycle = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["ctx", "add", "curated entry", "--body", "claim"]
    let cid = take 10 (head (words addOut))
    -- never curated: current, no curated line
    (_, sOut0, _) <- runIcarium db ["ctx", "show", cid]
    assertBool "starts current" ("status:   current" `isInfixOf` sOut0)
    assertBool "no curated line before any event" (not ("curated:" `isInfixOf` sOut0))
    -- guidance retires the entry and records the artifact
    (gCode, _, _) <- runIcarium db ["ctx", "curate", cid, "guidance", "--artifact", "docs/foo.md"]
    gCode @?= ExitSuccess
    (_, sOut1, _) <- runIcarium db ["ctx", "show", cid]
    assertBool "guidance retires" ("status:   retired" `isInfixOf` sOut1)
    assertBool "latest event rendered" ("curated:  guidance" `isInfixOf` sOut1)
    assertBool "artifact rendered" ("artifact: docs/foo.md" `isInfixOf` sOut1)
    -- retired entries hidden from ctx list by default, shown with --retired
    (_, lOut, _) <- runIcarium db ["ctx", "list"]
    assertBool "retired hidden from list" (not ("curated entry" `isInfixOf` lOut))
    (_, lrOut, _) <- runIcarium db ["ctx", "list", "--retired"]
    assertBool "shown under --retired" ("curated entry" `isInfixOf` lrOut)
    assertBool "[retired] marker" ("[retired]" `isInfixOf` lrOut)
    -- a later keep event revives the entry
    (kCode, _, _) <- runIcarium db ["ctx", "curate", cid, "keep"]
    kCode @?= ExitSuccess
    (_, sOut2, _) <- runIcarium db ["ctx", "show", cid]
    assertBool "keep revives" ("status:   current" `isInfixOf` sOut2)
    -- old flags are gone
    (uCode, _, _) <- runIcarium db ["ctx", "update", cid, "--stale"]
    assertBool "ctx update --stale removed" (uCode /= ExitSuccess)

testCtxCurateValidation :: IO ()
testCtxCurateValidation = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["ctx", "add", "validated entry", "--body", "claim"]
    let cid = take 10 (head (words addOut))
    -- guidance/rule/refactor require an artifact; error names the fix
    (c1, _, e1) <- runIcarium db ["ctx", "curate", cid, "guidance"]
    c1 @?= ExitFailure 2
    assertBool "error names --artifact" ("--artifact" `isInfixOf` e1)
    -- refactor artifact must resolve to a task id
    (c2, _, e2) <- runIcarium db ["ctx", "curate", cid, "refactor", "--artifact", "nosuchtask"]
    c2 @?= ExitFailure 2
    assertBool "refactor rejects unknown task" ("task" `isInfixOf` e2)
    (_, tOut, _) <- runIcarium db ["task", "add", "extracted refactor", "--state", "ready-headless"]
    let tid = take 10 (head (words tOut))
    (c3, _, _) <- runIcarium db ["ctx", "curate", cid, "refactor", "--artifact", tid]
    c3 @?= ExitSuccess
    -- keep rejects an artifact
    (c4, _, e4) <- runIcarium db ["ctx", "curate", cid, "keep", "--artifact", "docs/x.md"]
    c4 @?= ExitFailure 2
    assertBool "keep rejects artifact" ("keep" `isInfixOf` e4)
    -- stale artifact, when given, must resolve to a context entry
    (c5, _, e5) <- runIcarium db ["ctx", "curate", cid, "stale", "--artifact", "nosuchctx"]
    c5 @?= ExitFailure 2
    assertBool "stale rejects unknown ctx artifact" ("context" `isInfixOf` e5)
    -- bad disposition is a usage error listing the vocabulary
    (c6, _, e6) <- runIcarium db ["ctx", "curate", cid, "bogus"]
    assertBool "bad disposition rejected" (c6 /= ExitSuccess)
    assertBool "error lists dispositions" ("guidance" `isInfixOf` e6)

testCtxCurateQueue :: IO ()
testCtxCurateQueue = withTempDb $ \db -> do
    (_, aOut, _) <- runIcarium db ["ctx", "add", "queue alpha entry"]
    let aId = take 10 (head (words aOut))
    (_, bOut, _) <- runIcarium db ["ctx", "add", "queue beta entry"]
    let bId = take 10 (head (words bOut))
    (_, _, _) <- runIcarium db ["ctx", "curate", bId, "keep"]
    -- bare queue: never-curated only
    (code, out, _) <- runIcarium db ["ctx", "curate"]
    code @?= ExitSuccess
    assertBool "never-curated entry queued" (aId `isInfixOf` out)
    assertBool "never-curated label shown" ("never curated" `isInfixOf` out)
    assertBool "curated entry not in bare queue" (not (bId `isInfixOf` out))
    -- --older-than 0 adds the aged entry with its last disposition
    (code2, out2, _) <- runIcarium db ["ctx", "curate", "--older-than", "0"]
    code2 @?= ExitSuccess
    assertBool "aged entry joins queue" (bId `isInfixOf` out2)
    assertBool "aged entry shows last disposition" ("keep" `isInfixOf` out2)
    -- --json is machine-readable with last_curation null / object
    (code3, out3, _) <- runIcarium db ["ctx", "curate", "--older-than", "0", "--json"]
    code3 @?= ExitSuccess
    case decode (BLC.pack out3) :: Maybe [Object] of
        Nothing -> assertBool "queue --json parses" False
        Just objs -> do
            length objs @?= 2
            let lastCurations = [KM.lookup "last_curation" o | o <- objs]
            assertBool "one never-curated (null)" (Just Null `elem` lastCurations)
    -- empty queue exits 0
    (_, _, _) <- runIcarium db ["ctx", "curate", aId, "keep"]
    (code4, out4, _) <- runIcarium db ["ctx", "curate"]
    code4 @?= ExitSuccess
    assertBool "empty queue message" ("nothing to curate" `isInfixOf` out4)
