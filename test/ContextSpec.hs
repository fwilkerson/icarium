{- | Which context entries reach a prompt: category-matched auto-pull, the
derived-retirement rules that suppress them (ADR 0001), and the provenance
column that attributes an entry to the run that wrote it.
-}
module ContextSpec (tests) where

import Control.Monad (forM, void)
import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple (Connection, Only (..), Query (..), execute, query_)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Icarium.Id (newId)
import Icarium.Render (renderTaskPrompt)
import Icarium.Repo.Category qualified as RC
import Icarium.Repo.Context qualified as RK
import Icarium.Repo.Curation qualified as RCur
import Icarium.Repo.Edge qualified as RE
import Icarium.Types

import TestHelpers

tests :: TestTree
tests =
    testGroup
        "context"
        [ testGroup
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
            ]
        , testGroup
            "context provenance"
            [ testCase "overlapping dispatches each attribute their own entries" testCtxProvenanceOverlappingRuns
            ]
        ]

-- =============================================================
-- categoryMatchedContexts
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
        RC.attachContextCategory c kid domCat
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
-- Curation (ADR 0001)
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
    tid <- mkTaskRow c title
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

-- =============================================================
-- Provenance
-- =============================================================

{- | Scenario: two dispatches on one task overlap, and both write ctx. The
created_at window this replaced could not separate runs that share a range;
each run now reads its own rows off the provenance column.
-}
testCtxProvenanceOverlappingRuns :: IO ()
testCtxProvenanceOverlappingRuns = withTestDb $ \c -> do
    tid <- mkTaskRow c "Racing task"
    d1 <- newId
    d2 <- newId
    insertTestDispatch c d1 tid
    insertTestDispatch c d2 tid
    first <- mkCtxFrom c "from run one" (Just d1)
    second <- mkCtxFrom c "from run two" (Just d2)
    _handFiled <- mkCtxFrom c "filed by a human" Nothing

    one <- RK.contextsFromDispatch c d1
    map contextId one @?= [first]
    two <- RK.contextsFromDispatch c d2
    map contextId two @?= [second]
