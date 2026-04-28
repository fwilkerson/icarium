module RenderSpec (tests) where

import           Data.Text        (Text)
import qualified Data.Text        as T
import           Test.Tasty       (TestTree, testGroup)
import           Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import           Icarium.Render   (renderTaskHuman, renderTaskList)
import qualified Icarium.Render
import           Icarium.Types
import           TestHelpers      (minTask)

tests :: TestTree
tests = testGroup "render"
    [ testGroup "mkBar 5-cell Unicode bar" testMkBar
    , testGroup "renderTaskList grouped view"
        [ testCase "groups in READY/PLANNED/BLOCKED/IDEA order with counts" testGroupedHeaders
        , testCase "single-state filter suppresses group header"            testSingleStateNoHeader
        , testCase "blocked row replaces bar with truncated reason"         testBlockedReason
        , testCase "edge counts omitted when both zero, shown otherwise"    testEdgeCountFormat
        , testCase "NULL priority sorts last within group"                  testNullPrioritySort
        , testCase "90-char title truncated to 72 chars with UTF-8 ellipsis" testTitleTruncatedUtf8
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

mustJust :: String -> Maybe a -> IO a
mustJust msg = maybe (assertFailure msg) pure

-- =============================================================
-- mkBar tests
-- =============================================================

testMkBar :: [TestTree]
testMkBar =
    [ testCase "Nothing" $ Icarium.Render.mkBar Nothing   @?= "□ □ □ □ □"
    , testCase "5"       $ Icarium.Render.mkBar (Just 5)  @?= "■ ■ ◧ □ □"
    , testCase "10"      $ Icarium.Render.mkBar (Just 10) @?= "■ ■ ■ ■ ■"
    ]

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
    iReady   <- mustJust "READY position"   (lengthBefore "READY"   out)
    iPlanned <- mustJust "PLANNED position" (lengthBefore "PLANNED" out)
    iBlocked <- mustJust "BLOCKED position" (lengthBefore "BLOCKED" out)
    iIdea    <- mustJust "IDEA position"    (lengthBefore "IDEA"    out)
    assertBool "READY before PLANNED"   (iReady   < iPlanned)
    assertBool "PLANNED before BLOCKED" (iPlanned < iBlocked)
    assertBool "BLOCKED before IDEA"    (iBlocked < iIdea)
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

testNullPrioritySort :: IO ()
testNullPrioritySort = do
    let rows = [ mkRow "01ABCDEFGH01" "null-pri" Idea Nothing  [] 0 0 Nothing
               , mkRow "01ABCDEFGH02" "low-pri"  Idea (Just 1) [] 0 0 Nothing
               , mkRow "01ABCDEFGH03" "high-pri" Idea (Just 9) [] 0 0 Nothing
               ]
        out  = renderTaskList True rows [Idea]
        ls   = filter (\l -> "pri" `T.isInfixOf` l) (T.lines out)
    assertBool "non-empty rendered rows" (not (null ls))
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
    assertBool "row contains UTF-8 ellipsis" ("…" `T.isInfixOf` dataRow)
    assertBool "row does not contain raw 90-char title" (not (longTitle `T.isInfixOf` dataRow))
    let titlePart = T.take Icarium.Render.recommendedTitleMax (T.drop 14 dataRow)
    assertBool "title column is exactly 72 chars" (T.length titlePart == Icarium.Render.recommendedTitleMax)

-- =============================================================
-- task show links section tests
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

depTask :: TaskState -> Task
depTask st = minTask
    { taskId          = "01DDDD000000000000000000DD"
    , taskTitle       = "Dep task"
    , taskState       = st
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
    assertBool "## Links header"    ("## Links"  `T.isInfixOf` out)
    assertBool "(none) line"        ("(none)"    `T.isInfixOf` out)
    assertBool "no depends-on edge" (not ("depends-on" `T.isInfixOf` out))
    assertBool "no references edge" (not ("references" `T.isInfixOf` out))

testLinksOnlyDeps :: IO ()
testLinksOnlyDeps = do
    let dep = depTask Planned
        out = renderTaskHuman True minTask [] [dep] []
    assertBool "## Links header"   ("## Links"   `T.isInfixOf` out)
    assertBool "depends-on edge"   ("depends-on" `T.isInfixOf` out)
    assertBool "dep id prefix"     (T.take 10 (taskId dep) `T.isInfixOf` out)
    assertBool "dep state"         ("[planned]"  `T.isInfixOf` out)
    assertBool "no references"     (not ("references" `T.isInfixOf` out))
    assertBool "last glyph is └─" ("└─"         `T.isInfixOf` out)

testLinksOnlyRefs :: IO ()
testLinksOnlyRefs = do
    let ref = refKnow False
        out = renderTaskHuman True minTask [ref] [] []
    assertBool "## Links header"   ("## Links"   `T.isInfixOf` out)
    assertBool "references edge"   ("references" `T.isInfixOf` out)
    assertBool "ref id prefix"     (T.take 10 (knowledgeId ref) `T.isInfixOf` out)
    assertBool "no [STALE] suffix" (not ("[STALE]"    `T.isInfixOf` out))
    assertBool "no depends-on"     (not ("depends-on" `T.isInfixOf` out))
    assertBool "last glyph is └─" ("└─"         `T.isInfixOf` out)

testLinksBothKinds :: IO ()
testLinksBothKinds = do
    let dep = depTask Ready
        ref = refKnow False
        out = renderTaskHuman True minTask [ref] [dep] []
    assertBool "depends-on edge" ("depends-on" `T.isInfixOf` out)
    assertBool "references edge" ("references" `T.isInfixOf` out)
    iDep <- mustJust "depends-on position" (posOf "depends-on" out)
    iRef <- mustJust "references position" (posOf "references" out)
    assertBool "depends-on before references" (iDep < iRef)
    assertBool "branch glyph ├─"  ("├─" `T.isInfixOf` out)
    assertBool "last glyph └─"    ("└─" `T.isInfixOf` out)
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
