module RenderSpec (tests) where

import Data.Text (Text)
import Data.Text qualified as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import Icarium.Render (fmtSecs, renderTaskHuman, renderTaskList)
import Icarium.Render qualified
import Icarium.Types
import TestHelpers (minTask)

tests :: TestTree
tests =
    testGroup
        "render"
        [ testGroup "fmtSecs" testFmtSecs
        , testGroup "mkBar 5-cell Unicode bar" testMkBar
        , testGroup
            "renderTaskList flat view"
            [ testCase "flat: all rows present with state badges, no group headers" testFlatStateBadges
            , testCase "flat: no group headers ever" testNoGroupHeaders
            , testCase "blocked row shows bar plus hang-line reason" testBlockedReason
            , testCase "edge counts omitted when both zero, shown otherwise" testEdgeCountFormat
            , testCase "NULL priority sorts last" testNullPrioritySort
            , testCase "90-char title truncated to 72 chars with UTF-8 ellipsis" testTitleTruncatedUtf8
            ]
        , testGroup
            "renderDispatchList"
            [ testCase "title, duration, outcome badge, know badge, no branch/header" testDispatchListFormat
            ]
        , testGroup
            "task show links section"
            [ testCase "no edges renders (none)" testLinksNoEdges
            , testCase "only depends-on edges" testLinksOnlyDeps
            , testCase "only references edges" testLinksOnlyRefs
            , testCase "both kinds present, deps before refs" testLinksBothKinds
            , testCase "stale knowledge gets [STALE] suffix" testLinksStaleKnowledge
            , testCase "done task gets [done] suffix" testLinksTaskDone
            , testCase "blocked task gets [blocked] suffix" testLinksTaskBlocked
            , testCase "ASCII mode uses +- and \\- glyphs" testLinksAscii
            ]
        ]

mustJust :: String -> Maybe a -> IO a
mustJust msg = maybe (assertFailure msg) pure

-- =============================================================
-- fmtSecs tests
-- =============================================================

testFmtSecs :: [TestTree]
testFmtSecs =
    [ testCase (show s <> " -> " <> show expected) (fmtSecs s @?= expected)
    | (s, expected) <-
        [ (0, "0s")
        , (59, "59s")
        , (60, "1m")
        , (3599, "59m")
        , (3600, "1h 0m")
        , (3661, "1h 1m")
        ]
    ]

-- =============================================================
-- mkBar tests
-- =============================================================

testMkBar :: [TestTree]
testMkBar =
    [ testCase "Nothing" $ Icarium.Render.mkBar Nothing @?= "□ □ □ □ □"
    , testCase "5" $ Icarium.Render.mkBar (Just 5) @?= "■ ■ ◧ □ □"
    , testCase "10" $ Icarium.Render.mkBar (Just 10) @?= "■ ■ ■ ■ ■"
    ]

-- =============================================================
-- renderTaskList flat view tests
-- =============================================================

mkRow :: Text -> Text -> TaskState -> Maybe Int -> [Category] -> Int -> Int -> Maybe Text -> Icarium.Render.TaskRow
mkRow tid title st pri cats deps refs blockReason =
    Icarium.Render.TaskRow
        { Icarium.Render.trTask =
            Task
                { taskId = tid
                , taskTitle = title
                , taskBody = ""
                , taskState = st
                , taskPriority = pri
                , taskBlockReason = blockReason
                , taskCreatedAt = "2026-04-26 00:00:00"
                , taskUpdatedAt = "2026-04-26 00:00:00"
                }
        , Icarium.Render.trCats = cats
        , Icarium.Render.trDeps = deps
        , Icarium.Render.trRefs = refs
        }

testFlatStateBadges :: IO ()
testFlatStateBadges = do
    let rows =
            [ mkRow "01ABCDEFGH01" "ready task" Ready (Just 5) [] 0 0 Nothing
            , mkRow "01ABCDEFGH02" "planned task" Planned (Just 3) [] 0 0 Nothing
            , mkRow "01ABCDEFGH03" "idea task" Idea Nothing [] 0 0 Nothing
            ]
        out = renderTaskList True rows
    assertBool "ready task present" ("ready task" `T.isInfixOf` out)
    assertBool "planned task present" ("planned task" `T.isInfixOf` out)
    assertBool "idea task present" ("idea task" `T.isInfixOf` out)
    assertBool "[ready] badge" ("[ready]" `T.isInfixOf` out)
    assertBool "[planned] badge" ("[planned]" `T.isInfixOf` out)
    assertBool "[idea] badge" ("[idea]" `T.isInfixOf` out)
    -- Priority sorted: higher priority first
    let ls = T.lines out
        idx t = head [i | (i, l) <- zip [0 :: Int ..] ls, t `T.isInfixOf` l]
    assertBool "ready (pri 5) before planned (pri 3)" (idx "ready task" < idx "planned task")
    assertBool "planned (pri 3) before idea (null)" (idx "planned task" < idx "idea task")

testNoGroupHeaders :: IO ()
testNoGroupHeaders = do
    let rows = [mkRow "01ABCDEFGH01" "only ready" Ready (Just 5) [] 0 0 Nothing]
        out = renderTaskList True rows
    assertBool "no READY header" (not ("READY" `T.isInfixOf` out))
    assertBool "no PLANNED header" (not ("PLANNED" `T.isInfixOf` out))
    assertBool "still has the row" ("only ready" `T.isInfixOf` out)

testBlockedReason :: IO ()
testBlockedReason = do
    let longReason = T.replicate 80 "x"
        rows =
            [ mkRow "01ABCDEFGH01" "short" Blocked (Just 5) [] 0 0 (Just "nope")
            , mkRow "01ABCDEFGH02" "long" Blocked (Just 5) [] 0 0 (Just longReason)
            ]
        out = renderTaskList True rows
    assertBool "shows short reason" ("nope" `T.isInfixOf` out)
    assertBool "priority bar shown" ("■ ■ ◧ □ □" `T.isInfixOf` out)
    assertBool "long reason truncated" ((T.replicate 57 "x" <> "...") `T.isInfixOf` out)
    let hangLines = filter (T.isPrefixOf (T.replicate 14 " ")) (T.lines out)
    assertBool "hang line present" (not (null hangLines))
    assertBool "hang line contains reason" (any ("nope" `T.isInfixOf`) hangLines)

testEdgeCountFormat :: IO ()
testEdgeCountFormat = do
    let rows =
            [ mkRow "01ABCDEFGH01" "no edges" Ready (Just 5) [] 0 0 Nothing
            , mkRow "01ABCDEFGH02" "deps only" Ready (Just 5) [] 2 0 Nothing
            , mkRow "01ABCDEFGH03" "refs only" Ready (Just 5) [] 0 3 Nothing
            , mkRow "01ABCDEFGH04" "both" Ready (Just 5) [] 1 4 Nothing
            ]
        out = renderTaskList True rows
        line title = head $ filter (T.isInfixOf title) (T.lines out)
    assertBool "no edges → no bracket" (not ("[deps" `T.isInfixOf` line "no edges") && not ("[refs" `T.isInfixOf` line "no edges"))
    assertBool "deps-only" ("[deps:2]" `T.isInfixOf` line "deps only")
    assertBool "refs-only" ("[refs:3]" `T.isInfixOf` line "refs only")
    assertBool "both" ("[deps:1 refs:4]" `T.isInfixOf` line "both")

testNullPrioritySort :: IO ()
testNullPrioritySort = do
    let rows =
            [ mkRow "01ABCDEFGH01" "null-pri" Idea Nothing [] 0 0 Nothing
            , mkRow "01ABCDEFGH02" "low-pri" Idea (Just 1) [] 0 0 Nothing
            , mkRow "01ABCDEFGH03" "high-pri" Idea (Just 9) [] 0 0 Nothing
            ]
        out = renderTaskList True rows
        ls = filter (\l -> "pri" `T.isInfixOf` l) (T.lines out)
    assertBool "non-empty rendered rows" (not (null ls))
    let idx t = head [i | (i, l) <- zip [0 :: Int ..] ls, t `T.isInfixOf` l]
    (idx "high-pri" < idx "low-pri") @?= True
    (idx "low-pri" < idx "null-pri") @?= True

testTitleTruncatedUtf8 :: IO ()
testTitleTruncatedUtf8 = do
    let longTitle = T.replicate 90 "x"
        rows = [mkRow "01ABCDEFGH01" longTitle Ready (Just 5) [] 0 0 Nothing]
        out = renderTaskList True rows
        ls = T.lines out
    assertBool "has a data row" (not (null ls))
    let dataRow = head ls
    assertBool "row contains UTF-8 ellipsis" ("…" `T.isInfixOf` dataRow)
    assertBool "row does not contain raw 90-char title" (not (longTitle `T.isInfixOf` dataRow))
    let titlePart = T.take Icarium.Render.recommendedTitleMax (T.drop 14 dataRow)
    assertBool "title column is exactly 72 chars" (T.length titlePart == Icarium.Render.recommendedTitleMax)

-- =============================================================
-- renderDispatchList tests
-- =============================================================

minDispatch :: Text -> Text -> Maybe DispatchOutcome -> Dispatch
minDispatch did tid outcome =
    Dispatch
        { dispatchId = did
        , dispatchTaskId = tid
        , dispatchBranch = "dispatch/" <> did
        , dispatchBaseBranch = "main"
        , dispatchBaseSha = "abc"
        , dispatchPid = Nothing
        , dispatchModel = "claude-sonnet-4-6"
        , dispatchEffort = Medium
        , dispatchStartedAt = "2026-04-01 10:00:00"
        , dispatchHeartbeat = "2026-04-01 10:05:00"
        , dispatchEndedAt = case outcome of Nothing -> Nothing; Just _ -> Just "2026-04-01 10:12:00"
        , dispatchOutcome = outcome
        , dispatchMergeSha = Nothing
        , dispatchLastCommit = Nothing
        , dispatchNotes = Nothing
        , dispatchLogPath = Nothing
        , dispatchTokensIn = Nothing
        , dispatchTokensOut = Nothing
        , dispatchTokensCacheRead = Nothing
        }

testDispatchListFormat :: IO ()
testDispatchListFormat = do
    let d1 = minDispatch "01AAA0000000000000000000AA" "01TTT0000000000000000000AA" (Just OSuccess)
        d2 = minDispatch "01BBB0000000000000000000BB" "01TTT0000000000000000000BB" (Just OFailure)
        d3 = minDispatch "01CCC0000000000000000000CC" "01TTT0000000000000000000CC" Nothing
        rows =
            [ Icarium.Render.DispatchRow{Icarium.Render.drDispatch = d1, Icarium.Render.drTaskTitle = "Add unified search", Icarium.Render.drKnowCount = 1, Icarium.Render.drDuration = "12m"}
            , Icarium.Render.DispatchRow{Icarium.Render.drDispatch = d2, Icarium.Render.drTaskTitle = "FTS5 backend decision", Icarium.Render.drKnowCount = 0, Icarium.Render.drDuration = "47m"}
            , Icarium.Render.DispatchRow{Icarium.Render.drDispatch = d3, Icarium.Render.drTaskTitle = "Search CLI shape", Icarium.Render.drKnowCount = 0, Icarium.Render.drDuration = "3m (running)"}
            ]
        out = Icarium.Render.renderDispatchList True rows
    assertBool "dispatch-id prefix in output" ("01AAA00000" `T.isInfixOf` out)
    assertBool "task title present" ("Add unified search" `T.isInfixOf` out)
    assertBool "[know:1] badge shown" ("[know:1]" `T.isInfixOf` out)
    assertBool "no [know:0] badge" (not ("[know:0]" `T.isInfixOf` out))
    assertBool "[success] outcome badge" ("[success]" `T.isInfixOf` out)
    assertBool "[failure] outcome badge" ("[failure]" `T.isInfixOf` out)
    assertBool "[open] outcome badge" ("[open]" `T.isInfixOf` out)
    assertBool "branch column absent" (not ("dispatch/01AAA" `T.isInfixOf` out))
    assertBool "no column header row" (not ("task_id" `T.isInfixOf` out))
    assertBool "duration shown" ("12m" `T.isInfixOf` out)
    assertBool "(running) suffix on open" ("(running)" `T.isInfixOf` out)

-- =============================================================
-- task show links section tests
-- =============================================================

minKnowledge :: Knowledge
minKnowledge =
    Knowledge
        { knowledgeId = "01KNOW000000000000000000KK"
        , knowledgeTitle = "Test knowledge"
        , knowledgeBody = "Know body"
        , knowledgeStale = False
        , knowledgeCreatedAt = "2026-01-01T00:00:00Z"
        , knowledgeUpdatedAt = "2026-01-01T00:00:00Z"
        }

depTask :: TaskState -> Task
depTask st =
    minTask
        { taskId = "01DDDD000000000000000000DD"
        , taskTitle = "Dep task"
        , taskState = st
        , taskBlockReason = Nothing
        }

refKnow :: Bool -> Knowledge
refKnow stale =
    minKnowledge
        { knowledgeId = "01RRRR000000000000000000RR"
        , knowledgeTitle = "Ref knowledge"
        , knowledgeStale = stale
        }

testLinksNoEdges :: IO ()
testLinksNoEdges = do
    let out = renderTaskHuman True minTask [] [] []
    assertBool "## Links header" ("## Links" `T.isInfixOf` out)
    assertBool "(none) line" ("(none)" `T.isInfixOf` out)
    assertBool "no depends-on edge" (not ("depends-on" `T.isInfixOf` out))
    assertBool "no references edge" (not ("references" `T.isInfixOf` out))

testLinksOnlyDeps :: IO ()
testLinksOnlyDeps = do
    let dep = depTask Planned
        out = renderTaskHuman True minTask [] [dep] []
    assertBool "## Links header" ("## Links" `T.isInfixOf` out)
    assertBool "depends-on edge" ("depends-on" `T.isInfixOf` out)
    assertBool "dep id prefix" (T.take 10 (taskId dep) `T.isInfixOf` out)
    assertBool "dep state" ("[planned]" `T.isInfixOf` out)
    assertBool "no references" (not ("references" `T.isInfixOf` out))
    assertBool "last glyph is └─" ("└─" `T.isInfixOf` out)

testLinksOnlyRefs :: IO ()
testLinksOnlyRefs = do
    let ref = refKnow False
        out = renderTaskHuman True minTask [ref] [] []
    assertBool "## Links header" ("## Links" `T.isInfixOf` out)
    assertBool "references edge" ("references" `T.isInfixOf` out)
    assertBool "ref id prefix" (T.take 10 (knowledgeId ref) `T.isInfixOf` out)
    assertBool "no [STALE] suffix" (not ("[STALE]" `T.isInfixOf` out))
    assertBool "no depends-on" (not ("depends-on" `T.isInfixOf` out))
    assertBool "last glyph is └─" ("└─" `T.isInfixOf` out)

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
    assertBool "branch glyph ├─" ("├─" `T.isInfixOf` out)
    assertBool "last glyph └─" ("└─" `T.isInfixOf` out)
  where
    posOf needle h =
        let (pre, suf) = T.breakOn needle h
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
    let dep = (depTask Blocked){taskBlockReason = Just "waiting"}
        out = renderTaskHuman True minTask [] [dep] []
    assertBool "[blocked] suffix" ("[blocked]" `T.isInfixOf` out)

testLinksAscii :: IO ()
testLinksAscii = do
    let dep = depTask Planned
        ref = refKnow False
        out = renderTaskHuman False minTask [ref] [dep] []
    assertBool "ASCII branch glyph +- present" ("+-" `T.isInfixOf` out)
    assertBool "ASCII last glyph \\- present" ("\\-" `T.isInfixOf` out)
    assertBool "no UTF-8 branch glyph" (not ("├─" `T.isInfixOf` out))
    assertBool "no UTF-8 last glyph" (not ("└─" `T.isInfixOf` out))
