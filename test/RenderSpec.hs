{- | The rendering surface. Each top-level renderer gets one golden covering
the shape as a whole; separate cases exist only where an input cannot share
the golden's fixture (over-long text, ASCII mode, absent sections).
-}
module RenderSpec (tests) where

import Data.Text (Text)
import Data.Text qualified as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Icarium.Dispatch.LogResult (LogResult (..), LogUsage (..))
import Icarium.Dispatch.Merge (MergeOutcome (..))
import Icarium.Dispatch.Outcome (DispatchResult (..))
import Icarium.Dispatch.Payload (FutureNote (..), WorkerPayload (..), WorkerStatus (..))
import Icarium.Heartbeat (DispatchHealth (..))
import Icarium.Render (fmtSecs, renderTaskHuman, renderTaskList)
import Icarium.Render qualified
import Icarium.Types
import TestHelpers (minTask)

tests :: TestTree
tests =
    testGroup
        "render"
        [ testGroup "fmtSecs" (map goldenCase fmtSecsCases)
        , testGroup "mkBar 5-cell Unicode bar" (map goldenCase mkBarCases)
        , testGroup
            "renderTaskList flat view"
            [ testCase "golden: badges, priority sort, edge counts, blocked hang-line" testTaskListGolden
            , testCase "over-long title and block reason are both truncated" testTaskListTruncation
            ]
        , testGroup
            "renderDispatchList"
            [ testCase "golden: id, title, duration, ctx badge, outcome badge" testDispatchListGolden
            ]
        , testGroup "renderRunSummary" (map goldenCase runSummaryCases)
        , testGroup
            "dispatch line renderers"
            [ testCase "recovery notes with a surviving worktree" testRecoveryNotesWorktree
            , testCase "recovery notes without a worktree" testRecoveryNotesNoWorktree
            , testCase "landed / parked / merge tally lines" testMergeLines
            ]
        , testGroup
            "renderTaskHuman"
            [ testCase "golden: metadata, categories and the full links tree" testTaskHumanGolden
            , testCase "a task with no edges renders (none)" testLinksNoEdges
            , testCase "ASCII mode uses +- and \\- glyphs" testLinksAscii
            ]
        ]

-- | A table row: the name identifies the input, the pair is the proof.
goldenCase :: (String, Text, Text) -> TestTree
goldenCase (name, actual, expected) = testCase name (actual @?= expected)

fmtSecsCases :: [(String, Text, Text)]
fmtSecsCases =
    [ (show s, fmtSecs s, e)
    | (s, e) <- [(0, "0s"), (59, "59s"), (60, "1m"), (3599, "59m"), (3600, "1h 0m"), (3661, "1h 1m")]
    ]

mkBarCases :: [(String, Text, Text)]
mkBarCases =
    [ ("Nothing", Icarium.Render.mkBar Nothing, "□ □ □ □ □")
    , ("5", Icarium.Render.mkBar (Just 5), "■ ■ ◧ □ □")
    , ("10", Icarium.Render.mkBar (Just 10), "■ ■ ■ ■ ■")
    ]

-- =============================================================
-- renderTaskList
-- =============================================================

mkRow :: Text -> Text -> TaskState -> Maybe Int -> [Category] -> Int -> Int -> Maybe Text -> Icarium.Render.TaskRow
mkRow tid title st pri cats deps refs blockReason =
    Icarium.Render.TaskRow
        { Icarium.Render.trTask =
            minTask
                { taskId = tid
                , taskTitle = title
                , taskBody = ""
                , taskState = st
                , taskPriority = pri
                , taskBlockReason = blockReason
                }
        , Icarium.Render.trCats = cats
        , Icarium.Render.trDeps = deps
        , Icarium.Render.trRefs = refs
        }

{- | Deliberately fed out of priority order: the render sorts, and a golden
that arrives pre-sorted would not prove it.
-}
testTaskListGolden :: IO ()
testTaskListGolden = do
    let rows =
            [ mkRow "01DDDDDDDD04" "idea task" Idea Nothing [] 0 0 Nothing
            , mkRow "01BBBBBBBB02" "planned task" Planned (Just 5) [] 2 0 Nothing
            , mkRow "01AAAAAAAA01" "ready task" ReadyHeadless (Just 9) [] 1 4 Nothing
            , mkRow "01CCCCCCCC03" "blocked task" Blocked (Just 3) [] 0 3 (Just "upstream is parked")
            ]
    renderTaskList True rows
        @?= T.unlines
            [ "  01AAAAAAAA  ready task    ■ ■ ■ ■ ◧  [-]  [deps:1 refs:4]  [ready-headless]"
            , "  01BBBBBBBB  planned task  ■ ■ ◧ □ □  [-]  [deps:2]  [planned]"
            , "  01CCCCCCCC  blocked task  ■ ◧ □ □ □  [-]  [refs:3]  [blocked]"
            , "              upstream is parked"
            , "  01DDDDDDDD  idea task     □ □ □ □ □  [-]  [idea]"
            ]

testTaskListTruncation :: IO ()
testTaskListTruncation = do
    let longTitle = T.replicate 90 "x"
        longReason = T.replicate 80 "y"
        out = renderTaskList True [mkRow "01AAAAAAAA01" longTitle Blocked (Just 5) [] 0 0 (Just longReason)]
        mainLine = head (T.lines out)
        hangLine = T.lines out !! 1
    assertBool "title truncated with a UTF-8 ellipsis" ("…" `T.isInfixOf` mainLine)
    assertBool "raw title absent" (not (longTitle `T.isInfixOf` mainLine))
    T.length (T.take Icarium.Render.recommendedTitleMax (T.drop 14 mainLine)) @?= Icarium.Render.recommendedTitleMax
    assertBool "reason truncated at 57 chars" ((T.replicate 57 "y" <> "...") `T.isInfixOf` hangLine)
    assertBool "raw reason absent" (not (longReason `T.isInfixOf` hangLine))

-- =============================================================
-- renderDispatchList
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
        , dispatchReviewVerdict = Nothing
        , dispatchReviewerLogPath = Nothing
        , dispatchMergedAt = Nothing
        , dispatchBodyChanged = Nothing
        }

{- | All four outcome badges in one block. The branch never appears and no
header row is printed — both are visible in the golden by their absence.
-}
testDispatchListGolden :: IO ()
testDispatchListGolden = do
    let row d title ctxCount dur =
            Icarium.Render.DispatchRow
                { Icarium.Render.drDispatch = d
                , Icarium.Render.drTaskTitle = title
                , Icarium.Render.drCtxCount = ctxCount
                , Icarium.Render.drDuration = dur
                }
        merged = (minDispatch "01AAA0000000000000000000AA" "01TTT0000000000000000000AA" (Just OSuccess)){dispatchMergeSha = Just "def456"}
        failed = minDispatch "01BBB0000000000000000000BB" "01TTT0000000000000000000BB" (Just OFailure)
        open = minDispatch "01CCC0000000000000000000CC" "01TTT0000000000000000000CC" Nothing
        parked = minDispatch "01DDD0000000000000000000DD" "01TTT0000000000000000000DD" (Just OSuccess)
    Icarium.Render.renderDispatchList
        True
        [ row merged "Add unified search" 1 "12m"
        , row failed "FTS5 backend decision" 0 "47m"
        , row open "Search CLI shape" 0 "3m (running)"
        , row parked "Parked run" 0 "9m"
        ]
        @?= T.unlines
            [ "  01AAA00000   01TTT00000  Add unified search     12m           [ctx:1]  [success]"
            , "  01BBB00000   01TTT00000  FTS5 backend decision  47m                    [failure]"
            , "  01CCC00000   01TTT00000  Search CLI shape       3m (running)           [open]"
            , "  01DDD00000   01TTT00000  Parked run             9m                     [parked]"
            ]

-- =============================================================
-- renderRunSummary
-- =============================================================

minResult :: DispatchResult
minResult =
    DispatchResult
        { dresDispatchId = Just "01AAA0000000000000000000AA"
        , dresOutcome = OSuccess
        , dresBranch = "dispatch/01AAA0000000000000000000AA"
        , dresNotes = "gates passed"
        , dresLogPath = Nothing
        , dresBaseSha = Just "abc123"
        , dresPayload = Nothing
        , dresTaskTransition = Just (Done, Nothing)
        }

fullLog :: LogResult
fullLog =
    LogResult
        { lrNumTurns = Just 7
        , lrDurationMs = Just 12500
        , lrDurationApiMs = Just 9200
        , lrCostUsd = Just 0.1234
        , lrUsage = Just (LogUsage (Just 100) (Just 200) (Just 300))
        , lrResultText = Nothing
        }

runSummaryCases :: [(String, Text, Text)]
runSummaryCases =
    [
        ( "golden: full block with worker, log and files"
        , Icarium.Render.renderRunSummary
            minResult
                { dresPayload =
                    Just
                        WorkerPayload
                            { wpStatus = WSubmitted
                            , wpBlockReason = Nothing
                            , wpForFutureAgents = [FutureNote "one" "body", FutureNote "two" "body"]
                            }
                }
            (Just fullLog)
            ["src/A.hs", "src/B.hs"]
        , T.unlines
            [ ""
            , "dispatch: 01AAA0000000000000000000AA"
            , "outcome:  success"
            , "branch:   dispatch/01AAA0000000000000000000AA"
            , "notes:    gates passed"
            , "worker:   submitted; 2 for future agents"
            , "turns:    7"
            , "duration: 12.5s (api: 9.2s)"
            , "cost:     $0.1234"
            , "tokens:   in 100 / out 200 / cache 300"
            , "files:    src/A.hs"
            , "          src/B.hs"
            ]
        )
    ,
        ( "no log result and no files omits those lines"
        , Icarium.Render.renderRunSummary minResult Nothing []
        , T.unlines
            [ ""
            , "dispatch: 01AAA0000000000000000000AA"
            , "outcome:  success"
            , "branch:   dispatch/01AAA0000000000000000000AA"
            , "notes:    gates passed"
            ]
        )
    ,
        ( "dry run has no dispatch id"
        , Icarium.Render.renderRunSummary minResult{dresDispatchId = Nothing} Nothing []
        , T.unlines
            [ ""
            , "dispatch: (dry-run)"
            , "outcome:  success"
            , "branch:   dispatch/01AAA0000000000000000000AA"
            , "notes:    gates passed"
            ]
        )
    ,
        ( "more than 10 files truncates with an N more line"
        , Icarium.Render.renderRunSummary minResult Nothing ["src/F" <> T.pack (show n) <> ".hs" | n <- [1 :: Int .. 13]]
        , T.unlines $
            [ ""
            , "dispatch: 01AAA0000000000000000000AA"
            , "outcome:  success"
            , "branch:   dispatch/01AAA0000000000000000000AA"
            , "notes:    gates passed"
            , "files:    src/F1.hs"
            ]
                <> ["          src/F" <> T.pack (show n) <> ".hs" | n <- [2 :: Int .. 10]]
                <> ["          3 more"]
        )
    ]

-- =============================================================
-- dispatch line renderers
-- =============================================================

testRecoveryNotesWorktree :: IO ()
testRecoveryNotesWorktree =
    Icarium.Render.renderRecoveryNotes (DispatchHealth False True) (Just True) "deadbee"
        @?= "interrupted; alive=no; stale=yes; uncommitted=yes; worktree=removed; last_commit=deadbee"

testRecoveryNotesNoWorktree :: IO ()
testRecoveryNotesNoWorktree =
    Icarium.Render.renderRecoveryNotes (DispatchHealth True False) Nothing ""
        @?= "interrupted; alive=yes; stale=no; last_commit="

testMergeLines :: IO ()
testMergeLines = do
    let d = minDispatch "01AAA0000000000000000000AA" "01TTT0000000000000000000AA" (Just OSuccess)
    Icarium.Render.renderLanded d "def4567890abc"
        @?= "merged 01AAA00000: main -> def4567890"
    Icarium.Render.renderStillParked d "gates failed"
        @?= "dispatch 01AAA00000 parked: gates failed; fix and run `icarium dispatch merge 01AAA00000`"
    Icarium.Render.renderMergeAttempt d (MergeBlocked 1 "gates failed")
        @?= "blocked 01AAA00000: gates failed"
    Icarium.Render.renderMergeAttempt d (MergeStopped "no capacity")
        @?= "stopped 01AAA00000: no capacity"
    Icarium.Render.renderMergeTally 3 2 1 0 @?= "2 of 3 landed; 1 still parked"
    Icarium.Render.renderMergeTally 3 3 0 0 @?= "3 of 3 landed"
    Icarium.Render.renderMergeTally 4 1 1 2 @?= "1 of 4 landed; 1 still parked; 2 not attempted"

-- =============================================================
-- renderTaskHuman
-- =============================================================

minContext :: Text -> Text -> Context
minContext cid title =
    Context
        { contextId = cid
        , contextTitle = title
        , contextBody = "Context body"
        , contextCreatedAt = "2026-01-01T00:00:00Z"
        , contextUpdatedAt = "2026-01-01T00:00:00Z"
        }

depTask :: Text -> Text -> TaskState -> Task
depTask tid title st = minTask{taskId = tid, taskTitle = title, taskState = st}

{- | Every optional line and every edge kind at once. The tree orders
depends-on before derived-from before references, ids sorted within a kind,
and only the last edge takes the └─ glyph — all of which the golden pins.
-}
testTaskHumanGolden :: IO ()
testTaskHumanGolden = do
    let t =
            minTask
                { taskState = Blocked
                , taskPriority = Just 7
                , taskBlockReason = Just "upstream is parked"
                , taskNoCommit = True
                , taskClaimedBy = Just "worker-1"
                , taskClaimedAt = Just "2026-01-02T00:00:00Z"
                , taskRouting = Routing{rtModel = Just "claude-opus-5", rtEffort = Just High}
                }
        deps = [depTask "01DEP2000000000000000000BB" "Second dep" Blocked, depTask "01DEP1000000000000000000AA" "First dep" Planned]
        derived = [depTask "01DERIVED000000000000000CC" "Parent task" Done]
        refs = [minContext "01REFB000000000000000000EE" "Retired context", minContext "01REFA000000000000000000DD" "Live context"]
        cats = [Category "01CAT1" Domain "cli", Category "01CAT2" Discipline "haskell"]
    renderTaskHuman True t "/tmp/body.md" refs deps derived cats ["01REFB000000000000000000EE"]
        @?= T.unlines
            [ "id:        01TEST00000000000000000000"
            , "title:     Test task"
            , "state:     blocked"
            , "priority:  7"
            , "block_reason: upstream is parked"
            , "no-commit:   yes"
            , "model:     claude-opus-5"
            , "effort:    high"
            , "owner:     worker-1"
            , "claimed:   2026-01-02T00:00:00Z"
            , "created:   2026-01-01T00:00:00Z"
            , "updated:   2026-01-01T00:00:00Z"
            , "body:      /tmp/body.md"
            , "Categories:"
            , "  domain:     cli"
            , "  discipline: haskell"
            , ""
            , "## Links"
            , ""
            , "01TEST0000  Test task"
            , "├─ depends-on    01DEP10000  First dep  [planned]"
            , "├─ depends-on    01DEP20000  Second dep  [blocked]"
            , "├─ derived-from  01DERIVED0  Parent task  [done]"
            , "├─ references    01REFA0000  Live context"
            , "└─ references    01REFB0000  Retired context  [retired]"
            , ""
            ]

testLinksNoEdges :: IO ()
testLinksNoEdges = do
    let out = renderTaskHuman True minTask "/tmp/body" [] [] [] [] []
    assertBool "## Links header" ("## Links" `T.isInfixOf` out)
    assertBool "(none) line" ("(none)" `T.isInfixOf` out)
    assertBool "no edge rows" (not ("depends-on" `T.isInfixOf` out) && not ("references" `T.isInfixOf` out))

testLinksAscii :: IO ()
testLinksAscii = do
    let out =
            renderTaskHuman
                False
                minTask
                "/tmp/body"
                [minContext "01REFA000000000000000000DD" "Ref context"]
                [depTask "01DEP1000000000000000000AA" "Dep task" Planned]
                []
                []
                []
    assertBool "ASCII branch glyph +-" ("+-" `T.isInfixOf` out)
    assertBool "ASCII last glyph \\-" ("\\-" `T.isInfixOf` out)
    assertBool "no UTF-8 glyphs" (not ("├─" `T.isInfixOf` out) && not ("└─" `T.isInfixOf` out))
