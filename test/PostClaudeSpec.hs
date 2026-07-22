module PostClaudeSpec (tests) where

import Control.Exception (bracket)
import Data.Aeson (Value, encode, object, (.=))
import Data.ByteString.Lazy qualified as BL
import Data.List (isInfixOf)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Database.SQLite.Simple (Connection, close, open)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process.Typed (proc, readProcess, setWorkingDir)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Icarium.Config (CategoriesConfig (..), CommandsConfig (..), Config (..), DispatchConfig (..), ProjectConfig (..))
import Icarium.Db (migrateDb)
import Icarium.Dispatch.Outcome (
    DispatchCtx (..),
    DispatchResult (..),
    FinishArgs (..),
    applyOutcomeToTask,
    finishWith,
 )
import Icarium.Dispatch.Payload (
    Finding (..),
    FindingAxis (..),
    FutureNote (..),
    Severity (..),
    WorkerPayload (..),
    WorkerStatus (..),
 )
import Icarium.Dispatch.PostClaude (checkpointDirtyTree, handlePostClaude, writeWarnContextEntry)
import Icarium.Git qualified as Git
import Icarium.Repo.Category qualified as RC
import Icarium.Repo.Context qualified as RCx
import Icarium.Repo.Dispatch qualified as RD
import Icarium.Repo.Edge qualified as RE
import Icarium.Repo.Task qualified as RT
import Icarium.Schema (applySchema)
import Icarium.Types
import TestHelpers (insertTestDispatch, minTask, mkCat, withOutOfTreeDb, withTestDb, withTestRepo)

tests :: TestTree
tests =
    testGroup
        "PostClaude"
        [ testCase "checkpointDirtyTree commits dirty tree with wip message" testCheckpointDirtyTree
        , testCase "checkpointDirtyTree is no-op on clean tree" testCheckpointCleanTree
        , testCase "handlePostClaude failure leaves wip commit on dispatch branch" testHandlePostClaudeFailureCheckpoints
        , testCase "handlePostClaude no-commit success with agent commits is failure; branch retained" testNoCommitAgentCommittedAnyway
        , testCase "finishWith OFailure checkpoints staged changes and leaves base clean" testFinishWithWipCheckpoint
        , testCase "writeWarnContextEntry links the note back to its task" testWarnEntryLinksTask
        , testGroup
            "worker payload ingest"
            [ testCase "submitted: task done, one ctx per note, retrieval axes inherited" testIngestSubmitted
            , testCase "a failed run's notes are ingested too" testIngestOnFailure
            , testCase "no payload: outcome alone drives the transition" testIngestNoPayload
            , testCase "worker blocked in the log makes the dispatch a failure" testWorkerBlockedIsFailure
            ]
        ]

-- =============================================================
-- Worker payload ingest
-- =============================================================

dispatchResult :: DispatchOutcome -> Text -> Maybe WorkerPayload -> DispatchResult
dispatchResult outcome notes mPayload =
    DispatchResult
        { dresDispatchId = Just ingestDispatchId
        , dresOutcome = outcome
        , dresBranch = "dispatch/" <> ingestDispatchId
        , dresNotes = notes
        , dresLogPath = Nothing
        , dresBaseSha = Nothing
        , dresPayload = mPayload
        }

{- | A task tagged on all three axes, in a temp-dir DB so ctx body files
have somewhere to land.
-}
withIngestTask :: (Connection -> FilePath -> Task -> IO a) -> IO a
withIngestTask act =
    withSystemTempDirectory "icarium-ingest" $ \dir -> do
        let db = dir </> "icarium.db"
        bracket (open db) close $ \c -> do
            applySchema c
            migrateDb c
            tid <-
                RT.insertTask
                    c
                    RT.NewTask
                        { RT.ntTitle = "Parser task"
                        , RT.ntBody = ""
                        , RT.ntState = InProgress
                        , RT.ntPriority = Nothing
                        , RT.ntNoCommit = False
                        }
            mapM_
                (\(axis, name) -> mkCat c axis name >>= RC.attachTaskCategory c tid . categoryId)
                [(Domain, "dispatch"), (Discipline, "haskell"), (Kind, "enhancement")]
            insertTestDispatch c ingestDispatchId tid
            Just t <- RT.getTask c tid
            act c db t

-- | The run every 'dispatchResult' in this module claims to come from.
ingestDispatchId :: Text
ingestDispatchId = "01DISPATCH0000000000000000"

testIngestSubmitted :: IO ()
testIngestSubmitted = withIngestTask $ \c db t -> do
    let notes =
            [ FutureNote{fnTitle = "Worktrees share the object store", fnBody = "So a fetch in one is visible in all."}
            , FutureNote{fnTitle = "Gates run in the worktree", fnBody = "Not the invoking checkout."}
            ]
        payload = WorkerPayload{wpStatus = WSubmitted, wpBlockReason = Nothing, wpForFutureAgents = notes}
    applyOutcomeToTask c db t (dispatchResult OSuccess "gates passed" (Just payload))

    Just t' <- RT.getTask c (taskId t)
    taskState t' @?= Done

    edges <- RE.listEdges c Nothing (Just (taskId t)) (Just DerivedFrom)
    length edges @?= 2
    cxs <- mapM (RCx.getContext c . edgeSrcId) edges
    map (fmap contextTitle) cxs @?= map (Just . fnTitle) notes

    -- Kind is a workflow axis: it describes the work, not when the learning
    -- is relevant, so it must not ride along.
    cats <- RC.contextCategoriesFor c (edgeSrcId (head edges))
    map (\cat -> (categoryAxis cat, categoryName cat)) cats
        @?= [(Discipline, "haskell"), (Domain, "dispatch")]

{- | A run that blocked or failed gates still learned something, and that is
exactly what the next attempt needs — so the notes land whatever the outcome.
-}
testIngestOnFailure :: IO ()
testIngestOnFailure = withIngestTask $ \c db t -> do
    let payload =
            WorkerPayload
                { wpStatus = WBlocked
                , wpBlockReason = Just "the fixture DB has no 0014 migration"
                , wpForFutureAgents = [FutureNote{fnTitle = "Fixture DBs lag migrations", fnBody = "Regenerate before use."}]
                }
    applyOutcomeToTask c db t (dispatchResult OFailure "worker blocked: the fixture DB has no 0014 migration" (Just payload))

    Just t' <- RT.getTask c (taskId t)
    taskState t' @?= Blocked
    taskBlockReason t' @?= Just "worker blocked: the fixture DB has no 0014 migration"

    edges <- RE.listEdges c Nothing (Just (taskId t)) (Just DerivedFrom)
    length edges @?= 1

{- | One @claude --output-format stream-json@ result event carrying @p@ as the
worker's final message — the shape the gate reads the payload out of.
-}
workerResultLine :: Value -> BL.ByteString
workerResultLine p =
    encode $
        object
            [ "type" .= ("result" :: Text)
            , "result" .= TE.decodeUtf8 (BL.toStrict (encode p))
            ]

{- | The gate folds a worker's block into the dispatch /outcome/, not just the
task row: a block is the worker's one unilateral claim (ADR 0008), so the run
did not succeed and its branch must never reach auto-merge. The note recorded
is the worker's own reason, not the guard's generic "made no commits" — which
is why the block is checked ahead of the guards.
-}
testWorkerBlockedIsFailure :: IO ()
testWorkerBlockedIsFailure =
    withTestRepo $ \dir ->
        withOutOfTreeDb $ \dbPath -> withTestDb $ \conn -> do
            let did = "01TESTWORKERBLOCKED00000AA" :: Text
                branch = "dispatch/" <> did
                logPath = dir </> "worker.jsonl"
            -- Commit on the dispatch branch: the no-commits guard must not be
            -- what fires, or the test would pass for the wrong reason.
            gitIn_ dir ["checkout", "-b", T.unpack branch]
            writeFile (dir </> "agent-work.hs") "module A where"
            gitIn_ dir ["add", "agent-work.hs"]
            gitIn_ dir ["commit", "-m", "agent: partial work"]
            baseShaRaw <- gitIn dir ["rev-parse", "main"]
            let baseSha = T.pack (takeWhile (/= '"') (dropWhile (== '"') baseShaRaw))
            BL.writeFile logPath $
                workerResultLine $
                    object
                        [ "status" .= ("blocked" :: Text)
                        , "block_reason" .= ("policy forbids a force-push" :: Text)
                        , "for_future_agents" .= ([] :: [Value])
                        ]
            tid <-
                RT.insertTask
                    conn
                    RT.NewTask
                        { RT.ntTitle = "Test task"
                        , RT.ntBody = ""
                        , RT.ntState = ReadyHeadless
                        , RT.ntPriority = Nothing
                        , RT.ntNoCommit = False
                        }
            RD.insertDispatch
                conn
                did
                RD.NewDispatch
                    { RD.ndTaskId = tid
                    , RD.ndBranch = branch
                    , RD.ndBaseBranch = "main"
                    , RD.ndBaseSha = baseSha
                    , RD.ndModel = "claude-sonnet-4-6"
                    , RD.ndEffort = Medium
                    , RD.ndLogPath = Just logPath
                    , RD.ndPid = Nothing
                    }
            let dx =
                    DispatchCtx
                        { dxConn = conn
                        , dxDbPath = dbPath
                        , dxDid = did
                        , dxBranch = branch
                        , dxBase = "main"
                        , dxWorkDir = dir
                        }
            res <- handlePostClaude dx minCfg False ExitSuccess baseSha logPath
            dresOutcome res @?= OFailure
            assertBool
                ("notes carry the worker's reason: " <> T.unpack (dresNotes res))
                ("policy forbids a force-push" `isInfixOf` T.unpack (dresNotes res))
            fmap wpStatus (dresPayload res) @?= Just WBlocked

testIngestNoPayload :: IO ()
testIngestNoPayload = withIngestTask $ \c db t -> do
    applyOutcomeToTask c db t (dispatchResult OFailure "agent made no commits on dispatch branch" Nothing)
    Just t' <- RT.getTask c (taskId t)
    taskState t' @?= Blocked
    taskBlockReason t' @?= Just "agent made no commits on dispatch branch"

{- | A reviewer-warn note must carry full provenance: besides matching the
task's categories, it gets a `references` edge from the task so it is
reachable from the task in the graph, not only via shared tags.
-}
testWarnEntryLinksTask :: IO ()
testWarnEntryLinksTask =
    withSystemTempDirectory "icarium-warn" $ \dir -> do
        let db = dir </> "icarium.db"
        bracket (open db) close $ \c -> do
            applySchema c
            migrateDb c
            tid <-
                RT.insertTask
                    c
                    RT.NewTask
                        { RT.ntTitle = "Parser task"
                        , RT.ntBody = ""
                        , RT.ntState = ReadyHeadless
                        , RT.ntPriority = Nothing
                        , RT.ntNoCommit = False
                        }
            cat <- mkCat c Domain "cli"
            let did = "01WARNDISPATCH00000000000A"
            insertTestDispatch c did tid
            writeWarnContextEntry
                c
                db
                did
                minTask{taskId = tid}
                [cat]
                [Finding AxisStandards SevWarn (Just "src/Foo.hs") "possible Duplicated Code"]
            -- The reviewer entry is gate-written, so it carries the run that
            -- produced it; `dispatch show` reads it back off this column.
            fromRun <- RCx.contextsFromDispatch c did
            length fromRun @?= 1
            edges <- RE.listEdges c (Just tid) Nothing (Just References)
            case edges of
                [e] -> do
                    edgeSrcId e @?= tid
                    edgeDstKind e @?= ContextNode
                other -> assertBool ("expected one references edge, got " <> show (length other)) False

-- | Run git in a specific directory; return raw stdout bytes as String.
gitIn :: FilePath -> [String] -> IO String
gitIn dir args = do
    (_, out, _) <- readProcess (setWorkingDir dir (proc "git" args))
    pure (show out)

gitIn_ :: FilePath -> [String] -> IO ()
gitIn_ dir args = do
    _ <- readProcess (setWorkingDir dir (proc "git" args))
    pure ()

minCfg :: Config
minCfg =
    Config
        { cfgProject = ProjectConfig{pcIntegrationBranch = "main"}
        , cfgCommands = Just CommandsConfig{ccBuild = "true", ccTest = "true"}
        , cfgDispatch =
            DispatchConfig
                { dcModel = "claude-sonnet-4-6"
                , dcEffort = Medium
                , dcTools = []
                , dcAllowedTools = []
                , dcScratchDir = "/tmp"
                , dcMaxMinutesPerDispatch = 30
                , dcHeartbeatStaleSeconds = 300
                , dcLogRetentionRuns = 25
                , dcRetryStormThreshold = 3
                , dcWorktreeSetup = Nothing
                , dcWorktreeTeardown = Nothing
                , dcMcpConfig = Nothing
                , dcAgreementPath = Nothing
                }
        , cfgCategories = CategoriesConfig{catDomains = [], catDisciplines = [], catKinds = []}
        , cfgReview = Nothing
        }

testCheckpointDirtyTree :: IO ()
testCheckpointDirtyTree =
    withTestRepo $ \dir -> do
        writeFile (dir <> "/dirty.txt") "uncommitted work"
        checkpointed <- checkpointDirtyTree dir "TESTDID" "claude exited 1"
        assertBool "reports a checkpoint was made" checkpointed
        logOut <- gitIn dir ["log", "--oneline", "-1"]
        assertBool "wip commit message present" ("wip: dispatch TESTDID" `isInfixOf` logOut)
        assertBool "failed: note in commit" ("failed:" `isInfixOf` logOut)

testCheckpointCleanTree :: IO ()
testCheckpointCleanTree =
    withTestRepo $ \dir -> do
        before <- gitIn dir ["rev-parse", "HEAD"]
        checkpointed <- checkpointDirtyTree dir "TESTDID" "some error"
        assertBool "reports no checkpoint on a clean tree" (not checkpointed)
        after <- gitIn dir ["rev-parse", "HEAD"]
        before @?= after

testHandlePostClaudeFailureCheckpoints :: IO ()
testHandlePostClaudeFailureCheckpoints =
    withTestRepo $ \dir ->
        withOutOfTreeDb $ \dbPath ->
            withTestDb $ \conn -> do
                let did = "01TESTDISPATCH0000000000AA" :: Text
                    branch = "dispatch/" <> did
                -- create dispatch branch with one commit so it diverges from main
                gitIn_ dir ["checkout", "-b", T.unpack branch]
                writeFile (dir <> "/agent-work.hs") "module A where"
                gitIn_ dir ["add", "agent-work.hs"]
                gitIn_ dir ["commit", "-m", "agent: first edit"]
                -- grab the base sha from main
                baseShaRaw <- gitIn dir ["rev-parse", "main"]
                let baseSha = T.pack (takeWhile (/= '"') (dropWhile (== '"') baseShaRaw))
                -- leave a dirty file
                writeFile (dir <> "/in-progress.hs") "module B where -- incomplete"
                -- insert task and dispatch rows
                tid <-
                    RT.insertTask
                        conn
                        RT.NewTask
                            { RT.ntTitle = "Test task"
                            , RT.ntBody = ""
                            , RT.ntState = ReadyHeadless
                            , RT.ntPriority = Nothing
                            , RT.ntNoCommit = False
                            }
                RD.insertDispatch
                    conn
                    did
                    RD.NewDispatch
                        { RD.ndTaskId = tid
                        , RD.ndBranch = branch
                        , RD.ndBaseBranch = "main"
                        , RD.ndBaseSha = baseSha
                        , RD.ndModel = "claude-sonnet-4-6"
                        , RD.ndEffort = Medium
                        , RD.ndLogPath = Nothing
                        , RD.ndPid = Nothing
                        }
                let dx =
                        DispatchCtx
                            { dxConn = conn
                            , dxDbPath = dbPath
                            , dxDid = did
                            , dxBranch = branch
                            , dxBase = "main"
                            , dxWorkDir = dir
                            }
                res <- handlePostClaude dx minCfg False (ExitFailure 1) baseSha "/dev/null"
                -- outcome is failure
                dresOutcome res @?= OFailure
                -- dispatch branch still exists
                branchList <- gitIn dir ["branch", "--list", T.unpack branch]
                assertBool "dispatch branch retained" (T.unpack branch `isInfixOf` branchList)
                -- a wip commit is on the dispatch branch
                logOut <- gitIn dir ["log", T.unpack branch, "--oneline"]
                assertBool "wip commit on dispatch branch" ("wip: dispatch" `isInfixOf` logOut)

{- | finishWith with OFailure must snapshot staged/unstaged changes onto the
dispatch branch before the worktree is torn down — the branch is what
survives. Asserts (a) the worktree is clean after the checkpoint (everything
committed), (b) the dispatch branch carries the WIP commit, (c) the WIP
commit SHA is embedded in dresNotes for easy recovery.
-}
testFinishWithWipCheckpoint :: IO ()
testFinishWithWipCheckpoint =
    withTestRepo $ \dir ->
        withOutOfTreeDb $ \dbPath ->
            withTestDb $ \conn -> do
                let did = "01TESTWIPCHECKPOINT000000A" :: Text
                    branch = "dispatch/" <> did
                gitIn_ dir ["checkout", "-b", T.unpack branch]
                -- staged change that the agent never committed
                writeFile (dir <> "/staged.hs") "module Staged where"
                gitIn_ dir ["add", "staged.hs"]
                baseShaRaw <- gitIn dir ["rev-parse", "main"]
                let baseSha = T.pack (takeWhile (/= '"') (dropWhile (== '"') baseShaRaw))
                tid <-
                    RT.insertTask
                        conn
                        RT.NewTask
                            { RT.ntTitle = "WIP checkpoint task"
                            , RT.ntBody = ""
                            , RT.ntState = ReadyHeadless
                            , RT.ntPriority = Nothing
                            , RT.ntNoCommit = False
                            }
                RD.insertDispatch
                    conn
                    did
                    RD.NewDispatch
                        { RD.ndTaskId = tid
                        , RD.ndBranch = branch
                        , RD.ndBaseBranch = "main"
                        , RD.ndBaseSha = baseSha
                        , RD.ndModel = "claude-sonnet-4-6"
                        , RD.ndEffort = Medium
                        , RD.ndLogPath = Nothing
                        , RD.ndPid = Nothing
                        }
                let dx =
                        DispatchCtx
                            { dxConn = conn
                            , dxDbPath = dbPath
                            , dxDid = did
                            , dxBranch = branch
                            , dxBase = "main"
                            , dxWorkDir = dir
                            }
                res <- finishWith dx FinishArgs{faOutcome = OFailure, faSha = Nothing, faNotes = "agent timed out", faRetention = 25, faLogPath = Nothing, faBaseSha = Just baseSha, faPayload = Nothing}
                -- (a) worktree is clean: the staged change went into the WIP commit
                clean <- Git.isClean dir
                assertBool "worktree clean after WIP checkpoint" clean
                -- (b) dispatch branch carries the WIP commit
                logOut <- gitIn dir ["log", T.unpack branch, "--oneline"]
                assertBool "WIP commit on dispatch branch" ("WIP: dispatch" `isInfixOf` logOut)
                -- (c) WIP SHA is surfaced in dresNotes for dispatch show
                assertBool "notes contain wip_commit:" ("wip_commit:" `isInfixOf` T.unpack (dresNotes res))

{- | Regression: a --no-commit task whose agent committed anyway must be
treated as failure, with the dispatch branch retained for inspection. The
prior behavior silently swallowed the (failed) `git branch -d` and reported
success, orphaning the agent's commits.
-}
testNoCommitAgentCommittedAnyway :: IO ()
testNoCommitAgentCommittedAnyway =
    withTestRepo $ \dir ->
        withOutOfTreeDb $ \dbPath ->
            withTestDb $ \conn -> do
                let did = "01TESTNCBRANCH0000000000AA" :: Text
                    branch = "dispatch/" <> did
                -- agent commits on the dispatch branch despite no-commit flag
                gitIn_ dir ["checkout", "-b", T.unpack branch]
                writeFile (dir <> "/agent-commit.hs") "module A where"
                gitIn_ dir ["add", "agent-commit.hs"]
                gitIn_ dir ["commit", "-m", "agent: should not have committed"]
                baseShaRaw <- gitIn dir ["rev-parse", "main"]
                let baseSha = T.pack (takeWhile (/= '"') (dropWhile (== '"') baseShaRaw))
                tid <-
                    RT.insertTask
                        conn
                        RT.NewTask
                            { RT.ntTitle = "No-commit task"
                            , RT.ntBody = ""
                            , RT.ntState = ReadyHeadless
                            , RT.ntPriority = Nothing
                            , RT.ntNoCommit = True
                            }
                RD.insertDispatch
                    conn
                    did
                    RD.NewDispatch
                        { RD.ndTaskId = tid
                        , RD.ndBranch = branch
                        , RD.ndBaseBranch = "main"
                        , RD.ndBaseSha = baseSha
                        , RD.ndModel = "claude-sonnet-4-6"
                        , RD.ndEffort = Medium
                        , RD.ndLogPath = Nothing
                        , RD.ndPid = Nothing
                        }
                let dx =
                        DispatchCtx
                            { dxConn = conn
                            , dxDbPath = dbPath
                            , dxDid = did
                            , dxBranch = branch
                            , dxBase = "main"
                            , dxWorkDir = dir
                            }
                -- agent exited cleanly, tree is clean — but the branch has commits
                res <- handlePostClaude dx minCfg True ExitSuccess baseSha "/dev/null"
                dresOutcome res @?= OFailure
                branchList <- gitIn dir ["branch", "--list", T.unpack branch]
                assertBool "dispatch branch retained" (T.unpack branch `isInfixOf` branchList)
                logOut <- gitIn dir ["log", T.unpack branch, "--oneline"]
                assertBool "agent's commit still on branch" ("should not have committed" `isInfixOf` logOut)
