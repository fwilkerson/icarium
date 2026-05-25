module Icarium.Dispatch.PostClaude (
    PostClaudeResult (..),
    handlePostClaude,
    handlePostClaudeWithReview,
    checkpointDirtyTree,
    postClaudeGuard,
    runGate,
) where

import Control.Monad (forM_, unless, void, (>=>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple (Connection)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory)
import System.IO (hPutStrLn, stderr)
import System.Process.Typed (runProcess, shell)

import Icarium.Config (CommandsConfig (..), Config (..), DispatchConfig (..), ReviewConfig (..))
import Icarium.Dispatch.Outcome (DispatchCtx (..), DispatchResult, FinishArgs (..), finishWith)
import Icarium.Dispatch.Reviewer (ReviewResult (..), loadReviewerPrompt, runReviewer)
import Icarium.Git qualified as Git
import Icarium.Repo.Category qualified as RC
import Icarium.Repo.Context qualified as RCx
import Icarium.Repo.Dispatch qualified as RD
import Icarium.Types

data PostClaudeResult
    = PCDone DispatchResult
    | -- | Current dispatch closed with OFailure; Text = findings for next attempt.
      PCRetry DispatchResult Text

-- | Backward-compatible interface (no review). Used by tests.
handlePostClaude ::
    DispatchCtx ->
    Config ->
    Bool ->
    ExitCode ->
    Text ->
    FilePath ->
    IO DispatchResult
handlePostClaude dx cfg noCommit exit baseSha logPath = do
    res <- handlePostClaudeImpl dx cfg Nothing noCommit exit baseSha logPath
    pure $ case res of
        PCDone dr -> dr
        PCRetry dr _ -> dr

-- | Full interface used by the dispatch loop; runs reviewer when configured.
handlePostClaudeWithReview ::
    DispatchCtx ->
    Config ->
    Task ->
    Bool ->
    ExitCode ->
    Text ->
    FilePath ->
    IO PostClaudeResult
handlePostClaudeWithReview dx cfg task =
    handlePostClaudeImpl dx cfg (Just task)

-- =============================================================
-- Internal implementation
-- =============================================================

handlePostClaudeImpl ::
    DispatchCtx ->
    Config ->
    Maybe Task ->
    Bool ->
    ExitCode ->
    Text ->
    FilePath ->
    IO PostClaudeResult
handlePostClaudeImpl dx cfg mTask noCommit exit baseSha logPath = do
    let conn = dxConn dx
        did = dxDid dx
        branch = dxBranch dx
        base = dxBase dx
        ret = dcLogRetentionRuns (cfgDispatch cfg)
        maxMins = dcMaxMinutesPerDispatch (cfgDispatch cfg)
        finish o mSha notes =
            finishWith
                dx
                FinishArgs
                    { faOutcome = o
                    , faSha = mSha
                    , faNotes = notes
                    , faRetention = ret
                    , faLogPath = Just logPath
                    , faBaseSha = Just baseSha
                    }
        checkExit = case exit of
            ExitFailure 124 -> throwE ("timed out after " <> T.pack (show maxMins) <> " minutes")
            ExitFailure c -> throwE ("claude exited " <> T.pack (show c))
            ExitSuccess -> pure ()
        -- Returns Nothing for no-commit success, Just () when gates passed.
        preStep
            | noCommit = do
                checkExit
                porcelain <- liftIO Git.statusPorcelain
                let porcStripped = T.strip porcelain
                unless (T.null porcStripped) $
                    throwE $
                        "agent left uncommitted changes; refusing to merge\nuncommitted:\n"
                            <> T.intercalate "\n" (map ("  " <>) (T.lines porcStripped))
                mBranchSha <- liftIO (Git.revParse branch)
                case mBranchSha of
                    Right sha
                        | sha /= baseSha ->
                            throwE "no-commit task: agent left commits on dispatch branch (branch retained for inspection)"
                    _ -> pure ()
                gitStep "checkout base" (Git.checkout base)
                liftIO (void (Git.deleteBranch branch))
                pure Nothing
            | otherwise = do
                checkExit
                porcelain <- liftIO Git.statusPorcelain
                mBranchSha <- liftIO (Git.revParse branch)
                mapM_ throwE (postClaudeGuard porcelain mBranchSha baseSha)
                liftIO $ case mBranchSha of
                    Right sha -> RD.setLastCommit conn did sha
                    Left _ -> pure ()
                cc <- maybe (throwE "no [commands] section configured") pure (cfgCommands cfg)
                liftIO (runGate (ccBuild cc)) >>= either throwE pure
                liftIO (runGate (ccTest cc)) >>= either throwE pure
                pure (Just ())

    runExceptT preStep >>= \case
        Left notes -> do
            checkpointDirtyTree did notes
            PCDone <$> finish OFailure Nothing notes
        Right Nothing ->
            PCDone <$> finish OSuccess Nothing "no-commit task"
        Right (Just ()) ->
            runReviewThenMerge conn cfg mTask finish did branch base logPath maxMins baseSha

runReviewThenMerge ::
    Connection ->
    Config ->
    Maybe Task ->
    (DispatchOutcome -> Maybe Text -> Text -> IO DispatchResult) ->
    Text ->
    Text ->
    Text ->
    FilePath ->
    Int ->
    Text ->
    IO PostClaudeResult
runReviewThenMerge conn cfg mTask finish did branch base logPath maxMins baseSha = do
    let activeReview = do
            task <- mTask
            rc <- cfgReview cfg
            if rcEnabled rc then Just (task, rc) else Nothing
    mReviewResult <- case activeReview of
        Nothing -> pure Nothing
        Just (task, rcfg) -> do
            let reviewModel = fromMaybe (dcModel (cfgDispatch cfg)) (rcModel rcfg)
                reviewerLogPath = takeDirectory logPath <> "/" <> T.unpack did <> "-reviewer.jsonl"
            mSysPrompt <- loadReviewerPrompt (rcPromptPath rcfg)
            diffText <- Git.diffPatch baseSha
            rr <- runReviewer reviewModel mSysPrompt (taskTitle task) (taskBody task) diffText reviewerLogPath maxMins
            RD.setReviewInfo conn did (rrVerdict rr) (rrLogPath rr)
            hPutStrLn stderr ("[reviewer] verdict: " <> T.unpack (reviewVerdictText (rrVerdict rr)))
            pure (Just (task, rr))
    case mReviewResult of
        Just (_, rr) | rrVerdict rr == RVFail -> do
            let findings = rrFindings rr
            dr <- finish OFailure Nothing ("reviewer: fail\n" <> findings)
            pure (PCRetry dr findings)
        _ -> do
            mergeResult <- runExceptT doMerge
            case mergeResult of
                Left notes -> do
                    checkpointDirtyTree did notes
                    PCDone <$> finish OFailure Nothing notes
                Right mSha -> do
                    let notes = case mReviewResult of
                            Just (_, rr)
                                | rrVerdict rr == RVWarn ->
                                    "merged; reviewer warn\n" <> rrFindings rr
                            _ -> "merged"
                    dr <- finish OSuccess mSha notes
                    case mReviewResult of
                        Just (task, rr) | rrVerdict rr == RVWarn -> do
                            cats <- RC.taskCategoriesFor conn (taskId task)
                            writeWarnContextEntry conn task cats (rrFindings rr)
                        _ -> pure ()
                    pure (PCDone dr)
  where
    doMerge :: ExceptT Text IO (Maybe Text)
    doMerge = do
        gitStep "checkout base" (Git.checkout base)
        gitStep "ff-merge" (Git.ffMerge branch)
        liftIO (void (Git.deleteBranch branch))
        either (const Nothing) Just <$> liftIO (Git.revParse base)

writeWarnContextEntry :: Connection -> Task -> [Category] -> Text -> IO ()
writeWarnContextEntry conn task cats findings = do
    cid <-
        RCx.insertContext
            conn
            RCx.NewContext
                { RCx.ncTitle = "reviewer warn: " <> taskTitle task
                , RCx.ncBody = findings
                }
    forM_ cats $ \cat -> RC.attachContextCategory conn cid (categoryId cat)

{- | If the working tree is dirty, commit everything to the current branch with
a wip message. Preserves in-flight work on the dispatch branch so a human
can inspect it after a failure.
-}
checkpointDirtyTree :: Text -> Text -> IO ()
checkpointDirtyTree did note = do
    porcelain <- Git.statusPorcelain
    unless (T.null (T.strip porcelain)) $ do
        let shortNote = T.take 60 (T.takeWhile (/= '\n') note)
            msg = "wip: dispatch " <> did <> " (failed: " <> shortNote <> ")"
        void $ Git.commitAll msg

gitStep :: (Show e) => Text -> IO (Either e a) -> ExceptT Text IO a
gitStep label = liftIO >=> either (throwE . tag) pure
  where
    tag e = label <> ": " <> T.pack (show e)

{- | Pure guard logic for the post-claude checks. Returns Just an error
message if a guard fires, Nothing if both pass.
  * Dirty-tree guard fires when @porcelain@ (raw `git status --porcelain`
    output) is non-empty after stripping.
  * Empty-diff guard fires when the dispatch branch SHA equals baseSha
    (agent exited success but made no commits).
-}
postClaudeGuard :: Text -> Either e Text -> Text -> Maybe Text
postClaudeGuard porcelain mBranchSha baseSha
    | not (T.null porcStripped) = Just dirtyMsg
    | branchSha == Just baseSha = Just "agent made no commits on dispatch branch"
    | otherwise = Nothing
  where
    porcStripped = T.strip porcelain
    branchSha = either (const Nothing) Just mBranchSha
    dirtyMsg =
        "agent left uncommitted changes; refusing to merge\nuncommitted:\n"
            <> T.intercalate "\n" (map ("  " <>) (T.lines porcStripped))

{- | Run a shell command (as a single string, so users can include
pipes and &&). Returns () on exit 0; otherwise a short note.
-}
runGate :: Text -> IO (Either Text ())
runGate cmdText
    | T.null (T.strip cmdText) = pure (Right ())
    | otherwise = do
        code <- runProcess (shell (T.unpack cmdText))
        pure $ case code of
            ExitSuccess -> Right ()
            ExitFailure c -> Left (cmdText <> " -> exit " <> T.pack (show c))
