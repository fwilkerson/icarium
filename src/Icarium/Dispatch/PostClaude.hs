module Icarium.Dispatch.PostClaude (
    handlePostClaude,
    checkpointDirtyTree,
    postClaudeGuard,
    runGate,
) where

import Control.Monad (unless, void, (>=>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Text (Text)
import Data.Text qualified as T
import System.Exit (ExitCode (..))
import System.Process.Typed (runProcess, shell)

import Icarium.Config (CommandsConfig (..), Config (..), DispatchConfig (..))
import Icarium.Dispatch.Outcome (DispatchCtx (..), DispatchResult, FinishArgs (..), finishWith)
import Icarium.Git qualified as Git
import Icarium.Repo.Dispatch qualified as RD
import Icarium.Types

handlePostClaude ::
    DispatchCtx ->
    Config ->
    Bool ->
    ExitCode ->
    Text ->
    FilePath ->
    IO DispatchResult
handlePostClaude dx cfg noCommit exit baseSha logPath = do
    let conn = dxConn dx
        did = dxDid dx
        branch = dxBranch dx
        base = dxBase dx
        ret = dcLogRetentionRuns (cfgDispatch cfg)
        maxMins = dcMaxMinutesPerDispatch (cfgDispatch cfg)
        cc = cfgCommands cfg
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
        step
            | noCommit = do
                checkExit
                porcelain <- liftIO Git.statusPorcelain
                let porcStripped = T.strip porcelain
                unless (T.null porcStripped) $
                    throwE $
                        "agent left uncommitted changes; refusing to merge\nuncommitted:\n"
                            <> T.intercalate "\n" (map ("  " <>) (T.lines porcStripped))
                -- A no-commit task must not produce commits. If the agent
                -- committed anyway, refuse and leave the branch behind so
                -- the operator can inspect the unintended work.
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
                liftIO (runGate (ccBuild cc)) >>= either throwE pure
                liftIO (runGate (ccTest cc)) >>= either throwE pure
                gitStep "checkout base" (Git.checkout base)
                gitStep "ff-merge" (Git.ffMerge branch)
                liftIO (void (Git.deleteBranch branch))
                either (const Nothing) Just <$> liftIO (Git.revParse base)
    runExceptT step >>= \case
        Left notes -> do
            checkpointDirtyTree did notes
            finish OFailure Nothing notes
        Right mSha -> finish OSuccess mSha (if noCommit then "no-commit task" else "merged")

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
    output) is non-empty after stripping. The porcelain content is
    embedded in the message so the operator can see *what* was left
    behind without digging through the log.
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
