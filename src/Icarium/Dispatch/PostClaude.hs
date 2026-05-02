module Icarium.Dispatch.PostClaude (
    handlePostClaude,
    postClaudeGuard,
    runGate,
) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT, throwE)
import Data.Text (Text)
import Data.Text qualified as T
import System.Exit (ExitCode (..))
import System.Process.Typed (runProcess, shell)

import Icarium.Config (CommandsConfig (..), Config (..), DispatchConfig (..))
import Icarium.Dispatch.Outcome (DispatchCtx (..), DispatchResult, finishWith)
import Icarium.Git qualified as Git
import Icarium.Repo.Dispatch qualified as RD
import Icarium.Types

handlePostClaude ::
    DispatchCtx ->
    Config ->
    ExitCode ->
    Text ->
    FilePath ->
    IO DispatchResult
handlePostClaude dx cfg exit baseSha logPath = do
    let conn = dxConn dx
        did = dxDid dx
        branch = dxBranch dx
        base = dxBase dx
        ret = dcLogRetentionRuns (cfgDispatch cfg)
        maxMins = dcMaxMinutesPerDispatch (cfgDispatch cfg)
        cc = cfgCommands cfg
        finish o mSha notes =
            finishWith dx o mSha notes ret (Just logPath) (Just baseSha)
        step = do
            case exit of
                ExitFailure 124 -> throwE ("timed out after " <> T.pack (show maxMins) <> " minutes")
                ExitFailure c -> throwE ("claude exited " <> T.pack (show c))
                ExitSuccess -> pure ()
            porcelain <- liftIO Git.statusPorcelain
            mBranchSha <- liftIO (Git.revParse branch)
            mapM_ throwE (postClaudeGuard porcelain mBranchSha baseSha)
            liftIO $ case mBranchSha of
                Right sha -> RD.setLastCommit conn did sha
                Left _ -> pure ()
            liftIO (runGate (ccBuild cc)) >>= either throwE pure
            liftIO (runGate (ccTest cc)) >>= either throwE pure
            liftIO (Git.checkout base) >>= \case
                Left err -> throwE ("checkout base: " <> T.pack (show err))
                Right () -> pure ()
            liftIO (Git.ffMerge branch) >>= \case
                Left err -> throwE ("ff-merge: " <> T.pack (show err))
                Right () -> pure ()
            liftIO (void (Git.deleteBranch branch))
            either (const Nothing) Just <$> liftIO (Git.revParse base)
    runExceptT step >>= \case
        Left notes -> finish OFailure Nothing notes
        Right mSha -> finish OSuccess mSha "merged"

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
