{- | What happened to a dispatch, and why.

One pure classifier over the signals a finished run leaves behind. Every rule
that used to be spread across the post-claude pipeline, the reviewer and the
outcome writer lives here as an enumerable case, so the precedence between them
is readable in one place:

@exit > escalation > guard > gate > review@

Per ADR 0008 the participants report observations and icarium concludes: this
module is that conclusion. The IO shell still skips work it need not do (no
gates on a tree that already failed a guard) and passes 'Nothing' for the
stages it skipped — the precedence is here, the cheap-skip is there.
-}
module Icarium.Dispatch.Decide (
    GitSignals (..),
    DecisionInput (..),
    DecisionReason (..),
    Decision (..),
    decideOutcome,
    renderReason,
    reviewVerdict,
) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import System.Exit (ExitCode (..))

import Icarium.Dispatch.Payload (
    Finding,
    WorkerPayload (..),
    WorkerStatus (..),
    renderFindings,
    verdictFromFindings,
 )
import Icarium.Types

-- | What git says about the worktree and branch after the worker exited.
data GitSignals = GitSignals
    { gsPorcelain :: Text
    -- ^ Raw @git status --porcelain@ output.
    , gsBranchSha :: Either Text Text
    -- ^ The dispatch branch head; 'Left' when it would not resolve.
    , gsBaseSha :: Text
    }

-- | Everything a decision is made from, gathered by the IO shell.
data DecisionInput = DecisionInput
    { diNoCommit :: Bool
    , diExit :: ExitCode
    , diTimeoutMinutes :: Int
    -- ^ The wall-clock budget the timeout sentinel means; recorded in the note.
    , diPayload :: Maybe WorkerPayload
    -- ^ 'Nothing' when the worker left no decodable final message.
    , diGit :: GitSignals
    , diGate :: Maybe (Either Text ())
    -- ^ 'Nothing' when the gates did not run.
    , diReview :: Maybe (Either Text [Finding])
    -- ^ 'Nothing' when the reviewer did not run; 'Left' when its own run failed.
    }

{- | Why the dispatch ended as it did. Structured rather than prose so the
decision can be asserted on directly; the note a human reads comes from
'renderReason'.
-}
data DecisionReason
    = -- | Everything that ran, passed.
      Clean
    | -- | A no-commit task with nothing to land.
      NoCommitClean
    | -- | Passed, but the reviewer found something worth recording.
      ReviewerWarn [Finding]
    | ExitFailed Int
    | -- | Exit 124, the timeout sentinel; carries the budget in minutes.
      TimedOut Int
    | -- | The worker's escalation, carrying its own reason.
      Escalated Text
    | GuardFailed Text
    | -- | A gate's own note, e.g. @"exit 7 -> exit 7"@.
      GateFailed Text
    | -- | 'Left' when the reviewer run itself failed — a broken gate is not a pass.
      ReviewerFailed (Either Text [Finding])
    deriving (Show, Eq)

data Decision = Decision
    { dOutcome :: DispatchOutcome
    , dReason :: DecisionReason
    , dTaskTransition :: Maybe (TaskState, Maybe Text)
    {- ^ The state this dispatch implies for its task, and the block reason to
    record with it. What to write, not whether to: freshness is the writer's
    call (see 'Icarium.Dispatch.Outcome.applyOutcomeToTask').
    -}
    , dRetry :: Maybe Text
    -- ^ The findings text to feed the next attempt; 'Nothing' is terminal.
    }
    deriving (Show, Eq)

decideOutcome :: DecisionInput -> Decision
decideOutcome di
    | Just reason <- exitReason = failure reason
    | Just reason <- escalation = failure reason
    | Just msg <- guard = failure (GuardFailed msg)
    | diNoCommit di = success NoCommitClean
    | Just (Left note) <- diGate di = failure (GateFailed note)
    | otherwise = case diReview di of
        Nothing -> success Clean
        -- Whether the reviewer's own run failed is 'reviewVerdict''s call, so
        -- the fail-closed rule is stated once; a warn always has findings.
        Just outcome -> case (reviewVerdict outcome, outcome) of
            (RVPass, _) -> success Clean
            (RVWarn, Right findings) -> success (ReviewerWarn findings)
            _ -> retryable (ReviewerFailed outcome) (reviewReport outcome)
  where
    git = diGit di
    guard = postClaudeGuard (diNoCommit di) (gsPorcelain git) (gsBranchSha git) (gsBaseSha git)
    exitReason = case diExit di of
        ExitSuccess -> Nothing
        ExitFailure 124 -> Just (TimedOut (diTimeoutMinutes di))
        ExitFailure c -> Just (ExitFailed c)
    escalation = do
        p <- diPayload di
        case wpStatus p of
            WBlocked -> Just (Escalated (fromMaybe "no reason given" (wpBlockReason p)))
            WSubmitted -> Nothing

success :: DecisionReason -> Decision
success reason =
    Decision
        { dOutcome = OSuccess
        , dReason = reason
        , dTaskTransition = Just (Done, Nothing)
        , dRetry = Nothing
        }

failure :: DecisionReason -> Decision
failure reason =
    Decision
        { dOutcome = OFailure
        , dReason = reason
        , dTaskTransition = Just (Blocked, Just (renderReason reason))
        , dRetry = Nothing
        }

-- | A failure the next attempt can learn from; the Text is what it gets told.
retryable :: DecisionReason -> Text -> Decision
retryable reason findings = (failure reason){dRetry = Just findings}

{- | The dispatch note: what a human reads on @dispatch show@, and what a
failure records as the task's block reason.
-}
renderReason :: DecisionReason -> Text
renderReason = \case
    Clean -> "gates passed"
    NoCommitClean -> "no-commit task"
    ReviewerWarn fs -> "reviewer warn\n" <> renderFindings fs
    ExitFailed c -> "claude exited " <> T.pack (show c)
    TimedOut mins -> "timed out after " <> T.pack (show mins) <> " minutes"
    Escalated reason -> "worker blocked: " <> reason
    GuardFailed msg -> msg
    GateFailed note -> note
    ReviewerFailed outcome -> "reviewer: fail\n" <> reviewReport outcome

{- | What a human or the retrying worker reads: the findings table, or why the
reviewer's own run failed.
-}
reviewReport :: Either Text [Finding] -> Text
reviewReport = either id renderFindings

{- | Fails closed: a reviewer that timed out has no findings but is not a pass,
so the verdict comes from the run's own failure rather than from an empty list
it never received.
-}
reviewVerdict :: Either Text [Finding] -> ReviewVerdict
reviewVerdict = either (const RVFail) verdictFromFindings

{- | Pure guard logic for the post-claude checks. Returns Just an error
message if a guard fires, Nothing if all pass. Reached only through
'decideOutcome', which is where its precedence against the other signals
lives. The Bool is whether this is a no-commit task.
  * Dirty-tree guard fires in both modes when @porcelain@ (raw
    `git status --porcelain` output) is non-empty after stripping.
  * No-commit mode: fires when the branch SHA resolved and differs from
    baseSha — the agent committed despite being told not to.
  * Commit mode: fires when the branch SHA equals baseSha (agent exited
    success but made no commits).
-}
postClaudeGuard :: Bool -> Text -> Either e Text -> Text -> Maybe Text
postClaudeGuard noCommit porcelain mBranchSha baseSha
    | not (T.null porcStripped) = Just dirtyMsg
    | noCommit = case branchSha of
        Just sha | sha /= baseSha -> Just "no-commit task: agent left commits on dispatch branch (branch retained for inspection)"
        _ -> Nothing
    | branchSha == Just baseSha = Just "agent made no commits on dispatch branch"
    | otherwise = Nothing
  where
    porcStripped = T.strip porcelain
    branchSha = either (const Nothing) Just mBranchSha
    dirtyMsg =
        "agent left uncommitted changes; refusing to accept\nuncommitted:\n"
            <> T.intercalate "\n" (map ("  " <>) (T.lines porcStripped))
