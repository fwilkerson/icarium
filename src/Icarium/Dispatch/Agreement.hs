{- | The working agreement inlined into dispatch prompts — and only there;
@task show --prompt@ carries shared task content with no agreement, so
interactive builders never inherit headless lane rules (issue #11).

Projects replace the built-in body via @[dispatch] agreement_path@. The
task-specific escalation lines are appended by icarium either way, so the
escalation protocol survives a project file that omits it.
-}
module Icarium.Dispatch.Agreement (
    agreementSection,
    builtInAgreement,
    loadAgreementFile,
) where

import Control.Exception (SomeException, try)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

import Icarium.Types (Task, taskId)

{- | Agreement body used when no @agreement_path@ is configured.
Mirrored in this repo's @.icarium/agreement.md@ (built-in body plus
dogfood additions); keep the two in sync.
-}
builtInAgreement :: Text
builtInAgreement =
    T.intercalate
        "\n"
        [ "You are a headless dispatch working on this task, unattended. Guardrails:"
        , ""
        , "- There is no user. Permission denials are policy, not questions —"
        , "  never wait for input; work within what the allowed tools permit."
        , "- If you cannot proceed within policy, escalate: mark the task blocked"
        , "  with a reason (command below) and stop."
        , "- All task/context mutation MUST go through the `icarium` CLI."
        , "- Record anything you learn that future tasks would benefit from as a context entry:"
        , "    `icarium ctx add '<title>' --body-stdin <<'EOF'"
        , "       ...markdown..."
        , "     EOF`"
        , "- Commit your code before exiting; after the gates pass the program parks your branch for merge."
        , "- Test artifacts (snapshots, fixtures, scratch files) MUST go in `$ICARIUM_SCRATCH_DIR`,"
        , "  never in the working tree. The post-claude gate refuses to accept a dirty tree."
        ]

{- | Load the agreement override. Fails closed: an unreadable
@agreement_path@ is an error naming the path, not a silent fallback to
'builtInAgreement'. Must run before the worker starts, same posture as
'Icarium.Dispatch.Reviewer.loadReviewerPrompt'.
-}
loadAgreementFile :: Maybe Text -> IO (Either Text (Maybe Text))
loadAgreementFile Nothing = pure (Right Nothing)
loadAgreementFile (Just path) = do
    r <- try (TIO.readFile (T.unpack path)) :: IO (Either SomeException Text)
    case r of
        Left e -> pure (Left ("dispatch agreement_path unreadable: " <> path <> ": " <> T.pack (show e)))
        Right t -> pure (Right (Just t))

{- | Full prompt section: heading, agreement body (file override or
built-in), then the task-specific escalation lines.
-}
agreementSection :: Maybe Text -> Task -> Text
agreementSection mOverride t =
    T.unlines
        [ "## Working agreement"
        , ""
        , maybe builtInAgreement T.stripEnd mOverride
        , ""
        , "Task-specific protocol (applies regardless of the agreement above):"
        , ""
        , "- If blocked:  `icarium task update " <> taskId t <> " --state blocked --block-reason '<why>'`"
        ]
