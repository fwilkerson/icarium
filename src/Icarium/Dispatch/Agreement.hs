{- | The working agreement inlined into dispatch prompts — and only there;
@task show --prompt@ carries shared task content with no agreement, so
interactive builders never inherit headless lane rules (issue #11).

Projects replace the built-in body via @[dispatch] agreement_path@, and it
replaces the whole of it: nothing icarium needs is carried here. Everything a
worker hands back rides the schema in "Icarium.Dispatch.Payload", whose
property descriptions an override cannot weaken, and the gate performs the
mutations. So the agreement holds only cross-cutting judgement the schema
cannot localize to a field.

Anything icarium must guarantee reaches every worker — the scratch path —
rides its own section ('scratchSection'), outside the overridable body.
-}
module Icarium.Dispatch.Agreement (
    agreementSection,
    loadAgreementFile,
    scratchSection,
) where

import Control.Exception (SomeException, try)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

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
        , "- Commit your code before exiting; after the gates pass the program parks your branch for merge."
        ]

{- | The scratch rule, carried outside the agreement body so an
@agreement_path@ override cannot drop it — and with the path already
resolved: a headless worker cannot expand an env var (the built-in
read-only Bash classifier denies @printenv@, and @echo $VAR@ does not
expand), so naming one would leave it writing blind.
-}
scratchSection :: FilePath -> Text
scratchSection absScratch =
    T.unlines
        [ "## Scratch directory"
        , ""
        , "Test artifacts (snapshots, fixtures, scratch files) MUST go in"
        , "`" <> T.pack absScratch <> "`, never in the working tree."
        , "The post-claude gate refuses to accept a dirty tree."
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

-- | Full prompt section: heading, then the agreement body (file override or built-in).
agreementSection :: Maybe Text -> Text
agreementSection mOverride =
    T.unlines
        [ "## Working agreement"
        , ""
        , maybe builtInAgreement T.stripEnd mOverride
        ]
