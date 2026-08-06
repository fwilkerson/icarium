{- | What a retry attempt is told about the attempt that preceded it.

A retry is cut fresh from the integration branch, so it starts on a tree
that holds none of the prior attempt's work — but both worktrees share one
object database, so the prior branch's commits are readable from inside the
retry. Only the name is missing, and this is where it is supplied.
-}
module Icarium.Dispatch.Retry (
    PriorAttempt (..),
    RetryHandoff (..),
    retrySections,
) where

import Data.Text (Text)
import Data.Text qualified as T

data PriorAttempt = PriorAttempt
    { paAttempt :: Int
    -- ^ 1-based number of the attempt that produced this branch.
    , paBranch :: Text
    , paTipSha :: Text
    , paBaseSha :: Text
    }

data RetryHandoff = RetryHandoff
    { rhFindings :: Text
    , rhPrior :: Maybe PriorAttempt
    {- ^ Nothing when the prior branch tip could not be read: naming a sha
    the worker cannot resolve is worse than saying nothing.
    -}
    }

-- | The prompt sections a retry carries beyond a first attempt's prompt.
retrySections :: RetryHandoff -> Text
retrySections h =
    "\n## Reviewer findings from previous attempt\n\n"
        <> rhFindings h
        <> "\n"
        <> maybe "" priorAttemptSection (rhPrior h)

priorAttemptSection :: PriorAttempt -> Text
priorAttemptSection PriorAttempt{..} =
    T.unlines
        [ ""
        , "## Previous attempt"
        , ""
        , "Attempt " <> T.pack (show paAttempt) <> " is on branch `" <> paBranch <> "` at `" <> paTipSha <> "`, cut from `" <> paBaseSha <> "`."
        , "Its code is in this repository and readable from here: `git diff " <> paBaseSha <> " " <> paTipSha <> "`"
        , "shows what it built, `git show " <> paTipSha <> ":<path>` reads any file it wrote. It was"
        , "failed for the findings above and for nothing else — start from what it got"
        , "right rather than rebuilding it."
        ]
