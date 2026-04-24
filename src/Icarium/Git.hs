module Icarium.Git
    ( GitError(..)
    , runGit
    , isClean
    , currentBranch
    , revParse
    , createBranch
    , checkout
    , ffMerge
    , stashUntracked
    ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import System.Exit (ExitCode(..))
import System.Process.Typed (proc, readProcess)

-- | A failed git invocation.
data GitError = GitError
    { gitCmd    :: [String]
    , gitStderr :: Text
    , gitExit   :: Int
    } deriving (Show)

-- | Run @git@ with the given args. Returns stdout (stripped) on
-- success, a structured error otherwise.
runGit :: [String] -> IO (Either GitError Text)
runGit args = do
    (code, out, err) <- readProcess (proc "git" args)
    let outT = T.strip (decodeUtf8 (BL.toStrict out))
        errT = T.strip (decodeUtf8 (BL.toStrict err))
    pure $ case code of
        ExitSuccess   -> Right outT
        ExitFailure c -> Left GitError{ gitCmd = "git" : args
                                      , gitStderr = errT
                                      , gitExit = c }

isClean :: IO Bool
isClean = do
    r <- runGit ["status", "--porcelain"]
    pure $ case r of
        Right out -> T.null out
        Left  _   -> False

currentBranch :: IO (Either GitError Text)
currentBranch = runGit ["rev-parse", "--abbrev-ref", "HEAD"]

-- | Resolve a ref to its full SHA.
revParse :: Text -> IO (Either GitError Text)
revParse ref = runGit ["rev-parse", "--verify", T.unpack ref]

-- | Create and check out a new branch from the given base.
createBranch :: Text -> Text -> IO (Either GitError ())
createBranch name base = fmap (() <$) $ runGit
    ["checkout", "-b", T.unpack name, T.unpack base]

checkout :: Text -> IO (Either GitError ())
checkout branch = fmap (() <$) $ runGit ["checkout", T.unpack branch]

-- | Fast-forward the current branch to the named branch. Fails if
-- the FF isn't possible — we never want a merge commit.
ffMerge :: Text -> IO (Either GitError ())
ffMerge branch = fmap (() <$) $ runGit
    ["merge", "--ff-only", T.unpack branch]

-- | Stash working-tree changes including untracked files with a
-- deterministic message. Used by recovery to preserve in-flight work.
stashUntracked :: Text -> IO (Either GitError ())
stashUntracked msg = fmap (() <$) $ runGit
    ["stash", "push", "-u", "-m", T.unpack msg]
