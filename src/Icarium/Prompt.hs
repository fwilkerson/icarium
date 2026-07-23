{- | Assemble a task's prompt body from its DB parts.

The single source of the prompt core shared by @task show --prompt@ (which
exists to preview what dispatch will send) and 'Icarium.Dispatch.Internal'.
Dispatch appends its agreement/scratch/findings sections after this; the CLI
prints it verbatim.

The 'Task' is taken already resolved: the two callers resolve the file-backed
body differently (the CLI reads the body file, dispatch sweeps it back into
the DB column) and that difference is deliberate.
-}
module Icarium.Prompt (
    taskPromptBody,
) where

import Control.Monad (unless)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Database.SQLite.Simple (Connection)
import System.IO (stderr)

import Icarium.Render (renderTaskPrompt, untaggedPromptWarning)
import Icarium.Repo.Category qualified as RC
import Icarium.Repo.Context qualified as RCx
import Icarium.Repo.Edge qualified as RE
import Icarium.Types

-- | How many category-matched entries the auto-pull may add.
catMatchCap :: Int
catMatchCap = 5

{- | The rendered prompt for @t@: its body plus referenced context, deduped
category-matched context, and dependency tasks. Warns on stderr when the task
carries no retrieval-axis category, so a piped prompt still surfaces it.
-}
taskPromptBody :: Connection -> Task -> IO Text
taskPromptBody conn t = do
    refs <- RE.referencedContexts conn (taskId t)
    cats <- RC.taskCategoriesFor conn (taskId t)
    catMatch <- RCx.categoryMatchedContexts conn cats catMatchCap
    deps <- RE.dependencyTasks conn (taskId t)
    unless (hasRetrievalAxis cats) $
        mapM_ (TIO.hPutStrLn stderr) (untaggedPromptWarning (taskId t))
    let refIds = map contextId refs
        dedupedCat = filter (\cx -> contextId cx `notElem` refIds) catMatch
    pure (renderTaskPrompt t refs dedupedCat deps)
