{- | Create a node (task or context) and persist its body in one step.

A body lives in two places that must stay in sync: the @body@ column (source
of truth for prompts + FTS) and the on-disk markdown file read by
@task cat@ / @ctx cat@. These helpers write both, so no caller can create a
node with a DB body but no file (or vice versa). Prefer them over a bare
'RT.insertTask' / 'RCx.insertContext' followed by a hand-rolled 'persistBody'.
-}
module Icarium.Node (
    createTaskWithBody,
    createContextWithBody,
) where

import Data.Text (Text)
import Database.SQLite.Simple (Connection)

import Icarium.Bodies (persistBody)
import Icarium.Repo.Context (NewContext (..))
import Icarium.Repo.Context qualified as RCx
import Icarium.Repo.Task (NewTask (..))
import Icarium.Repo.Task qualified as RT
import Icarium.Types (NodeKind (..))

-- | Insert the task row (+ FTS) and write its body file. Returns id and path.
createTaskWithBody :: Connection -> FilePath -> NewTask -> IO (Text, FilePath)
createTaskWithBody conn db nt = do
    tid <- RT.insertTask conn nt
    fp <- persistBody db TaskNode tid (ntBody nt)
    pure (tid, fp)

-- | Insert the context row (+ FTS) and write its body file. Returns id and path.
createContextWithBody :: Connection -> FilePath -> NewContext -> IO (Text, FilePath)
createContextWithBody conn db nc = do
    cid <- RCx.insertContext conn nc
    fp <- persistBody db ContextNode cid (ncBody nc)
    pure (cid, fp)
