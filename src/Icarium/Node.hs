{- | Assemble a node (task or context) from its parts and the DB.

A body lives in two places that must stay in sync: the @body@ column (source
of truth for prompts + FTS) and the on-disk markdown file read by
@task cat@ / @ctx cat@. 'createTaskWithBody' / 'createContextWithBody' write
both, so no caller can create a node with a DB body but no file (or vice
versa). Prefer them over a bare 'RT.insertTask' / 'RCx.insertContext'
followed by a hand-rolled 'persistBody'.

'autoDeriveDeps' and 'inheritedContextCategories' decide what a new context
picks up from the dispatched task named by @ICARIUM_TASK_ID@.
-}
module Icarium.Node (
    createTaskWithBody,
    createContextWithBody,
    autoDeriveDeps,
    inheritedContextCategories,
) where

import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple (Connection)

import Icarium.Bodies (persistBody)
import Icarium.Repo.Category qualified as RC
import Icarium.Repo.Context (NewContext (..))
import Icarium.Repo.Context qualified as RCx
import Icarium.Repo.Task (NewTask (..))
import Icarium.Repo.Task qualified as RT
import Icarium.Types (Category (..), CategoryAxis (..), NodeKind (..), taskId)

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

{- | If no explicit --derived-from was supplied and ICARIUM_TASK_ID is set,
returns a singleton edge pointing at the dispatched task.
Explicit list non-empty → empty result (explicit wins).

A miss is silent: dispatch injects the full task id, so there is no prefix
left to fail to resolve and nothing actionable to warn about.
-}
autoDeriveDeps :: Connection -> [Text] -> Maybe String -> IO [(NodeKind, Text)]
autoDeriveDeps _ (_ : _) _ = pure []
autoDeriveDeps _ [] Nothing = pure []
autoDeriveDeps c [] (Just tid) = do
    mt <- RT.getTask c (T.pack tid)
    pure [(TaskNode, taskId t) | t <- maybe [] pure mt]

{- | Per-axis category inheritance: for each axis with no explicit flag, copy
that axis's categories from the task named by ICARIUM_TASK_ID.

Axis eligibility is not decided here: 'RC.attachContextCategory' drops
whatever cannot ride on a context.
-}
inheritedContextCategories ::
    Connection ->
    -- | explicit --domain
    Maybe Text ->
    -- | explicit --discipline
    Maybe Text ->
    -- | ICARIUM_TASK_ID
    Maybe String ->
    IO [Category]
inheritedContextCategories _ (Just _) (Just _) _ = pure []
inheritedContextCategories _ _ _ Nothing = pure []
inheritedContextCategories conn mDomain mDisc (Just tid) = do
    allCats <- RC.taskCategoriesFor conn (T.pack tid)
    pure (filter (not . overriddenByFlag . categoryAxis) allCats)
  where
    overriddenByFlag = \case
        Domain -> isJust mDomain
        Discipline -> isJust mDisc
        Kind -> False
