{- | Facade over the per-entity renderers, for callers that want one import.
The renderers themselves live in "Icarium.Render.Task", ".Context",
".Dispatch", ".Search" and ".Graph"; shared layout primitives in ".Internal".
-}
module Icarium.Render (
    -- * Tasks
    renderTaskHuman,
    renderTaskPrompt,
    untaggedPromptWarning,
    untaggedAddNudge,
    TaskRow (..),
    renderTaskList,
    mkBar,

    -- * Context
    renderContext,
    isRetired,
    ContextRow (..),
    renderContextList,
    CurationQueueRow (..),
    renderCurationQueue,
    formatLinkedCount,
    ContextChildRow (..),
    renderContextChildren,
    ContextTreeNode (..),
    renderContextTree,

    -- * Graph
    renderEdgeLine,
    renderCategory,

    -- * Dispatch
    renderDispatch,
    DispatchRow (..),
    renderDispatchList,
    renderDispatchStats,
    fmtSecs,

    -- * Search
    SearchHitRow (..),
    renderSearchList,

    -- * Shared
    recommendedTitleMax,
) where

import Icarium.Render.Context
import Icarium.Render.Dispatch
import Icarium.Render.Graph
import Icarium.Render.Internal (recommendedTitleMax)
import Icarium.Render.Search
import Icarium.Render.Task
