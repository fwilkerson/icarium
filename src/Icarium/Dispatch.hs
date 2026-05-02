module Icarium.Dispatch (
    DispatchRequest (..),
    DispatchResult (..),
    dispatch,
    applyOutcomeToTask,
) where

import Icarium.Dispatch.Internal (
    DispatchRequest (..),
    DispatchResult (..),
    dispatch,
 )
import Icarium.Dispatch.Outcome (applyOutcomeToTask)
