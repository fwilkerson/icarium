module Icarium.Dispatch
    ( DispatchRequest(..)
    , DispatchResult(..)
    , dispatch
    , applyOutcomeToTask
    ) where

import           Icarium.Dispatch.Internal (DispatchRequest (..), DispatchResult (..),
                                            applyOutcomeToTask, dispatch)
