-- | Text helpers with no home of their own.
module Icarium.Text (
    tshow,
) where

import Data.Text (Text)
import Data.Text qualified as T

-- | 'show' straight into 'Text'.
tshow :: (Show a) => a -> Text
tshow = T.pack . show
