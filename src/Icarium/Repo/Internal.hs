module Icarium.Repo.Internal (
    escapeLike,
) where

import Data.Text (Text)
import Data.Text qualified as T

-- | Escape LIKE special characters so they match literally.
escapeLike :: Text -> Text
escapeLike = T.concatMap esc
  where
    esc c
        | c `elem` ['%', '_', '\\'] = T.pack ['\\', c]
        | otherwise = T.singleton c
