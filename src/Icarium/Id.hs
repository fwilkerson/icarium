module Icarium.Id (newId) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.ULID (getULID)

newId :: IO Text
newId = T.pack . show <$> getULID
