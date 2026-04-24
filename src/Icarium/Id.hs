module Icarium.Id (newId) where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.ULID (getULID)

newId :: IO Text
newId = T.pack . show <$> getULID
