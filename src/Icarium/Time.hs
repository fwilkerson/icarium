module Icarium.Time (parseDbTime) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)

parseDbTime :: Text -> Maybe UTCTime
parseDbTime = parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" . T.unpack
