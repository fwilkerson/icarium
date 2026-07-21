module Icarium.Dispatch.LogResult (
    LogUsage (..),
    LogResult (..),
    readLogResult,
    fmtMs,
) where

import Data.Aeson (FromJSON (..), decode, withObject, (.:?))
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import System.Directory (doesFileExist)
import Text.Printf (printf)

data LogUsage = LogUsage
    { luInputTokens :: Maybe Int
    , luOutputTokens :: Maybe Int
    , luCacheReads :: Maybe Int
    }

instance FromJSON LogUsage where
    parseJSON = withObject "LogUsage" $ \o ->
        LogUsage
            <$> o .:? "input_tokens"
            <*> o .:? "output_tokens"
            <*> o .:? "cache_read_input_tokens"

data LogResult = LogResult
    { lrNumTurns :: Maybe Int
    , lrDurationMs :: Maybe Int
    , lrDurationApiMs :: Maybe Int
    , lrCostUsd :: Maybe Double
    , lrUsage :: Maybe LogUsage
    , lrResultText :: Maybe Text
    }

instance FromJSON LogResult where
    parseJSON = withObject "LogResult" $ \o ->
        LogResult
            <$> o .:? "num_turns"
            <*> o .:? "duration_ms"
            <*> o .:? "duration_api_ms"
            <*> o .:? "total_cost_usd"
            <*> o .:? "usage"
            <*> o .:? "result"

readLogResult :: FilePath -> IO (Maybe LogResult)
readLogResult path = do
    exists <- doesFileExist path
    if not exists
        then pure Nothing
        else do
            ls <- T.lines <$> TIO.readFile path
            let isResult l = "\"type\":\"result\"" `T.isInfixOf` l
                resultLines = filter isResult ls
            pure $ listToMaybe $ mapMaybe parseLine (reverse resultLines)
  where
    parseLine l = decode (BL.fromStrict (TE.encodeUtf8 l))

fmtMs :: Int -> Text
fmtMs ms
    | ms >= 1000 = T.pack (printf "%.1fs" (fromIntegral ms / 1000.0 :: Double))
    | otherwise = T.pack (show ms) <> "ms"
