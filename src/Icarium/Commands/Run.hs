module Icarium.Commands.Run (Options, parser, run) where

import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Database.SQLite.Simple (Connection)
import Options.Applicative
import System.IO (hPutStrLn, stderr)

import Icarium.Commands.Util (effortReader, fatal)
import qualified Icarium.Commands.Dispatch as CD
import Icarium.Config (Config, DispatchConfig(..), cfgDispatch, defaultConfigPath, loadConfig)
import Icarium.Db (defaultDbPath, withDb)
import qualified Icarium.Dispatch as D
import qualified Icarium.Repo.Task as RT
import Icarium.Types

data Options = Options
    { oMax    :: Maybe Int
    , oModel  :: Maybe Text
    , oEffort :: Maybe Effort
    , oBase   :: Maybe Text
    , oDryRun :: Bool
    }

parser :: Parser Options
parser = Options
    <$> optional (option auto (long "max" <> metavar "N"
                 <> help "Cap the number of dispatches (default from config)"))
    <*> optional (T.pack <$> strOption (long "model"  <> metavar "MODEL"))
    <*> optional (option effortReader  (long "effort" <> metavar "LEVEL"))
    <*> optional (T.pack <$> strOption (long "base-branch" <> metavar "NAME"))
    <*> switch   (long "dry-run" <> help "Dry-run each dispatch; do not touch git or call claude")

-- | Loop until the ready queue drains or we hit the max.
run :: Options -> IO ()
run o = do
    cfg <- loadConfig defaultConfigPath >>= \case
        Left  e  -> fatal 2 ("config parse error:\n" <> e)
        Right c  -> pure c
    let cap = fromMaybe (dcMaxDispatchesPerRun (cfgDispatch cfg)) (oMax o)
    when (cap <= 0) $ fatal 2 "max must be > 0"
    withDb defaultDbPath (loop o cfg cap 0)

loop :: Options -> Config -> Int -> Int -> Connection -> IO ()
loop opts cfg cap !i conn
    | i >= cap = hPutStrLn stderr
        ("icarium: reached max dispatches (" <> show cap <> "); stopping")
    | otherwise = do
        ts <- RT.listTasks conn [] True
        case ts of
            [] -> hPutStrLn stderr "icarium: ready queue empty; stopping"
            (t : _) -> do
                hPutStrLn stderr $ "icarium: dispatching " <> T.unpack (taskId t)
                res <- D.dispatch conn D.DispatchRequest
                    { D.drTask           = t
                    , D.drConfig         = cfg
                    , D.drDryRun         = oDryRun opts
                    , D.drModelOverride  = oModel  opts
                    , D.drEffortOverride = oEffort opts
                    , D.drBaseOverride   = oBase   opts
                    }
                D.applyOutcomeToTask conn t res
                TIO.hPutStrLn stderr $
                    "icarium: " <> dispatchOutcomeText (D.dresOutcome res)
                    <> " — " <> D.dresNotes res
                CD.printSummary res
                loop opts cfg cap (i + 1) conn
