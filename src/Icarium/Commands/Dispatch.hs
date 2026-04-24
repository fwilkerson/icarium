module Icarium.Commands.Dispatch (Options, parser, run) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative

import Icarium.Commands.Util
import Icarium.Config (defaultConfigPath, loadConfig)
import Icarium.Db (defaultDbPath, withDb)
import qualified Icarium.Dispatch as D
import qualified Icarium.Repo.Task as RT
import Icarium.Types

data Options = Options
    { oTaskId :: Text
    , oModel  :: Maybe Text
    , oEffort :: Maybe Effort
    , oBase   :: Maybe Text
    , oDryRun :: Bool
    }

parser :: Parser Options
parser = Options
    <$> (T.pack <$> strArgument (metavar "TASK_ID"))
    <*> optional (T.pack <$> strOption (long "model"  <> metavar "MODEL"))
    <*> optional (option effortReader (long "effort" <> metavar "LEVEL"
                                     <> help "low | medium | high"))
    <*> optional (T.pack <$> strOption (long "base-branch" <> metavar "NAME"))
    <*> switch (long "dry-run" <> help "Build the plan and prompt; don't cut git or call claude")

run :: Options -> IO ()
run o = do
    cfg <- loadConfig defaultConfigPath >>= \case
        Left  e  -> fatal 2 ("config parse error:\n" <> e)
        Right c  -> pure c
    withDb defaultDbPath $ \c -> do
        mt <- RT.getTask c (oTaskId o)
        case mt of
            Nothing -> fatal 1 ("task not found: " <> T.unpack (oTaskId o))
            Just task -> do
                res <- D.dispatch c D.DispatchRequest
                    { D.drTask            = task
                    , D.drConfig          = cfg
                    , D.drDryRun          = oDryRun o
                    , D.drModelOverride   = oModel o
                    , D.drEffortOverride  = oEffort o
                    , D.drBaseOverride    = oBase  o
                    }
                D.applyOutcomeToTask c task res
                summarize res

summarize :: D.DispatchResult -> IO ()
summarize r = do
    let idPart = maybe "(dry-run)" id (D.dresDispatchId r)
    TIO.putStrLn ""
    TIO.putStrLn $ "dispatch: "  <> idPart
    TIO.putStrLn $ "outcome:  "  <> dispatchOutcomeText (D.dresOutcome r)
    TIO.putStrLn $ "branch:   "  <> D.dresBranch r
    TIO.putStrLn $ "notes:    "  <> D.dresNotes  r
    case D.dresOutcome r of
        OSuccess -> pure ()
        _        -> fatal 3 "dispatch did not succeed"
