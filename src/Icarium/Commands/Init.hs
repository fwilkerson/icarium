module Icarium.Commands.Init (Options, parser, run) where

import Control.Monad (when)
import qualified Data.Text.IO as TIO
import Options.Applicative
import System.Directory (doesFileExist)
import System.Exit (exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr)

import Icarium.Config (defaultConfigPath, defaultConfigText)
import Icarium.Db (defaultDbPath, initDb)

data Options = Options
    { optForce :: Bool
    }

parser :: Parser Options
parser = Options
    <$> switch (long "force" <> help "Overwrite existing config / reapply schema")

run :: Options -> IO ()
run o = do
    dbExists     <- doesFileExist defaultDbPath
    configExists <- doesFileExist defaultConfigPath

    when (dbExists && not (optForce o)) $
        fatal 2 ("database already exists: " <> defaultDbPath
                 <> " (use --force to reinitialize)")
    when (configExists && not (optForce o)) $
        fatal 2 ("config already exists: " <> defaultConfigPath
                 <> " (use --force to overwrite)")

    when (dbExists && optForce o) $
        ioError (userError "refusing to clobber existing DB; remove it manually")

    initDb defaultDbPath
    TIO.writeFile defaultConfigPath defaultConfigText

    putStrLn $ "created  " <> defaultDbPath
    putStrLn $ "created  " <> defaultConfigPath
    putStrLn   "initialized."

fatal :: Int -> String -> IO a
fatal code msg = do
    hPutStrLn stderr ("icarium: error: " <> msg)
    exitWith (ExitFailure code)
