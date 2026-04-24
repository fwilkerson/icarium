module Icarium.Commands.Init (Options, parser, run) where

import           Control.Monad       (when)
import qualified Data.Text.IO        as TIO
import           Options.Applicative
import           System.Directory    (doesFileExist)
import           System.Exit         (ExitCode (..), exitWith)
import           System.IO           (hPutStrLn, stderr)

import           Icarium.Config      (defaultConfigPath, defaultConfigText)
import           Icarium.Db          (defaultDbPath, initDb)

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

    -- DB is protected: we refuse to clobber existing task/knowledge state.
    when (dbExists && not (optForce o)) $
        fatal 2 ("database already exists: " <> defaultDbPath
                 <> " (use --force to reinitialize; this will NOT delete the file)")
    when (dbExists && optForce o) $
        ioError (userError "refusing to clobber existing DB; remove it manually")

    initDb defaultDbPath
    putStrLn $ "created  " <> defaultDbPath

    -- Config is forgiving: on a fresh clone the repo may already ship a
    -- tuned icarium.toml; don't error in that case, just keep it.
    if configExists && not (optForce o)
        then putStrLn $ "exists   " <> defaultConfigPath <> " (kept)"
        else do
            TIO.writeFile defaultConfigPath defaultConfigText
            putStrLn $ "created  " <> defaultConfigPath

    putStrLn "initialized."

fatal :: Int -> String -> IO a
fatal code msg = do
    hPutStrLn stderr ("icarium: error: " <> msg)
    exitWith (ExitFailure code)
