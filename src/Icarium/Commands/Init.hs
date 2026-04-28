module Icarium.Commands.Init (Options, parser, run) where

import           Control.Monad             (when)
import qualified Data.Text.IO              as TIO
import           Options.Applicative
import           System.Directory          (doesFileExist)
import           System.Exit               (ExitCode (..), exitWith)
import           System.IO                 (hPutStrLn, stderr)

import qualified Data.Text                 as T
import           Icarium.Commands.Category (SyncReport (..), syncCategories)
import           Icarium.Config            (Config (..), defaultConfigPath, defaultConfigText,
                                            loadConfig)
import           Icarium.Db                (initDb, withDb)
import           Icarium.Types             (categoryAxisText)

newtype Options = Options
    { optForce :: Bool
    }

parser :: Parser Options
parser = Options
    <$> switch (long "force" <> help "Overwrite existing icarium.toml if present (errors if DB already exists)")

run :: FilePath -> Options -> IO ()
run dbPath o = do
    dbExists     <- doesFileExist dbPath
    configExists <- doesFileExist defaultConfigPath

    -- DB is protected: we refuse to clobber existing task/knowledge state.
    when (dbExists && not (optForce o)) $
        fatal 2 ("database already exists: " <> dbPath
                 <> " (use --force to reinitialize; this will NOT delete the file)")
    when (dbExists && optForce o) $
        ioError (userError "refusing to clobber existing DB; remove it manually")

    initDb dbPath
    putStrLn $ "created  " <> dbPath

    -- Config is forgiving: on a fresh clone the repo may already ship a
    -- tuned icarium.toml; don't error in that case, just keep it.
    if configExists && not (optForce o)
        then putStrLn $ "exists   " <> defaultConfigPath <> " (kept)"
        else do
            TIO.writeFile defaultConfigPath defaultConfigText
            putStrLn $ "created  " <> defaultConfigPath

    -- Seed categories from toml into the fresh DB.
    config <- loadConfig defaultConfigPath >>= either (fatal 2) pure
    withDb dbPath $ \conn -> do
        rpt <- syncCategories conn (cfgCategories config) False
        mapM_ (\(ax, n) -> putStrLn $
            "seeded   " <> T.unpack (categoryAxisText ax) <> ":" <> T.unpack n) (srInserted rpt)

    putStrLn "initialized."

fatal :: Int -> String -> IO a
fatal code msg = do
    hPutStrLn stderr ("icarium: error: " <> msg)
    exitWith (ExitFailure code)
