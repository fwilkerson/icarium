module Icarium.Commands.Doctor (Options, parser, run) where

import Control.Exception (bracket)
import Database.SQLite.Simple (close)
import Options.Applicative
import System.Directory (doesFileExist, findExecutable)
import System.Exit (exitWith, ExitCode(..))

import Icarium.Config (defaultConfigPath, loadConfig)
import Icarium.Db (defaultDbPath, dbSchemaVersion, openDb)
import Icarium.Schema (schemaVersion)

data Options = Options

parser :: Parser Options
parser = pure Options

data Check = Check
    { checkName   :: String
    , checkResult :: Either String String
    }

run :: Options -> IO ()
run _ = do
    checks <- sequence
        [ checkConfig
        , checkFile   "database" defaultDbPath
        , checkSchema
        , checkBinary "claude"
        , checkBinary "git"
        ]
    mapM_ printCheck checks
    if any (isLeft . checkResult) checks
        then exitWith (ExitFailure 2)
        else putStrLn "all checks passed."

checkFile :: String -> FilePath -> IO Check
checkFile name path = do
    e <- doesFileExist path
    pure $ Check name $
        if e then Right path
             else Left  ("missing: " <> path)

-- | Loads and parses the config — not just existence-checks it.
checkConfig :: IO Check
checkConfig = do
    e <- doesFileExist defaultConfigPath
    if not e
        then pure $ Check "config" (Left ("missing: " <> defaultConfigPath))
        else do
            r <- loadConfig defaultConfigPath
            pure $ Check "config" $ case r of
                Right _  -> Right defaultConfigPath
                Left msg -> Left ("parse error\n" <> msg)

checkBinary :: String -> IO Check
checkBinary name = do
    r <- findExecutable name
    pure $ Check ("bin:" <> name) $
        maybe (Left "not on PATH") Right r

checkSchema :: IO Check
checkSchema = do
    e <- doesFileExist defaultDbPath
    if not e
        then pure $ Check "schema" (Left "no database")
        else do
            v <- bracket (openDb defaultDbPath) close dbSchemaVersion
            let expected = fromIntegral schemaVersion :: Integer
                actual   = fromIntegral v             :: Integer
            pure $ Check "schema" $
                if actual == expected
                    then Right ("v" <> show actual)
                    else Left ("expected v" <> show expected
                               <> ", got v" <> show actual)

printCheck :: Check -> IO ()
printCheck c = case checkResult c of
    Right msg -> putStrLn $ "  ok    " <> pad 10 (checkName c) <> "  " <> msg
    Left  msg -> putStrLn $ "  FAIL  " <> pad 10 (checkName c) <> "  " <> msg
  where
    pad n s = s <> replicate (max 0 (n - length s)) ' '

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False
