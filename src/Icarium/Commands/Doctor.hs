module Icarium.Commands.Doctor (Options, parser, run) where

import           Control.Exception      (bracket)
import           Control.Monad          (filterM)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Time              (UTCTime, defaultTimeLocale, diffUTCTime, getCurrentTime,
                                         parseTimeM)
import           Database.SQLite.Simple (close)
import           Options.Applicative
import           System.Directory       (doesFileExist, findExecutable)
import           System.Exit            (ExitCode (..), exitWith)
import           System.Process.Typed   (nullStream, proc, runProcess, setStderr, setStdout)

import           Icarium.Config         (DispatchConfig (..), cfgDispatch, defaultConfigPath, loadConfig)
import           Icarium.Db             (dbSchemaVersion, defaultDbPath, openDb)
import qualified Icarium.Repo.Dispatch  as RD
import           Icarium.Schema         (schemaVersion)
import           Icarium.Types          (Dispatch (..))

data Options = Options

parser :: Parser Options
parser = pure Options

data CheckResult = OK String | WARN String | FAIL String

data Check = Check
    { checkName   :: String
    , checkResult :: CheckResult
    }

run :: Options -> IO ()
run _ = do
    basic <- sequence
        [ checkConfig
        , checkFile   "database" defaultDbPath
        , checkSchema
        , checkBinary "claude"
        , checkBinary "git"
        ]
    orphans <- checkOrphanedDispatches
    let checks = basic ++ orphans
    mapM_ printCheck checks
    if any isFail checks
        then exitWith (ExitFailure 2)
        else putStrLn "all checks passed."

isFail :: Check -> Bool
isFail (Check _ (FAIL _)) = True
isFail _                  = False

checkFile :: String -> FilePath -> IO Check
checkFile name path = do
    e <- doesFileExist path
    pure $ Check name $
        if e then OK path
             else FAIL ("missing: " <> path)

checkConfig :: IO Check
checkConfig = do
    e <- doesFileExist defaultConfigPath
    if not e
        then pure $ Check "config" (FAIL ("missing: " <> defaultConfigPath))
        else do
            r <- loadConfig defaultConfigPath
            pure $ Check "config" $ case r of
                Right _  -> OK defaultConfigPath
                Left msg -> FAIL ("parse error\n" <> msg)

checkBinary :: String -> IO Check
checkBinary name = do
    r <- findExecutable name
    pure $ Check ("bin:" <> name) $
        maybe (FAIL "not on PATH") OK r

checkSchema :: IO Check
checkSchema = do
    e <- doesFileExist defaultDbPath
    if not e
        then pure $ Check "schema" (FAIL "no database")
        else do
            v <- bracket (openDb defaultDbPath) close dbSchemaVersion
            let expected = fromIntegral schemaVersion :: Integer
                actual   = fromIntegral v             :: Integer
            pure $ Check "schema" $
                if actual == expected
                    then OK ("v" <> show actual)
                    else FAIL ("expected v" <> show expected
                               <> ", got v" <> show actual)

checkOrphanedDispatches :: IO [Check]
checkOrphanedDispatches = do
    dbOk  <- doesFileExist defaultDbPath
    cfgOk <- doesFileExist defaultConfigPath
    if not dbOk || not cfgOk
        then pure []
        else do
            mcfg <- either (const Nothing) Just <$> loadConfig defaultConfigPath
            case mcfg of
                Nothing  -> pure []
                Just cfg -> do
                    let staleSec = dcHeartbeatStaleSeconds (cfgDispatch cfg)
                    now <- getCurrentTime
                    bracket (openDb defaultDbPath) close $ \conn -> do
                        open    <- RD.listOpenDispatches conn
                        orphans <- filterM (isOrphanedDispatch now staleSec) open
                        pure $ if null orphans
                            then [Check "dispatches" (OK "no orphaned dispatches")]
                            else map toWarn orphans
  where
    toWarn d = Check "dispatch"
        (WARN $ T.unpack (T.take 8 (dispatchId d))
             <> ": pid dead or heartbeat stale."
             <> " Run `icarium dispatch recover` to reconcile.")

isOrphanedDispatch :: UTCTime -> Int -> Dispatch -> IO Bool
isOrphanedDispatch now staleSec d = do
    alive <- maybe (pure False) checkPidAlive (dispatchPid d)
    stale <- checkHeartbeatStale now staleSec (dispatchHeartbeat d)
    pure (not alive || stale)

checkPidAlive :: Int -> IO Bool
checkPidAlive pid = do
    code <- runProcess
        $ setStdout nullStream
        $ setStderr nullStream
        $ proc "kill" ["-0", show pid]
    pure $ case code of
        ExitSuccess   -> True
        ExitFailure _ -> False

checkHeartbeatStale :: UTCTime -> Int -> Text -> IO Bool
checkHeartbeatStale now thresholdSec hbText =
    pure $ case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S"
                           (T.unpack hbText) of
        Nothing -> True
        Just hb -> diffUTCTime now hb > fromIntegral thresholdSec

printCheck :: Check -> IO ()
printCheck c = case checkResult c of
    OK   msg -> putStrLn $ "  ok    " <> pad 10 (checkName c) <> "  " <> msg
    WARN msg -> putStrLn $ "  WARN  " <> pad 10 (checkName c) <> "  " <> msg
    FAIL msg -> putStrLn $ "  FAIL  " <> pad 10 (checkName c) <> "  " <> msg
  where
    pad n s = s <> replicate (max 0 (n - length s)) ' '
