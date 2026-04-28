module Icarium.Commands.Dispatch (Command, parser, run, printSummary) where

import           Control.Monad          (forM_, unless, void, when)
import           Data.Aeson             (FromJSON (..), decode, withObject, (.:?))
import qualified Data.ByteString.Lazy   as BL
import           Data.Either            (fromRight)
import           Data.IORef             (IORef, modifyIORef, newIORef, readIORef)
import           Data.Maybe             (fromMaybe, isNothing, listToMaybe, mapMaybe)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import qualified Data.Text.IO           as TIO
import           Data.Time              (UTCTime, defaultTimeLocale, diffUTCTime, getCurrentTime,
                                         parseTimeM)
import           Database.SQLite.Simple (Connection)
import           Options.Applicative
import           System.Directory       (doesFileExist)
import           System.Exit            (ExitCode (..))
import           System.IO              (hPutStrLn, stderr)
import           System.Posix.Signals   (Handler (..), installHandler, raiseSignal, sigINT)
import           System.Process.Typed   (nullStream, proc, runProcess, setStderr, setStdout)
import           Text.Printf            (printf)

import qualified Icarium.Git            as Git

import           Icarium.Commands.Util
import           Icarium.Config         (Config, DispatchConfig (..), cfgDispatch,
                                         defaultConfigPath, loadConfig)
import           Icarium.Db             (withDb)
import qualified Icarium.Dispatch       as D
import qualified Icarium.Render         as Render
import qualified Icarium.Repo.Dispatch  as RD
import qualified Icarium.Repo.Edge      as RE
import qualified Icarium.Repo.Task      as RT
import           Icarium.Types

data Command
    = Run     RunOpts
    | List    ListOpts
    | Show    ShowOpts
    | Logs    LogsOpts
    | Recover RecoverOpts

parser :: Parser Command
parser = subparser
    ( subcmd "run"
        "Run one dispatch (with TASK_ID) or drain the ready queue in priority order (no TASK_ID, optionally --max N)."
        (Run  <$> runP)
   <> subcmd "list"    "List dispatches (alias: ls)"               (List    <$> listP)
   <> subcmd "ls"      "List dispatches (alias: list)"             (List    <$> listP)
   <> subcmd "show"    "Show a single dispatch"                    (Show    <$> showP)
   <> subcmd "logs"    "Print the jsonl event log"                 (Logs    <$> logsP)
   <> subcmd "recover"
        "Reconcile dispatches whose orchestrator died mid-run: mark outcome interrupted, move task to blocked with structured notes."
        (Recover <$> recoverP)
    )
    <|> (List <$> listP)

run :: FilePath -> Command -> IO ()
run db = \case
    Run     o -> runRun     db o
    List    o -> runList    db o
    Show    o -> runShow    db o
    Logs    o -> runLogs    db o
    Recover o -> runRecover db o

-- =============================================================
-- run  (single-task dispatch or queue drain)
-- =============================================================

data RunOpts = RunOpts
    { rTaskId :: Maybe Text
    , rMax    :: Maybe Int
    , rModel  :: Maybe Text
    , rEffort :: Maybe Effort
    , rBase   :: Maybe Text
    , rDryRun :: Bool
    }

runP :: Parser RunOpts
runP = RunOpts
    <$> optional (T.pack <$> strArgument (metavar "TASK_ID"))
    <*> optional (option auto (long "max" <> metavar "N"
           <> help "Cap dispatches in queue mode (ignored with TASK_ID)"))
    <*> optional (T.pack <$> strOption (long "model"  <> metavar "MODEL"
           <> help "Override the model for this dispatch"))
    <*> optional (option effortReader (long "effort" <> metavar "LEVEL"
                                     <> help "low | medium | high"))
    <*> optional (T.pack <$> strOption (long "base-branch" <> metavar "NAME"
           <> help "Override the base branch for git operations"))
    <*> switch (long "dry-run" <> help "Build the plan and prompt; don't cut git or call claude")

runRun :: FilePath -> RunOpts -> IO ()
runRun db o = do
    cfg <- loadConfig defaultConfigPath >>= \case
        Left  e  -> fatal 2 ("config parse error:\n" <> e)
        Right c  -> pure c
    case rTaskId o of
        Just rawId ->
            withDb db $ \c -> do
                tid <- resolveOrFatal (RT.resolveTaskId c rawId)
                mt <- RT.getTask c tid
                case mt of
                    Nothing   -> fatal 1 ("task not found: " <> T.unpack tid)
                    Just task -> do
                        res <- D.dispatch c D.DispatchRequest
                            { D.drTask            = task
                            , D.drConfig          = cfg
                            , D.drDryRun          = rDryRun o
                            , D.drModelOverride   = rModel o
                            , D.drEffortOverride  = rEffort o
                            , D.drBaseOverride    = rBase  o
                            }
                        D.applyOutcomeToTask c task res
                        summarize res
        Nothing -> do
            let cap = fromMaybe (dcMaxDispatchesPerRun (cfgDispatch cfg)) (rMax o)
            when (cap <= 0) $ fatal 2 "max must be > 0"
            sigCount <- newIORef (0 :: Int)
            let sigHandler = do
                    modifyIORef sigCount (+1)
                    n <- readIORef sigCount
                    when (n >= 2) $ do
                        void $ installHandler sigINT Default Nothing
                        raiseSignal sigINT
            void $ installHandler sigINT (Catch sigHandler) Nothing
            withDb db (drainLoop o cfg cap 0 sigCount)

drainLoop :: RunOpts -> Config -> Int -> Int -> IORef Int -> Connection -> IO ()
drainLoop opts cfg cap !i sigCount conn
    | i >= cap = hPutStrLn stderr
        ("icarium: reached max dispatches (" <> show cap <> "); stopping")
    | otherwise = do
        ts <- RT.listTasks conn [] True Nothing Nothing
        case ts of
            [] -> hPutStrLn stderr "icarium: ready queue empty; stopping"
            (t : _) -> do
                hPutStrLn stderr $ "icarium: dispatching " <> T.unpack (taskId t)
                res <- D.dispatch conn D.DispatchRequest
                    { D.drTask           = t
                    , D.drConfig         = cfg
                    , D.drDryRun         = rDryRun opts
                    , D.drModelOverride  = rModel  opts
                    , D.drEffortOverride = rEffort opts
                    , D.drBaseOverride   = rBase   opts
                    }
                D.applyOutcomeToTask conn t res
                TIO.hPutStrLn stderr $
                    "icarium: " <> dispatchOutcomeText (D.dresOutcome res)
                    <> " \x2014 " <> D.dresNotes res
                printSummary res
                n <- readIORef sigCount
                if n >= 1
                    then hPutStrLn stderr
                        "icarium: SIGINT received; stopping after current dispatch"
                    else drainLoop opts cfg cap (i + 1) sigCount conn

-- =============================================================
-- Log parsing
-- =============================================================

data LogUsage = LogUsage
    { luInputTokens  :: Maybe Int
    , luOutputTokens :: Maybe Int
    , luCacheReads   :: Maybe Int
    }

instance FromJSON LogUsage where
    parseJSON = withObject "LogUsage" $ \o ->
        LogUsage
            <$> o .:? "input_tokens"
            <*> o .:? "output_tokens"
            <*> o .:? "cache_read_input_tokens"

data LogResult = LogResult
    { lrNumTurns      :: Maybe Int
    , lrDurationMs    :: Maybe Int
    , lrDurationApiMs :: Maybe Int
    , lrCostUsd       :: Maybe Double
    , lrUsage         :: Maybe LogUsage
    , lrResultText    :: Maybe Text
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
    if not exists then pure Nothing
    else do
        ls <- T.lines <$> TIO.readFile path
        let isResult l = "\"type\":\"result\"" `T.isInfixOf` l
            resultLines = filter isResult ls
        pure $ listToMaybe $ mapMaybe parseLine (reverse resultLines)
  where
    parseLine l = decode (BL.fromStrict (TE.encodeUtf8 l))

gitChangedFiles :: Text -> IO [Text]
gitChangedFiles baseSha = do
    r <- Git.runGit ["diff", "--name-only", T.unpack baseSha <> "..HEAD"]
    case r of
        Left  _   -> pure []
        Right out -> pure (filter (not . T.null) (T.lines out))

fmtMs :: Int -> Text
fmtMs ms
    | ms >= 1000 = T.pack (printf "%.1fs" (fromIntegral ms / 1000.0 :: Double))
    | otherwise  = T.pack (show ms) <> "ms"

trimResult :: Text -> Text
trimResult t =
    let ls      = filter (not . T.null) (T.lines t)
        lastLine = case ls of { [] -> t; _ -> last ls }
    in if T.length lastLine > 200 then T.take 197 lastLine <> "..." else lastLine

-- | Print the enriched summary block; does not exit on failure.
printSummary :: D.DispatchResult -> IO ()
printSummary r = do
    let idPart = fromMaybe "(dry-run)" (D.dresDispatchId r)
    TIO.putStrLn ""
    TIO.putStrLn $ "dispatch: " <> idPart
    TIO.putStrLn $ "outcome:  " <> dispatchOutcomeText (D.dresOutcome r)
    TIO.putStrLn $ "branch:   " <> D.dresBranch r
    TIO.putStrLn $ "notes:    " <> D.dresNotes  r
    case D.dresLogPath r of
        Nothing -> pure ()
        Just lp -> do
            mLR <- readLogResult lp
            case mLR of
                Nothing -> pure ()
                Just lr -> do
                    mapM_ TIO.putStrLn
                        [ "turns:    " <> maybe "-" (T.pack . show) (lrNumTurns lr)
                        , "duration: " <> maybe "-" fmtMs (lrDurationMs lr)
                            <> maybe "" (\a -> " (api: " <> fmtMs a <> ")") (lrDurationApiMs lr)
                        , "cost:     " <> maybe "-" (T.pack . printf "$%.4f") (lrCostUsd lr)
                        , "tokens:   " <> fmtTokens (lrUsage lr)
                        ]
                    case lrResultText lr >>= \t -> if T.null t then Nothing else Just t of
                        Nothing -> pure ()
                        Just t  -> TIO.putStrLn $ "result:   " <> trimResult t
    case D.dresBaseSha r of
        Nothing  -> pure ()
        Just sha -> do
            files <- gitChangedFiles sha
            case files of
                [] -> pure ()
                _  -> do
                    let shown = take 10 files
                        extra = length files - length shown
                        pad   = T.replicate 10 " "
                        extraLine = [T.pack (show extra) <> " more" | extra > 0]
                        allItems  = shown ++ extraLine
                    TIO.putStrLn $ "files:    " <> T.intercalate ("\n" <> pad) allItems
  where
    fmtTokens Nothing  = "-"
    fmtTokens (Just u) =
        "in "    <> maybe "-" (T.pack . show) (luInputTokens  u)
        <> " / out " <> maybe "-" (T.pack . show) (luOutputTokens u)
        <> " / cache " <> maybe "-" (T.pack . show) (luCacheReads   u)

summarize :: D.DispatchResult -> IO ()
summarize r = do
    printSummary r
    case D.dresOutcome r of
        OSuccess -> pure ()
        _        -> fatal 3 "dispatch did not succeed"

-- =============================================================
-- list
-- =============================================================

data ListOpts = ListOpts
    { lTask    :: Maybe Text
    , lOutcome :: Maybe DispatchOutcome
    }

listP :: Parser ListOpts
listP = ListOpts
    <$> optional (T.pack <$> strOption (long "task" <> metavar "TASK_ID"
                                        <> help "Only dispatches for this task"))
    <*> optional (option outcomeReader (long "outcome" <> metavar "OUTCOME"
                                        <> help "success | failure | interrupted"))

outcomeReader :: ReadM DispatchOutcome
outcomeReader = eitherReader $ \s ->
    case parseDispatchOutcome (T.pack s) of
        Just o  -> Right o
        Nothing -> Left ("invalid outcome: " <> s)

runList :: FilePath -> ListOpts -> IO ()
runList db o = withDb db $ \c -> do
    mTaskId <- traverse (resolveOrFatal . RT.resolveTaskId c) (lTask o)
    ds <- RD.listDispatches c mTaskId
    let filtered = case lOutcome o of
            Nothing -> ds
            Just want -> filter ((Just want ==) . dispatchOutcome) ds
    TIO.putStr (Render.renderDispatchList filtered)

-- =============================================================
-- show
-- =============================================================

newtype ShowOpts = ShowOpts { sId :: Text }

showP :: Parser ShowOpts
showP = ShowOpts . T.pack
    <$> strArgument (metavar "DISPATCH_ID")

runShow :: FilePath -> ShowOpts -> IO ()
runShow db o = withDb db $ \c -> do
    did <- RD.resolveDispatchId c (sId o) >>= \case
        Left err -> fatal 1 err
        Right x  -> pure x
    md <- RD.getDispatch c did
    case md of
        Nothing -> fatal 1 ("dispatch not found: " <> T.unpack did)
        Just d -> do
            mt <- RT.getTask c (dispatchTaskId d)
            ks <- RE.knowledgeDerivedFromDispatch c (dispatchTaskId d)
                    (dispatchStartedAt d) (dispatchEndedAt d)
            TIO.putStr (Render.renderDispatch d mt ks)

-- =============================================================
-- logs
-- =============================================================

data LogsOpts = LogsOpts
    { gId   :: Text
    , gTail :: Maybe Int
    }

logsP :: Parser LogsOpts
logsP = LogsOpts . T.pack
    <$> strArgument (metavar "DISPATCH_ID")
    <*> optional (option auto (long "tail" <> metavar "N"
                              <> help "Print only the last N lines"))

runLogs :: FilePath -> LogsOpts -> IO ()
runLogs db o = withDb db $ \c -> do
    did <- RD.resolveDispatchId c (gId o) >>= \case
        Left err -> fatal 1 err
        Right x  -> pure x
    md <- RD.getDispatch c did
    case md of
        Nothing -> fatal 1 ("dispatch not found: " <> T.unpack did)
        Just d  -> case dispatchLogPath d of
            Nothing -> fatal 1 ("no log recorded for dispatch " <> T.unpack did)
            Just p  -> do
                let path = T.unpack p
                exists <- doesFileExist path
                unless exists $
                    fatal 1 ("log file missing: " <> path)
                contents <- readFile path
                let ls = lines contents
                    out = case gTail o of
                        Nothing -> ls
                        Just n  -> drop (max 0 (length ls - n)) ls
                mapM_ putStrLn out

-- =============================================================
-- recover  (reconcile orphaned dispatches)
-- =============================================================

newtype RecoverOpts = RecoverOpts
    { recDispatchId :: Maybe Text
    }

recoverP :: Parser RecoverOpts
recoverP = RecoverOpts
    <$> optional (T.pack <$> strArgument (metavar "DISPATCH_ID"))

runRecover :: FilePath -> RecoverOpts -> IO ()
runRecover db o = do
    cfg <- loadConfig defaultConfigPath >>= \case
        Left  e -> fatal 2 ("config parse error:\n" <> e)
        Right c -> pure c
    let staleSec = dcHeartbeatStaleSeconds (cfgDispatch cfg)
    withDb db $ \c -> do
        open <- case recDispatchId o of
            Just raw -> do
                did <- RD.resolveDispatchId c raw >>= \case
                    Left err -> fatal 1 err
                    Right x  -> pure x
                md <- RD.getDispatch c did
                pure $ case md of
                    Just d | isNothing (dispatchOutcome d) -> [d]
                    _ -> []
            Nothing  -> RD.listOpenDispatches c
        if null open
            then TIO.putStrLn "no open dispatches"
            else do
                now <- getCurrentTime
                forM_ open (reconcileDispatch c now staleSec)

reconcileDispatch :: Connection -> UTCTime -> Int -> Dispatch -> IO ()
reconcileDispatch c now staleSec d = do
    alive <- maybe (pure False) isPidAlive (dispatchPid d)
    stale <- isHeartbeatStale now staleSec (dispatchHeartbeat d)
    if alive && not stale
        then pure ()
        else do
            uncommitted <- fmap not Git.isClean
            lastCommit  <- fmap (fromRight "") (Git.revParse (dispatchBranch d))
            let notes = T.intercalate "; "
                    [ "interrupted"
                    , "alive=" <> boolText alive
                    , "stale=" <> boolText stale
                    , "uncommitted=" <> boolText uncommitted
                    , "last_commit=" <> lastCommit
                    ]
            RD.finishDispatch c (dispatchId d) OInterrupted Nothing (Just notes)
            void $ RT.updateTask c (dispatchTaskId d) RT.emptyUpdate
                { RT.tuState       = Just Blocked
                , RT.tuBlockReason = Just (Just notes)
                }
            TIO.putStrLn $ "dispatch:" <> dispatchId d
                <> "  task:" <> dispatchTaskId d
                <> "  branch:" <> dispatchBranch d
                <> "  " <> notes

isPidAlive :: Int -> IO Bool
isPidAlive pid = do
    code <- runProcess
        $ setStdout nullStream
        $ setStderr nullStream
        $ proc "kill" ["-0", show pid]
    pure $ case code of
        ExitSuccess   -> True
        ExitFailure _ -> False

isHeartbeatStale :: UTCTime -> Int -> Text -> IO Bool
isHeartbeatStale now thresholdSec hbText =
    pure $ case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S"
                           (T.unpack hbText) of
        Nothing -> True
        Just hb -> diffUTCTime now hb > fromIntegral thresholdSec

boolText :: Bool -> Text
boolText True  = "yes"
boolText False = "no"
