module Icarium.Commands.Dispatch (Command, parser, run, printSummary) where

import Control.Monad (forM, forM_, unless, void, when)
import Data.Either (fromRight)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.List (nub)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Database.SQLite.Simple (Connection)
import Options.Applicative
import System.Directory (doesFileExist)
import System.IO (hPutStrLn, stderr)
import System.Posix.Signals (Handler (..), installHandler, raiseSignal, sigINT)
import Text.Printf (printf)

import Icarium.Dispatch.LogResult (
    LogResult (..),
    LogUsage (..),
    fmtMs,
    readLogResult,
    trimResult,
 )
import Icarium.Git qualified as Git

import Icarium.Commands.Util
import Icarium.Config (
    Config,
    DispatchConfig (..),
    cfgDispatch,
    defaultConfigPath,
    loadConfig,
 )
import Icarium.Db (parseDbTime, withDb)
import Icarium.Dispatch qualified as D
import Icarium.Heartbeat (heartbeatStale, pidAlive)
import Icarium.Render qualified as Render
import Icarium.Repo.Dispatch qualified as RD
import Icarium.Repo.Edge qualified as RE
import Icarium.Repo.Task qualified as RT
import Icarium.Types

data Command
    = Run RunOpts
    | List ListOpts
    | Show ShowOpts
    | Logs LogsOpts
    | Recover RecoverOpts

parser :: Parser Command
parser =
    subparser
        ( subcmd
            "run"
            "Run one dispatch (with TASK_ID) or drain the ready queue in priority order (no TASK_ID, optionally --max N)."
            (Run <$> runP)
            <> subcmd "list" "List dispatches (alias: ls)" (List <$> listP)
            <> subcmd "show" "Show a single dispatch" (Show <$> showP)
            <> subcmd "logs" "Print the jsonl event log" (Logs <$> logsP)
            <> subcmd
                "recover"
                "Reconcile dispatches whose orchestrator died mid-run: mark outcome interrupted, move task to blocked with structured notes."
                (Recover <$> recoverP)
        )

run :: FilePath -> Command -> IO ()
run db = \case
    Run o -> runRun db o
    List o -> runList db o
    Show o -> runShow db o
    Logs o -> runLogs db o
    Recover o -> runRecover db o

-- =============================================================
-- run  (single-task dispatch or queue drain)
-- =============================================================

data RunOpts = RunOpts
    { rTaskId :: Maybe Text
    , rMax :: Maybe Int
    , rModel :: Maybe Text
    , rEffort :: Maybe Effort
    , rBase :: Maybe Text
    , rDryRun :: Bool
    }

runP :: Parser RunOpts
runP =
    RunOpts
        <$> optional (T.pack <$> strArgument (metavar "TASK_ID"))
        <*> optional
            ( option
                auto
                ( long "max"
                    <> metavar "N"
                    <> help "Cap dispatches in queue mode (ignored with TASK_ID)"
                )
            )
        <*> optional (textOption "model" "MODEL" "Override the model for this dispatch")
        <*> optional
            ( option
                effortReader
                ( long "effort"
                    <> metavar "LEVEL"
                    <> help "low | medium | high"
                )
            )
        <*> optional (textOption "base-branch" "NAME" "Override the base branch for git operations")
        <*> switch (long "dry-run" <> help "Build the plan and prompt; don't cut git or call claude")

runRun :: FilePath -> RunOpts -> IO ()
runRun db o = do
    cfg <-
        loadConfig defaultConfigPath >>= \case
            Left e -> fatal 2 ("config parse error:\n" <> e)
            Right c -> pure c
    case rTaskId o of
        Just rawId ->
            withDb db $ \c -> do
                tid <- resolveOrFatal (RT.resolveTaskId c rawId)
                mt <- RT.getTask c tid
                case mt of
                    Nothing -> fatal 1 ("task not found: " <> T.unpack tid)
                    Just task -> do
                        res <-
                            D.dispatch
                                c
                                D.DispatchRequest
                                    { D.drTask = task
                                    , D.drConfig = cfg
                                    , D.drDbPath = db
                                    , D.drDryRun = rDryRun o
                                    , D.drModelOverride = rModel o
                                    , D.drEffortOverride = rEffort o
                                    , D.drBaseOverride = rBase o
                                    }
                        D.applyOutcomeToTask c task res
                        summarize res
        Nothing -> do
            mapM_ (\n -> when (n <= 0) $ fatal 2 "max must be > 0") (rMax o)
            sigCount <- newIORef (0 :: Int)
            let sigHandler = do
                    modifyIORef sigCount (+ 1)
                    n <- readIORef sigCount
                    when (n >= 2) $ do
                        void $ installHandler sigINT Default Nothing
                        raiseSignal sigINT
            void $ installHandler sigINT (Catch sigHandler) Nothing
            withDb db $ \conn -> do
                let ctx =
                        DrainCtx
                            { dctxDb = db
                            , dctxOpts = o
                            , dctxCfg = cfg
                            , dctxMCap = rMax o
                            , dctxSigCount = sigCount
                            , dctxConn = conn
                            }
                drainLoop ctx 0

data DrainCtx = DrainCtx
    { dctxDb :: FilePath
    , dctxOpts :: RunOpts
    , dctxCfg :: Config
    , dctxMCap :: Maybe Int
    , dctxSigCount :: IORef Int
    , dctxConn :: Connection
    }

drainLoop :: DrainCtx -> Int -> IO ()
drainLoop ctx !i
    | Just cap <- dctxMCap ctx
    , i >= cap =
        hPutStrLn stderr ("icarium: reached max dispatches (" <> show cap <> "); stopping")
    | otherwise = do
        let conn = dctxConn ctx
            opts = dctxOpts ctx
            cfg = dctxCfg ctx
            db = dctxDb ctx
        ts <- RT.listTasks conn [] True Nothing Nothing
        case ts of
            [] -> hPutStrLn stderr "icarium: ready queue empty; stopping"
            (t : _) -> do
                hPutStrLn stderr $ "icarium: dispatching " <> T.unpack (taskId t)
                res <-
                    D.dispatch
                        conn
                        D.DispatchRequest
                            { D.drTask = t
                            , D.drConfig = cfg
                            , D.drDbPath = db
                            , D.drDryRun = rDryRun opts
                            , D.drModelOverride = rModel opts
                            , D.drEffortOverride = rEffort opts
                            , D.drBaseOverride = rBase opts
                            }
                D.applyOutcomeToTask conn t res
                TIO.hPutStrLn stderr $
                    "icarium: "
                        <> dispatchOutcomeText (D.dresOutcome res)
                        <> " \x2014 "
                        <> D.dresNotes res
                printSummary res
                n <- readIORef (dctxSigCount ctx)
                if n >= 1
                    then
                        hPutStrLn
                            stderr
                            "icarium: SIGINT received; stopping after current dispatch"
                    else drainLoop ctx (i + 1)

-- | Print the enriched summary block; does not exit on failure.
printSummary :: D.DispatchResult -> IO ()
printSummary r = do
    let idPart = fromMaybe "(dry-run)" (D.dresDispatchId r)
    TIO.putStrLn ""
    TIO.putStrLn $ "dispatch: " <> idPart
    TIO.putStrLn $ "outcome:  " <> dispatchOutcomeText (D.dresOutcome r)
    TIO.putStrLn $ "branch:   " <> D.dresBranch r
    TIO.putStrLn $ "notes:    " <> D.dresNotes r
    case D.dresLogPath r of
        Nothing -> pure ()
        Just lp -> do
            mLR <- readLogResult lp
            case mLR of
                Nothing -> pure ()
                Just lr -> do
                    mapM_
                        TIO.putStrLn
                        [ "turns:    " <> maybe "-" (T.pack . show) (lrNumTurns lr)
                        , "duration: "
                            <> maybe "-" fmtMs (lrDurationMs lr)
                            <> maybe "" (\a -> " (api: " <> fmtMs a <> ")") (lrDurationApiMs lr)
                        , "cost:     " <> maybe "-" (T.pack . printf "$%.4f") (lrCostUsd lr)
                        , "tokens:   " <> fmtTokens (lrUsage lr)
                        ]
                    case lrResultText lr >>= \t -> if T.null t then Nothing else Just t of
                        Nothing -> pure ()
                        Just t -> TIO.putStrLn $ "result:   " <> trimResult t
    case D.dresBaseSha r of
        Nothing -> pure ()
        Just sha -> do
            files <- Git.changedFiles sha
            case files of
                [] -> pure ()
                _ -> do
                    let shown = take 10 files
                        extra = length files - length shown
                        pad = T.replicate 10 " "
                        extraLine = [T.pack (show extra) <> " more" | extra > 0]
                        allItems = shown ++ extraLine
                    TIO.putStrLn $ "files:    " <> T.intercalate ("\n" <> pad) allItems
  where
    fmtTokens Nothing = "-"
    fmtTokens (Just u) =
        "in "
            <> maybe "-" (T.pack . show) (luInputTokens u)
            <> " / out "
            <> maybe "-" (T.pack . show) (luOutputTokens u)
            <> " / cache "
            <> maybe "-" (T.pack . show) (luCacheReads u)

summarize :: D.DispatchResult -> IO ()
summarize r = do
    printSummary r
    case D.dresOutcome r of
        OSuccess -> pure ()
        _ -> fatal 3 "dispatch did not succeed"

-- =============================================================
-- list
-- =============================================================

formatDispatchDuration :: UTCTime -> Dispatch -> Text
formatDispatchDuration now d =
    case parseDbTime (dispatchStartedAt d) of
        Nothing -> ""
        Just start ->
            let (diff, isOpen) = case dispatchEndedAt d >>= parseDbTime of
                    Just end -> (diffUTCTime end start, False)
                    Nothing -> (diffUTCTime now start, True)
                secs = max 0 (round (toRational diff) :: Int)
                body = Render.fmtSecs secs
             in if isOpen then body <> " (running)" else body

data ListOpts = ListOpts
    { lTask :: Maybe Text
    , lOutcome :: Maybe DispatchOutcome
    }

listP :: Parser ListOpts
listP =
    ListOpts
        <$> optional (textOption "task" "TASK_ID" "Only dispatches for this task")
        <*> optional
            ( option
                outcomeReader
                ( long "outcome"
                    <> metavar "OUTCOME"
                    <> help "success | failure | interrupted"
                )
            )

outcomeReader :: ReadM DispatchOutcome
outcomeReader = eitherReader $ \s ->
    case parseDispatchOutcome (T.pack s) of
        Just o -> Right o
        Nothing -> Left ("invalid outcome: " <> s)

runList :: FilePath -> ListOpts -> IO ()
runList db o = withDb db $ \c -> do
    mTaskId <- traverse (resolveOrFatal . RT.resolveTaskId c) (lTask o)
    ds <- RD.listDispatches c mTaskId
    let filtered = case lOutcome o of
            Nothing -> ds
            Just want -> filter ((Just want ==) . dispatchOutcome) ds
    now <- getCurrentTime
    let taskIds = nub (map dispatchTaskId filtered)
    tasks <- RT.getTasksByIds c taskIds
    let titleMap = [(taskId t, taskTitle t) | t <- tasks]
    knowCounts <- forM filtered $ \d ->
        length
            <$> RE.knowledgeDerivedFromDispatch
                c
                (dispatchTaskId d)
                (dispatchStartedAt d)
                (dispatchEndedAt d)
    let rows =
            zipWith
                ( \d kc ->
                    Render.DispatchRow
                        { Render.drDispatch = d
                        , Render.drTaskTitle = fromMaybe "" (lookup (dispatchTaskId d) titleMap)
                        , Render.drKnowCount = kc
                        , Render.drDuration = formatDispatchDuration now d
                        }
                )
                filtered
                knowCounts
    utf8 <- detectUtf8
    TIO.putStr (Render.renderDispatchList utf8 rows)

-- =============================================================
-- show
-- =============================================================

newtype ShowOpts = ShowOpts {sId :: Text}

showP :: Parser ShowOpts
showP =
    ShowOpts . T.pack
        <$> strArgument (metavar "DISPATCH_ID")

runShow :: FilePath -> ShowOpts -> IO ()
runShow db o = withDb db $ \c -> do
    did <- resolveOrFatal (RD.resolveDispatchId c (sId o))
    md <- RD.getDispatch c did
    case md of
        Nothing -> fatal 1 ("dispatch not found: " <> T.unpack did)
        Just d -> do
            mt <- RT.getTask c (dispatchTaskId d)
            ks <-
                RE.knowledgeDerivedFromDispatch
                    c
                    (dispatchTaskId d)
                    (dispatchStartedAt d)
                    (dispatchEndedAt d)
            TIO.putStr (Render.renderDispatch d mt ks)

-- =============================================================
-- logs
-- =============================================================

data LogsOpts = LogsOpts
    { gId :: Text
    , gTail :: Maybe Int
    }

logsP :: Parser LogsOpts
logsP =
    LogsOpts . T.pack
        <$> strArgument (metavar "DISPATCH_ID")
        <*> optional
            ( option
                auto
                ( long "tail"
                    <> metavar "N"
                    <> help "Print only the last N lines"
                )
            )

runLogs :: FilePath -> LogsOpts -> IO ()
runLogs db o = withDb db $ \c -> do
    did <- resolveOrFatal (RD.resolveDispatchId c (gId o))
    md <- RD.getDispatch c did
    case md of
        Nothing -> fatal 1 ("dispatch not found: " <> T.unpack did)
        Just d -> case dispatchLogPath d of
            Nothing -> fatal 1 ("no log recorded for dispatch " <> T.unpack did)
            Just p -> do
                let path = T.unpack p
                exists <- doesFileExist path
                unless exists $
                    fatal 1 ("log file missing: " <> path)
                contents <- readFile path
                let ls = lines contents
                    out = case gTail o of
                        Nothing -> ls
                        Just n -> drop (max 0 (length ls - n)) ls
                mapM_ putStrLn out

-- =============================================================
-- recover  (reconcile orphaned dispatches)
-- =============================================================

newtype RecoverOpts = RecoverOpts
    { recDispatchId :: Maybe Text
    }

recoverP :: Parser RecoverOpts
recoverP =
    RecoverOpts
        <$> optional (T.pack <$> strArgument (metavar "DISPATCH_ID"))

runRecover :: FilePath -> RecoverOpts -> IO ()
runRecover db o = do
    cfg <-
        loadConfig defaultConfigPath >>= \case
            Left e -> fatal 2 ("config parse error:\n" <> e)
            Right c -> pure c
    let staleSec = dcHeartbeatStaleSeconds (cfgDispatch cfg)
    withDb db $ \c -> do
        open <- case recDispatchId o of
            Just raw -> do
                did <- resolveOrFatal (RD.resolveDispatchId c raw)
                md <- RD.getDispatch c did
                pure $ case md of
                    Just d | isNothing (dispatchOutcome d) -> [d]
                    _ -> []
            Nothing -> RD.listOpenDispatches c
        if null open
            then TIO.putStrLn "no open dispatches"
            else do
                now <- getCurrentTime
                forM_ open (reconcileDispatch c now staleSec)

reconcileDispatch :: Connection -> UTCTime -> Int -> Dispatch -> IO ()
reconcileDispatch c now staleSec d = do
    alive <- maybe (pure False) pidAlive (dispatchPid d)
    let stale = heartbeatStale now staleSec (dispatchHeartbeat d)
    if alive && not stale
        then pure ()
        else do
            uncommitted <- fmap not Git.isClean
            lastCommit <- fmap (fromRight "") (Git.revParse (dispatchBranch d))
            let notes =
                    T.intercalate
                        "; "
                        [ "interrupted"
                        , "alive=" <> boolText alive
                        , "stale=" <> boolText stale
                        , "uncommitted=" <> boolText uncommitted
                        , "last_commit=" <> lastCommit
                        ]
            RD.finishDispatch c (dispatchId d) OInterrupted Nothing (Just notes)
            void $
                RT.updateTask
                    c
                    (dispatchTaskId d)
                    RT.emptyUpdate
                        { RT.tuState = Just Blocked
                        , RT.tuBlockReason = Just (Just notes)
                        }
            TIO.putStrLn $
                "dispatch:"
                    <> dispatchId d
                    <> "  task:"
                    <> dispatchTaskId d
                    <> "  branch:"
                    <> dispatchBranch d
                    <> "  "
                    <> notes

boolText :: Bool -> Text
boolText True = "yes"
boolText False = "no"
