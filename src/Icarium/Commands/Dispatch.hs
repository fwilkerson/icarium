module Icarium.Commands.Dispatch (Command, parser, run, printSummary) where

import           Control.Monad         (unless)
import           Data.Aeson            (FromJSON (..), decode, encode, object, withObject, (.:?),
                                        (.=))
import qualified Data.ByteString.Lazy  as BL
import           Data.Maybe            (fromMaybe, listToMaybe, mapMaybe)
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import qualified Data.Text.IO          as TIO
import           Options.Applicative
import           System.Directory      (doesFileExist)
import           Text.Printf           (printf)

import qualified Icarium.Git           as Git

import           Icarium.Commands.Util
import           Icarium.Config        (defaultConfigPath, loadConfig)
import           Icarium.Db            (defaultDbPath, withDb)
import qualified Icarium.Dispatch      as D
import qualified Icarium.Repo.Dispatch as RD
import qualified Icarium.Repo.Edge     as RE
import qualified Icarium.Repo.Task     as RT
import           Icarium.Types

data Command
    = Run   RunOpts
    | List  ListOpts
    | Show  ShowOpts
    | Logs  LogsOpts

parser :: Parser Command
parser = subparser
    ( subcmd "run"  "Dispatch a task to a headless agent" (Run  <$> runP)
   <> subcmd "list" "List dispatches"                     (List <$> listP)
   <> subcmd "show" "Show a single dispatch"              (Show <$> showP)
   <> subcmd "logs" "Print the jsonl event log"           (Logs <$> logsP)
    )

run :: Command -> IO ()
run = \case
    Run o  -> runRun  o
    List o -> runList o
    Show o -> runShow o
    Logs o -> runLogs o

-- =============================================================
-- run  (the original dispatch behavior, unchanged)
-- =============================================================

data RunOpts = RunOpts
    { rTaskId :: Text
    , rModel  :: Maybe Text
    , rEffort :: Maybe Effort
    , rBase   :: Maybe Text
    , rDryRun :: Bool
    }

runP :: Parser RunOpts
runP = RunOpts . T.pack
    <$> strArgument (metavar "TASK_ID")
    <*> optional (T.pack <$> strOption (long "model"  <> metavar "MODEL"
           <> help "Override the model for this dispatch"))
    <*> optional (option effortReader (long "effort" <> metavar "LEVEL"
                                     <> help "low | medium | high"))
    <*> optional (T.pack <$> strOption (long "base-branch" <> metavar "NAME"
           <> help "Override the base branch for git operations"))
    <*> switch (long "dry-run" <> help "Build the plan and prompt; don't cut git or call claude")

runRun :: RunOpts -> IO ()
runRun o = do
    cfg <- loadConfig defaultConfigPath >>= \case
        Left  e  -> fatal 2 ("config parse error:\n" <> e)
        Right c  -> pure c
    withDb defaultDbPath $ \c -> do
        mt <- RT.getTask c (rTaskId o)
        case mt of
            Nothing   -> fatal 1 ("task not found: " <> T.unpack (rTaskId o))
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
                        extraLine = if extra > 0
                                    then [T.pack (show extra) <> " more"]
                                    else []
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
    , lJson    :: Bool
    }

listP :: Parser ListOpts
listP = ListOpts
    <$> optional (T.pack <$> strOption (long "task" <> metavar "TASK_ID"
                                        <> help "Only dispatches for this task"))
    <*> optional (option outcomeReader (long "outcome" <> metavar "OUTCOME"
                                        <> help "success | failure | interrupted"))
    <*> switch (long "json" <> help "Output JSON array instead of human-formatted table")

outcomeReader :: ReadM DispatchOutcome
outcomeReader = eitherReader $ \s ->
    case parseDispatchOutcome (T.pack s) of
        Just o  -> Right o
        Nothing -> Left ("invalid outcome: " <> s)

runList :: ListOpts -> IO ()
runList o = withDb defaultDbPath $ \c -> do
    ds <- RD.listDispatches c (lTask o)
    let filtered = case lOutcome o of
            Nothing -> ds
            Just want -> filter ((Just want ==) . dispatchOutcome) ds
    if lJson o
        then BL.putStr (encode filtered) >> putStrLn ""
        else TIO.putStr (renderDispatchList filtered)

renderDispatchList :: [Dispatch] -> Text
renderDispatchList ds =
    let header = T.intercalate "  "
            [ padR 12 "id"
            , padR 12 "task_id"
            , padR 11 "outcome"
            , padR 34 "branch"
            , "started_at"
            ]
        rows = map row ds
        row d = T.intercalate "  "
            [ padR 12 (T.take 10 (dispatchId d))
            , padR 12 (T.take 10 (dispatchTaskId d))
            , padR 11 (maybe "open" dispatchOutcomeText (dispatchOutcome d))
            , padR 34 (dispatchBranch d)
            , dispatchStartedAt d
            ]
    in T.unlines (header : rows)

padR :: Int -> Text -> Text
padR n t
    | T.length t >= n = t
    | otherwise       = t <> T.replicate (n - T.length t) " "

-- =============================================================
-- show
-- =============================================================

data ShowOpts = ShowOpts
    { sId   :: Text
    , sJson :: Bool
    }

showP :: Parser ShowOpts
showP = ShowOpts . T.pack
    <$> strArgument (metavar "DISPATCH_ID")
    <*> switch (long "json" <> help "Output JSON instead of human-formatted text")

runShow :: ShowOpts -> IO ()
runShow o = withDb defaultDbPath $ \c -> do
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
            if sJson o
                then BL.putStr (encode (object
                        [ "dispatch"            .= d
                        , "task"                .= mt
                        , "knowledge_produced"  .= ks
                        ])) >> putStrLn ""
                else TIO.putStr (renderDispatch d mt ks)

renderDispatch :: Dispatch -> Maybe Task -> [Knowledge] -> Text
renderDispatch d mt ks = T.unlines $
    [ field "id"            (T.take 10 (dispatchId d))
    , field "task_id"       (T.take 10 (dispatchTaskId d))
    , field "task_title"    (maybe "(task missing)" taskTitle mt)
    , field "branch"        (dispatchBranch d)
    , field "base_branch"   (dispatchBaseBranch d)
    , field "base_sha"      (dispatchBaseSha d)
    , field "pid"           (maybe "" (T.pack . show) (dispatchPid d))
    , field "model"         (dispatchModel d)
    , field "effort"        (effortText (dispatchEffort d))
    , field "started_at"    (dispatchStartedAt d)
    , field "heartbeat_at"  (dispatchHeartbeat d)
    , field "ended_at"      (fromMaybe "" (dispatchEndedAt d))
    , field "outcome"       (maybe "open" dispatchOutcomeText (dispatchOutcome d))
    , field "merge_sha"     (fromMaybe "" (dispatchMergeSha d))
    , field "last_commit"   (fromMaybe "" (dispatchLastCommit d))
    , field "log_path"      (fromMaybe "" (dispatchLogPath d))
    , field "notes"         (fromMaybe "" (dispatchNotes d))
    , ""
    , "Knowledge added:"
    ] ++ knowledgeLines
  where
    field k v = padR 14 (k <> ":") <> " " <> v
    knowledgeLines = case ks of
        [] -> ["  (none)"]
        _  -> map (\k -> "  " <> T.take 10 (knowledgeId k) <> "  " <> knowledgeTitle k) ks

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

runLogs :: LogsOpts -> IO ()
runLogs o = withDb defaultDbPath $ \c -> do
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
