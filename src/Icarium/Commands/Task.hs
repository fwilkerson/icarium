module Icarium.Commands.Task (Command, parser, run) where

import           Control.Monad          (forM_, unless, void, when)
import           Data.Aeson             (encode, object, (.=))
import qualified Data.ByteString.Lazy   as BL
import           Data.Maybe             (fromMaybe)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Database.SQLite.Simple (Connection)
import           Options.Applicative
import           System.Exit            (ExitCode (..), exitWith)

import           Icarium.Commands.Util
import           Icarium.Db             (defaultDbPath, withDb)
import qualified Icarium.Render         as Render
import qualified Icarium.Repo.Category  as RC
import qualified Icarium.Repo.Edge      as RE
import qualified Icarium.Repo.Knowledge as RK
import qualified Icarium.Repo.Task      as RT
import           Icarium.Types

data Command
    = Add AddOpts
    | List ListOpts
    | Show ShowOpts
    | Update UpdateOpts
    | Rm RmOpts
    | Next NextOpts

parser :: Parser Command
parser = subparser
    ( subcmd "add"      "Add a task"                                 (Add     <$> addP)
   <> subcmd "show"     "Show a task"                                (Show    <$> showP)
   <> subcmd "update"   "Update a task"                              (Update  <$> updateP)
   <> subcmd "rm"       "Delete a task"                              (Rm      <$> rmP)
   <> subcmd "next"     "Print next ready task id; exit 1 if empty"  (Next    <$> nextP)
    )
    <|> (List <$> listP)

run :: Command -> IO ()
run = \case
    Add o     -> runAdd o
    List o    -> runList o
    Show o    -> runShow o
    Update o  -> runUpdate o
    Rm o      -> runRm o
    Next o    -> runNext o

-- =============================================================
-- add
-- =============================================================

data AddOpts = AddOpts
    { aTitle       :: Text
    , aBody        :: BodyInput
    , aState       :: TaskState
    , aPriority    :: Maybe Int
    , aDomains     :: [Text]
    , aDisciplines :: [Text]
    , aDependsOn   :: [Text]
    , aReferences  :: [Text]
    }

addP :: Parser AddOpts
addP = AddOpts . T.pack
    <$> strArgument (metavar "TITLE" <> help "Task title. Keep ≤ 72 chars; longer titles are truncated in `task list`.")
    <*> bodyInputParser
    <*> option taskStateReader
            ( long "state" <> metavar "STATE" <> value Planned
           <> help "idea | planned | ready (default: planned)" )
    <*> optional (option auto (long "priority" <> metavar "N"
           <> help "Set priority (lower number = higher priority)"))
    <*> many (T.pack <$> strOption (long "domain" <> metavar "NAME"
           <> help "Tag with this domain category"))
    <*> many (T.pack <$> strOption (long "discipline" <> metavar "NAME"
           <> help "Tag with this discipline category"))
    <*> many (T.pack <$> strOption (long "depends-on" <> metavar "TASK_ID"
           <> help "Add a depends_on edge to TASK_ID"))
    <*> many (T.pack <$> strOption (long "references" <> metavar "KNOWLEDGE_ID"
           <> help "Add a references edge to KNOWLEDGE_ID"))

runAdd :: AddOpts -> IO ()
runAdd o = withDb defaultDbPath $ \c -> do
    body <- resolveBody (aBody o)
    unless (aState o `elem` [Idea, Planned, Ready]) $
        fatal 2 "on add: state must be idea | planned | ready"

    -- Pre-validate referenced categories and nodes so we fail before insert.
    domains <- mapM (requireCategory c Domain)     (aDomains o)
    disc    <- mapM (requireCategory c Discipline) (aDisciplines o)
    depIds  <- mapM (requireTask c)      (aDependsOn o)
    refIds  <- mapM (requireKnowledge c) (aReferences o)

    tid <- RT.insertTask c RT.NewTask
        { RT.ntTitle    = aTitle o
        , RT.ntBody     = body
        , RT.ntState    = aState o
        , RT.ntPriority = aPriority o
        }
    forM_ (domains <> disc) $ \cat ->
        RC.attachTaskCategory c tid (categoryId cat)
    forM_ depIds $ \depId ->
        void $ RE.insertEdge c DependsOn TaskNode tid TaskNode depId
    forM_ refIds $ \refId ->
        void $ RE.insertEdge c References TaskNode tid KnowledgeNode refId
    TIO.putStrLn tid

requireCategory :: Connection -> CategoryAxis -> Text -> IO Category
requireCategory c axis name = do
    mc <- RC.findCategory c axis name
    case mc of
        Just cat -> pure cat
        Nothing  -> fatal 2 ("unknown " <> T.unpack (categoryAxisText axis)
                                       <> ": " <> T.unpack name)

-- | Resolve a task input (ULID prefix) to a canonical ULID.
requireTask :: Connection -> Text -> IO Text
requireTask c input = do
    r <- RT.resolveTaskId c input
    case r of
        Right tid -> pure tid
        Left err  -> fatal 2 err

-- | Resolve a knowledge input (ULID prefix) to a canonical ULID.
requireKnowledge :: Connection -> Text -> IO Text
requireKnowledge c input = do
    r <- RK.resolveKnowledgeId c input
    case r of
        Right kid -> pure kid
        Left err  -> fatal 2 err

-- =============================================================
-- list
-- =============================================================

data ListOpts = ListOpts
    { lStates     :: [TaskState]
    , lAll        :: Bool
    , lReady      :: Bool
    , lDomain     :: Maybe Text
    , lDiscipline :: Maybe Text
    , lJson       :: Bool
    }

listP :: Parser ListOpts
listP = ListOpts
    <$> many (option taskStateReader (long "state" <> metavar "STATE"
           <> help "Filter by task state (repeatable)"))
    <*> switch (long "all"   <> help "Include done tasks")
    <*> switch (long "ready" <> help "Only state=ready tasks with all depends-on satisfied (matches what `dispatch run` and `task next` pick)")
    <*> optional (T.pack <$> strOption (long "domain"     <> metavar "NAME"
           <> help "Filter by domain category"))
    <*> optional (T.pack <$> strOption (long "discipline" <> metavar "NAME"
           <> help "Filter by discipline category"))
    <*> switch (long "json" <> help "Output JSON array instead of human-formatted table")

defaultActiveStates :: [TaskState]
defaultActiveStates = [Idea, Planned, Ready, InProgress, Blocked, Abandoned]

runList :: ListOpts -> IO ()
runList o = withDb defaultDbPath $ \c -> do
    forM_ (lDomain o)     $ \n -> void $ requireCategory c Domain     n
    forM_ (lDiscipline o) $ \n -> void $ requireCategory c Discipline n
    let effectiveStates
            | lAll o                 = []
            | not (null (lStates o)) = lStates o
            | otherwise              = defaultActiveStates
        displayFilter
            | lReady o               = [Ready]
            | not (null (lStates o)) = lStates o
            | otherwise              = []
    ts <- RT.listTasks c effectiveStates (lReady o) (lDomain o) (lDiscipline o)
    if lJson o
        then BL.putStr (encode ts) >> putStrLn ""
        else do
            taskRows <- buildTaskRows c ts
            utf8     <- detectUtf8
            TIO.putStr (Render.renderTaskList utf8 taskRows displayFilter)

-- | Load per-task categories and edge counts in batch, build TaskRow list.
buildTaskRows :: Connection -> [Task] -> IO [Render.TaskRow]
buildTaskRows c ts = do
    let ids = map taskId ts
    catsBatch   <- RC.taskCategoriesBatch c ids
    countsBatch <- RE.taskEdgeCounts      c ids
    pure
        [ Render.TaskRow
            { Render.trTask = t
            , Render.trCats = fromMaybe [] (lookup (taskId t) catsBatch)
            , Render.trDeps = fst counts
            , Render.trRefs = snd counts
            }
        | t <- ts
        , let counts = fromMaybe (0, 0) (lookup (taskId t) countsBatch)
        ]

-- =============================================================
-- show
-- =============================================================

data ShowFormat = SFHuman | SFJson | SFPrompt

showFormatReader :: ReadM ShowFormat
showFormatReader = eitherReader $ \s -> case s of
    "human"  -> Right SFHuman
    "json"   -> Right SFJson
    "prompt" -> Right SFPrompt
    _        -> Left ("invalid format: " <> s <> "; expected human, json, or prompt")

data ShowOpts = ShowOpts
    { sId     :: Text
    , sFormat :: ShowFormat
    }

showP :: Parser ShowOpts
showP = ShowOpts . T.pack
    <$> strArgument (metavar "TASK_ID")
    <*> option showFormatReader
            (  long "format"
            <> metavar "FORMAT"
            <> value SFHuman
            <> help "Output format: human (default), json, or prompt"
            )

runShow :: ShowOpts -> IO ()
runShow o = withDb defaultDbPath $ \c -> do
    eId <- RT.resolveTaskId c (sId o)
    tid <- case eId of
        Left err -> fatal 1 err
        Right x  -> pure x
    Just t <- RT.getTask c tid
    refs <- RE.referencedKnowledge c (taskId t)
    deps <- RE.dependencyTasks     c (taskId t)
    cats <- RC.taskCategoriesFor   c (taskId t)
    case sFormat o of
        SFJson -> BL.putStr (encode (object
                [ "task"       .= t
                , "deps"       .= deps
                , "refs"       .= refs
                , "categories" .= cats
                ])) >> putStrLn ""
        _ -> do
            catMatch <- RK.categoryMatchedKnowledge c cats 5
            let refIds     = map knowledgeId refs
                dedupedCat = filter (\k -> knowledgeId k `notElem` refIds) catMatch
            utf8 <- detectUtf8
            TIO.putStr $ case sFormat o of
                SFPrompt -> Render.renderTaskPrompt t refs dedupedCat deps
                _        -> Render.renderTaskHuman utf8 t refs deps cats

-- =============================================================
-- update
-- =============================================================

data UpdateOpts = UpdateOpts
    { uId          :: Text
    , uState       :: Maybe TaskState
    , uPriority    :: Maybe Int
    , uTitle       :: Maybe Text
    , uBody        :: BodyInput
    , uBlockReason :: Maybe Text
    }

updateP :: Parser UpdateOpts
updateP = UpdateOpts . T.pack
    <$> strArgument (metavar "TASK_ID")
    <*> optional (option taskStateReader (long "state" <> metavar "STATE"
           <> help "Set new task state"))
    <*> optional (option auto (long "priority" <> metavar "N"
           <> help "Set priority (lower = higher priority)"))
    <*> optional (T.pack <$> strOption (long "title" <> metavar "TEXT"
           <> help "Replace task title. Keep ≤ 72 chars; longer titles are truncated in `task list`."))
    <*> bodyInputParser
    <*> optional (T.pack <$> strOption (long "block-reason" <> metavar "TEXT"
           <> help "Reason for blocked state (required with --state blocked)"))

runUpdate :: UpdateOpts -> IO ()
runUpdate o = withDb defaultDbPath $ \c -> do
    when (uState o == Just Blocked && uBlockReason o == Nothing) $
        fatal 2 "--state blocked requires --block-reason"
    eId <- RT.resolveTaskId c (uId o)
    tid <- case eId of
        Left err -> fatal 1 err
        Right x  -> pure x
    body <- case uBody o of
        BodyNone -> pure Nothing
        b        -> Just <$> resolveBody b
    let upd = RT.emptyUpdate
            { RT.tuTitle       = uTitle o
            , RT.tuBody        = body
            , RT.tuState       = uState o
            , RT.tuPriority    = fmap Just (uPriority o)
            , RT.tuBlockReason = fmap Just (uBlockReason o)
            }
    ok <- RT.updateTask c tid upd
    if ok then TIO.putStrLn ("updated " <> tid)
          else fatal 1 ("task not found: " <> T.unpack (uId o))

-- =============================================================
-- rm
-- =============================================================

data RmOpts = RmOpts { rId :: Text }

rmP :: Parser RmOpts
rmP = RmOpts . T.pack <$> strArgument (metavar "TASK_ID")

runRm :: RmOpts -> IO ()
runRm o = withDb defaultDbPath $ \c -> do
    eId <- RT.resolveTaskId c (rId o)
    tid <- case eId of
        Left err -> fatal 1 err
        Right x  -> pure x
    ok <- RT.deleteTask c tid
    if ok then TIO.putStrLn ("deleted " <> tid)
          else fatal 1 ("task not found: " <> T.unpack (rId o))

-- =============================================================
-- next
-- =============================================================

data NextOpts = NextOpts

nextP :: Parser NextOpts
nextP = pure NextOpts

runNext :: NextOpts -> IO ()
runNext _ = withDb defaultDbPath $ \c -> do
    ts <- RT.listTasks c [] True Nothing Nothing
    case ts of
        []      -> exitWith (ExitFailure 1)
        (t : _) -> TIO.putStrLn (taskId t)
