module Icarium.Commands.Task (Command, parser, run) where

import           Control.Monad          (forM_, unless, void, when)
import           Data.Maybe             (isNothing)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Database.SQLite.Simple (Connection)
import           Options.Applicative

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

parser :: Parser Command
parser = subparser
    ( subcmd "add"    "Add a task"       (Add    <$> addP)
   <> subcmd "list"   "List tasks"       (List   <$> listP)
   <> subcmd "show"   "Show a task"      (Show   <$> showP)
   <> subcmd "update" "Update a task"    (Update <$> updateP)
   <> subcmd "rm"     "Delete a task"    (Rm     <$> rmP)
    )

run :: Command -> IO ()
run = \case
    Add o    -> runAdd o
    List o   -> runList o
    Show o   -> runShow o
    Update o -> runUpdate o
    Rm o     -> runRm o

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
    <$> strArgument (metavar "TITLE")
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
    domains  <- mapM (requireCategory c Domain)     (aDomains o)
    disc     <- mapM (requireCategory c Discipline) (aDisciplines o)
    mapM_ (requireTask c)      (aDependsOn o)
    mapM_ (requireKnowledge c) (aReferences o)

    tid <- RT.insertTask c RT.NewTask
        { RT.ntTitle    = aTitle o
        , RT.ntBody     = body
        , RT.ntState    = aState o
        , RT.ntPriority = aPriority o
        }
    forM_ (domains <> disc) $ \cat ->
        RC.attachTaskCategory c tid (categoryId cat)
    forM_ (aDependsOn o) $ \depId ->
        void $ RE.insertEdge c DependsOn TaskNode tid TaskNode depId
    forM_ (aReferences o) $ \kid ->
        void $ RE.insertEdge c References TaskNode tid KnowledgeNode kid
    TIO.putStrLn tid

requireCategory :: Connection -> CategoryAxis -> Text -> IO Category
requireCategory c axis name = do
    mc <- RC.findCategory c axis name
    case mc of
        Just cat -> pure cat
        Nothing  -> fatal 2 ("unknown " <> T.unpack (categoryAxisText axis)
                                       <> ": " <> T.unpack name)

requireTask :: Connection -> Text -> IO ()
requireTask c tid = do
    mt <- RT.getTask c tid
    when (isNothing mt) $
        fatal 2 ("unknown task: " <> T.unpack tid)

requireKnowledge :: Connection -> Text -> IO ()
requireKnowledge c kid = do
    mk <- RK.getKnowledge c kid
    when (isNothing mk) $
        fatal 2 ("unknown knowledge: " <> T.unpack kid)

-- =============================================================
-- list
-- =============================================================

data ListOpts = ListOpts
    { lStates     :: [TaskState]
    , lReady      :: Bool
    , lDomain     :: Maybe Text
    , lDiscipline :: Maybe Text
    }

listP :: Parser ListOpts
listP = ListOpts
    <$> many (option taskStateReader (long "state" <> metavar "STATE"
           <> help "Filter by task state (repeatable)"))
    <*> switch (long "ready" <> help "Only tasks ready to dispatch")
    <*> optional (T.pack <$> strOption (long "domain"     <> metavar "NAME"
           <> help "Filter by domain category"))
    <*> optional (T.pack <$> strOption (long "discipline" <> metavar "NAME"
           <> help "Filter by discipline category"))

runList :: ListOpts -> IO ()
runList o = withDb defaultDbPath $ \c -> do
    forM_ (lDomain o)     $ \n -> void $ requireCategory c Domain     n
    forM_ (lDiscipline o) $ \n -> void $ requireCategory c Discipline n
    ts <- RT.listTasks c (lStates o) (lReady o) (lDomain o) (lDiscipline o)
    TIO.putStr (Render.renderTaskList ts)

-- =============================================================
-- show
-- =============================================================

data ShowOpts = ShowOpts
    { sId     :: Text
    , sPrompt :: Bool
    }

showP :: Parser ShowOpts
showP = ShowOpts . T.pack
    <$> strArgument (metavar "TASK_ID")
    <*> switch (long "prompt" <> help "Render the dispatch prompt instead")

runShow :: ShowOpts -> IO ()
runShow o = withDb defaultDbPath $ \c -> do
    mt <- RT.getTask c (sId o)
    case mt of
        Nothing -> fatal 1 ("task not found: " <> T.unpack (sId o))
        Just t  -> do
            refs     <- RE.referencedKnowledge c (taskId t)
            deps     <- RE.dependencyTasks     c (taskId t)
            cats     <- RC.taskCategoriesFor   c (taskId t)
            catMatch <- RK.categoryMatchedKnowledge c cats 5
            let refIds     = map knowledgeId refs
                dedupedCat = filter (\k -> knowledgeId k `notElem` refIds) catMatch
            TIO.putStr $ if sPrompt o
                then Render.renderTaskPrompt t refs dedupedCat deps
                else Render.renderTaskHuman  t refs deps cats

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
           <> help "Replace task title"))
    <*> bodyInputParser
    <*> optional (T.pack <$> strOption (long "block-reason" <> metavar "TEXT"
           <> help "Reason for blocked state (required with --state blocked)"))

runUpdate :: UpdateOpts -> IO ()
runUpdate o = withDb defaultDbPath $ \c -> do
    when (uState o == Just Blocked && isNothing (uBlockReason o)) $
        fatal 2 "--state blocked requires --block-reason"
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
    ok <- RT.updateTask c (uId o) upd
    if ok then TIO.putStrLn ("updated " <> uId o)
          else fatal 1 ("task not found: " <> T.unpack (uId o))

-- =============================================================
-- rm
-- =============================================================

data RmOpts = RmOpts { rId :: Text }

rmP :: Parser RmOpts
rmP = RmOpts . T.pack <$> strArgument (metavar "TASK_ID")

runRm :: RmOpts -> IO ()
runRm o = withDb defaultDbPath $ \c -> do
    ok <- RT.deleteTask c (rId o)
    if ok then TIO.putStrLn ("deleted " <> rId o)
          else fatal 1 ("task not found: " <> T.unpack (rId o))
