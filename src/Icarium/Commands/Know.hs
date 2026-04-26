module Icarium.Commands.Know (Command, parser, run) where

import           Control.Monad          (forM_, void, when)
import           Data.Aeson             (encode, object, (.=))
import qualified Data.ByteString.Lazy   as BL
import           Data.Foldable          (for_)
import           Data.Maybe             (isNothing)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Database.SQLite.Simple (Connection)
import           Options.Applicative
import           System.Environment     (lookupEnv)

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
    ( subcmd "add"    "Add a knowledge entry"    (Add    <$> addP)
   <> subcmd "list"   "List knowledge"           (List   <$> listP)
   <> subcmd "show"   "Show a knowledge entry"   (Show   <$> showP)
   <> subcmd "update" "Update a knowledge entry" (Update <$> updateP)
   <> subcmd "rm"     "Delete a knowledge entry" (Rm     <$> rmP)
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
    , aDomains     :: [Text]
    , aDisciplines :: [Text]
    , aDerivedFrom :: [Text]
    , aSupersedes  :: Maybe Text
    }

addP :: Parser AddOpts
addP = AddOpts . T.pack
    <$> strArgument (metavar "TITLE")
    <*> bodyInputParser
    <*> many (T.pack <$> strOption (long "domain" <> metavar "NAME"
           <> help "Tag with this domain category"))
    <*> many (T.pack <$> strOption (long "discipline" <> metavar "NAME"
           <> help "Tag with this discipline category"))
    <*> many (T.pack <$> strOption (long "derived-from" <> metavar "ID"
                <> help "Task or knowledge id this was derived from"))
    <*> optional (T.pack <$> strOption (long "supersedes" <> metavar "KNOWLEDGE_ID"
           <> help "Mark this entry as superseding KNOWLEDGE_ID"))

runAdd :: AddOpts -> IO ()
runAdd o = withDb defaultDbPath $ \c -> do
    body <- resolveBody (aBody o)

    -- Pre-validate category names and any referenced ids.
    domains <- mapM (requireCategory c Domain)     (aDomains o)
    disc    <- mapM (requireCategory c Discipline) (aDisciplines o)
    derived <- mapM (resolveNode c) (aDerivedFrom o)
    for_ (aSupersedes o) (requireKnowledge c)

    -- Per-axis inheritance: if an axis has no explicit flag and
    -- ICARIUM_TASK_ID is set, copy that axis's categories from the task.
    inheritedCats <- if null (aDomains o) || null (aDisciplines o)
        then do
            mTaskId <- lookupEnv "ICARIUM_TASK_ID"
            case mTaskId of
                Nothing  -> pure []
                Just tid -> do
                    allCats <- RC.taskCategoriesFor c (T.pack tid)
                    pure $ filter (\cat ->
                        (null (aDomains o)     && categoryAxis cat == Domain) ||
                        (null (aDisciplines o) && categoryAxis cat == Discipline)
                        ) allCats
        else pure []

    kid <- RK.insertKnowledge c RK.NewKnowledge
        { RK.nkTitle = aTitle o, RK.nkBody = body }
    forM_ (domains <> disc <> inheritedCats) $ \cat ->
        RC.attachKnowledgeCategory c kid (categoryId cat)
    forM_ derived $ \(nkind, nid) ->
        void $ RE.insertEdge c DerivedFrom KnowledgeNode kid nkind nid
    case aSupersedes o of
        Just target ->
            void $ RE.insertEdge c Supersedes KnowledgeNode kid KnowledgeNode target
        Nothing -> pure ()
    TIO.putStrLn kid

-- | Look up an id that could refer to either a task or a knowledge entry.
-- Returns the @NodeKind@ alongside the id so edge inserts are correctly typed.
resolveNode :: Connection -> Text -> IO (NodeKind, Text)
resolveNode c nid = do
    mt <- RT.getTask c nid
    case mt of
        Just _  -> pure (TaskNode, nid)
        Nothing -> do
            mk <- RK.getKnowledge c nid
            case mk of
                Just _  -> pure (KnowledgeNode, nid)
                Nothing -> fatal 2 ("unknown node: " <> T.unpack nid)

requireCategory :: Connection -> CategoryAxis -> Text -> IO Category
requireCategory c axis name = do
    mc <- RC.findCategory c axis name
    case mc of
        Just cat -> pure cat
        Nothing  -> fatal 2 ("unknown " <> T.unpack (categoryAxisText axis)
                                       <> ": " <> T.unpack name)

requireKnowledge :: Connection -> Text -> IO ()
requireKnowledge c kid = do
    mk <- RK.getKnowledge c kid
    when (isNothing mk) $ fatal 2 ("unknown knowledge: " <> T.unpack kid)

-- =============================================================
-- list
-- =============================================================

data ListOpts = ListOpts
    { lStale      :: Bool
    , lDomain     :: Maybe Text
    , lDiscipline :: Maybe Text
    , lJson       :: Bool
    }

listP :: Parser ListOpts
listP = ListOpts
    <$> switch (long "stale" <> help "Only entries flagged stale")
    <*> optional (T.pack <$> strOption (long "domain"     <> metavar "NAME"
           <> help "Filter by domain category"))
    <*> optional (T.pack <$> strOption (long "discipline" <> metavar "NAME"
           <> help "Filter by discipline category"))
    <*> switch (long "json" <> help "Output JSON array instead of human-formatted table")

runList :: ListOpts -> IO ()
runList o = withDb defaultDbPath $ \c -> do
    forM_ (lDomain o)     $ \n -> void $ requireCategory c Domain     n
    forM_ (lDiscipline o) $ \n -> void $ requireCategory c Discipline n
    ks <- RK.listKnowledge c (lStale o) (lDomain o) (lDiscipline o)
    if lJson o
        then BL.putStr (encode ks) >> putStrLn ""
        else TIO.putStr (Render.renderKnowledgeList ks)

-- =============================================================
-- show
-- =============================================================

data ShowOpts = ShowOpts
    { sId   :: Text
    , sJson :: Bool
    }

showP :: Parser ShowOpts
showP = ShowOpts . T.pack
    <$> strArgument (metavar "KNOWLEDGE_ID")
    <*> switch (long "json" <> help "Output JSON instead of human-formatted text")

runShow :: ShowOpts -> IO ()
runShow o = withDb defaultDbPath $ \c -> do
    mk <- RK.getKnowledge c (sId o)
    case mk of
        Nothing -> fatal 1 ("knowledge not found: " <> T.unpack (sId o))
        Just k  -> do
            cats <- RC.knowledgeCategoriesFor c (knowledgeId k)
            if sJson o
                then BL.putStr (encode (object
                        [ "knowledge" .= k
                        , "categories" .= cats
                        ])) >> putStrLn ""
                else TIO.putStr (Render.renderKnowledge k cats)

-- =============================================================
-- update
-- =============================================================

data UpdateOpts = UpdateOpts
    { uId          :: Text
    , uTitle       :: Maybe Text
    , uBody        :: BodyInput
    , uStale       :: Maybe Bool
    , uDomains     :: [Text]
    , uDisciplines :: [Text]
    }

updateP :: Parser UpdateOpts
updateP = UpdateOpts . T.pack
    <$> strArgument (metavar "KNOWLEDGE_ID")
    <*> optional (T.pack <$> strOption (long "title" <> metavar "TEXT"
           <> help "Replace entry title"))
    <*> bodyInputParser
    <*> optional staleFlag
    <*> many (T.pack <$> strOption (long "domain"     <> metavar "NAME"
           <> help "Tag with this domain category"))
    <*> many (T.pack <$> strOption (long "discipline" <> metavar "NAME"
           <> help "Tag with this discipline category"))
  where
    staleFlag =
            flag' True  (long "stale" <> help "Mark as stale")
        <|> flag' False (long "fresh" <> help "Clear stale flag")

runUpdate :: UpdateOpts -> IO ()
runUpdate o = withDb defaultDbPath $ \c -> do
    body <- case uBody o of
        BodyNone -> pure Nothing
        b        -> Just <$> resolveBody b
    domains <- mapM (requireCategory c Domain)     (uDomains o)
    disc    <- mapM (requireCategory c Discipline) (uDisciplines o)
    let upd = RK.emptyUpdate
            { RK.kuTitle = uTitle o
            , RK.kuBody  = body
            , RK.kuStale = uStale o
            }
    ok <- RK.updateKnowledge c (uId o) upd
    if ok then do
        forM_ (domains <> disc) $ \cat ->
            RC.attachKnowledgeCategory c (uId o) (categoryId cat)
        TIO.putStrLn ("updated " <> uId o)
    else fatal 1 ("knowledge not found: " <> T.unpack (uId o))

-- =============================================================
-- rm
-- =============================================================

data RmOpts = RmOpts { rId :: Text }

rmP :: Parser RmOpts
rmP = RmOpts . T.pack <$> strArgument (metavar "KNOWLEDGE_ID")

runRm :: RmOpts -> IO ()
runRm o = withDb defaultDbPath $ \c -> do
    ok <- RK.deleteKnowledge c (rId o)
    if ok then TIO.putStrLn ("deleted " <> rId o)
          else fatal 1 ("knowledge not found: " <> T.unpack (rId o))
