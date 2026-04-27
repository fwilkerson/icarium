module Icarium.Commands.Know (Command, parser, run, autoDeriveDeps) where

import           Control.Monad          (forM_, void)
import           Data.Maybe             (catMaybes, isNothing)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Database.SQLite.Simple (Connection)
import           Options.Applicative
import           System.Environment     (lookupEnv)
import           System.IO              (hPutStrLn, stderr)

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
    ( subcmd "add"      "Add a knowledge entry"    (Add     <$> addP)
   <> subcmd "show"     "Show a knowledge entry"   (Show    <$> showP)
   <> subcmd "update"   "Update a knowledge entry" (Update  <$> updateP)
   <> subcmd "rm"       "Delete a knowledge entry" (Rm      <$> rmP)
    )
    <|> (List <$> listP)

run :: Command -> IO ()
run = \case
    Add o     -> runAdd o
    List o    -> runList o
    Show o    -> runShow o
    Update o  -> runUpdate o
    Rm o      -> runRm o

-- =============================================================
-- add
-- =============================================================

data AddOpts = AddOpts
    { aTitle       :: Text
    , aBody        :: BodyInput
    , aDomain      :: Maybe Text
    , aDiscipline  :: Maybe Text
    , aDerivedFrom :: [Text]
    , aSupersedes  :: Maybe Text
    }

addP :: Parser AddOpts
addP = AddOpts . T.pack
    <$> strArgument (metavar "TITLE" <> help "Entry title. Keep ≤ 72 chars; longer titles are truncated in `know list`.")
    <*> bodyInputParser
    <*> optional (T.pack <$> strOption (long "domain" <> metavar "NAME"
           <> help "Tag with this domain category"))
    <*> optional (T.pack <$> strOption (long "discipline" <> metavar "NAME"
           <> help "Tag with this discipline category"))
    <*> many (T.pack <$> strOption (long "derived-from" <> metavar "ID"
                <> help "Task or knowledge id this was derived from"))
    <*> optional (T.pack <$> strOption (long "supersedes" <> metavar "KNOWLEDGE_ID"
           <> help "Mark this entry as superseding KNOWLEDGE_ID"))

runAdd :: AddOpts -> IO ()
runAdd o = withDb defaultDbPath $ \c -> do
    body <- resolveBody (aBody o)

    -- Pre-validate category names and any referenced ids.
    mDomain <- mapM (requireCategory c Domain)     (aDomain o)
    mDisc   <- mapM (requireCategory c Discipline) (aDiscipline o)
    derived <- mapM (resolveNode c) (aDerivedFrom o)
    mSupersedesId <- mapM (requireKnowledge c) (aSupersedes o)

    mTaskId <- lookupEnv "ICARIUM_TASK_ID"

    -- Per-axis inheritance: if an axis has no explicit flag and
    -- ICARIUM_TASK_ID is set, copy that axis's categories from the task.
    inheritedCats <- if isNothing (aDomain o) || isNothing (aDiscipline o)
        then case mTaskId of
            Nothing  -> pure []
            Just tid -> do
                allCats <- RC.taskCategoriesFor c (T.pack tid)
                pure $ filter (\cat ->
                    (isNothing (aDomain o)     && categoryAxis cat == Domain) ||
                    (isNothing (aDiscipline o) && categoryAxis cat == Discipline)
                    ) allCats
        else pure []

    -- Auto-derive from ICARIUM_TASK_ID when no --derived-from was given.
    autoDerived <- autoDeriveDeps c (aDerivedFrom o) mTaskId

    kid <- RK.insertKnowledge c RK.NewKnowledge
        { RK.nkTitle = aTitle o, RK.nkBody = body }
    forM_ (catMaybes [mDomain, mDisc] <> inheritedCats) $ \cat ->
        RC.attachKnowledgeCategory c kid (categoryId cat)
    forM_ (derived <> autoDerived) $ \(nkind, nid) ->
        void $ RE.insertEdge c DerivedFrom KnowledgeNode kid nkind nid
    case mSupersedesId of
        Just target ->
            void $ RE.insertEdge c Supersedes KnowledgeNode kid KnowledgeNode target
        Nothing -> pure ()
    TIO.putStrLn kid

-- | If no explicit --derived-from was supplied and ICARIUM_TASK_ID is set,
-- returns a singleton edge pointing at the dispatched task.
-- Explicit list non-empty → empty result (explicit wins).
-- Task missing/ambiguous → warns to stderr and returns empty.
autoDeriveDeps :: Connection -> [Text] -> Maybe String -> IO [(NodeKind, Text)]
autoDeriveDeps _ (_:_) _        = pure []
autoDeriveDeps _ []   Nothing   = pure []
autoDeriveDeps c []   (Just tid) = do
    tasks <- RT.getTasksByPrefix c (T.pack tid)
    case tasks of
        [t] -> pure [(TaskNode, taskId t)]
        []  -> do
            hPutStrLn stderr ("warn: ICARIUM_TASK_ID=" <> tid
                <> " not found; skipping auto derived-from edge")
            pure []
        _   -> do
            hPutStrLn stderr ("warn: ICARIUM_TASK_ID=" <> tid
                <> " ambiguous; skipping auto derived-from edge")
            pure []

-- | Look up an id that could refer to either a task or a knowledge entry
-- via ULID prefix match.
resolveNode :: Connection -> Text -> IO (NodeKind, Text)
resolveNode c input = do
    ts <- RT.getTasksByPrefix c input
    ks <- RK.getKnowledgesByPrefix c input
    case (ts, ks) of
        ([t], [] ) -> pure (TaskNode, taskId t)
        ([], [k] ) -> pure (KnowledgeNode, knowledgeId k)
        ([], []  ) -> fatal 2 ("unknown node: " <> T.unpack input)
        _          -> fatal 2 ("ambiguous id: " <> T.unpack input)

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
    { lStale      :: Bool
    , lAll        :: Bool
    , lDomain     :: Maybe Text
    , lDiscipline :: Maybe Text
    }

listP :: Parser ListOpts
listP = ListOpts
    <$> switch (long "stale" <> help "Only entries flagged stale")
    <*> switch (long "all"   <> help "Include stale entries")
    <*> optional (T.pack <$> strOption (long "domain"     <> metavar "NAME"
           <> help "Filter by domain category"))
    <*> optional (T.pack <$> strOption (long "discipline" <> metavar "NAME"
           <> help "Filter by discipline category"))

runList :: ListOpts -> IO ()
runList o = withDb defaultDbPath $ \c -> do
    forM_ (lDomain o)     $ \n -> void $ requireCategory c Domain     n
    forM_ (lDiscipline o) $ \n -> void $ requireCategory c Discipline n
    let staleFilter
            | lStale o  = Just True   -- stale only
            | lAll o    = Nothing     -- show all
            | otherwise = Just False  -- hide stale (default)
    ks <- RK.listKnowledge c staleFilter (lDomain o) (lDiscipline o)
    utf8 <- detectUtf8
    TIO.putStr (Render.renderKnowledgeList utf8 ks)

-- =============================================================
-- show
-- =============================================================

newtype ShowOpts = ShowOpts { sId :: Text }

showP :: Parser ShowOpts
showP = ShowOpts . T.pack
    <$> strArgument (metavar "KNOWLEDGE_ID")

runShow :: ShowOpts -> IO ()
runShow o = withDb defaultDbPath $ \c -> do
    eId <- RK.resolveKnowledgeId c (sId o)
    kid <- case eId of
        Left err -> fatal 1 err
        Right x  -> pure x
    Just k <- RK.getKnowledge c kid
    cats <- RC.knowledgeCategoriesFor c (knowledgeId k)
    TIO.putStr (Render.renderKnowledge k cats)

-- =============================================================
-- update
-- =============================================================

data UpdateOpts = UpdateOpts
    { uId         :: Text
    , uTitle      :: Maybe Text
    , uBody       :: BodyInput
    , uStale      :: Maybe Bool
    , uDomain     :: Maybe Text
    , uDiscipline :: Maybe Text
    }

updateP :: Parser UpdateOpts
updateP = UpdateOpts . T.pack
    <$> strArgument (metavar "KNOWLEDGE_ID")
    <*> optional (T.pack <$> strOption (long "title" <> metavar "TEXT"
           <> help "Replace entry title. Keep ≤ 72 chars; longer titles are truncated in `know list`."))
    <*> bodyInputParser
    <*> staleFlag
    <*> optional (T.pack <$> strOption (long "domain" <> metavar "NAME"
           <> help "Replace domain category; empty string clears"))
    <*> optional (T.pack <$> strOption (long "discipline" <> metavar "NAME"
           <> help "Replace discipline category; empty string clears"))

staleFlag :: Parser (Maybe Bool)
staleFlag =
        (Just True  <$ switch (long "stale"     <> help "Mark entry as stale"))
    <|> (Just False <$ switch (long "not-stale" <> help "Mark entry as not stale"))
    <|> pure Nothing

runUpdate :: UpdateOpts -> IO ()
runUpdate o = withDb defaultDbPath $ \c -> do
    eId <- RK.resolveKnowledgeId c (uId o)
    kid <- case eId of
        Left err -> fatal 1 err
        Right x  -> pure x
    body <- case uBody o of
        BodyNone -> pure Nothing
        b        -> Just <$> resolveBody b
    -- Validate categories before any mutation.
    mDomCat  <- resolveAxisFlag c Domain     (uDomain o)
    mDiscCat <- resolveAxisFlag c Discipline (uDiscipline o)
    let upd = RK.emptyUpdate
            { RK.kuTitle = uTitle o
            , RK.kuBody  = body
            , RK.kuStale = uStale o
            }
    ok <- RK.updateKnowledge c kid upd
    if ok then do
        -- Apply replacements: detach all of that axis, then attach new one if given.
        forM_ mDomCat $ \mCat -> do
            RC.detachKnowledgeCategoriesByAxis c kid Domain
            forM_ mCat $ \cat -> RC.attachKnowledgeCategory c kid (categoryId cat)
        forM_ mDiscCat $ \mCat -> do
            RC.detachKnowledgeCategoriesByAxis c kid Discipline
            forM_ mCat $ \cat -> RC.attachKnowledgeCategory c kid (categoryId cat)
        TIO.putStrLn ("updated " <> kid)
    else fatal 1 ("knowledge not found: " <> T.unpack (uId o))

-- =============================================================
-- rm
-- =============================================================

newtype RmOpts = RmOpts { rId :: Text }

rmP :: Parser RmOpts
rmP = RmOpts . T.pack <$> strArgument (metavar "KNOWLEDGE_ID")

runRm :: RmOpts -> IO ()
runRm o = withDb defaultDbPath $ \c -> do
    eId <- RK.resolveKnowledgeId c (rId o)
    kid <- case eId of
        Left err -> fatal 1 err
        Right x  -> pure x
    ok <- RK.deleteKnowledge c kid
    if ok then TIO.putStrLn ("deleted " <> kid)
          else fatal 1 ("knowledge not found: " <> T.unpack (rId o))
