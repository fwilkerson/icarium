module Icarium.Commands.Ctx (Command, parser, run, autoDeriveDeps) where

import Control.Monad (forM_, void)
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Database.SQLite.Simple (Connection)
import Options.Applicative
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)

import Icarium.Commands.Util
import Icarium.Db (withDb)
import Icarium.Render qualified as Render
import Icarium.Repo.Category qualified as RC
import Icarium.Repo.Context qualified as RK
import Icarium.Repo.Edge qualified as RE
import Icarium.Repo.Task qualified as RT
import Icarium.Types

data Command
    = Add AddOpts
    | List ListOpts
    | Show ShowOpts
    | Update UpdateOpts
    | Rm RmOpts

parser :: Parser Command
parser =
    subparser
        ( subcmd "add" "Add a context entry" (Add <$> addP)
            <> subcmd "list" "List context entries (alias: ls)" (List <$> listP)
            <> subcmd "show" "Show a context entry" (Show <$> showP)
            <> subcmd "update" "Update a context entry" (Update <$> updateP)
            <> subcmd "rm" "Delete a context entry" (Rm <$> rmP)
        )

run :: FilePath -> Command -> IO ()
run db = \case
    Add o -> runAdd db o
    List o -> runList db o
    Show o -> runShow db o
    Update o -> runUpdate db o
    Rm o -> runRm db o

-- =============================================================
-- add
-- =============================================================

data AddOpts = AddOpts
    { aTitle :: Text
    , aBody :: BodyInput
    , aDomain :: Maybe Text
    , aDiscipline :: Maybe Text
    , aDerivedFrom :: [Text]
    , aSupersedes :: Maybe Text
    }

addP :: Parser AddOpts
addP =
    AddOpts . T.pack
        <$> strArgument (metavar "TITLE" <> help "Entry title. Keep ≤ 72 chars; longer titles are truncated in `ctx list`.")
        <*> bodyInputParser
        <*> optional (textOption "domain" "NAME" "Tag with this domain category")
        <*> optional (textOption "discipline" "NAME" "Tag with this discipline category")
        <*> many (textOption "derived-from" "ID" "Shorthand for `icarium link add <THIS_CONTEXT> derived-from <ID>`; may be repeated")
        <*> optional (textOption "supersedes" "CONTEXT_ID" "Mark this entry as superseding CONTEXT_ID")

runAdd :: FilePath -> AddOpts -> IO ()
runAdd db o = withDb db $ \c -> do
    body <- resolveBody (aBody o)

    -- Pre-validate category names and any referenced ids.
    mDomain <- mapM (requireCategory c Domain) (aDomain o)
    mDisc <- mapM (requireCategory c Discipline) (aDiscipline o)
    derived <- mapM (resolveNode c) (aDerivedFrom o)
    mSupersedesId <- mapM (requireContext c) (aSupersedes o)

    mTaskId <- lookupEnv "ICARIUM_TASK_ID"

    -- Per-axis inheritance: if an axis has no explicit flag and
    -- ICARIUM_TASK_ID is set, copy that axis's categories from the task.
    inheritedCats <-
        if isNothing (aDomain o) || isNothing (aDiscipline o)
            then maybe (pure []) (loadInherited c) mTaskId
            else pure []

    -- Auto-derive from ICARIUM_TASK_ID when no --derived-from was given.
    autoDerived <- autoDeriveDeps c (aDerivedFrom o) mTaskId

    kid <-
        RK.insertContext
            c
            RK.NewContext
                { RK.ncTitle = aTitle o
                , RK.ncBody = body
                }
    forM_ (catMaybes [mDomain, mDisc] <> inheritedCats) $ \cat ->
        RC.attachContextCategory c kid (categoryId cat)
    forM_ (derived <> autoDerived) $ \(nkind, nid) ->
        void $ RE.insertEdge c DerivedFrom ContextNode kid nkind nid
    case mSupersedesId of
        Just target ->
            void $ RE.insertEdge c Supersedes ContextNode kid ContextNode target
        Nothing -> pure ()
    TIO.putStrLn kid
  where
    loadInherited conn tid = do
        allCats <- RC.taskCategoriesFor conn (T.pack tid)
        pure $
            filter
                ( \cat ->
                    (isNothing (aDomain o) && categoryAxis cat == Domain)
                        || (isNothing (aDiscipline o) && categoryAxis cat == Discipline)
                )
                allCats

{- | If no explicit --derived-from was supplied and ICARIUM_TASK_ID is set,
returns a singleton edge pointing at the dispatched task.
Explicit list non-empty → empty result (explicit wins).
Task missing/ambiguous → warns to stderr and returns empty.
-}
autoDeriveDeps :: Connection -> [Text] -> Maybe String -> IO [(NodeKind, Text)]
autoDeriveDeps _ (_ : _) _ = pure []
autoDeriveDeps _ [] Nothing = pure []
autoDeriveDeps c [] (Just tid) = do
    tasks <- RT.getTasksByPrefix c (T.pack tid)
    case tasks of
        [t] -> pure [(TaskNode, taskId t)]
        [] -> do
            hPutStrLn
                stderr
                ( "warn: ICARIUM_TASK_ID="
                    <> tid
                    <> " not found; skipping auto derived-from edge"
                )
            pure []
        _ -> do
            hPutStrLn
                stderr
                ( "warn: ICARIUM_TASK_ID="
                    <> tid
                    <> " ambiguous; skipping auto derived-from edge"
                )
            pure []

-- =============================================================
-- list
-- =============================================================

data ListOpts = ListOpts
    { lStale :: Bool
    , lAll :: Bool
    , lDomain :: Maybe Text
    , lDiscipline :: Maybe Text
    }

listP :: Parser ListOpts
listP =
    ListOpts
        <$> switch (long "stale" <> help "Only entries flagged stale")
        <*> switch (long "all" <> help "Include stale entries")
        <*> optional (textOption "domain" "NAME" "Filter by domain category")
        <*> optional (textOption "discipline" "NAME" "Filter by discipline category")

runList :: FilePath -> ListOpts -> IO ()
runList db o = withDb db $ \c -> do
    forM_ (lDomain o) $ \n -> void $ requireCategory c Domain n
    forM_ (lDiscipline o) $ \n -> void $ requireCategory c Discipline n
    let staleFilter
            | lStale o = Just True -- stale only
            | lAll o = Nothing -- show all
            | otherwise = Just False -- hide stale (default)
    ks <- RK.listContexts c staleFilter (lDomain o) (lDiscipline o)
    rows <- buildContextRows c ks
    utf8 <- detectUtf8
    TIO.putStr (Render.renderContextList utf8 rows)

buildContextRows :: Connection -> [Context] -> IO [Render.ContextRow]
buildContextRows c ks = do
    let ids = map contextId ks
    catsBatch <- RC.contextCategoriesBatch c ids
    countsBatch <- RE.contextInboundCounts c ids
    pure
        [ Render.ContextRow
            { Render.crContext = k
            , Render.crCats = fromMaybe [] (lookup (contextId k) catsBatch)
            , Render.crLinked = fromMaybe 0 (lookup (contextId k) countsBatch)
            }
        | k <- ks
        ]

-- =============================================================
-- show
-- =============================================================

data ShowOpts = ShowOpts
    { sId :: Text
    , sBody :: Bool
    }

showP :: Parser ShowOpts
showP =
    ShowOpts . T.pack
        <$> strArgument (metavar "CONTEXT_ID")
        <*> switch (long "body" <> help "Print only the context body and nothing else")

runShow :: FilePath -> ShowOpts -> IO ()
runShow db o = withDb db $ \c -> do
    kid <- resolveOrFatal (RK.resolveContextId c (sId o))
    mk <- RK.getContext c kid
    k <- maybe (fatal 1 ("context not found: " <> T.unpack kid)) pure mk
    if sBody o
        then TIO.putStr (contextBody k)
        else do
            cats <- RC.contextCategoriesFor c (contextId k)
            TIO.putStr (Render.renderContext k cats)

-- =============================================================
-- update
-- =============================================================

data UpdateOpts = UpdateOpts
    { uId :: Text
    , uTitle :: Maybe Text
    , uBody :: BodyInput
    , uStale :: Maybe Bool
    , uDomain :: Maybe Text
    , uDiscipline :: Maybe Text
    }

updateP :: Parser UpdateOpts
updateP =
    UpdateOpts . T.pack
        <$> strArgument (metavar "CONTEXT_ID")
        <*> optional (textOption "title" "TEXT" "Replace entry title. Keep ≤ 72 chars; longer titles are truncated in `ctx list`.")
        <*> bodyInputParser
        <*> staleFlag
        <*> optional (textOption "domain" "NAME" "Replace domain category; empty string clears")
        <*> optional (textOption "discipline" "NAME" "Replace discipline category; empty string clears")

staleFlag :: Parser (Maybe Bool)
staleFlag =
    (Just True <$ switch (long "stale" <> help "Mark entry as stale"))
        <|> (Just False <$ switch (long "not-stale" <> help "Mark entry as not stale"))
        <|> pure Nothing

runUpdate :: FilePath -> UpdateOpts -> IO ()
runUpdate db o = withDb db $ \c -> do
    kid <- resolveOrFatal (RK.resolveContextId c (uId o))
    body <- case uBody o of
        BodyNone -> pure Nothing
        b -> Just <$> resolveBody b
    -- Validate categories before any mutation.
    mDomCat <- resolveAxisFlag c Domain (uDomain o)
    mDiscCat <- resolveAxisFlag c Discipline (uDiscipline o)
    let upd =
            RK.emptyUpdate
                { RK.cuTitle = uTitle o
                , RK.cuBody = body
                , RK.cuStale = uStale o
                }
    ok <- RK.updateContext c kid upd
    if ok
        then do
            -- Apply replacements: detach all of that axis, then attach new one if given.
            forM_ mDomCat $ \mCat -> do
                RC.detachContextCategoriesByAxis c kid Domain
                forM_ mCat $ \cat -> RC.attachContextCategory c kid (categoryId cat)
            forM_ mDiscCat $ \mCat -> do
                RC.detachContextCategoriesByAxis c kid Discipline
                forM_ mCat $ \cat -> RC.attachContextCategory c kid (categoryId cat)
            TIO.putStrLn ("updated " <> kid)
        else fatal 1 ("context not found: " <> T.unpack (uId o))

-- =============================================================
-- rm
-- =============================================================

newtype RmOpts = RmOpts {rId :: Text}

rmP :: Parser RmOpts
rmP = RmOpts . T.pack <$> strArgument (metavar "CONTEXT_ID")

runRm :: FilePath -> RmOpts -> IO ()
runRm db o = withDb db $ \c -> do
    kid <- resolveOrFatal (RK.resolveContextId c (rId o))
    ok <- RK.deleteContext c kid
    if ok
        then TIO.putStrLn ("deleted " <> kid)
        else fatal 1 ("context not found: " <> T.unpack (rId o))
