module Icarium.Commands.Ctx (Command, parser, run, autoDeriveDeps) where

import Control.Monad (forM, forM_, void, when)
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Database.SQLite.Simple (Connection)
import Options.Applicative
import System.Directory (doesFileExist, removeFile)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)

import Icarium.Bodies (bodiesDir, ctxBodyPath, readBody)
import Icarium.Commands.Util
import Icarium.Db (withDb)
import Icarium.Node (createContextWithBody)
import Icarium.Render qualified as Render
import Icarium.Render.Json qualified as Json
import Icarium.Repo.Category qualified as RC
import Icarium.Repo.Context qualified as RCx
import Icarium.Repo.Curation qualified as RCur
import Icarium.Repo.Edge qualified as RE
import Icarium.Repo.Task qualified as RT
import Icarium.Types

data Command
    = Add AddOpts
    | List ListOpts
    | Show ShowOpts
    | Update UpdateOpts
    | Curate CurateOpts
    | Rm RmOpts
    | Path PathOpts
    | Cat CatOpts
    | Children ChildrenOpts
    | Tree TreeOpts
    | Exists ExistsOpts

parser :: Parser Command
parser =
    subparser
        ( subcmd "add" "Add a context entry. Prints <id> and body path; Write your markdown to that path (no temp draft needed)." (Add <$> addP)
            <> subcmd "list" "List context entries (alias: ls)" (List <$> listP)
            <> subcmd "show" "Show context metadata. The body is intentionally not printed: Read $(icarium ctx path <id>) so a subsequent Edit can succeed (Claude Code's Edit tool requires a prior Read of the same path)." (Show <$> showP)
            <> subcmd "update" "Update context metadata. To edit the body: Read $(icarium ctx path <id>) then Edit." (Update <$> updateP)
            <> subcmd "curate" "Record a curation disposition for an entry (guidance | rule | refactor | keep | stale). Bare form lists the curation queue: never-curated entries, plus aged ones with --older-than." (Curate <$> curateP)
            <> subcmd "rm" "Delete a context entry" (Rm <$> rmP)
            <> subcmd "path" "Print body file path for a context entry (the body is a markdown file you Read/Edit directly)." (Path <$> pathP)
            <> subcmd "cat" "Print body of a context entry to stdout. Exit non-zero if the body file is missing." (Cat <$> catP)
            <> subcmd "children" "List direct context children of a context entry (entries that derive from, reference, or supersede it)." (Children <$> childrenP)
            <> subcmd "tree" "Recursive tree of context children, indented. Cycle-safe." (Tree <$> treeP)
            <> subcmd "exists" "Check whether a context id or prefix resolves uniquely. Exit 0 = found, 1 = not found, 2 = ambiguous." (Exists <$> existsP)
        )

run :: FilePath -> Command -> IO ()
run db = \case
    Add o -> runAdd db o
    List o -> runList db o
    Show o -> runShow db o
    Update o -> runUpdate db o
    Curate o -> runCurate db o
    Rm o -> runRm db o
    Path o -> runPath db o
    Cat o -> runCat db o
    Children o -> runChildren db o
    Tree o -> runTree db o
    Exists o -> runExists db o

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

    (cxid, fp) <-
        createContextWithBody
            c
            db
            RCx.NewContext
                { RCx.ncTitle = aTitle o
                , RCx.ncBody = body
                }
    forM_ (catMaybes [mDomain, mDisc] <> inheritedCats) $ \cat ->
        RC.attachContextCategory c cxid (categoryId cat)
    forM_ (derived <> autoDerived) $ \(nkind, nid) ->
        void $ RE.insertEdge c DerivedFrom ContextNode cxid nkind nid
    case mSupersedesId of
        Just target ->
            void $ RE.insertEdge c Supersedes ContextNode cxid ContextNode target
        Nothing -> pure ()
    TIO.putStrLn cxid
    TIO.putStrLn (T.pack fp)
    when (T.null body) $ do
        hPutStrLn stderr ("# next: Write your markdown to " <> fp)
        hPutStrLn stderr ("# to edit later: Read $(icarium ctx path " <> T.unpack cxid <> ") then Edit")
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
    { lRetired :: Bool
    , lAll :: Bool
    , lDomain :: Maybe Text
    , lDiscipline :: Maybe Text
    , lLimit :: Maybe Int
    , lJson :: Bool
    }

listP :: Parser ListOpts
listP =
    ListOpts
        <$> switch (long "retired" <> help "Only retired entries (latest curation disposition is guidance/rule/refactor/stale)")
        <*> switch (long "all" <> help "Include retired entries and older versions superseded by another entry. By default ctx list shows only current heads (not retired, not superseded).")
        <*> optional (textOption "domain" "NAME" "Filter by domain category")
        <*> optional (textOption "discipline" "NAME" "Filter by discipline category")
        <*> optional
            ( option
                auto
                ( long "limit"
                    <> metavar "N"
                    <> help "Return at most N entries"
                )
            )
        <*> jsonFlag

runList :: FilePath -> ListOpts -> IO ()
runList db o = withDb db $ \c -> do
    forM_ (lDomain o) $ \n -> void $ requireCategory c Domain n
    forM_ (lDiscipline o) $ \n -> void $ requireCategory c Discipline n
    let retiredFilter
            | lRetired o = Just True -- retired only
            | lAll o = Nothing -- show all
            | otherwise = Just False -- hide retired (default)
    let includeSuperseded = lAll o
    cxs0 <- RCx.listContexts c retiredFilter includeSuperseded (lDomain o) (lDiscipline o)
    let cxs = maybe cxs0 (`take` cxs0) (lLimit o)
    rows <- buildContextRows c cxs
    if lJson o
        then BLC.putStrLn (Json.renderContextListJson rows)
        else do
            utf8 <- detectUtf8
            TIO.putStr (Render.renderContextList utf8 rows)

buildContextRows :: Connection -> [Context] -> IO [Render.ContextRow]
buildContextRows c cxs = do
    let ids = map contextId cxs
    catsBatch <- RC.contextCategoriesBatch c ids
    countsBatch <- RE.contextInboundCounts c ids
    retiredIds <- RCur.retiredContextIds c ids
    pure
        [ Render.ContextRow
            { Render.crContext = cx
            , Render.crCats = fromMaybe [] (lookup (contextId cx) catsBatch)
            , Render.crLinked = fromMaybe 0 (lookup (contextId cx) countsBatch)
            , Render.crRetired = contextId cx `elem` retiredIds
            }
        | cx <- cxs
        ]

-- =============================================================
-- show
-- =============================================================

data ShowOpts = ShowOpts
    { sId :: Text
    , sJson :: Bool
    }

showP :: Parser ShowOpts
showP =
    ShowOpts . T.pack
        <$> strArgument (metavar "CONTEXT_ID")
        <*> jsonFlag

runShow :: FilePath -> ShowOpts -> IO ()
runShow db o = withDb db $ \c -> do
    cxid <- resolveOrFatal (RCx.resolveContextId c (sId o))
    mcx <- RCx.getContext c cxid
    cx <- maybe (fatal 1 ("context not found: " <> T.unpack cxid)) pure mcx
    cats <- RC.contextCategoriesFor c (contextId cx)
    mEvent <- RCur.latestCuration c (contextId cx)
    let bodyPath = T.pack (ctxBodyPath (bodiesDir db) cxid)
    if sJson o
        then BLC.putStrLn (Json.renderContextShowJson cx cats bodyPath mEvent)
        else TIO.putStr (Render.renderContext cx cats bodyPath mEvent)

-- =============================================================
-- update
-- =============================================================

data UpdateOpts = UpdateOpts
    { uId :: Text
    , uTitle :: Maybe Text
    , uDomain :: Maybe Text
    , uDiscipline :: Maybe Text
    }

updateP :: Parser UpdateOpts
updateP =
    UpdateOpts . T.pack
        <$> strArgument (metavar "CONTEXT_ID")
        <*> optional (textOption "title" "TEXT" "Replace entry title. Keep ≤ 72 chars; longer titles are truncated in `ctx list`.")
        <*> optional (textOption "domain" "NAME" "Replace domain category; empty string clears")
        <*> optional (textOption "discipline" "NAME" "Replace discipline category; empty string clears")

runUpdate :: FilePath -> UpdateOpts -> IO ()
runUpdate db o = withDb db $ \c -> do
    cxid <- resolveOrFatal (RCx.resolveContextId c (uId o))
    -- Validate categories before any mutation.
    mDomCat <- resolveAxisFlag c Domain (uDomain o)
    mDiscCat <- resolveAxisFlag c Discipline (uDiscipline o)
    let upd = RCx.emptyUpdate{RCx.cuTitle = uTitle o}
    ok <- RCx.updateContext c cxid upd
    if ok
        then do
            -- Apply replacements: detach all of that axis, then attach new one if given.
            forM_ mDomCat $ \mCat -> do
                RC.detachContextCategoriesByAxis c cxid Domain
                forM_ mCat $ \cat -> RC.attachContextCategory c cxid (categoryId cat)
            forM_ mDiscCat $ \mCat -> do
                RC.detachContextCategoriesByAxis c cxid Discipline
                forM_ mCat $ \cat -> RC.attachContextCategory c cxid (categoryId cat)
            TIO.putStrLn ("updated " <> cxid)
        else fatal 1 ("context not found: " <> T.unpack (uId o))

-- =============================================================
-- curate
-- =============================================================

data CurateOpts = CurateOpts
    { cId :: Maybe Text
    , cDisposition :: Maybe Text
    , cArtifact :: Maybe Text
    , cNote :: Maybe Text
    , cOlderThan :: Maybe Int
    , cJson :: Bool
    }

curateP :: Parser CurateOpts
curateP =
    CurateOpts
        <$> optional (T.pack <$> strArgument (metavar "CONTEXT_ID" <> help "Entry to curate; omit to list the queue"))
        <*> optional (T.pack <$> strArgument (metavar "DISPOSITION" <> help "guidance | rule | refactor | keep | stale"))
        <*> optional (textOption "artifact" "X" "Where the content went: doc/skill path (guidance), rule/test name (rule), task id (refactor), superseding entry id (stale, optional). Rejected for keep.")
        <*> optional (textOption "note" "TEXT" "Freeform note on the decision")
        <*> optional
            ( option
                auto
                ( long "older-than"
                    <> metavar "DAYS"
                    <> help "Queue form only: also list entries whose latest curation event is at least DAYS days old"
                )
            )
        <*> jsonFlag

runCurate :: FilePath -> CurateOpts -> IO ()
runCurate db o = case (cId o, cDisposition o) of
    (Nothing, _) -> runCurateQueue db o
    (Just cid, Nothing) ->
        fatal 2 $
            "missing disposition; usage: icarium ctx curate "
                <> T.unpack cid
                <> " <guidance|rule|refactor|keep|stale>"
    (Just cid, Just dispText) -> case parseDisposition dispText of
        Nothing ->
            fatal 2 $
                "invalid disposition: "
                    <> T.unpack dispText
                    <> "; accepted: guidance, rule, refactor, keep, stale"
        Just disp -> runCurateRecord db o cid disp

runCurateRecord :: FilePath -> CurateOpts -> Text -> Disposition -> IO ()
runCurateRecord db o cid disp = do
    when (isJust (cOlderThan o)) $
        fatal 2 "--older-than applies to the queue form (bare `icarium ctx curate`)"
    withDb db $ \c -> do
        cxid <- resolveOrFatal (RCx.resolveContextId c cid)
        artifact <- resolveArtifact c disp (cArtifact o)
        void $ RCur.insertCuration c cxid disp artifact (cNote o)
        TIO.putStrLn ("curated " <> cxid <> " " <> dispositionText disp)

{- | Enforce the artifact rule per disposition; the sweep must leave a
trail for content that went somewhere. Task/context artifacts are
resolved to canonical full ids before storage.
-}
resolveArtifact :: Connection -> Disposition -> Maybe Text -> IO (Maybe Text)
resolveArtifact c disp mArtifact = case (disp, mArtifact) of
    (Guidance, Nothing) -> missing "guidance" "the destination doc/skill path"
    (Rule, Nothing) -> missing "rule" "the rule/invariant/test name"
    (Refactor, Nothing) -> missing "refactor" "the filed task id"
    (Refactor, Just a) -> Just <$> requireTask c a
    (Stale, Just a) -> Just <$> requireContext c a
    (Keep, Just _) -> fatal 2 "keep records no artifact; drop --artifact"
    (_, a) -> pure a
  where
    missing name what =
        fatal 2 (name <> " requires " <> what <> ": rerun with --artifact <X>")

runCurateQueue :: FilePath -> CurateOpts -> IO ()
runCurateQueue db o = do
    when (isJust (cArtifact o) || isJust (cNote o)) $
        fatal 2 "--artifact/--note apply to the record form: icarium ctx curate <CONTEXT_ID> <DISPOSITION>"
    withDb db $ \c -> do
        queue <- RCur.curationQueue c (cOlderThan o)
        rows <- forM queue $ \(cx, mEvent) -> do
            cats <- RC.contextCategoriesFor c (contextId cx)
            pure
                Render.CurationQueueRow
                    { Render.cqContext = cx
                    , Render.cqCats = cats
                    , Render.cqLastEvent = mEvent
                    }
        if cJson o
            then BLC.putStrLn (Json.renderCurationQueueJson rows)
            else do
                utf8 <- detectUtf8
                TIO.putStr (Render.renderCurationQueue utf8 rows)

-- =============================================================
-- rm
-- =============================================================

newtype RmOpts = RmOpts {rId :: Text}

rmP :: Parser RmOpts
rmP = RmOpts . T.pack <$> strArgument (metavar "CONTEXT_ID")

runRm :: FilePath -> RmOpts -> IO ()
runRm db o = withDb db $ \c -> do
    cxid <- resolveOrFatal (RCx.resolveContextId c (rId o))
    ok <- RCx.deleteContext c cxid
    if ok
        then do
            let fp = ctxBodyPath (bodiesDir db) cxid
            exists <- doesFileExist fp
            when exists $ removeFile fp
            TIO.putStrLn ("deleted " <> cxid)
        else fatal 1 ("context not found: " <> T.unpack (rId o))

-- =============================================================
-- path
-- =============================================================

newtype PathOpts = PathOpts {pId :: Text}

pathP :: Parser PathOpts
pathP = PathOpts . T.pack <$> strArgument (metavar "CONTEXT_ID")

runPath :: FilePath -> PathOpts -> IO ()
runPath db o = withDb db $ \c -> do
    cxid <- resolveOrFatal (RCx.resolveContextId c (pId o))
    TIO.putStrLn (T.pack (ctxBodyPath (bodiesDir db) cxid))

-- =============================================================
-- cat
-- =============================================================

newtype CatOpts = CatOpts {catId :: Text}

catP :: Parser CatOpts
catP = CatOpts . T.pack <$> strArgument (metavar "CONTEXT_ID")

runCat :: FilePath -> CatOpts -> IO ()
runCat db o = withDb db $ \c -> do
    cxid <- resolveOrFatal (RCx.resolveContextId c (catId o))
    TIO.putStr =<< readBody (ctxBodyPath (bodiesDir db) cxid)

-- =============================================================
-- children
-- =============================================================

data ChildrenOpts = ChildrenOpts
    { chId :: Text
    , chKind :: Maybe EdgeKind
    }

childrenP :: Parser ChildrenOpts
childrenP =
    ChildrenOpts . T.pack
        <$> strArgument (metavar "CONTEXT_ID")
        <*> optional
            ( option
                edgeKindReader
                ( long "kind"
                    <> metavar "KIND"
                    <> help "Filter by edge kind (derived-from | references | supersedes)"
                )
            )

runChildren :: FilePath -> ChildrenOpts -> IO ()
runChildren db o = withDb db $ \c -> do
    cxid <- resolveOrFatal (RCx.resolveContextId c (chId o))
    edges <- RE.ctxChildEdges c cxid (chKind o)
    case edges of
        [] -> TIO.putStrLn "(no children)"
        _ -> do
            let kindW = maximum (map (T.length . edgeKindDisplay . edgeKind) edges)
            forM_ edges $ \e -> do
                let childId = edgeSrcId e
                mcx <- RCx.getContext c childId
                case mcx of
                    Nothing -> pure ()
                    Just cx ->
                        TIO.putStrLn $
                            padr kindW (edgeKindDisplay (edgeKind e))
                                <> "  "
                                <> T.take 10 childId
                                <> "  "
                                <> contextTitle cx

-- =============================================================
-- tree
-- =============================================================

newtype TreeOpts = TreeOpts {tId :: Text}

treeP :: Parser TreeOpts
treeP = TreeOpts . T.pack <$> strArgument (metavar "CONTEXT_ID")

runTree :: FilePath -> TreeOpts -> IO ()
runTree db o = withDb db $ \c -> do
    cxid <- resolveOrFatal (RCx.resolveContextId c (tId o))
    mcx <- RCx.getContext c cxid
    cx <- maybe (fatal 1 ("context not found: " <> T.unpack cxid)) pure mcx
    TIO.putStrLn (T.take 10 cxid <> "  " <> contextTitle cx)
    printBranch c [cxid] cxid 1
  where
    printBranch c visited parentId depth = do
        edges <- RE.ctxChildEdges c parentId Nothing
        forM_ edges $ \e -> do
            let childId = edgeSrcId e
                indent = T.replicate (depth * 2) " "
                kindStr = edgeKindDisplay (edgeKind e)
            if childId `elem` visited
                then TIO.putStrLn (indent <> "[cycle: " <> T.take 10 childId <> "]")
                else do
                    mcx <- RCx.getContext c childId
                    case mcx of
                        Nothing -> pure ()
                        Just cx -> do
                            TIO.putStrLn $
                                indent
                                    <> kindStr
                                    <> "  "
                                    <> T.take 10 childId
                                    <> "  "
                                    <> contextTitle cx
                            printBranch c (childId : visited) childId (depth + 1)

-- =============================================================
-- exists
-- =============================================================

data ExistsOpts = ExistsOpts
    { exId :: Text
    , exVerbose :: Bool
    }

existsP :: Parser ExistsOpts
existsP =
    ExistsOpts . T.pack
        <$> strArgument (metavar "CONTEXT_ID")
        <*> switch (long "verbose" <> short 'v' <> help "Print the resolved full id on stdout")

runExists :: FilePath -> ExistsOpts -> IO ()
runExists db o = withDb db $ \c -> do
    cxs <- RCx.getContextsByPrefix c (exId o)
    case cxs of
        [cx] -> do
            when (exVerbose o) $ TIO.putStrLn (contextId cx)
        [] -> exitWith (ExitFailure 1)
        _ -> do
            hPutStrLn stderr $
                "ambiguous: "
                    <> T.unpack (exId o)
                    <> " matches "
                    <> show (length cxs)
                    <> " contexts"
            exitWith (ExitFailure 2)

padr :: Int -> Text -> Text
padr n s = s <> T.replicate (max 0 (n - T.length s)) " "
