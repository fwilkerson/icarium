module Icarium.Commands.Ctx (Command, parser, run) where

import Control.Monad (forM, forM_, void, when)
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Database.SQLite.Simple (Connection)
import Options.Applicative
import System.Environment (lookupEnv)
import System.IO (stderr)

import Icarium.Bodies (bodiesDir, ctxBodyPath)
import Icarium.Commands.Node qualified as Node
import Icarium.Commands.Util
import Icarium.Db (withDb)
import Icarium.Events qualified as Ev
import Icarium.Node (autoDeriveDeps, createContextWithBody, inheritedContextCategories)
import Icarium.Render qualified as Render
import Icarium.Render.Json qualified as Json
import Icarium.Repo.Category qualified as RC
import Icarium.Repo.Context qualified as RCx
import Icarium.Repo.Curation qualified as RCur
import Icarium.Repo.Edge qualified as RE
import Icarium.Types

data Command
    = Add AddOpts
    | List ListOpts
    | Show ShowOpts
    | Update UpdateOpts
    | Curate CurateOpts
    | Rm Text
    | Path Text
    | Cat Text
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
            <> subcmd "rm" "Delete a context entry" (Rm <$> Node.nodeIdArg ContextNode)
            <> subcmd "path" "Print body file path for a context entry (the body is a markdown file you Read/Edit directly)." (Path <$> Node.nodeIdArg ContextNode)
            <> subcmd "cat" "Print body of a context entry to stdout. Exit non-zero if the body file is missing." (Cat <$> Node.nodeIdArg ContextNode)
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
    Rm t -> Node.runRm ContextNode db t
    Path t -> Node.runPath ContextNode db t
    Cat t -> Node.runCat ContextNode db t
    Children o -> runChildren db o
    Tree o -> runTree db o
    Exists o -> Node.runExists ContextNode db (exVerbose o) (exId o)

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
    derived <- mapM (Node.resolveNode c) (aDerivedFrom o)
    mSupersedesId <- mapM (Node.requireContext c) (aSupersedes o)

    mTaskId <- lookupEnv "ICARIUM_TASK_ID"

    inheritedCats <- inheritedContextCategories c (aDomain o) (aDiscipline o) mTaskId
    autoDerived <- autoDeriveDeps c (aDerivedFrom o) mTaskId

    (cxid, fp) <-
        createContextWithBody
            c
            db
            RCx.NewContext
                { RCx.ncTitle = aTitle o
                , RCx.ncBody = body
                , -- Hand-filed: `ctx add` has no run behind it. The gate sets
                  -- provenance directly rather than shelling out to this.
                  RCx.ncSourceDispatch = Nothing
                }
    forM_ (catMaybes [mDomain, mDisc] <> inheritedCats) (RC.attachContextCategory c cxid)
    forM_ (derived <> autoDerived) $ \(nkind, nid) ->
        void $ RE.insertEdge c DerivedFrom ContextNode cxid nkind nid
    case mSupersedesId of
        Just target ->
            void $ RE.insertEdge c Supersedes ContextNode cxid ContextNode target
        Nothing -> pure ()
    Ev.emit db "ctx add" (Ev.CtxCreated cxid)
    TIO.putStrLn cxid
    TIO.putStrLn (T.pack fp)
    when (T.null body) $
        mapM_ (TIO.hPutStrLn stderr) (Render.emptyBodyNudge ContextNode cxid fp)

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
    catFilters <-
        resolveCatFilters c [(Domain, lDomain o), (Discipline, lDiscipline o)]
    let retiredFilter
            | lRetired o = Just True -- retired only
            | lAll o = Nothing -- show all
            | otherwise = Just False -- hide retired (default)
    let includeSuperseded = lAll o
    cxs0 <- RCx.listContexts c retiredFilter includeSuperseded catFilters
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
    ShowOpts
        <$> Node.nodeIdArg ContextNode
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
    UpdateOpts
        <$> Node.nodeIdArg ContextNode
        <*> optional (textOption "title" "TEXT" "Replace entry title. Keep ≤ 72 chars; longer titles are truncated in `ctx list`.")
        <*> optional (textOption "domain" "NAME" "Replace domain category; empty string clears")
        <*> optional (textOption "discipline" "NAME" "Replace discipline category; empty string clears")

runUpdate :: FilePath -> UpdateOpts -> IO ()
runUpdate db o = withDb db $ \c -> do
    cxid <- resolveOrFatal (RCx.resolveContextId c (uId o))
    -- Validate categories before any mutation.
    mDomCat <- resolveAxisFlag c Domain (uDomain o)
    mDiscCat <- resolveAxisFlag c Discipline (uDiscipline o)
    -- Read the pre-update row and its tags here: the event is only honest if
    -- the write moved something, and after the write there is nothing to compare.
    mBefore <- RCx.getContext c cxid
    beforeCats <- RC.contextCategoriesFor c cxid
    let upd = RCx.emptyUpdate{RCx.cuTitle = uTitle o}
    ok <- RCx.updateContext c cxid upd
    if ok
        then do
            -- Apply replacements: detach all of that axis, then attach new one if given.
            forM_ mDomCat $ \mCat -> do
                RC.detachContextCategoriesByAxis c cxid Domain
                forM_ mCat (RC.attachContextCategory c cxid)
            forM_ mDiscCat $ \mCat -> do
                RC.detachContextCategoriesByAxis c cxid Discipline
                forM_ mCat (RC.attachContextCategory c cxid)
            let titleMoved = case (uTitle o, mBefore) of
                    (Just new, Just before) -> new /= contextTitle before
                    _ -> False
                axisMoved ax mChange = case mChange of
                    Nothing -> False
                    Just mCat ->
                        map categoryId (filter ((== ax) . categoryAxis) beforeCats)
                            /= maybe [] (pure . categoryId) mCat
            -- A flag that restates what is already stored moved nothing, and
            -- an event for it would record a mutation that never happened.
            when (titleMoved || axisMoved Domain mDomCat || axisMoved Discipline mDiscCat) $
                Ev.emit db "ctx update" (Ev.CtxUpdated cxid)
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
        artifact <- requireArtifact c disp (cArtifact o)
        void $ RCur.insertCuration c cxid disp artifact (cNote o)
        Ev.emit db "ctx curate" (Ev.CtxCurated cxid disp artifact)
        TIO.putStrLn ("curated " <> cxid <> " " <> dispositionText disp)

{- | Enforce the artifact rule per disposition; the sweep must leave a
trail for content that went somewhere. Task/context artifacts are
resolved to canonical full ids before storage.
-}
requireArtifact :: Connection -> Disposition -> Maybe Text -> IO (Maybe Text)
requireArtifact c disp mArtifact = case (disp, mArtifact) of
    (Guidance, Nothing) -> missing "guidance" "the destination doc/skill path"
    (Rule, Nothing) -> missing "rule" "the rule/invariant/test name"
    (Refactor, Nothing) -> missing "refactor" "the filed task id"
    (Refactor, Just a) -> Just <$> Node.requireTask c a
    (Stale, Just a) -> Just <$> Node.requireContext c a
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
-- children
-- =============================================================

data ChildrenOpts = ChildrenOpts
    { chId :: Text
    , chKind :: Maybe EdgeKind
    , chJson :: Bool
    }

childrenP :: Parser ChildrenOpts
childrenP =
    ChildrenOpts
        <$> Node.nodeIdArg ContextNode
        <*> optional
            ( option
                edgeKindReader
                ( long "kind"
                    <> metavar "KIND"
                    <> help "Filter by edge kind (derived-from | references | supersedes)"
                )
            )
        <*> jsonFlag

runChildren :: FilePath -> ChildrenOpts -> IO ()
runChildren db o = withDb db $ \c -> do
    cxid <- resolveOrFatal (RCx.resolveContextId c (chId o))
    children <- RE.ctxChildContexts c cxid (chKind o)
    let rows = [Render.ContextChildRow{Render.ccKind = k, Render.ccContext = cx} | (k, cx) <- children]
    if chJson o
        then BLC.putStrLn (Json.renderContextChildrenJson rows)
        else TIO.putStr (Render.renderContextChildren rows)

-- =============================================================
-- tree
-- =============================================================

data TreeOpts = TreeOpts
    { tId :: Text
    , tJson :: Bool
    }

treeP :: Parser TreeOpts
treeP =
    TreeOpts
        <$> Node.nodeIdArg ContextNode
        <*> jsonFlag

runTree :: FilePath -> TreeOpts -> IO ()
runTree db o = withDb db $ \c -> do
    cxid <- resolveOrFatal (RCx.resolveContextId c (tId o))
    mcx <- RCx.getContext c cxid
    cx <- maybe (fatal 1 ("context not found: " <> T.unpack cxid)) pure mcx
    root <- buildNode c [cxid] cx
    if tJson o
        then BLC.putStrLn (Json.renderContextTreeJson root)
        else TIO.putStr (Render.renderContextTree root)
  where
    -- @visited@ is the path from the root, not every node seen: a diamond
    -- still renders both ways in, only a true back-edge is cut.
    buildNode c visited cx = do
        children <- RE.ctxChildContexts c (contextId cx) Nothing
        kids <- forM children $ \(kind, child) ->
            if contextId child `elem` visited
                then pure (kind, cycleNode child)
                else do
                    node <- buildNode c (contextId child : visited) child
                    pure (kind, node)
        pure Render.ContextTreeNode{Render.ctnContext = cx, Render.ctnCycle = False, Render.ctnChildren = kids}
    cycleNode child =
        Render.ContextTreeNode{Render.ctnContext = child, Render.ctnCycle = True, Render.ctnChildren = []}

-- =============================================================
-- exists
-- =============================================================

data ExistsOpts = ExistsOpts
    { exId :: Text
    , exVerbose :: Bool
    }

existsP :: Parser ExistsOpts
existsP =
    ExistsOpts
        <$> Node.nodeIdArg ContextNode
        <*> switch (long "verbose" <> short 'v' <> help "Print the resolved full id on stdout")
