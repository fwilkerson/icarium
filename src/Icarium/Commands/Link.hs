module Icarium.Commands.Link (Command, parser, run) where

import           Control.Monad          (when)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Database.SQLite.Simple (Connection)
import           Options.Applicative

import           Icarium.Commands.Util
import           Icarium.Db             (defaultDbPath, withDb)
import qualified Icarium.Render         as Render
import qualified Icarium.Repo.Edge      as RE
import qualified Icarium.Repo.Knowledge as RK
import qualified Icarium.Repo.Task      as RT
import           Icarium.Types

data Command
    = Add AddOpts
    | List ListOpts
    | Rm RmOpts

parser :: Parser Command
parser = subparser
    ( subcmd "add"  "Add an edge"     (Add  <$> addP)
   <> subcmd "list" "List edges"      (List <$> listP)
   <> subcmd "rm"   "Delete an edge"  (Rm   <$> rmP)
    )

run :: Command -> IO ()
run = \case
    Add o  -> runAdd o
    List o -> runList o
    Rm o   -> runRm o

-- =============================================================
-- add
-- =============================================================

data AddOpts = AddOpts
    { aSrc  :: Text
    , aKind :: EdgeKind
    , aDst  :: Text
    }

addP :: Parser AddOpts
addP = AddOpts . T.pack
    <$> strArgument (metavar "SRC_ID")
    <*> argument edgeKindReader
            (metavar "KIND"
             <> help "depends_on | references | derived_from | supersedes")
    <*> (T.pack <$> strArgument (metavar "DST_ID"))

-- | Resolve a node id to (kind, id), failing if not found.
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

-- | Validate that the edge endpoints match the edge kind's typing rules.
-- Mirrors the CHECK constraint in schema.sql; catching it here gives a
-- friendlier error than a SQLITE_CONSTRAINT.
checkTyping :: EdgeKind -> NodeKind -> NodeKind -> Either String ()
checkTyping DependsOn   TaskNode      TaskNode      = Right ()
checkTyping References  TaskNode      KnowledgeNode = Right ()
checkTyping DerivedFrom KnowledgeNode TaskNode      = Right ()
checkTyping DerivedFrom KnowledgeNode KnowledgeNode = Right ()
checkTyping Supersedes  KnowledgeNode KnowledgeNode = Right ()
checkTyping k sk dk = Left $
    "edge " <> T.unpack (edgeKindText k) <> " requires "
    <> expectedShape k <> " but got "
    <> T.unpack (nodeKindText sk) <> " -> " <> T.unpack (nodeKindText dk)

expectedShape :: EdgeKind -> String
expectedShape DependsOn   = "task -> task"
expectedShape References  = "task -> knowledge"
expectedShape DerivedFrom = "knowledge -> (task|knowledge)"
expectedShape Supersedes  = "knowledge -> knowledge"

runAdd :: AddOpts -> IO ()
runAdd o = withDb defaultDbPath $ \c -> do
    (srcKind, srcId) <- resolveNode c (aSrc o)
    (dstKind, dstId) <- resolveNode c (aDst o)
    case checkTyping (aKind o) srcKind dstKind of
        Left msg -> fatal 2 msg
        Right () -> do
            when (srcKind == dstKind && srcId == dstId) $
                fatal 2 "self-edges are not allowed"
            eid <- RE.insertEdge c (aKind o) srcKind srcId dstKind dstId
            TIO.putStrLn eid

-- =============================================================
-- list
-- =============================================================

data ListOpts = ListOpts
    { lFrom :: Maybe Text
    , lTo   :: Maybe Text
    , lKind :: Maybe EdgeKind
    }

listP :: Parser ListOpts
listP = ListOpts
    <$> optional (T.pack <$> strOption (long "from" <> metavar "ID"))
    <*> optional (T.pack <$> strOption (long "to"   <> metavar "ID"))
    <*> optional (option edgeKindReader (long "kind" <> metavar "KIND"))

runList :: ListOpts -> IO ()
runList o = withDb defaultDbPath $ \c -> do
    es <- RE.listEdges c (lFrom o) (lTo o) (lKind o)
    case es of
        [] -> TIO.putStrLn "(no edges)"
        _  -> mapM_ (TIO.putStrLn . Render.renderEdgeLine) es

-- =============================================================
-- rm
-- =============================================================

data RmOpts = RmOpts { rId :: Text }

rmP :: Parser RmOpts
rmP = RmOpts . T.pack <$> strArgument (metavar "EDGE_ID")

runRm :: RmOpts -> IO ()
runRm o = withDb defaultDbPath $ \c -> do
    ok <- RE.deleteEdge c (rId o)
    if ok then TIO.putStrLn ("deleted " <> rId o)
          else fatal 1 ("edge not found: " <> T.unpack (rId o))
