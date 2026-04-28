module Icarium.Commands.Link (Command, parser, run) where

import           Control.Monad          (when)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Database.SQLite.Simple (Connection)
import           Options.Applicative

import           Icarium.Commands.Util
import           Icarium.Db             (withDb)
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
    ( subcmd "add"   "Add an edge"              (Add  <$> addP)
   <> subcmd "list"  "List edges (alias: ls)"   (List <$> listP)
   <> subcmd "ls"    "List edges (alias: list)" (List <$> listP)
   <> subcmd "rm"    "Delete an edge"           (Rm   <$> rmP)
    )
    <|> (List <$> listP)

run :: FilePath -> Command -> IO ()
run db = \case
    Add o  -> runAdd db o
    List o -> runList db o
    Rm o   -> runRm db o

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
             <> help "depends-on | references | derived-from | supersedes")
    <*> (T.pack <$> strArgument (metavar "DST_ID"))

-- | Resolve a node id (ULID prefix or full) to (kind, canonical id).
resolveNode :: Connection -> Text -> IO (NodeKind, Text)
resolveNode c input = do
    ts <- RT.getTasksByPrefix c input
    ks <- RK.getKnowledgesByPrefix c input
    case (ts, ks) of
        ([t], [] ) -> pure (TaskNode, taskId t)
        ([], [k] ) -> pure (KnowledgeNode, knowledgeId k)
        ([], []  ) -> fatal 2 ("unknown node: " <> T.unpack input)
        _          -> fatal 2 ("ambiguous id: " <> T.unpack input)

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
    "edge " <> T.unpack (edgeKindDisplay k) <> " requires "
    <> expectedShape k <> " but got "
    <> T.unpack (nodeKindText sk) <> " -> " <> T.unpack (nodeKindText dk)

expectedShape :: EdgeKind -> String
expectedShape DependsOn   = "task -> task"
expectedShape References  = "task -> knowledge"
expectedShape DerivedFrom = "knowledge -> (task|knowledge)"
expectedShape Supersedes  = "knowledge -> knowledge"

runAdd :: FilePath -> AddOpts -> IO ()
runAdd db o = withDb db $ \c -> do
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
    <$> optional (T.pack <$> strOption (long "from" <> metavar "ID"
           <> help "Only edges outgoing from this node"))
    <*> optional (T.pack <$> strOption (long "to"   <> metavar "ID"
           <> help "Only edges incoming to this node"))
    <*> optional (option edgeKindReader (long "kind" <> metavar "KIND"
           <> help "Filter by edge kind"))

runList :: FilePath -> ListOpts -> IO ()
runList db o = withDb db $ \c -> do
    mFrom <- traverse (fmap snd . resolveNode c) (lFrom o)
    mTo   <- traverse (fmap snd . resolveNode c) (lTo o)
    es <- RE.listEdges c mFrom mTo (lKind o)
    case es of
        [] -> TIO.putStrLn "(no edges)"
        _  -> mapM_ (TIO.putStrLn . Render.renderEdgeLine) es

-- =============================================================
-- rm
-- =============================================================

newtype RmOpts = RmOpts { rId :: Text }

rmP :: Parser RmOpts
rmP = RmOpts . T.pack <$> strArgument (metavar "EDGE_ID")

runRm :: FilePath -> RmOpts -> IO ()
runRm db o = withDb db $ \c -> do
    eid <- RE.resolveEdgeId c (rId o) >>= \case
        Left err -> fatal 1 err
        Right x  -> pure x
    ok <- RE.deleteEdge c eid
    if ok then TIO.putStrLn ("deleted " <> eid)
          else fatal 1 ("edge not found: " <> T.unpack (rId o))
