module Icarium.Commands.Link (Command, parser, run) where

import Control.Monad (when)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Options.Applicative

import Icarium.Commands.Util
import Icarium.Db (withDb)
import Icarium.Render qualified as Render
import Icarium.Repo.Edge qualified as RE
import Icarium.Types

data Command
    = Add AddOpts
    | List ListOpts
    | Rm RmOpts

parser :: Parser Command
parser =
    subparser
        ( subcmd "add" "Add an edge" (Add <$> addP)
            <> subcmd "list" "List edges (alias: ls)" (List <$> listP)
            <> subcmd "rm" "Delete an edge" (Rm <$> rmP)
        )

run :: FilePath -> Command -> IO ()
run db = \case
    Add o -> runAdd db o
    List o -> runList db o
    Rm o -> runRm db o

-- =============================================================
-- add
-- =============================================================

data AddOpts = AddOpts
    { aSrc :: Text
    , aKind :: EdgeKind
    , aDst :: Text
    }

addP :: Parser AddOpts
addP =
    AddOpts . T.pack
        <$> strArgument (metavar "SRC_ID")
        <*> argument
            edgeKindReader
            ( metavar "KIND"
                <> help "depends-on | references | derived-from | supersedes. For derived-from, `ctx add --derived-from <ID>` is the convenience equivalent."
            )
        <*> (T.pack <$> strArgument (metavar "DST_ID"))

{- | Validate that the edge endpoints match the edge kind's typing rules.
Mirrors the CHECK constraint in schema.sql; catching it here gives a
friendlier error than a SQLITE_CONSTRAINT.
-}
checkTyping :: EdgeKind -> NodeKind -> NodeKind -> Either String ()
checkTyping DependsOn TaskNode TaskNode = Right ()
checkTyping References TaskNode ContextNode = Right ()
checkTyping DerivedFrom ContextNode TaskNode = Right ()
checkTyping DerivedFrom ContextNode ContextNode = Right ()
checkTyping Supersedes ContextNode ContextNode = Right ()
checkTyping k sk dk =
    Left $
        "edge "
            <> T.unpack (edgeKindDisplay k)
            <> " requires "
            <> expectedShape k
            <> " but got "
            <> T.unpack (nodeKindText sk)
            <> " -> "
            <> T.unpack (nodeKindText dk)

expectedShape :: EdgeKind -> String
expectedShape DependsOn = "task -> task"
expectedShape References = "task -> context"
expectedShape DerivedFrom = "context -> (task|context)"
expectedShape Supersedes = "context -> context"

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
    , lTo :: Maybe Text
    , lKind :: Maybe EdgeKind
    }

listP :: Parser ListOpts
listP =
    ListOpts
        <$> optional (textOption "from" "ID" "Only edges outgoing from this node")
        <*> optional (textOption "to" "ID" "Only edges incoming to this node")
        <*> optional
            ( option
                edgeKindReader
                ( long "kind"
                    <> metavar "KIND"
                    <> help "Filter by edge kind"
                )
            )

runList :: FilePath -> ListOpts -> IO ()
runList db o = withDb db $ \c -> do
    mFrom <- traverse (fmap snd . resolveNode c) (lFrom o)
    mTo <- traverse (fmap snd . resolveNode c) (lTo o)
    es <- RE.listEdges c mFrom mTo (lKind o)
    case es of
        [] -> TIO.putStrLn "(no edges)"
        _ -> do
            TIO.putStrLn "EDGE_ID     KIND  FROM  ->  TO"
            mapM_ (TIO.putStrLn . Render.renderEdgeLine) es

-- =============================================================
-- rm
-- =============================================================

newtype RmOpts = RmOpts {rId :: Text}

rmP :: Parser RmOpts
rmP = RmOpts . T.pack <$> strArgument (metavar "EDGE_ID")

runRm :: FilePath -> RmOpts -> IO ()
runRm db o = withDb db $ \c -> do
    eid <- resolveOrFatal (RE.resolveEdgeId c (rId o))
    ok <- RE.deleteEdge c eid
    if ok
        then TIO.putStrLn ("deleted " <> eid)
        else fatal 1 ("edge not found: " <> T.unpack (rId o))
