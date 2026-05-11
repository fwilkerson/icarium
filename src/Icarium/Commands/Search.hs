module Icarium.Commands.Search (Options, parser, run) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Database.SQLite.Simple (Connection)
import Options.Applicative

import Icarium.Commands.Util
import Icarium.Db (withDb)
import Icarium.Render (SearchHitRow (..))
import Icarium.Render qualified as Render
import Icarium.Repo.Category qualified as RC
import Icarium.Repo.Search qualified as RS
import Icarium.Types

data Options = Options
    { oQuery :: Text
    , oKind :: Maybe NodeKind
    , oLimit :: Int
    , oNoSnippet :: Bool
    }

parser :: Parser Options
parser =
    Options . T.pack
        <$> strArgument
            ( metavar "QUERY"
                <> help
                    "Search terms. Multiple words are ANDed (any order). \
                    \Quote a phrase for exact-substring match. \
                    \Use OR between tokens for union. \
                    \Bare words also match their underscore-joined form (e.g. 'client credentials' finds 'client_credentials')."
            )
        <*> optional
            ( option
                kindReader
                ( long "kind"
                    <> metavar "task|ctx"
                    <> help "Narrow to tasks or context entries only"
                )
            )
        <*> option
            auto
            ( long "limit"
                <> metavar "N"
                <> value 10
                <> showDefault
                <> help "Maximum number of results"
            )
        <*> switch (long "no-snippet" <> help "One line per hit; suppress body snippet")

kindReader :: ReadM NodeKind
kindReader = eitherReader $ \case
    "task" -> Right TaskNode
    "ctx" -> Right ContextNode
    s -> Left ("invalid kind: " <> s <> "; accepted: task, ctx")

run :: FilePath -> Options -> IO ()
run db o = withDb db $ \c -> do
    hits <- RS.searchEntries c (oQuery o) (oKind o) (oLimit o)
    rows <- buildRows c hits
    utf8 <- detectUtf8
    isTty <- detectTty
    TIO.putStr (Render.renderSearchList utf8 isTty (oNoSnippet o) (oQuery o) rows)

buildRows :: Connection -> [RS.SearchHit] -> IO [SearchHitRow]
buildRows c hits = do
    let taskIds = [RS.hitId h | h <- hits, RS.hitKind h == TaskNode]
        ctxIds = [RS.hitId h | h <- hits, RS.hitKind h == ContextNode]
    taskCats <- RC.taskCategoriesBatch c taskIds
    ctxCats <- RC.contextCategoriesBatch c ctxIds
    let allCats = taskCats ++ ctxCats
    pure [SearchHitRow h (fromMaybe [] (lookup (RS.hitId h) allCats)) | h <- hits]
