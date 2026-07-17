module Icarium.Commands.Search (Options, parser, run) where

import Control.Monad (forM_, void, when)
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Database.SQLite.Simple (Connection)
import Options.Applicative

import Icarium.Commands.Util
import Icarium.Db (withDbSync)
import Icarium.Render (SearchHitRow (..))
import Icarium.Render qualified as Render
import Icarium.Render.Json qualified as Json
import Icarium.Repo.Category qualified as RC
import Icarium.Repo.Search qualified as RS
import Icarium.Types

data Options = Options
    { oQuery :: Text
    , oKind :: Maybe NodeKind
    , oLimit :: Int
    , oNoSnippet :: Bool
    , oDomains :: [Text]
    , oDisciplines :: [Text]
    , oExcludeDomains :: [Text]
    , oExcludeDisciplines :: [Text]
    , oTitleOnly :: Bool
    , oBodyOnly :: Bool
    , oJson :: Bool
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
        <*> many (textOption "domain" "NAME" "Include only entries tagged with this domain (repeatable; OR within axis, AND across axes)")
        <*> many (textOption "discipline" "NAME" "Include only entries tagged with this discipline (repeatable; OR within axis, AND across axes)")
        <*> many (textOption "exclude-domain" "NAME" "Exclude entries tagged with this domain (repeatable)")
        <*> many (textOption "exclude-discipline" "NAME" "Exclude entries tagged with this discipline (repeatable)")
        <*> switch (long "title-only" <> help "Match only against entry titles (mutually exclusive with --body-only)")
        <*> switch (long "body-only" <> help "Match only against entry bodies (mutually exclusive with --title-only)")
        <*> jsonFlag

kindReader :: ReadM NodeKind
kindReader = eitherReader $ \case
    "task" -> Right TaskNode
    "ctx" -> Right ContextNode
    s -> Left ("invalid kind: " <> s <> "; accepted: task, ctx")

run :: FilePath -> Options -> IO ()
run db o = do
    when (oTitleOnly o && oBodyOnly o) $
        fatal 1 "--title-only and --body-only are mutually exclusive"
    withDbSync db $ \c -> do
        validateCats c
        let scope
                | oTitleOnly o = RS.ScopeTitle
                | oBodyOnly o = RS.ScopeBody
                | otherwise = RS.ScopeAll
            filters =
                RS.SearchFilters
                    { RS.sfKind = oKind o
                    , RS.sfDomains = oDomains o
                    , RS.sfDisciplines = oDisciplines o
                    , RS.sfExcludeDomains = oExcludeDomains o
                    , RS.sfExcludeDisciplines = oExcludeDisciplines o
                    , RS.sfScope = scope
                    }
        (total, hits) <- RS.searchEntries c (oQuery o) filters (oLimit o)
        rows <- buildRows c hits
        if oJson o
            then BLC.putStrLn (Json.renderSearchJson rows)
            else do
                utf8 <- detectUtf8
                isTty <- detectTty
                TIO.putStr (Render.renderSearchList utf8 isTty (oNoSnippet o) (oQuery o) total rows)
  where
    validateCats c = do
        forM_ (oDomains o) $ \n -> void $ requireCategory c Domain n
        forM_ (oDisciplines o) $ \n -> void $ requireCategory c Discipline n
        forM_ (oExcludeDomains o) $ \n -> void $ requireCategory c Domain n
        forM_ (oExcludeDisciplines o) $ \n -> void $ requireCategory c Discipline n

buildRows :: Connection -> [RS.SearchHit] -> IO [SearchHitRow]
buildRows c hits = do
    let taskIds = [RS.hitId h | h <- hits, RS.hitKind h == TaskNode]
        ctxIds = [RS.hitId h | h <- hits, RS.hitKind h == ContextNode]
    taskCats <- RC.taskCategoriesBatch c taskIds
    ctxCats <- RC.contextCategoriesBatch c ctxIds
    let allCats = taskCats ++ ctxCats
    pure [SearchHitRow h (fromMaybe [] (lookup (RS.hitId h) allCats)) | h <- hits]
