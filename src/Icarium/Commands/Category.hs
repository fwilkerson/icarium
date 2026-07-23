module Icarium.Commands.Category (Command, parser, run) where

import Control.Monad (forM_, unless, void, when)
import Data.Char (isAlphaNum, isAscii)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Options.Applicative
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)

import Icarium.Categories (SyncReport (..), syncCategories)
import Icarium.Commands.Util
import Icarium.Config (
    CategoriesConfig (..),
    Config (..),
    addCategoryToToml,
    defaultConfigPath,
 )
import Icarium.Db (withDb)
import Icarium.Render qualified as Render
import Icarium.Repo.Category qualified as RC
import Icarium.Types

data Command
    = Add AddOpts
    | List ListOpts
    | Sync SyncOpts

parser :: Parser Command
parser =
    subparser
        ( subcmd
            "add"
            "Register a category (updates icarium.toml and the DB; idempotent)."
            (Add <$> addP)
            <> subcmd "list" "List categories (alias: ls)" (List <$> listP)
            <> subcmd
                "sync"
                "Reconcile icarium.toml [categories] → DB. Use --prune to delete DB-only categories."
                (Sync <$> syncP)
        )

run :: FilePath -> Command -> IO ()
run db = \case
    Add o -> runAdd db o
    List o -> runList db o
    Sync o -> runSync db o

-- ------------------------------------------------------------------ add

data AddOpts = AddOpts
    { aAxis :: CategoryAxis
    , aName :: Text
    }

addP :: Parser AddOpts
addP =
    AddOpts
        <$> option
            axisReader
            ( long "axis"
                <> metavar "AXIS"
                <> help "Axis to register under (domain | discipline | kind)."
            )
        <*> (T.pack <$> strArgument (metavar "NAME" <> help "Category name (letters, digits, . _ -)."))

runAdd :: FilePath -> AddOpts -> IO ()
runAdd db o = do
    let name = aName o
        axis = aAxis o
        label = categoryAxisText axis <> ":" <> name
    unless (validCategoryName name) $
        fatal 2 ("invalid category name: '" <> T.unpack name <> "'; use letters, digits, '.', '_', '-'")
    config <- requireConfig
    let tomlNames = case axis of
            Domain -> catDomains (cfgCategories config)
            Discipline -> catDisciplines (cfgCategories config)
            Kind -> catKinds (cfgCategories config)
        inToml = name `elem` tomlNames
    withDb db $ \c -> do
        mExisting <- RC.findCategory c axis name
        if inToml && isJust mExisting
            then TIO.putStrLn ("already registered " <> label)
            else do
                unless inToml $ do
                    src <- TIO.readFile defaultConfigPath
                    case addCategoryToToml axis name src of
                        Left err -> fatal 2 err
                        Right newSrc -> TIO.writeFile defaultConfigPath newSrc
                when (isNothing mExisting) $
                    void (RC.insertCategory c axis name)
                TIO.putStrLn ("registered " <> label)

validCategoryName :: Text -> Bool
validCategoryName n =
    not (T.null n) && T.all (\ch -> isAscii ch && (isAlphaNum ch || ch `elem` (".-_" :: String))) n

-- ------------------------------------------------------------------ list

newtype ListOpts = ListOpts {lAxis :: Maybe CategoryAxis}

listP :: Parser ListOpts
listP =
    ListOpts
        <$> optional
            ( option
                axisReader
                ( long "axis"
                    <> metavar "AXIS"
                    <> help "Filter by axis (domain | discipline | kind)."
                )
            )

runList :: FilePath -> ListOpts -> IO ()
runList db o = withDb db $ \c -> do
    cats <- RC.listCategories c (lAxis o)
    case cats of
        [] -> TIO.putStrLn "(no categories)"
        _ -> mapM_ (TIO.putStrLn . Render.renderCategory) cats

-- ------------------------------------------------------------------ sync

newtype SyncOpts = SyncOpts {sPrune :: Bool}

syncP :: Parser SyncOpts
syncP =
    SyncOpts
        <$> switch
            ( long "prune"
                <> help "Delete DB-only categories with no attachments (fails if any are in use)"
            )

runSync :: FilePath -> SyncOpts -> IO ()
runSync db o = do
    config <- requireConfig
    withDb db $ \conn -> do
        rpt <- syncCategories conn (cfgCategories config) (sPrune o)
        forM_ (srInserted rpt) $ \(ax, n) ->
            TIO.putStrLn $ "inserted  " <> categoryAxisText ax <> ":" <> n
        forM_ (srPruned rpt) $ \cat ->
            TIO.putStrLn $ "deleted   " <> catLabel cat
        forM_ (srOrphans rpt) $ \cat ->
            hPutStrLn stderr $ "orphan: " <> T.unpack (catLabel cat)
        forM_ (srBlocking rpt) $ \(cat, nodeIds) -> do
            hPutStrLn stderr $
                "blocking: "
                    <> T.unpack (catLabel cat)
                    <> " → "
                    <> T.unpack (T.intercalate ", " nodeIds)
        if null (srOrphans rpt) && null (srBlocking rpt)
            then
                when (null (srInserted rpt) && null (srPruned rpt)) $
                    TIO.putStrLn "in sync"
            else exitWith (ExitFailure 1)

-- ------------------------------------------------------------------ helpers

catLabel :: Category -> Text
catLabel cat = categoryAxisText (categoryAxis cat) <> ":" <> categoryName cat
