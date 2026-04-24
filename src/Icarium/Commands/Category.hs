module Icarium.Commands.Category (Command, parser, run) where

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO
import           Options.Applicative

import           Icarium.Commands.Util
import           Icarium.Db            (defaultDbPath, withDb)
import qualified Icarium.Render        as Render
import qualified Icarium.Repo.Category as RC
import           Icarium.Types

data Command
    = Add AddOpts
    | List ListOpts
    | Rm RmOpts

parser :: Parser Command
parser = subparser
    ( subcmd "add"  "Add a category"    (Add  <$> addP)
   <> subcmd "list" "List categories"   (List <$> listP)
   <> subcmd "rm"   "Delete a category" (Rm   <$> rmP)
    )

run :: Command -> IO ()
run = \case
    Add o  -> runAdd o
    List o -> runList o
    Rm o   -> runRm o

data AddOpts = AddOpts
    { aAxis :: CategoryAxis
    , aName :: Text
    }

addP :: Parser AddOpts
addP = AddOpts
    <$> argument axisReader (metavar "AXIS" <> help "domain | discipline")
    <*> (T.pack <$> strArgument (metavar "NAME"))

runAdd :: AddOpts -> IO ()
runAdd o = withDb defaultDbPath $ \c -> do
    existing <- RC.findCategory c (aAxis o) (aName o)
    case existing of
        Just cat -> TIO.putStrLn (categoryId cat)   -- idempotent
        Nothing  -> do
            cid <- RC.insertCategory c (aAxis o) (aName o)
            TIO.putStrLn cid

data ListOpts = ListOpts { lAxis :: Maybe CategoryAxis }

listP :: Parser ListOpts
listP = ListOpts
    <$> optional (option axisReader (long "axis" <> metavar "AXIS"))

runList :: ListOpts -> IO ()
runList o = withDb defaultDbPath $ \c -> do
    cats <- RC.listCategories c (lAxis o)
    case cats of
        [] -> TIO.putStrLn "(no categories)"
        _  -> mapM_ (TIO.putStrLn . Render.renderCategory) cats

data RmOpts = RmOpts
    { rAxis :: CategoryAxis
    , rName :: Text
    }

rmP :: Parser RmOpts
rmP = RmOpts
    <$> argument axisReader (metavar "AXIS")
    <*> (T.pack <$> strArgument (metavar "NAME"))

runRm :: RmOpts -> IO ()
runRm o = withDb defaultDbPath $ \c -> do
    ok <- RC.deleteCategory c (rAxis o) (rName o)
    if ok then TIO.putStrLn ("deleted " <> categoryAxisText (rAxis o) <> ":" <> rName o)
          else fatal 1 ("category not found: "
                         <> T.unpack (categoryAxisText (rAxis o))
                         <> ":" <> T.unpack (rName o))
