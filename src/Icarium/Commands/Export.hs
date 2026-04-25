module Icarium.Commands.Export (Options, parser, run) where

import           Data.Aeson             (encode, object, (.=))
import qualified Data.ByteString.Lazy   as BL
import           Options.Applicative
import           System.IO              (stdout)

import           Icarium.Db             (defaultDbPath, withDb)
import qualified Icarium.Repo.Category  as RC
import qualified Icarium.Repo.Dispatch  as RD
import qualified Icarium.Repo.Edge      as RE
import qualified Icarium.Repo.Knowledge as RK
import qualified Icarium.Repo.Task      as RT
import           Icarium.Types          ()

data Options = Options
    { optOut :: Maybe FilePath
    }

parser :: Parser Options
parser = Options
    <$> optional (strOption
          ( long "out"
         <> metavar "PATH"
         <> help "Write output to PATH instead of stdout" ))

run :: Options -> IO ()
run Options{..} = withDb defaultDbPath $ \conn -> do
    tasks      <- RT.listTasks conn [] False Nothing Nothing
    knowledge  <- RK.listKnowledge conn False Nothing Nothing
    edges      <- RE.listEdges conn Nothing Nothing Nothing
    categories <- RC.listCategories conn Nothing
    dispatches <- RD.listDispatches conn Nothing
    let snapshot = object
            [ "schema_version" .= (1 :: Int)
            , "tasks"          .= tasks
            , "knowledge"      .= knowledge
            , "edges"          .= edges
            , "categories"     .= categories
            , "dispatches"     .= dispatches
            ]
        bytes = encode snapshot
    case optOut of
        Nothing   -> BL.hPutStr stdout bytes >> putStrLn ""
        Just path -> BL.writeFile path bytes
