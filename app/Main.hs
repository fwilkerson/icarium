module Main (main) where

import qualified Icarium.Commands.Category as Category
import qualified Icarium.Commands.Dispatch as Dispatch
import qualified Icarium.Commands.Doctor   as Doctor
import qualified Icarium.Commands.Init     as Init
import qualified Icarium.Commands.Know     as Know
import qualified Icarium.Commands.Link     as Link
import qualified Icarium.Commands.Task     as Task
import           Icarium.Db                (defaultDbPath)
import           Options.Applicative

data Command
    = CmdInit     Init.Options
    | CmdDoctor   Doctor.Options
    | CmdTask     Task.Command
    | CmdKnow     Know.Command
    | CmdLink     Link.Command
    | CmdCategory Category.Command
    | CmdDispatch Dispatch.Command

data Args = Args
    { argsDb  :: FilePath
    , argsCmd :: Command
    }

main :: IO ()
main = do
    args <- execParser parser
    runCmd (argsDb args) (argsCmd args)

runCmd :: FilePath -> Command -> IO ()
runCmd db (CmdInit o)     = Init.run db o
runCmd db (CmdDoctor o)   = Doctor.run db o
runCmd db (CmdTask c)     = Task.run db c
runCmd db (CmdKnow c)     = Know.run db c
runCmd db (CmdLink c)     = Link.run db c
runCmd db (CmdCategory c) = Category.run db c
runCmd db (CmdDispatch c) = Dispatch.run db c

parser :: ParserInfo Args
parser = info (argsP <**> helper)
    ( fullDesc
   <> progDesc "Task/knowledge/dispatch tool for headless-agent workflows"
   <> header   "icarium" )

argsP :: Parser Args
argsP = Args
    <$> strOption
            ( long "db" <> metavar "PATH"
           <> value defaultDbPath <> showDefault
           <> help "SQLite database file" )
    <*> cmdP

cmdP :: Parser Command
cmdP = subparser
    ( command "init"
        (info (CmdInit <$> Init.parser <**> helper)
              (progDesc "Initialize a project: create DB, apply schema, write config"))
   <> command "doctor"
        (info (CmdDoctor <$> Doctor.parser <**> helper)
              (progDesc "Verify DB schema version, foreign-key integrity, missing categories, and orphan rows."))
   <> command "task"
        (info (CmdTask <$> Task.parser <**> helper)
              (progDesc "Manage tasks (list is the default). Example: icarium task add \"Refactor X\" --domain cli --discipline haskell"))
   <> command "know"
        (info (CmdKnow <$> Know.parser <**> helper)
              (progDesc "Manage knowledge entries (list is the default). Example: icarium know --discipline planning"))
   <> command "link"
        (info (CmdLink <$> Link.parser <**> helper)
              (progDesc "Manage typed edges between nodes (list is the default). Example: icarium link add depends-on TASK_A TASK_B"))
   <> command "category"
        (info (CmdCategory <$> Category.parser <**> helper)
              (progDesc "Manage category vocabulary (list is the default). Example: icarium category --axis domain"))
   <> command "dispatch"
        (info (CmdDispatch <$> Dispatch.parser <**> helper)
              (progDesc "Manage dispatches (run, show, logs, recover; bare = list). Example: icarium dispatch run --max 5"))
    )
