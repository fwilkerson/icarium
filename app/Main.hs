module Main (main) where

import qualified Icarium.Commands.Category as Category
import qualified Icarium.Commands.Dispatch as Dispatch
import qualified Icarium.Commands.Doctor   as Doctor
import qualified Icarium.Commands.Init     as Init
import qualified Icarium.Commands.Know     as Know
import qualified Icarium.Commands.Link     as Link
import qualified Icarium.Commands.Task     as Task
import           Options.Applicative

data Command
    = CmdInit     Init.Options
    | CmdDoctor   Doctor.Options
    | CmdTask     Task.Command
    | CmdKnow     Know.Command
    | CmdLink     Link.Command
    | CmdCategory Category.Command
    | CmdDispatch Dispatch.Command

main :: IO ()
main = run =<< execParser parser

run :: Command -> IO ()
run (CmdInit o)     = Init.run o
run (CmdDoctor o)   = Doctor.run o
run (CmdTask c)     = Task.run c
run (CmdKnow c)     = Know.run c
run (CmdLink c)     = Link.run c
run (CmdCategory c) = Category.run c
run (CmdDispatch c) = Dispatch.run c

parser :: ParserInfo Command
parser = info (commands <**> helper)
    ( fullDesc
   <> progDesc "Task/knowledge/dispatch tool for headless-agent workflows"
   <> header   "icarium" )

commands :: Parser Command
commands = subparser
    ( command "init"
        (info (CmdInit <$> Init.parser <**> helper)
              (progDesc "Initialize a project: create DB, apply schema, write config"))
   <> command "doctor"
        (info (CmdDoctor <$> Doctor.parser <**> helper)
              (progDesc "Check project health"))
   <> command "task"
        (info (CmdTask <$> Task.parser <**> helper)
              (progDesc "Manage tasks (list is the default)"))
   <> command "know"
        (info (CmdKnow <$> Know.parser <**> helper)
              (progDesc "Manage knowledge entries (list is the default)"))
   <> command "link"
        (info (CmdLink <$> Link.parser <**> helper)
              (progDesc "Manage typed edges between nodes (list is the default)"))
   <> command "category"
        (info (CmdCategory <$> Category.parser <**> helper)
              (progDesc "Manage category vocabulary (list is the default)"))
   <> command "dispatch"
        (info (CmdDispatch <$> Dispatch.parser <**> helper)
              (progDesc "Manage dispatches (run, show, logs, recover; bare = list)"))
    )
