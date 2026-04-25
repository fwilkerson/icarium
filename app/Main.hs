module Main (main) where

import qualified Icarium.Commands.Category as Category
import qualified Icarium.Commands.Dispatch as Dispatch
import qualified Icarium.Commands.Doctor   as Doctor
import qualified Icarium.Commands.Export   as Export
import qualified Icarium.Commands.Import   as Import
import qualified Icarium.Commands.Init     as Init
import qualified Icarium.Commands.Know     as Know
import qualified Icarium.Commands.Link     as Link
import qualified Icarium.Commands.Next     as Next
import qualified Icarium.Commands.Recover  as Recover
import qualified Icarium.Commands.Run      as Run
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
    | CmdNext     Next.Options
    | CmdRun      Run.Options
    | CmdRecover  Recover.Options
    | CmdExport   Export.Options
    | CmdImport   Import.Options

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
run (CmdNext o)     = Next.run o
run (CmdRun o)      = Run.run o
run (CmdRecover o)  = Recover.run o
run (CmdExport o)   = Export.run o
run (CmdImport o)   = Import.run o

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
              (progDesc "Manage tasks"))
   <> command "know"
        (info (CmdKnow <$> Know.parser <**> helper)
              (progDesc "Manage knowledge entries"))
   <> command "link"
        (info (CmdLink <$> Link.parser <**> helper)
              (progDesc "Manage typed edges between nodes"))
   <> command "category"
        (info (CmdCategory <$> Category.parser <**> helper)
              (progDesc "Manage category vocabulary"))
   <> command "dispatch"
        (info (CmdDispatch <$> Dispatch.parser <**> helper)
              (progDesc "Manage dispatches (run, list, show, logs)"))
   <> command "next"
        (info (CmdNext <$> Next.parser <**> helper)
              (progDesc "Print the next ready task id; exit 1 if queue empty"))
   <> command "run"
        (info (CmdRun <$> Run.parser <**> helper)
              (progDesc "Loop dispatching ready tasks until empty or --max"))
   <> command "recover"
        (info (CmdRecover <$> Recover.parser <**> helper)
              (progDesc "Reconcile orphaned in-progress dispatches"))
   <> command "export"
        (info (CmdExport <$> Export.parser <**> helper)
              (progDesc "Dump all data as a JSON snapshot"))
   <> command "import"
        (info (CmdImport <$> Import.parser <**> helper)
              (progDesc "Load a JSON snapshot into the DB"))
    )
