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
import           System.Environment        (getArgs)

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
    -- Rewrite `ls` to `list` immediately after a noun group so `task ls`,
    -- `know ls`, etc. work without registering ls in each subparser (which
    -- would cause it to appear in --help).
    argv <- lsToList <$> getArgs
    args <- handleParseResult (execParserPure defaultPrefs parser argv)
    runCmd (argsDb args) (argsCmd args)

nounGroups :: [String]
nounGroups = ["task", "know", "dispatch", "link", "category"]

-- | Rewrite `ls` to `list` when it directly follows a noun group name.
lsToList :: [String] -> [String]
lsToList (n : "ls" : rest) | n `elem` nounGroups = n : "list" : rest
lsToList (a : rest)                               = a : lsToList rest
lsToList []                                       = []

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
   <> progDesc "Task/knowledge/dispatch tool for headless-agent workflows. IDs are ULIDs; any unique prefix is accepted; lists show 10-char prefixes, show/create output prints the full id."
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
              (progDesc "Verify config, DB file, schema version, claude and git binaries, and orphaned dispatches."))
   <> command "task"
        (info (CmdTask <$> Task.parser <**> helper)
              (progDesc "Manage tasks. Example: icarium task list --state ready"))
   <> command "know"
        (info (CmdKnow <$> Know.parser <**> helper)
              (progDesc "Manage knowledge entries. Example: icarium know list --discipline planning"))
   <> command "link"
        (info (CmdLink <$> Link.parser <**> helper)
              (progDesc "Manage typed edges between nodes (add, list, rm). Example: icarium link add TASK_A depends-on TASK_B"))
   <> command "category"
        (info (CmdCategory <$> Category.parser <**> helper)
              (progDesc "Manage category vocabulary. Example: icarium category list --axis domain"))
   <> command "dispatch"
        (info (CmdDispatch <$> Dispatch.parser <**> helper)
              (progDesc "Manage dispatches (run, list, show, logs, recover). Example: icarium dispatch run --max 5"))
    )
