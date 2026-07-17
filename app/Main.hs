module Main (main) where

import Data.Version (showVersion)
import Icarium.Commands.Agents qualified as Agents
import Icarium.Commands.Category qualified as Category
import Icarium.Commands.Ctx qualified as Ctx
import Icarium.Commands.Dispatch qualified as Dispatch
import Icarium.Commands.Doctor qualified as Doctor
import Icarium.Commands.Init qualified as Init
import Icarium.Commands.Link qualified as Link
import Icarium.Commands.Reindex qualified as Reindex
import Icarium.Commands.Search qualified as Search
import Icarium.Commands.Task qualified as Task
import Icarium.Db (defaultDbPath)
import Options.Applicative
import Paths_icarium (version)
import System.Environment (getArgs, lookupEnv)

data Command
    = CmdInit Init.Options
    | CmdDoctor Doctor.Options
    | CmdTask Task.Command
    | CmdCtx Ctx.Command
    | CmdLink Link.Command
    | CmdCategory Category.Command
    | CmdDispatch Dispatch.Command
    | CmdSearch Search.Options
    | CmdReindex Reindex.Command
    | CmdAgents Agents.Command

data Args = Args
    { argsDb :: FilePath
    , argsCmd :: Command
    }

main :: IO ()
main = do
    mEnvDb <- lookupEnv "ICARIUM_DB"
    let defaultDb = case mEnvDb of
            Just db | not (null db) -> db
            _ -> defaultDbPath
    argv <- helpForBareNoun . lsToList <$> getArgs
    args <- handleParseResult (execParserPure defaultPrefs (parser defaultDb) argv)
    runCmd (argsDb args) (argsCmd args)

nounGroups :: [String]
nounGroups = ["task", "ctx", "dispatch", "link", "category"]

{- | Rewrite `ls` to `list` when it directly follows a noun group name.
Lets `task ls`, `ctx ls`, etc. work without registering `ls` in each
subparser (which would cause it to show up in `--help`).
-}
lsToList :: [String] -> [String]
lsToList (n : "ls" : rest) | n `elem` nounGroups = n : "list" : rest
lsToList (a : rest) = a : lsToList rest
lsToList [] = []

{- | If argv ends with a noun group (no subcommand given), append `--help`
so the user gets the full help with available commands instead of just
a "Missing: COMMAND" usage line.

Guards against false-positive: if the preceding token starts with `--`,
the noun is an option value (e.g. `search query --kind task`), not a
bare subcommand.
-}
helpForBareNoun :: [String] -> [String]
helpForBareNoun [] = ["--help"]
helpForBareNoun args
    | last args `elem` nounGroups
    , not (lastPrecededByFlag args) =
        args ++ ["--help"]
    | otherwise = args
  where
    lastPrecededByFlag xs = case reverse xs of
        (_ : prev : _) -> "--" `isPrefixOf` prev
        _ -> False
    isPrefixOf pfx s = take (length pfx) s == pfx

runCmd :: FilePath -> Command -> IO ()
runCmd db (CmdInit o) = Init.run db o
runCmd db (CmdDoctor o) = Doctor.run db o
runCmd db (CmdTask c) = Task.run db c
runCmd db (CmdCtx c) = Ctx.run db c
runCmd db (CmdLink c) = Link.run db c
runCmd db (CmdCategory c) = Category.run db c
runCmd db (CmdDispatch c) = Dispatch.run db c
runCmd db (CmdSearch o) = Search.run db o
runCmd db (CmdReindex c) = Reindex.run db c
runCmd _ (CmdAgents c) = Agents.run c

versionP :: Parser (a -> a)
versionP =
    infoOption
        ("icarium " <> showVersion version)
        (long "version" <> short 'V' <> help "Print version and exit")

parser :: FilePath -> ParserInfo Args
parser defaultDb =
    info
        (argsP defaultDb <**> helper <**> versionP)
        ( fullDesc
            <> progDesc "Task/context/dispatch tool for headless-agent workflows. IDs are ULIDs; any unique prefix is accepted; lists show 10-char prefixes, show/create output prints the full id. New agents: run `icarium agents` for a quickstart."
            <> header "icarium"
        )

argsP :: FilePath -> Parser Args
argsP defaultDb =
    Args
        <$> strOption
            ( long "db"
                <> metavar "PATH"
                <> value defaultDb
                <> showDefault
                <> help "SQLite database file"
            )
        <*> cmdP

cmdP :: Parser Command
cmdP =
    subparser
        ( command
            "init"
            ( info
                (CmdInit <$> Init.parser <**> helper)
                (progDesc "Initialize a project: create DB, apply schema, write config")
            )
            <> command
                "doctor"
                ( info
                    (CmdDoctor <$> Doctor.parser <**> helper)
                    (progDesc "Verify config, DB file, schema version, claude and git binaries, orphaned dispatches, and body files.")
                )
            <> command
                "task"
                ( info
                    (CmdTask <$> Task.parser <**> helper)
                    (progDesc "Manage tasks. Example: icarium task list --state ready")
                )
            <> command
                "ctx"
                ( info
                    (CmdCtx <$> Ctx.parser <**> helper)
                    (progDesc "Manage context entries. Example: icarium ctx list --discipline planning")
                )
            <> command
                "link"
                ( info
                    (CmdLink <$> Link.parser <**> helper)
                    (progDesc "Manage typed edges between nodes (add, list, rm). Example: icarium link add TASK_A depends-on TASK_B")
                )
            <> command
                "category"
                ( info
                    (CmdCategory <$> Category.parser <**> helper)
                    (progDesc "Manage category vocabulary (add, list, sync). Example: icarium category add --axis domain infra")
                )
            <> command
                "dispatch"
                ( info
                    (CmdDispatch <$> Dispatch.parser <**> helper)
                    (progDesc "Manage dispatches (run, list, show, logs, recover). Example: icarium dispatch run --max 5")
                )
            <> command
                "search"
                ( info
                    (CmdSearch <$> Search.parser <**> helper)
                    (progDesc "Search tasks and context by title and body. Example: icarium search fts5")
                )
            <> command
                "reindex"
                ( info
                    (CmdReindex <$> Reindex.parser <**> helper)
                    (progDesc "Rebuild the FTS5 body index from current tasks and context")
                )
            <> command
                "agents"
                ( info
                    (CmdAgents <$> Agents.parser <**> helper)
                    (progDesc "Print an agent quickstart: storage model, body read/write flow, common commands")
                )
        )
