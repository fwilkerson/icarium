module Main (main) where

import Options.Applicative
import qualified Icarium.Commands.Init as Init
import qualified Icarium.Commands.Doctor as Doctor

data Command
    = CmdInit Init.Options
    | CmdDoctor Doctor.Options

main :: IO ()
main = run =<< execParser parser

run :: Command -> IO ()
run (CmdInit o)   = Init.run o
run (CmdDoctor o) = Doctor.run o

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
              (progDesc "Check project health (config, DB, external tools)"))
    )
