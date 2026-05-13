module Icarium.Commands.Reindex (Command, parser, run) where

import Data.Text.IO qualified as TIO
import Options.Applicative

import Icarium.Db (withDb)
import Icarium.Repo.Fts qualified as Fts

data Command = Reindex

parser :: Parser Command
parser = pure Reindex

run :: FilePath -> Command -> IO ()
run db Reindex = withDb db $ \c -> do
    Fts.reindexAll c
    TIO.putStrLn "reindexed"
