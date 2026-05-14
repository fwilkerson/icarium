module Icarium.Commands.Reindex (Command, parser, run) where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Options.Applicative

import Icarium.Db (withDb)
import Icarium.Repo.Fts qualified as Fts

data Command = Reindex

parser :: Parser Command
parser = pure Reindex

run :: FilePath -> Command -> IO ()
run db Reindex = withDb db $ \c -> do
    (taskCount, ctxCount) <- Fts.reindexAll c
    TIO.putStrLn $
        "reindexed "
            <> T.pack (show taskCount)
            <> " tasks, "
            <> T.pack (show ctxCount)
            <> " context entries"
