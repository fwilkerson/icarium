module Icarium.Commands.Next (Options, parser, run) where

import qualified Data.Text.IO as TIO
import Options.Applicative
import System.Exit (exitWith, ExitCode(..))

import Icarium.Db (defaultDbPath, withDb)
import qualified Icarium.Repo.Task as RT
import Icarium.Types (taskId)

data Options = Options

parser :: Parser Options
parser = pure Options

-- | Print the id of the next dispatchable task, or exit 1 if the
-- ready queue is empty. Priority DESC, then created_at ASC — same
-- order the @ready_tasks@ view produces.
run :: Options -> IO ()
run _ = withDb defaultDbPath $ \c -> do
    ts <- RT.listTasks c [] True
    case ts of
        []      -> exitWith (ExitFailure 1)
        (t : _) -> TIO.putStrLn (taskId t)
