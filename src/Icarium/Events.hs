{- | The append-only event log: one JSON object per line, under
@.icarium/events.jsonl@.

Polling @doctor@/@list@ does not scale past one watcher, so every
state-changing write also appends a line here. Consumers tail the file.

Retention is deliberately unbounded in v1 — nothing rotates or prunes this
file, unlike @dispatch.log_retention_runs@. A retention knob without a
consumer would be guessing at what a watcher may still need to replay; add
one when something actually reads the log.
-}
module Icarium.Events (
    Event (..),
    Actor,
    nodeDeleted,
    eventLogPath,
    renderEvent,
    emit,
) where

import Control.Exception (bracket_)
import Data.Aeson (object, (.=))
import Data.Aeson qualified as A
import Data.Aeson.Types (Pair)
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import GHC.IO.Handle.Lock (LockMode (..), hLock, hUnlock)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import System.IO (BufferMode (..), IOMode (..), hFlush, hSetBinaryMode, hSetBuffering, withFile)

import Icarium.Types (
    DispatchOutcome,
    Disposition,
    NodeKind (..),
    TaskState (..),
    dispatchOutcomeText,
    dispositionText,
    taskStateText,
 )

{- | What caused the event: the CLI command (@\"task update\"@) or the
subsystem (@\"dispatch\"@). Kept separate from the event itself because the
same transition can be driven from either.
-}
type Actor = Text

-- | One recorded happening. Closed by design: a consumer can enumerate it.
data Event
    = -- | id, initial state
      TaskCreated Text TaskState
    | -- | id, old state, new state
      TaskUpdated Text TaskState TaskState
    | -- | id, state claimed from, owner
      TaskClaimed Text TaskState Text
    | -- | id
      TaskDeleted Text
    | -- | id
      CtxCreated Text
    | -- | id
      CtxUpdated Text
    | -- | id, disposition, artifact
      CtxCurated Text Disposition (Maybe Text)
    | -- | id
      CtxDeleted Text
    | -- | dispatch id, task id, branch
      DispatchStarted Text Text Text
    | -- | dispatch id, task id, outcome
      DispatchFinished Text Text DispatchOutcome
    | -- | dispatch id, task id, the worker's reason
      DispatchEscalated Text Text Text
    deriving (Show, Eq)

-- | The deleted-event for a node kind: the shared @rm@ runner names the kind.
nodeDeleted :: NodeKind -> Text -> Event
nodeDeleted TaskNode = TaskDeleted
nodeDeleted ContextNode = CtxDeleted

-- | The log that sits beside the given database file.
eventLogPath :: FilePath -> FilePath
eventLogPath db = takeDirectory db </> "events.jsonl"

-- | One log line, without its terminating newline.
renderEvent :: UTCTime -> Actor -> Event -> BL.ByteString
renderEvent ts actor ev =
    A.encode (object (common <> specific ev))
  where
    common =
        [ "ts" .= iso8601Show ts
        , "actor" .= actor
        ]

specific :: Event -> [Pair]
specific = \case
    TaskCreated tid st ->
        header "task.created" "task" tid <> ["to" .= taskStateText st]
    TaskUpdated tid old new ->
        header "task.updated" "task" tid
            <> ["from" .= taskStateText old, "to" .= taskStateText new]
    TaskClaimed tid old owner ->
        header "task.claimed" "task" tid
            <> [ "from" .= taskStateText old
               , "to" .= taskStateText InProgress
               , "owner" .= owner
               ]
    TaskDeleted tid -> header "task.deleted" "task" tid
    CtxCreated cxid -> header "ctx.created" "ctx" cxid
    CtxUpdated cxid -> header "ctx.updated" "ctx" cxid
    CtxCurated cxid disp artifact ->
        header "ctx.curated" "ctx" cxid
            <> ["to" .= dispositionText disp]
            <> maybe [] (\a -> ["artifact" .= a]) artifact
    CtxDeleted cxid -> header "ctx.deleted" "ctx" cxid
    DispatchStarted did tid branch ->
        header "dispatch.started" "dispatch" did <> ["task" .= tid, "branch" .= branch]
    DispatchFinished did tid outcome ->
        header "dispatch.finished" "dispatch" did
            <> ["task" .= tid, "to" .= dispatchOutcomeText outcome]
    DispatchEscalated did tid reason ->
        header "dispatch.escalated" "dispatch" did <> ["task" .= tid, "reason" .= reason]

header :: Text -> Text -> Text -> [Pair]
header name kind eid = ["event" .= name, "kind" .= kind, "id" .= eid]

{- | Append one event to the log beside @db@. Takes the database path
because that is what every call site already threads.

Concurrent writers never interleave: the line is written under an exclusive
lock on a handle opened in append mode, so a second process blocks until the
first has flushed a whole line.
-}
emit :: FilePath -> Actor -> Event -> IO ()
emit db actor ev = do
    ts <- getCurrentTime
    let path = eventLogPath db
    createDirectoryIfMissing True (takeDirectory path)
    withFile path AppendMode $ \h -> do
        hSetBinaryMode h True
        hSetBuffering h (BlockBuffering Nothing)
        bracket_ (hLock h ExclusiveLock) (hUnlock h) $ do
            BL.hPut h (renderEvent ts actor ev <> "\n")
            hFlush h
