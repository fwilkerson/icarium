module Icarium.Types (
    -- * Enums
    TaskState (..),
    readyStates,
    allTaskStates,
    taskStateText,
    taskStateCli,
    parseTaskState,
    parseTaskStateDb,
    Effort (..),
    allEfforts,
    effortText,
    parseEffort,
    EdgeKind (..),
    edgeKindDbText,
    edgeKindDisplay,
    parseEdgeKind,
    parseEdgeKindDb,
    NodeKind (..),
    nodeKindText,
    CategoryAxis (..),
    categoryAxisText,
    parseCategoryAxis,
    retrievalAxes,
    isRetrievalAxis,
    hasRetrievalAxis,
    DispatchOutcome (..),
    dispatchOutcomeText,
    parseDispatchOutcome,
    ReviewVerdict (..),
    reviewVerdictText,
    Disposition (..),
    dispositionText,
    parseDisposition,
    dispositionRetires,

    -- * Records
    Routing (..),
    Task (..),
    Context (..),
    Edge (..),
    Category (..),
    Dispatch (..),
    CurationEvent (..),

    -- * DB column layout
    -- $columns
    taskCols,
    contextCols,
    edgeCols,
    dispatchCols,
    curationCols,
) where

import Control.Applicative ((<|>))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable (Typeable)
import Database.SQLite.Simple (FromRow (..), ToRow (..), field)
import Database.SQLite.Simple.FromField (
    Field,
    FromField (..),
    ResultError (..),
    returnError,
 )
import Database.SQLite.Simple.Ok (Ok)
import Database.SQLite.Simple.ToField (ToField (..))

{- $columns
Each list names its table's columns in @FromRow@ order, and lives next to
the instance it must match — the two halves used to sit in different
modules, where no reader could check the pairing. Render with
'Icarium.Repo.Internal.qualified'; every @SELECT@ that builds the record
must use it, since a hand-written copy drifts and fails as a runtime
conversion error rather than a type error.

Table order is /not/ the invariant: 'taskCols' deliberately differs from
@spec\/schema.sql@, and nothing feeds @SELECT *@ into a @FromRow@. What
must hold is that the names match the table as a set and the order matches
the @field@ chain — both asserted in @SchemaSpec@.
-}

-- =============================================================
-- Enums
-- =============================================================

{- | 'ReadyHeadless' is work a dispatched agent may take unattended;
'ReadyInteractive' is the same specification bar for work a human at a
keyboard must do. Neither name is bare: an unqualified @ready@ once meant
the headless queue on this axis and the interactive one on the selector
axis. Semantics of every state: @docs/adr/0007-task-state-semantics.md@.
-}
data TaskState
    = Idea
    | Planned
    | ReadyHeadless
    | ReadyInteractive
    | InProgress
    | Done
    | Blocked
    | Abandoned
    deriving (Show, Eq)

{- | The states from which a task may be claimed: specified, unstarted work.
The deps gate is separate — see the @ready_tasks@ view.
-}
readyStates :: [TaskState]
readyStates = [ReadyHeadless, ReadyInteractive]

-- | Every state, in lifecycle order. Drives the CLI's invalid-value message.
allTaskStates :: [TaskState]
allTaskStates =
    [Idea, Planned, ReadyHeadless, ReadyInteractive, InProgress, Done, Blocked, Abandoned]

taskStateText :: TaskState -> Text
taskStateText = \case
    Idea -> "idea"
    Planned -> "planned"
    ReadyHeadless -> "ready_headless"
    ReadyInteractive -> "ready_interactive"
    InProgress -> "in_progress"
    Done -> "done"
    Blocked -> "blocked"
    Abandoned -> "abandoned"

{- | How the CLI spells a state, in flags, badges and errors: the stored
name with underscores hyphenated. Storage and CLI share one vocabulary, so
this is a transliteration and never a separate name.
-}
taskStateCli :: TaskState -> Text
taskStateCli = T.replace "_" "-" . taskStateText

{- | CLI-facing parser. Canonical spelling is hyphenated (@in-progress@,
@ready-headless@), but the underscore form is accepted too: @task show@
prints the stored @in_progress@, and pasting that back into @--state@ must
not error.

Bare @ready@ is deliberately absent rather than aliased: an alias would
reinstate the ambiguity one layer down, silently resolved.
-}
parseTaskState :: Text -> Maybe TaskState
parseTaskState = \case
    "in-progress" -> Just InProgress
    "ready-headless" -> Just ReadyHeadless
    "ready-interactive" -> Just ReadyInteractive
    other -> parseTaskStateDb other

-- | Storage parser: accepts underscore form stored in the DB.
parseTaskStateDb :: Text -> Maybe TaskState
parseTaskStateDb = \case
    "idea" -> Just Idea
    "planned" -> Just Planned
    "ready_headless" -> Just ReadyHeadless
    "ready_interactive" -> Just ReadyInteractive
    "in_progress" -> Just InProgress
    "done" -> Just Done
    "blocked" -> Just Blocked
    "abandoned" -> Just Abandoned
    _ -> Nothing

data Effort = Low | Medium | High | XHigh | Max deriving (Show, Eq)

-- | Every level, cheapest first. Drives the CLI's choice lists.
allEfforts :: [Effort]
allEfforts = [Low, Medium, High, XHigh, Max]

effortText :: Effort -> Text
effortText = \case
    Low -> "low"
    Medium -> "medium"
    High -> "high"
    XHigh -> "xhigh"
    Max -> "max"

parseEffort :: Text -> Maybe Effort
parseEffort = \case
    "low" -> Just Low
    "medium" -> Just Medium
    "high" -> Just High
    "xhigh" -> Just XHigh
    "max" -> Just Max
    _ -> Nothing

data EdgeKind = DependsOn | References | DerivedFrom | Supersedes
    deriving (Show, Eq)

edgeKindDbText :: EdgeKind -> Text
edgeKindDbText = \case
    DependsOn -> "depends_on"
    References -> "references"
    DerivedFrom -> "derived_from"
    Supersedes -> "supersedes"

edgeKindDisplay :: EdgeKind -> Text
edgeKindDisplay = \case
    DependsOn -> "depends-on"
    References -> "references"
    DerivedFrom -> "derived-from"
    Supersedes -> "supersedes"

-- | CLI-facing parser: accepts hyphen forms only.
parseEdgeKind :: Text -> Maybe EdgeKind
parseEdgeKind = \case
    "depends-on" -> Just DependsOn
    "references" -> Just References
    "derived-from" -> Just DerivedFrom
    "supersedes" -> Just Supersedes
    _ -> Nothing

-- | Storage parser: accepts underscore forms stored in the DB/JSON.
parseEdgeKindDb :: Text -> Maybe EdgeKind
parseEdgeKindDb = \case
    "depends_on" -> Just DependsOn
    "references" -> Just References
    "derived_from" -> Just DerivedFrom
    "supersedes" -> Just Supersedes
    _ -> Nothing

data NodeKind = TaskNode | ContextNode deriving (Show, Eq)

nodeKindText :: NodeKind -> Text
nodeKindText TaskNode = "task"
nodeKindText ContextNode = "context"

parseNodeKind :: Text -> Maybe NodeKind
parseNodeKind = \case
    "task" -> Just TaskNode
    "context" -> Just ContextNode
    _ -> Nothing

{- | Category axes, split by job:

* /Retrieval/ — 'Domain' (where it is relevant) and 'Discipline' (who it is
  relevant for). Carried by both tasks and context, and they exist to match
  the two.
* /Workflow/ — 'Kind' (what shape the work is: bug, chore, refactor…).
  Task-only. It describes the work, not when a learning is relevant, so it
  has no retrieval job.

See 'retrievalAxes' for why the split is load-bearing.
-}
data CategoryAxis = Domain | Discipline | Kind deriving (Show, Eq)

categoryAxisText :: CategoryAxis -> Text
categoryAxisText Domain = "domain"
categoryAxisText Discipline = "discipline"
categoryAxisText Kind = "kind"

parseCategoryAxis :: Text -> Maybe CategoryAxis
parseCategoryAxis = \case
    "domain" -> Just Domain
    "discipline" -> Just Discipline
    "kind" -> Just Kind
    _ -> Nothing

{- | The axes that participate in context auto-pull, in match order.

Every axis listed here /narrows/ the pull: @categoryMatchedContexts@ builds
one clause per axis present on the task and ANDs them. A workflow axis must
therefore stay out — a task tagged @kind=bug@ would otherwise only match
context also tagged @kind=bug@, and no context written before the axis
existed has one, so auto-pull would silently go quiet.
-}
retrievalAxes :: [CategoryAxis]
retrievalAxes = [Domain, Discipline]

-- | Is this axis accepted on context entries? Workflow axes are task-only.
isRetrievalAxis :: CategoryAxis -> Bool
isRetrievalAxis = (`elem` retrievalAxes)

{- | Will this category list pull any context at all? False means
@categoryMatchedContexts@ short-circuits to @[]@ — note that a task tagged
only on a workflow axis (@kind@) is just as context-free as an untagged one.
-}
hasRetrievalAxis :: [Category] -> Bool
hasRetrievalAxis = any (isRetrievalAxis . categoryAxis)

-- =============================================================
-- FromField / ToField for enums
-- =============================================================

enumFromField :: (Typeable a) => String -> (Text -> Maybe a) -> Field -> Ok a
enumFromField label p f = do
    t <- fromField f
    case p t of
        Just x -> pure x
        Nothing -> returnError ConversionFailed f ("invalid " <> label <> ": " <> T.unpack t)

instance FromField TaskState where fromField = enumFromField "task state" parseTaskStateDb
instance FromField Effort where fromField = enumFromField "effort" parseEffort
instance FromField EdgeKind where fromField = enumFromField "edge kind" parseEdgeKindDb
instance FromField NodeKind where fromField = enumFromField "node kind" parseNodeKind
instance FromField CategoryAxis where fromField = enumFromField "category axis" parseCategoryAxis

instance ToField TaskState where toField = toField . taskStateText
instance ToField Effort where toField = toField . effortText
instance ToField EdgeKind where toField = toField . edgeKindDbText
instance ToField NodeKind where toField = toField . nodeKindText
instance ToField CategoryAxis where toField = toField . categoryAxisText

-- =============================================================
-- Records (FromRow column order matches SELECT ordering used in Repo)
-- =============================================================

{- | Which agent a dispatch runs as. 'Nothing' in a field = inherit the
next source down (task, then the @[dispatch]@ config defaults), so the
'Semigroup' is per-field left-biased choice and folding
@flag \<\> task@ is the whole precedence rule.

One record so a new routing knob is a field here, an entry in 'ToRow' /
'FromRow', and a flag in the parser — not an edit at every call site.
-}
data Routing = Routing
    { rtModel :: Maybe Text
    , rtEffort :: Maybe Effort
    }
    deriving (Show, Eq)

instance Semigroup Routing where
    a <> b =
        Routing
            { rtModel = rtModel a <|> rtModel b
            , rtEffort = rtEffort a <|> rtEffort b
            }

instance Monoid Routing where
    mempty = Routing{rtModel = Nothing, rtEffort = Nothing}

-- Column order matches the tail of 'taskCols'.
instance FromRow Routing where
    fromRow = Routing <$> field <*> field

instance ToRow Routing where
    toRow Routing{..} = [toField rtModel, toField rtEffort]

data Task = Task
    { taskId :: Text
    , taskTitle :: Text
    , taskBody :: Text
    , taskState :: TaskState
    , taskPriority :: Maybe Int
    , taskBlockReason :: Maybe Text
    , taskCreatedAt :: Text
    , taskUpdatedAt :: Text
    , taskNoCommit :: Bool
    , taskClaimedBy :: Maybe Text
    , taskClaimedAt :: Maybe Text
    , taskRouting :: Routing
    }
    deriving (Show)

instance FromRow Task where
    fromRow =
        Task
            <$> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> fromRow

-- | @tasks@ columns; the @model@/@effort@ tail is the nested 'Routing'.
taskCols :: [Text]
taskCols =
    [ "id"
    , "title"
    , "body"
    , "state"
    , "priority"
    , "block_reason"
    , "created_at"
    , "updated_at"
    , "no_commit"
    , "claimed_by"
    , "claimed_at"
    , "model"
    , "effort"
    ]

data Context = Context
    { contextId :: Text
    , contextTitle :: Text
    , contextBody :: Text
    , contextCreatedAt :: Text
    , contextUpdatedAt :: Text
    }
    deriving (Show, Eq)

instance FromRow Context where
    fromRow =
        Context
            <$> field
            <*> field
            <*> field
            <*> field
            <*> field

-- | @context@ columns.
contextCols :: [Text]
contextCols = ["id", "title", "body", "created_at", "updated_at"]

data Edge = Edge
    { edgeId :: Text
    , edgeKind :: EdgeKind
    , edgeSrcKind :: NodeKind
    , edgeSrcId :: Text
    , edgeDstKind :: NodeKind
    , edgeDstId :: Text
    , edgeCreatedAt :: Text
    }
    deriving (Show)

instance FromRow Edge where
    fromRow =
        Edge
            <$> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field

-- | @edges@ columns.
edgeCols :: [Text]
edgeCols = ["id", "kind", "src_kind", "src_id", "dst_kind", "dst_id", "created_at"]

data Category = Category
    { categoryId :: Text
    , categoryAxis :: CategoryAxis
    , categoryName :: Text
    }
    deriving (Show)

instance FromRow Category where
    fromRow = Category <$> field <*> field <*> field

data DispatchOutcome = OSuccess | OFailure | OInterrupted deriving (Show, Eq)

dispatchOutcomeText :: DispatchOutcome -> Text
dispatchOutcomeText = \case
    OSuccess -> "success"
    OFailure -> "failure"
    OInterrupted -> "interrupted"

parseDispatchOutcome :: Text -> Maybe DispatchOutcome
parseDispatchOutcome = \case
    "success" -> Just OSuccess
    "failure" -> Just OFailure
    "interrupted" -> Just OInterrupted
    _ -> Nothing

instance FromField DispatchOutcome where
    fromField = enumFromField "dispatch outcome" parseDispatchOutcome
instance ToField DispatchOutcome where
    toField = toField . dispatchOutcomeText

data ReviewVerdict = RVPass | RVWarn | RVFail deriving (Show, Eq)

reviewVerdictText :: ReviewVerdict -> Text
reviewVerdictText = \case
    RVPass -> "pass"
    RVWarn -> "warn"
    RVFail -> "fail"

parseReviewVerdict :: Text -> Maybe ReviewVerdict
parseReviewVerdict = \case
    "pass" -> Just RVPass
    "warn" -> Just RVWarn
    "fail" -> Just RVFail
    _ -> Nothing

instance FromField ReviewVerdict where
    fromField = enumFromField "review verdict" parseReviewVerdict
instance ToField ReviewVerdict where
    toField = toField . reviewVerdictText

-- | Outcome of curating a context entry (ADR 0001).
data Disposition = Guidance | Rule | Refactor | Keep | Stale
    deriving (Show, Eq)

dispositionText :: Disposition -> Text
dispositionText = \case
    Guidance -> "guidance"
    Rule -> "rule"
    Refactor -> "refactor"
    Keep -> "keep"
    Stale -> "stale"

parseDisposition :: Text -> Maybe Disposition
parseDisposition = \case
    "guidance" -> Just Guidance
    "rule" -> Just Rule
    "refactor" -> Just Refactor
    "keep" -> Just Keep
    "stale" -> Just Stale
    _ -> Nothing

{- | ADR 0001: an entry whose latest disposition is anything but 'keep'
is retired. Mirrors the SQL @retired_context@ view.
-}
dispositionRetires :: Disposition -> Bool
dispositionRetires = (/= Keep)

instance FromField Disposition where
    fromField = enumFromField "disposition" parseDisposition
instance ToField Disposition where
    toField = toField . dispositionText

-- | A row in @context_curation@; append-only, latest event wins.
data CurationEvent = CurationEvent
    { curationId :: Text
    , curationContextId :: Text
    , curationDisposition :: Disposition
    , curationArtifact :: Maybe Text
    , curationNote :: Maybe Text
    , curationCreatedAt :: Text
    }
    deriving (Show, Eq)

instance FromRow CurationEvent where
    fromRow =
        CurationEvent
            <$> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field

{- | @context_curation@ columns. Also selected from the
@context_latest_curation@ view, which is @SELECT cc.*@ over the same
table — so the view may carry columns this list does not name.
-}
curationCols :: [Text]
curationCols = ["id", "context_id", "disposition", "artifact", "note", "created_at"]

-- | A row in the @dispatches@ table.
data Dispatch = Dispatch
    { dispatchId :: Text
    , dispatchTaskId :: Text
    , dispatchBranch :: Text
    , dispatchBaseBranch :: Text
    , dispatchBaseSha :: Text
    , dispatchPid :: Maybe Int
    , dispatchModel :: Text
    , dispatchEffort :: Effort
    , dispatchStartedAt :: Text
    , dispatchHeartbeat :: Text
    , dispatchEndedAt :: Maybe Text
    , dispatchOutcome :: Maybe DispatchOutcome
    , dispatchMergeSha :: Maybe Text
    , dispatchLastCommit :: Maybe Text
    , dispatchNotes :: Maybe Text
    , dispatchLogPath :: Maybe Text
    , dispatchTokensIn :: Maybe Int
    , dispatchTokensOut :: Maybe Int
    , dispatchTokensCacheRead :: Maybe Int
    , dispatchReviewVerdict :: Maybe ReviewVerdict
    , dispatchReviewerLogPath :: Maybe Text
    , dispatchMergedAt :: Maybe Text
    , dispatchBodyChanged :: Maybe Bool
    }
    deriving (Show)

instance FromRow Dispatch where
    fromRow =
        Dispatch
            <$> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field

-- | @dispatches@ columns.
dispatchCols :: [Text]
dispatchCols =
    [ "id"
    , "task_id"
    , "branch"
    , "base_branch"
    , "base_sha"
    , "pid"
    , "model"
    , "effort"
    , "started_at"
    , "heartbeat_at"
    , "ended_at"
    , "outcome"
    , "merge_sha"
    , "last_commit"
    , "notes"
    , "log_path"
    , "tokens_in"
    , "tokens_out"
    , "tokens_cache_read"
    , "review_verdict"
    , "reviewer_log_path"
    , "merged_at"
    , "body_changed"
    ]
