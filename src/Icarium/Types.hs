module Icarium.Types (
    -- * Enums
    TaskState (..),
    taskStateText,
    parseTaskState,
    parseTaskStateDb,
    Effort (..),
    effortText,
    parseEffort,
    EdgeKind (..),
    edgeKindDbText,
    edgeKindDisplay,
    parseEdgeKind,
    parseEdgeKindDb,
    NodeKind (..),
    nodeKindText,
    parseNodeKind,
    CategoryAxis (..),
    categoryAxisText,
    parseCategoryAxis,
    DispatchOutcome (..),
    dispatchOutcomeText,
    parseDispatchOutcome,
    ReviewVerdict (..),
    reviewVerdictText,
    parseReviewVerdict,

    -- * Records
    Task (..),
    Context (..),
    Edge (..),
    Category (..),
    Dispatch (..),
) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable (Typeable)
import Database.SQLite.Simple (FromRow (..), field)
import Database.SQLite.Simple.FromField (
    Field,
    FromField (..),
    ResultError (..),
    returnError,
 )
import Database.SQLite.Simple.Ok (Ok)
import Database.SQLite.Simple.ToField (ToField (..))

-- =============================================================
-- Enums
-- =============================================================

data TaskState = Idea | Planned | Ready | InProgress | Done | Blocked | Abandoned
    deriving (Show, Eq)

taskStateText :: TaskState -> Text
taskStateText = \case
    Idea -> "idea"
    Planned -> "planned"
    Ready -> "ready"
    InProgress -> "in_progress"
    Done -> "done"
    Blocked -> "blocked"
    Abandoned -> "abandoned"

-- | CLI-facing parser: accepts hyphen form for InProgress.
parseTaskState :: Text -> Maybe TaskState
parseTaskState = \case
    "idea" -> Just Idea
    "planned" -> Just Planned
    "ready" -> Just Ready
    "in-progress" -> Just InProgress
    "done" -> Just Done
    "blocked" -> Just Blocked
    "abandoned" -> Just Abandoned
    _ -> Nothing

-- | Storage parser: accepts underscore form stored in the DB.
parseTaskStateDb :: Text -> Maybe TaskState
parseTaskStateDb = \case
    "idea" -> Just Idea
    "planned" -> Just Planned
    "ready" -> Just Ready
    "in_progress" -> Just InProgress
    "done" -> Just Done
    "blocked" -> Just Blocked
    "abandoned" -> Just Abandoned
    _ -> Nothing

data Effort = Low | Medium | High | XHigh | Max deriving (Show, Eq)

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

data CategoryAxis = Domain | Discipline deriving (Show, Eq)

categoryAxisText :: CategoryAxis -> Text
categoryAxisText Domain = "domain"
categoryAxisText Discipline = "discipline"

parseCategoryAxis :: Text -> Maybe CategoryAxis
parseCategoryAxis = \case
    "domain" -> Just Domain
    "discipline" -> Just Discipline
    _ -> Nothing

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

data Context = Context
    { contextId :: Text
    , contextTitle :: Text
    , contextBody :: Text
    , contextStale :: Bool
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
            <*> field

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

{- | A row in the @dispatches@ table. Columns kept in the order the
schema declares them so @FromRow@ lines up with @SELECT *@.
-}
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
