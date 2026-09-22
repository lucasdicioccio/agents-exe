{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Core types for the SystemToolbox module.

This module defines the fundamental types used by the system toolbox,
including trace events, query results, errors, and configuration.
-}
module System.Agents.Tools.SystemToolbox.Types (
    -- * Trace events
    Trace (..),

    -- * Configuration
    SessionIntrospectionConfig (..),
    defaultSessionIntrospectionConfig,
    ReadSessionParams (..),
    defaultReadSessionParams,

    -- * Tool descriptions
    ToolDescription (..),

    -- * Toolbox state
    Toolbox (..),

    -- * Query results
    QueryResult (..),
    AttachFileResult (..),

    -- * Async tool-call status
    GetToolCallStatusParams (..),
    ToolCallStatusResult (..),
    ListRunningToolCallsResult (..),
    RunningToolCallInfo (..),
    CancelToolCallParams (..),
    CancelToolCallResult (..),
    WaitParams (..),
    WaitResult (..),
    maxWaitSeconds,
    SendMessageParams (..),
    SendMessageResult (..),
    SpawnSessionParams (..),
    SpawnSessionResult (..),

    -- * Query errors
    QueryError (..),

    -- * Utilities
    formatExecutionTime,
) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, (.=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types ((.!=))
import Data.Text (Text)
import Data.Time (NominalDiffTime, UTCTime)
import GHC.Generics (Generic)

import System.Agents.Base (
    SessionIntrospectionScope (..),
    SystemToolCapability (..),
    SystemToolboxDescription (..),
 )
import System.Agents.FileSandbox (FileSandbox)
import System.Agents.Media.Types (MediaType (..))
import System.Agents.OS.Conversation.Types (ToolCallProgress)
import System.Agents.Session.Types (SessionId (..))
import qualified System.Agents.SessionStore as SessionStore


-------------------------------------------------------------------------------
-- Trace Events
-------------------------------------------------------------------------------

{- | Trace events for monitoring system toolbox operations.

These events allow tracking of:
* System info requests
* File attachment requests
* Query execution progress
* System info retrieval timing
* Capability errors
* Session introspection operations
-}
data Trace
    = -- | System information was requested for a capability
      SystemInfoRequestedTrace !Text
    | -- | File attachment was requested
      FileAttachRequestedTrace !FilePath
    | -- | File was successfully attached
      FileAttachSuccessTrace !FilePath !MediaType !Int
    | -- | File attachment failed
      FileAttachErrorTrace !FilePath !Text
    | -- | System information was retrieved successfully
      SystemInfoRetrievedTrace !Text !NominalDiffTime
    | -- | Error occurred while retrieving system information
      SystemInfoErrorTrace !Text !Text
    | -- | Session introspection was requested
      SessionIntrospectionRequestedTrace !Text
    | -- | Session introspection succeeded
      SessionIntrospectionSuccessTrace !Text !Int
    | -- | Session introspection failed
      SessionIntrospectionErrorTrace !Text !Text
    deriving (Show)

-------------------------------------------------------------------------------
-- Configuration
-------------------------------------------------------------------------------

{- | Configuration for session introspection capabilities.

This is passed to the toolbox to configure how session introspection
operations should behave (scope, limits, etc.).
-}
data SessionIntrospectionConfig = SessionIntrospectionConfig
    { introspectionCatalog :: SessionStore.SessionCatalog
    -- ^ Sessions the capabilities can list and read
    , introspectionCurrentSessionId :: Maybe SessionId
    -- ^ Current session ID for scope filtering
    , introspectionCurrentForkedFrom :: Maybe SessionId
    -- ^ Current session's forkedFromSessionId for lineage tracking
    , introspectionScope :: SessionIntrospectionScope
    -- ^ Scope of accessible sessions
    , introspectionMaxResults :: Int
    -- ^ Max sessions to return in list operations
    , introspectionIncludeToolOutputs :: Bool
    -- ^ Whether to include tool outputs in read operations
    }
    deriving (Show)

-- | Default session introspection configuration.
defaultSessionIntrospectionConfig :: SessionStore.SessionCatalog -> SessionIntrospectionConfig
defaultSessionIntrospectionConfig catalog =
    SessionIntrospectionConfig
        { introspectionCatalog = catalog
        , introspectionCurrentSessionId = Nothing
        , introspectionCurrentForkedFrom = Nothing
        , introspectionScope = ScopeSubtree
        , introspectionMaxResults = 50
        , introspectionIncludeToolOutputs = True
        }

{- | Parameters for the read-session capability.

Controls slicing and content filtering when reading session data.
-}
data ReadSessionParams = ReadSessionParams
    { rspTakeN :: Maybe Int
    -- ^ Take last N turns (alternative to offset/limit)
    , rspDropN :: Maybe Int
    -- ^ Drop first N turns (alternative to offset)
    , rspOffset :: Maybe Int
    -- ^ Starting turn index (0-based, alternative to drop_n)
    , rspLimit :: Maybe Int
    -- ^ Max turns to return (alternative to take_n)
    , rspIncludeThinking :: Bool
    -- ^ Include LLM thinking/reasoning content (default: false)
    , rspIncludeToolResponses :: Bool
    -- ^ Include tool call responses (default: false)
    }
    deriving (Show, Eq)

-- | Default read session parameters.
defaultReadSessionParams :: ReadSessionParams
defaultReadSessionParams =
    ReadSessionParams
        { rspTakeN = Nothing
        , rspDropN = Nothing
        , rspOffset = Nothing
        , rspLimit = Nothing
        , rspIncludeThinking = False
        , rspIncludeToolResponses = False
        }

-------------------------------------------------------------------------------
-- Tool Description
-------------------------------------------------------------------------------

{- | Description of a system tool.

Contains metadata about a specific system information tool, including
its name, description, and associated toolbox information.
-}
data ToolDescription = ToolDescription
    { toolDescriptionName :: Text
    -- ^ Name of the tool (e.g., "get_date")
    , toolDescriptionDescription :: Text
    -- ^ Human-readable description of what the tool does
    , toolDescriptionToolboxName :: Text
    -- ^ Name of the toolbox this tool belongs to
    }
    deriving (Show)

-------------------------------------------------------------------------------
-- Toolbox State
-------------------------------------------------------------------------------

{- | Runtime state for a system toolbox.

The toolbox maintains:
* Toolbox name and description
* List of enabled capabilities
* Optional environment variable filter pattern
* The original configuration description used to create this toolbox
* Session introspection configuration (for session-related capabilities)
* Optional file sandbox for attach-file capability
-}
data Toolbox = Toolbox
    { toolboxName :: Text
    , toolboxDescription :: Text
    , toolboxCapabilities :: [SystemToolCapability]
    , toolboxEnvVarFilter :: Maybe Text
    , toolboxConfig :: SystemToolboxDescription
    -- ^ Original configuration description used to create this toolbox
    , toolboxSessionIntrospection :: Maybe SessionIntrospectionConfig
    -- ^ Configuration for session introspection capabilities
    , toolboxFileSandbox :: Maybe FileSandbox
    -- ^ Optional file sandbox for attach-file capability
    }

-------------------------------------------------------------------------------
-- Query Results
-------------------------------------------------------------------------------

{- | Result of a system information query.

Contains:
* Capability name
* Data as JSON value
* Execution time
-}
data QueryResult = QueryResult
    { resultCapability :: Text
    , resultData :: Aeson.Value
    , resultExecutionTime :: NominalDiffTime
    }
    deriving (Show)

-- | JSON serialization for QueryResult.
instance ToJSON QueryResult where
    toJSON result =
        Aeson.object
            [ "capability" .= resultCapability result
            , "data" .= resultData result
            , "executionTime" .= formatExecutionTime (resultExecutionTime result)
            ]

{- | Result of a file attachment operation.

Contains:
* File path
* MIME type
* Base64-encoded file content
* File size in bytes
-}
data AttachFileResult = AttachFileResult
    { attachFilePath :: FilePath
    , attachMimeType :: Text
    , attachBase64Data :: Text
    , attachFileSize :: Int
    }
    deriving (Show)

-- | JSON serialization for AttachFileResult.
instance ToJSON AttachFileResult where
    toJSON result =
        Aeson.object
            [ "path" .= attachFilePath result
            , "mimeType" .= attachMimeType result
            , "base64Data" .= attachBase64Data result
            , "size" .= attachFileSize result
            ]


-------------------------------------------------------------------------------
-- Async Tool-Call Status
-------------------------------------------------------------------------------

{- | Parameters for the @get-tool-call-status@ capability.

All fields except 'toolCallId' are optional and use the documented defaults
when omitted.
-}
data GetToolCallStatusParams = GetToolCallStatusParams
    { toolCallId :: Text
    -- ^ The tool-call id to query
    , includeProgress :: Bool
    -- ^ Whether to include progress entries in the result (default: True)
    , waitForCompletion :: Bool
    -- ^ Whether to block until the call reaches a final state (default: False)
    , timeoutSeconds :: Int
    -- ^ Maximum seconds to wait when 'waitForCompletion' is True (default: 5)
    }
    deriving (Show, Eq, Generic)

instance FromJSON GetToolCallStatusParams where
    parseJSON = Aeson.withObject "GetToolCallStatusParams" $ \v ->
        GetToolCallStatusParams
            <$> v .: "tool_call_id"
            <*> v .:? "include_progress" .!= True
            <*> v .:? "wait_for_completion" .!= False
            <*> v .:? "timeout_seconds" .!= 5

instance ToJSON GetToolCallStatusParams where
    toJSON p =
        Aeson.object
            [ "tool_call_id" .= toolCallId p
            , "include_progress" .= includeProgress p
            , "wait_for_completion" .= waitForCompletion p
            , "timeout_seconds" .= timeoutSeconds p
            ]

{- | Result of a @get-tool-call-status@ query.

Provides a stable view of the tool-call entity, including its current
status, timing, final result (if any), and progress history.
-}
data ToolCallStatusResult = ToolCallStatusResult
    { tcsrToolCallId :: Text
    -- ^ The queried tool-call id
    , tcsrStatus :: Text
    -- ^ Human-readable status: pending, running, completed, failed, cancelled, orphaned
    , tcsrToolName :: Maybe Text
    -- ^ Name of the tool being called, when known
    , tcsrStartedAt :: Maybe UTCTime
    -- ^ When the call started executing
    , tcsrCompletedAt :: Maybe UTCTime
    -- ^ When the call reached a final state
    , tcsrResult :: Maybe Value
    -- ^ Final JSON result, when completed successfully
    , tcsrProgress :: [ToolCallProgress]
    -- ^ Progress updates, newest first
    , tcsrIsFinal :: Bool
    -- ^ Whether the call has reached a terminal state
    }
    deriving (Show, Eq, Generic)

toolCallStatusResultOptions :: Aeson.Options
toolCallStatusResultOptions =
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 4
        }

instance ToJSON ToolCallStatusResult where
    toJSON = Aeson.genericToJSON toolCallStatusResultOptions

instance FromJSON ToolCallStatusResult where
    parseJSON = Aeson.genericParseJSON toolCallStatusResultOptions

{- | Result of a @list-running-tool-calls@ query.
-}
newtype ListRunningToolCallsResult = ListRunningToolCallsResult
    { ltcRunning :: [RunningToolCallInfo]
    }
    deriving (Show, Eq, Generic)

instance ToJSON ListRunningToolCallsResult where
    toJSON result =
        Aeson.object ["running" .= ltcRunning result]

instance FromJSON ListRunningToolCallsResult where
    parseJSON = Aeson.withObject "ListRunningToolCallsResult" $ \v ->
        ListRunningToolCallsResult <$> v .: "running"

{- | Description of a single running tool call.
-}
data RunningToolCallInfo = RunningToolCallInfo
    { rtciToolCallId :: Text
    -- ^ Tool-call id
    , rtciToolName :: Text
    -- ^ Name of the tool being called
    , rtciStatus :: Text
    -- ^ Current status: pending or running
    , rtciStartedAt :: Maybe UTCTime
    -- ^ When the call started executing
    }
    deriving (Show, Eq, Generic)

runningToolCallInfoOptions :: Aeson.Options
runningToolCallInfoOptions =
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 4
        }

instance ToJSON RunningToolCallInfo where
    toJSON = Aeson.genericToJSON runningToolCallInfoOptions

instance FromJSON RunningToolCallInfo where
    parseJSON = Aeson.genericParseJSON runningToolCallInfoOptions

{- | Parameters for the @cancel-tool-call@ capability.
-}
data CancelToolCallParams = CancelToolCallParams
    { ctpToolCallId :: Text
    -- ^ The tool-call id to cancel
    , ctpReason :: Maybe Text
    -- ^ Optional human-readable reason for cancellation
    }
    deriving (Show, Eq, Generic)

instance FromJSON CancelToolCallParams where
    parseJSON = Aeson.withObject "CancelToolCallParams" $ \v ->
        CancelToolCallParams
            <$> v .: "tool_call_id"
            <*> v .:? "reason"

instance ToJSON CancelToolCallParams where
    toJSON p =
        Aeson.object $
            ["tool_call_id" .= ctpToolCallId p]
                ++ ["reason" .= r | Just r <- [ctpReason p]]

{- | Result of a @cancel-tool-call@ query.
-}
data CancelToolCallResult = CancelToolCallResult
    { ccrToolCallId :: Text
    -- ^ The cancelled tool-call id
    , ccrCancelled :: Bool
    -- ^ Whether the call was actually cancelled
    , ccrPreviousStatus :: Text
    -- ^ Status before the cancellation attempt
    }
    deriving (Show, Eq, Generic)

cancelToolCallResultOptions :: Aeson.Options
cancelToolCallResultOptions =
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 3
        }

instance ToJSON CancelToolCallResult where
    toJSON = Aeson.genericToJSON cancelToolCallResultOptions

instance FromJSON CancelToolCallResult where
    parseJSON = Aeson.genericParseJSON cancelToolCallResultOptions

-------------------------------------------------------------------------------
-- wait (todos/session-mailbox.md, Phase 2, §3)
-------------------------------------------------------------------------------

{- | Parameters for the @wait@ capability.

'waitFor' is one of: a specific @tool_call_id@ (the LLM's own provider id,
as shown on a running placeholder or by @list-running-tool-calls@),
@"any-call"@ (any of the caller's own running calls), or @"mail"@ (only
mail). Unlike the spec's @{"for": [...]}@ list form, this accepts a single
target — reporting which one woke the call needs no list disambiguation,
and a caller that wants several calls covered can use @"any-call"@.
-}
data WaitParams = WaitParams
    { waitFor :: Text
    -- ^ A @tool_call_id@, @"any-call"@, or @"mail"@
    , waitTimeoutSeconds :: Int
    -- ^ Maximum seconds to wait, capped by 'maxWaitSeconds' (default 30)
    }
    deriving (Show, Eq, Generic)

instance FromJSON WaitParams where
    parseJSON = Aeson.withObject "WaitParams" $ \v ->
        WaitParams
            <$> v .: "for"
            <*> v .:? "timeout_seconds" .!= 30

instance ToJSON WaitParams where
    toJSON p =
        Aeson.object
            [ "for" .= waitFor p
            , "timeout_seconds" .= waitTimeoutSeconds p
            ]

{- | Result of a @wait@ query: which of the first-to-happen conditions woke
it (@"call"@, @"mail"@, or @"timeout"@), and the specific tool-call id when
woken by a call.
-}
data WaitResult = WaitResult
    { wrWokenBy :: Text
    -- ^ @"call"@, @"mail"@, or @"timeout"@
    , wrToolCallId :: Maybe Text
    -- ^ The tool-call id that became final, when 'wrWokenBy' is @"call"@
    }
    deriving (Show, Eq, Generic)

instance ToJSON WaitResult where
    toJSON r =
        Aeson.object $
            ["woken_by" .= wrWokenBy r]
                ++ ["tool_call_id" .= tid | Just tid <- [wrToolCallId r]]

instance FromJSON WaitResult where
    parseJSON = Aeson.withObject "WaitResult" $ \v ->
        WaitResult
            <$> v .: "woken_by"
            <*> v .:? "tool_call_id"

-- | Hard cap on 'waitTimeoutSeconds', per the spec: "two agents waiting on
-- each other time out instead of deadlocking".
maxWaitSeconds :: Int
maxWaitSeconds = 300

-------------------------------------------------------------------------------
-- send-message (todos/session-mailbox.md, Phase 4, §5)
-------------------------------------------------------------------------------

{- | Parameters for a @send-message@ call: agent-to-agent mail, addressed by
'SessionId' text.
-}
data SendMessageParams = SendMessageParams
    { smpTo :: Text
    -- ^ The recipient session's id, as text
    , smpText :: Text
    , smpInReplyTo :: Maybe Text
    -- ^ A 'System.Agents.Session.Types.MessageId' this message answers, as text
    , smpExpectsReply :: Bool
    , smpInterrupt :: Bool
    }
    deriving (Show, Eq, Generic)

instance FromJSON SendMessageParams where
    parseJSON = Aeson.withObject "SendMessageParams" $ \v ->
        SendMessageParams
            <$> v .: "to"
            <*> v .: "text"
            <*> v .:? "in_reply_to"
            <*> v .:? "expects_reply" .!= False
            <*> v .:? "interrupt" .!= False

instance ToJSON SendMessageParams where
    toJSON p =
        Aeson.object $
            [ "to" .= smpTo p
            , "text" .= smpText p
            , "expects_reply" .= smpExpectsReply p
            , "interrupt" .= smpInterrupt p
            ]
                ++ ["in_reply_to" .= r | Just r <- [smpInReplyTo p]]

-- | Result of a @send-message@ call: the receipt plus the recipient's status.
data SendMessageResult = SendMessageResult
    { smrMessageId :: Text
    , smrSeq :: Int
    , smrDuplicate :: Bool
    , smrRecipientStatus :: Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON SendMessageResult where
    toJSON r =
        Aeson.object
            [ "message_id" .= smrMessageId r
            , "seq" .= smrSeq r
            , "duplicate" .= smrDuplicate r
            , "recipient_status" .= smrRecipientStatus r
            ]

instance FromJSON SendMessageResult where
    parseJSON = Aeson.withObject "SendMessageResult" $ \v ->
        SendMessageResult
            <$> v .: "message_id"
            <*> v .: "seq"
            <*> v .: "duplicate"
            <*> v .: "recipient_status"

-------------------------------------------------------------------------------
-- spawn-session (todos/session-mailbox.md, Phase 4, §5)
-------------------------------------------------------------------------------

{- | Parameters for a @spawn-session@ call: start one of this agent's own
helpers running as a detached child session (not call\/return -- it
outlives this tool call and answers by mail via @send-message@).
-}
data SpawnSessionParams = SpawnSessionParams
    { sspAgent :: Text
    -- ^ One of the caller's helper slugs, as for @prompt_agent_\<slug\>@
    , sspMessage :: Text
    }
    deriving (Show, Eq, Generic)

instance FromJSON SpawnSessionParams where
    parseJSON = Aeson.withObject "SpawnSessionParams" $ \v ->
        SpawnSessionParams
            <$> v .: "agent"
            <*> v .: "message"

instance ToJSON SpawnSessionParams where
    toJSON p =
        Aeson.object
            [ "agent" .= sspAgent p
            , "message" .= sspMessage p
            ]

-- | Result of a @spawn-session@ call: the new session's id.
newtype SpawnSessionResult = SpawnSessionResult
    { ssrSessionId :: Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON SpawnSessionResult where
    toJSON r = Aeson.object ["session_id" .= ssrSessionId r]

instance FromJSON SpawnSessionResult where
    parseJSON = Aeson.withObject "SpawnSessionResult" $ \v ->
        SpawnSessionResult <$> v .: "session_id"

-------------------------------------------------------------------------------
-- Query Errors
-------------------------------------------------------------------------------

-- | Errors that can occur during system info queries.
data QueryError
    = -- | The requested capability is not enabled in the toolbox
      CapabilityNotEnabledError !Text
    | -- | Error occurred while gathering system information
      SystemInfoError !Text
    | -- | File not found for attach-file
      FileNotFoundError !FilePath
    | -- | File too large for attachment
      FileTooLargeError !FilePath !Int
    | -- | Unsupported file type
      UnsupportedFileTypeError !FilePath !Text
    | -- | File access denied by sandbox
      FileAccessDeniedError !FilePath !Text
    | -- | Session store not configured for session introspection
      SessionStoreNotConfiguredError
    | -- | Session not found
      SessionNotFoundError !Text
    | -- | Invalid session ID format
      InvalidSessionIdError !Text
    | -- | Access denied based on scope
      SessionAccessDeniedError !Text !SessionIntrospectionScope
    | -- | Missing required parameter
      MissingParameterError !Text
    | -- | Command refused by the configured filter
      CommandRefusedError !Text
    | -- | The configured filter produced invalid output
      InvalidFilterOutputError !Text
    deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Utility Functions
-------------------------------------------------------------------------------

-- | Format execution time as seconds with 3 decimal places.
formatExecutionTime :: NominalDiffTime -> Double
formatExecutionTime = realToFrac
