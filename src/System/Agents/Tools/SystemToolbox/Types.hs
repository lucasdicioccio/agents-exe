{-# LANGUAGE OverloadedRecordDot #-}
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
    QueryError (..),

    -- * Utilities
    formatExecutionTime,
) where

import Data.Aeson (ToJSON (..), (.=))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Data.Time (NominalDiffTime)

import System.Agents.Base (
    SessionIntrospectionScope (..),
    SystemToolCapability (..),
    SystemToolboxDescription (..),
 )
import System.Agents.Media.Types (MediaType (..))
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
    { introspectionStore :: SessionStore.SessionStore
    -- ^ Session store for accessing session files
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
defaultSessionIntrospectionConfig :: SessionStore.SessionStore -> SessionIntrospectionConfig
defaultSessionIntrospectionConfig store =
    SessionIntrospectionConfig
        { introspectionStore = store
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
    deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Utility Functions
-------------------------------------------------------------------------------

-- | Format execution time as seconds with 3 decimal places.
formatExecutionTime :: NominalDiffTime -> Double
formatExecutionTime = realToFrac
