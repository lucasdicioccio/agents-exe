{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- | Provides a runtime for gathering system information.

This module implements the system toolbox functionality, including:

* System information gathering (date, OS info, environment variables, etc.)
* File attachment capability for multi-modal LLM interactions
* Session introspection capabilities (list-sessions, search-sessions, read-session, get-session-stats)
* Directory listing with metadata, filtering, and scope enforcement
* Capability-based access control
* Result formatting for LLM consumption
* Tracing for debugging and monitoring

The toolbox can be configured to expose different system information capabilities,
providing contextual information about the running environment to agents.

This module re-exports functionality from the submodules for backward compatibility.
For more granular imports, use the submodules directly.

Example usage:

@
import System.Agents.Tools.SystemToolbox as System
import System.Agents.Base (SystemToolCapability(..))

main :: IO ()
main = do
    let desc = SystemToolboxDescription
            { systemToolboxName = "system"
            , systemToolboxDescription = "System information"
            , systemToolboxCapabilities = [SystemToolDate, SystemToolHostname]
            , systemToolboxEnvVarFilter = Nothing
            }
    result <- System.initializeToolbox tracer desc
    case result of
        Right toolbox -> do
            -- Execute a query for the date capability
            result <- System.executeQuery toolbox "date"
            case result of
                Right queryResult -> print queryResult
                Left err -> print err
        Left err -> putStrLn $ "Failed to initialize: " ++ err
@
-}
module System.Agents.Tools.SystemToolbox (
    -- * Core types (re-exported from Types)
    Trace (..),
    Toolbox (..),
    ToolDescription (..),
    QueryError (..),
    QueryResult (..),
    AttachFileResult (..),

    -- * Initialization (re-exported from Core)
    initializeToolbox,
    initializeToolboxWithSessionIntrospection,

    -- * Query execution (re-exported from Core)
    executeQuery,
    executeQueryWithParams,

    -- * Capability utilities (re-exported from Core)
    capabilityFromName,
    capabilityToName,
    defaultTimeoutSeconds,

    -- * File attachment (re-exported from Attachment)
    executeAttachFile,
    maxFileSize,

    -- * Directory listing (re-exported from Directory)
    executeListDirectory,

    -- * Media type detection (re-exported from Media)
    detectMediaType,
    getFileExtension,
    mediaTypeToMime,

    -- * Session introspection helpers (re-exported from Types and Session)
    SessionIntrospectionConfig (..),
    defaultSessionIntrospectionConfig,
    ReadSessionParams (..),
    defaultReadSessionParams,

    -- * Session utilities (re-exported from Session)
    canAccessSession,
    filterSessionsByScope,
    sessionMatchesScope,
    isParentSession,
    isChildSession,
    isCurrentSession,
    parseSessionId,
    sessionIdToConversationId,
    conversationIdToText,
    showScope,
    applySlicing,
    formatSessionAsCondensedText,
    truncateText,
    extractToolCallName,
    isSessionFileLocked,

    -- * Wait-for-event (re-exported from Wait)
    WaitForEventParams (..),
    EventTypeSelector (..),
    Named (..),
    EventCombinator (..),
    EventCondition (..),
    ConditionCombinator (..),
    WaitForEventResult (..),
    waitForEvent,

    -- * Result formatting (re-exported from Formatting)
    formatResults,
) where

-- Re-export all functionality from submodules
import System.Agents.Tools.SystemToolbox.Attachment
import System.Agents.Tools.SystemToolbox.Core
import System.Agents.Tools.SystemToolbox.Directory
import System.Agents.Tools.SystemToolbox.Formatting
import System.Agents.Tools.SystemToolbox.Media
import System.Agents.Tools.SystemToolbox.Session
import System.Agents.Tools.SystemToolbox.Types
import System.Agents.Tools.SystemToolbox.Wait
