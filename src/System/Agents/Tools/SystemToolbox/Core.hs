{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Core functionality for the SystemToolbox.

This module provides the main query execution logic and initialization
functions for the system toolbox.
-}
module System.Agents.Tools.SystemToolbox.Core (
    -- * Initialization
    initializeToolbox,
    initializeToolboxWithSessionIntrospection,

    -- * Query execution
    executeQuery,
    executeQueryWithParams,

    -- * Capability utilities
    capabilityFromName,
    capabilityToName,
    defaultTimeoutSeconds,

    -- * Helper functions
    getCapabilityInfoWithTimeout,
    getCapabilityInfoInternal,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Exception (SomeException, try)
import qualified Data.Aeson as Aeson
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (diffUTCTime, getCurrentTime)
import Prod.Tracer (Tracer (..), runTracer)

import System.Agents.Base (SystemToolCapability (..), SystemToolboxDescription (..), defaultFileSandboxConfig)
import System.Agents.FileSandbox (FileSandbox (..))
import System.Agents.Tools.SystemToolbox.Session (
    getListSessionsInfo,
    getReadSessionInfo,
    getSearchSessionsInfo,
    getSessionStatsInfo,
 )
import System.Agents.Tools.SystemToolbox.SystemInfo (
    getDateInfo,
    getEnvVarsInfo,
    getHostnameInfo,
    getOperatingSystemInfo,
    getProcessInfo,
    getRunningUserInfo,
    getUptimeInfo,
    getWorkingDirectoryInfo,
 )
import System.Agents.Tools.SystemToolbox.Types (
    QueryError (..),
    QueryResult (..),
    ReadSessionParams (..),
    SessionIntrospectionConfig (..),
    Toolbox (..),
    Trace (..),
    defaultReadSessionParams,
 )

-------------------------------------------------------------------------------
-- Initialization
-------------------------------------------------------------------------------

{- | Initialize a system toolbox from a description.

This function creates a 'Toolbox' value from a 'SystemToolboxDescription',
validating the configuration and preparing the runtime state.

Returns an error if the configuration is invalid.

Note: This function does not configure session introspection. Use
'initializeToolboxWithSessionIntrospection' if you need session-related
capabilities like @list-sessions@, @search-sessions@, etc.
-}
initializeToolbox ::
    Tracer IO Trace ->
    SystemToolboxDescription ->
    IO (Either String Toolbox)
initializeToolbox tracer desc =
    initializeToolboxWithSessionIntrospection tracer desc Nothing

{- | Initialize a system toolbox with optional session introspection configuration.

This function creates a 'Toolbox' value from a 'SystemToolboxDescription'
and optionally configures session introspection capabilities.

The session introspection configuration is required for session-related
capabilities like @list-sessions@, @search-sessions@, @read-session@, and
@get-session-stats@. If not provided, these capabilities will return
"Session store not configured" errors.

Returns an error if the configuration is invalid.
-}
initializeToolboxWithSessionIntrospection ::
    Tracer IO Trace ->
    SystemToolboxDescription ->
    Maybe SessionIntrospectionConfig ->
    IO (Either String Toolbox)
initializeToolboxWithSessionIntrospection _tracer desc mSessionConfig = do
    -- Validate that we have at least one capability
    if null desc.systemToolboxCapabilities
        then pure $ Left "System toolbox must have at least one capability enabled"
        else do
            -- Create file sandbox if attach-file capability is enabled
            let config = fromMaybe defaultFileSandboxConfig desc.systemToolboxFileSandbox
            mFileSandbox <-
                if SystemToolAttachFile `elem` desc.systemToolboxCapabilities
                    then do
                        createdAt <- getCurrentTime
                        let sandbox =
                                FileSandbox
                                    { sandboxId = error "sandboxId not used for validation" -- We don't need ID for validation
                                    , sandboxConfig = config
                                    , sandboxCreatedAt = createdAt
                                    }
                        pure $ Just sandbox
                    else pure Nothing

            let toolbox =
                    Toolbox
                        { toolboxName = desc.systemToolboxName
                        , toolboxDescription = desc.systemToolboxDescription
                        , toolboxCapabilities = desc.systemToolboxCapabilities
                        , toolboxEnvVarFilter = desc.systemToolboxEnvVarFilter
                        , toolboxConfig = desc
                        , toolboxSessionIntrospection = mSessionConfig
                        , toolboxFileSandbox = mFileSandbox
                        }
            pure $ Right toolbox

-------------------------------------------------------------------------------
-- Query Execution
-------------------------------------------------------------------------------

{- | Execute a query for a specific capability.

This function:
1. Checks if the requested capability is enabled
2. Gathers the system information for that capability
3. Returns a 'QueryResult' with the data

Returns:
* 'Right QueryResult' on successful execution
* 'Left QueryError' if capability is not enabled or system info cannot be gathered

Note: For the 'attach-file' capability, use 'executeAttachFile' instead.
-}
executeQuery :: Tracer IO Trace -> Toolbox -> Text -> IO (Either QueryError QueryResult)
executeQuery tracer toolbox capabilityName =
    executeQueryWithParams tracer toolbox capabilityName Nothing Nothing Nothing

{- | Execute a query for a specific capability with optional parameters.

This extended version supports session introspection capabilities that require
additional parameters:
* 'read-session' - requires session_id
* 'search-sessions' - requires query

Parameters:
* tracer - for logging/debugging
* toolbox - the system toolbox
* capabilityName - the capability to query
* mSessionId - optional session ID for read-session
* mQuery - optional search query for search-sessions
* mReadParams - optional parameters for read-session slicing and filtering

Returns:
* 'Right QueryResult' on successful execution
* 'Left QueryError' if capability is not enabled or system info cannot be gathered
-}
executeQueryWithParams ::
    Tracer IO Trace ->
    Toolbox ->
    Text ->
    Maybe Text ->
    Maybe Text ->
    Maybe ReadSessionParams ->
    IO (Either QueryError QueryResult)
executeQueryWithParams tracer toolbox capabilityName mSessionId mQuery mReadParams = do
    runTracer tracer (SystemInfoRequestedTrace capabilityName)
    startTime <- getCurrentTime

    -- Find the capability
    case capabilityFromName capabilityName of
        Nothing -> do
            let err = "Unknown capability: " <> capabilityName
            runTracer tracer (SystemInfoErrorTrace capabilityName err)
            pure $ Left $ SystemInfoError err
        Just capability -> do
            -- Check if capability is enabled
            if capability `notElem` toolboxCapabilities toolbox
                then pure $ Left $ CapabilityNotEnabledError capabilityName
                else do
                    -- Gather the information with optional parameters
                    result <- getCapabilityInfoWithTimeout capability toolbox mSessionId mQuery mReadParams
                    endTime <- getCurrentTime
                    let execTime = diffUTCTime endTime startTime

                    case result of
                        Left err -> do
                            runTracer tracer (SystemInfoErrorTrace capabilityName err)
                            pure $ Left $ SystemInfoError err
                        Right (_, value) -> do
                            runTracer tracer (SystemInfoRetrievedTrace capabilityName execTime)
                            pure $
                                Right $
                                    QueryResult
                                        { resultCapability = capabilityName
                                        , resultData = value
                                        , resultExecutionTime = execTime
                                        }

-------------------------------------------------------------------------------
-- Capability Utilities
-------------------------------------------------------------------------------

-- | Convert a capability name text to the corresponding SystemToolCapability.
capabilityFromName :: Text -> Maybe SystemToolCapability
capabilityFromName name = case name of
    "date" -> Just SystemToolDate
    "operating-system" -> Just SystemToolOperatingSystem
    "env-vars" -> Just SystemToolEnvVars
    "running-user" -> Just SystemToolRunningUser
    "hostname" -> Just SystemToolHostname
    "working-directory" -> Just SystemToolWorkingDirectory
    "process-info" -> Just SystemToolProcessInfo
    "uptime" -> Just SystemToolUptime
    "attach-file" -> Just SystemToolAttachFile
    "list-sessions" -> Just SystemToolListSessions
    "search-sessions" -> Just SystemToolSearchSessions
    "read-session" -> Just SystemToolReadSession
    "get-session-stats" -> Just SystemToolGetSessionStats
    _ -> Nothing

-- | Get the name for a capability.
capabilityToName :: SystemToolCapability -> Text
capabilityToName SystemToolDate = "date"
capabilityToName SystemToolOperatingSystem = "operating-system"
capabilityToName SystemToolEnvVars = "env-vars"
capabilityToName SystemToolRunningUser = "running-user"
capabilityToName SystemToolHostname = "hostname"
capabilityToName SystemToolWorkingDirectory = "working-directory"
capabilityToName SystemToolProcessInfo = "process-info"
capabilityToName SystemToolUptime = "uptime"
capabilityToName SystemToolAttachFile = "attach-file"
capabilityToName SystemToolListSessions = "list-sessions"
capabilityToName SystemToolSearchSessions = "search-sessions"
capabilityToName SystemToolReadSession = "read-session"
capabilityToName SystemToolGetSessionStats = "get-session-stats"
capabilityToName SystemToolListDirectory = "list-directory"

-- | Default timeout for system info gathering (5 seconds).
defaultTimeoutSeconds :: Int
defaultTimeoutSeconds = 5

-------------------------------------------------------------------------------
-- Helper Functions
-------------------------------------------------------------------------------

{- | Gather system information for a capability with a timeout.

Uses race to implement a timeout, preventing the toolbox from hanging
on slow or blocking system calls.
-}
getCapabilityInfoWithTimeout ::
    SystemToolCapability ->
    Toolbox ->
    Maybe Text ->
    Maybe Text ->
    Maybe ReadSessionParams ->
    IO (Either Text (Text, Aeson.Value))
getCapabilityInfoWithTimeout capability toolbox mSessionId mQuery mReadParams = do
    let timeoutMicros = defaultTimeoutSeconds * 1000000
    result <- race (threadDelay timeoutMicros) (getCapabilityInfoInternal capability toolbox mSessionId mQuery mReadParams)
    case result of
        Left () -> pure $ Left "Timeout while gathering system information"
        Right val -> pure val

{- | Internal function to gather system information.

This function handles each capability type and returns the appropriate
data as a JSON value.
-}
getCapabilityInfoInternal ::
    SystemToolCapability ->
    Toolbox ->
    Maybe Text ->
    Maybe Text ->
    Maybe ReadSessionParams ->
    IO (Either Text (Text, Aeson.Value))
getCapabilityInfoInternal capability toolbox mSessionId mQuery mReadParams = do
    result <- try $ do
        (name, value) <- case capability of
            SystemToolDate -> getDateInfo
            SystemToolOperatingSystem -> getOperatingSystemInfo
            SystemToolEnvVars -> getEnvVarsInfo (toolboxEnvVarFilter toolbox)
            SystemToolRunningUser -> getRunningUserInfo
            SystemToolHostname -> getHostnameInfo
            SystemToolWorkingDirectory -> getWorkingDirectoryInfo
            SystemToolProcessInfo -> getProcessInfo
            SystemToolUptime -> getUptimeInfo
            SystemToolAttachFile -> error "Use executeAttachFile for attach-file capability"
            SystemToolListSessions -> getListSessionsInfo (toolboxSessionIntrospection toolbox)
            SystemToolSearchSessions -> getSearchSessionsInfo (toolboxSessionIntrospection toolbox) mQuery
            SystemToolReadSession -> getReadSessionInfo (toolboxSessionIntrospection toolbox) mSessionId (fromMaybe defaultReadSessionParams mReadParams)
            SystemToolGetSessionStats -> getSessionStatsInfo (toolboxSessionIntrospection toolbox)
            SystemToolListDirectory -> pure ("list-directory", Aeson.String "list-directory capability not yet implemented for system toolbox")
        pure (name, value)
    case result of
        Left (e :: SomeException) -> pure $ Left $ Text.pack $ show e
        Right val -> pure $ Right val
