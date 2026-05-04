{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Provides a runtime for gathering system information.

This module implements the system toolbox functionality, including:

* System information gathering (date, OS info, environment variables, etc.)
* File attachment capability for multi-modal LLM interactions
* Session introspection capabilities (list-sessions, search-sessions, read-session, get-session-stats)
* Capability-based access control
* Result formatting for LLM consumption
* Tracing for debugging and monitoring

The toolbox can be configured to expose different system information capabilities,
providing contextual information about the running environment to agents.

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
    -- * Core types
    Trace (..),
    Toolbox (..),
    ToolDescription (..),
    QueryError (..),
    QueryResult (..),
    AttachFileResult (..),

    -- * Initialization
    initializeToolbox,
    initializeToolboxWithSessionIntrospection,

    -- * Query execution
    executeQuery,
    executeQueryWithParams,
    executeAttachFile,
    getCapabilityInfo,

    -- * Result formatting
    formatResults,

    -- * Media type detection
    detectMediaType,
    getFileExtension,

    -- * Session introspection helpers
    SessionIntrospectionConfig (..),
    defaultSessionIntrospectionConfig,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Exception (SomeException, try, bracket, IOException)
import Control.Monad (forM)
import Data.Aeson (ToJSON (..), Value (..), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LByteString
import Data.List (isInfixOf)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import qualified Data.Time as Time
import qualified Data.UUID as UUID
import Prod.Tracer (Tracer (..), runTracer)
import System.Directory (doesFileExist, getCurrentDirectory, getModificationTime)
import System.Environment (getEnvironment, lookupEnv)
import System.FilePath (takeExtension)
import System.IO (IOMode(..), openBinaryFile, hClose)
import System.IO.Error (isDoesNotExistError, ioeGetErrorString)
import System.Posix.Process (getParentProcessID, getProcessID)
import System.Posix.User (getEffectiveGroupID, getEffectiveUserID, getUserEntryForID, homeDirectory, userName, userShell)
import qualified System.Process as Process

import System.Agents.Base (
    ConversationId (..),
    SessionIntrospectionScope (..),
    SystemToolCapability (..),
    SystemToolboxDescription (..),
 )
import System.Agents.Media.Types (ApplicationType (..), AudioType (..), ImageType (..), MediaType (..), TextSubtype (..), VideoType (..))
import System.Agents.Session.Types (
    Session (..),
    SessionId (..),
    Turn (..),
    UserTurnContent (..),
    LlmTurnContent (..),
    PartialUserTurnContent (..),
    UserQuery (..),
    LlmResponse (..),
    SystemPrompt (..),
 )
import qualified System.Agents.SessionStore as SessionStore

-------------------------------------------------------------------------------
-- Core Types
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
            let toolbox =
                    Toolbox
                        { toolboxName = desc.systemToolboxName
                        , toolboxDescription = desc.systemToolboxDescription
                        , toolboxCapabilities = desc.systemToolboxCapabilities
                        , toolboxEnvVarFilter = desc.systemToolboxEnvVarFilter
                        , toolboxConfig = desc
                        , toolboxSessionIntrospection = mSessionConfig
                        }
            pure $ Right toolbox

-------------------------------------------------------------------------------
-- Media Type Detection
-------------------------------------------------------------------------------

{- | Detect media type from file extension.

Returns Nothing if the file type is not recognized as a supported media type.
-}
detectMediaType :: FilePath -> Maybe MediaType
detectMediaType path =
    case Text.toLower $ Text.pack $ takeExtension path of
        -- Images
        ".png" -> Just $ MediaImage ImagePNG
        ".jpg" -> Just $ MediaImage ImageJPEG
        ".jpeg" -> Just $ MediaImage ImageJPEG
        ".gif" -> Just $ MediaImage ImageGIF
        ".webp" -> Just $ MediaImage ImageWebP
        ".svg" -> Just $ MediaImage ImageSVG
        -- Audio
        ".mp3" -> Just $ MediaAudio AudioMP3
        ".wav" -> Just $ MediaAudio AudioWAV
        ".ogg" -> Just $ MediaAudio AudioOGG
        ".aac" -> Just $ MediaAudio AudioAAC
        ".flac" -> Just $ MediaAudio AudioFLAC
        -- Video
        ".mp4" -> Just $ MediaVideo VideoMP4
        ".webm" -> Just $ MediaVideo VideoWebM
        ".mov" -> Just $ MediaVideo VideoMOV
        ".avi" -> Just $ MediaVideo VideoAVI
        -- Documents
        ".pdf" -> Just $ MediaApplication AppPDF
        ".json" -> Just $ MediaApplication AppJSON
        ".xml" -> Just $ MediaApplication AppXML
        ".zip" -> Just $ MediaApplication AppZip
        -- Text files (as octet-stream for generic handling)
        ".txt" -> Just $ MediaApplication AppOctetStream
        ".md" -> Just $ MediaApplication AppOctetStream
        ".csv" -> Just $ MediaApplication AppOctetStream
        -- Unknown
        _ -> Nothing

-- | Get file extension helper.
getFileExtension :: FilePath -> Text
getFileExtension = Text.toLower . Text.pack . takeExtension

-- | Convert MediaType to MIME type string.
mediaTypeToMime :: MediaType -> Text
mediaTypeToMime (MediaImage ImagePNG) = "image/png"
mediaTypeToMime (MediaImage ImageJPEG) = "image/jpeg"
mediaTypeToMime (MediaImage ImageGIF) = "image/gif"
mediaTypeToMime (MediaImage ImageWebP) = "image/webp"
mediaTypeToMime (MediaImage ImageSVG) = "image/svg+xml"
mediaTypeToMime (MediaAudio AudioMP3) = "audio/mp3"
mediaTypeToMime (MediaAudio AudioWAV) = "audio/wav"
mediaTypeToMime (MediaAudio AudioOGG) = "audio/ogg"
mediaTypeToMime (MediaAudio AudioAAC) = "audio/aac"
mediaTypeToMime (MediaAudio AudioFLAC) = "audio/flac"
mediaTypeToMime (MediaAudio AudioMPEG) = "audio/mpeg"
mediaTypeToMime (MediaVideo VideoMP4) = "video/mp4"
mediaTypeToMime (MediaVideo VideoWebM) = "video/webm"
mediaTypeToMime (MediaVideo VideoMOV) = "video/quicktime"
mediaTypeToMime (MediaVideo VideoAVI) = "video/avi"
mediaTypeToMime (MediaVideo VideoOGG) = "video/ogg"
mediaTypeToMime (MediaApplication AppPDF) = "application/pdf"
mediaTypeToMime (MediaApplication AppJSON) = "application/json"
mediaTypeToMime (MediaApplication AppXML) = "application/xml"
mediaTypeToMime (MediaApplication AppOctetStream) = "application/octet-stream"
mediaTypeToMime (MediaApplication AppZip) = "application/zip"
mediaTypeToMime (MediaText TextPlain) = "text/plain"
mediaTypeToMime (MediaText TextHTML) = "text/html"
mediaTypeToMime (MediaText TextCSS) = "text/css"
mediaTypeToMime (MediaText TextCSV) = "text/csv"
mediaTypeToMime (MediaText TextMarkdown) = "text/markdown"

-------------------------------------------------------------------------------
-- File Attachment
-------------------------------------------------------------------------------

-- | Maximum file size for attachment (50MB)
maxFileSize :: Int
maxFileSize = 50 * 1024 * 1024

{- | Execute file attachment.

This function reads a file from disk and returns it as a base64-encoded
attachment suitable for multi-modal LLM interactions.

Returns:
* 'Right AttachFileResult' on successful attachment
* 'Left QueryError' if capability not enabled, file not found, or file too large
-}
executeAttachFile ::
    Tracer IO Trace ->
    Toolbox ->
    FilePath ->
    IO (Either QueryError AttachFileResult)
executeAttachFile tracer toolbox filePath = do
    runTracer tracer (FileAttachRequestedTrace filePath)

    -- Check if capability is enabled
    if SystemToolAttachFile `notElem` toolboxCapabilities toolbox
        then do
            let err = CapabilityNotEnabledError "attach-file"
            runTracer tracer (FileAttachErrorTrace filePath "Capability not enabled")
            pure $ Left err
        else do
            -- Check if file exists
            fileExists <- doesFileExist filePath
            if not fileExists
                then do
                    let err = FileNotFoundError filePath
                    runTracer tracer (FileAttachErrorTrace filePath "File not found")
                    pure $ Left err
                else do
                    -- Read file
                    result <- try $ BS.readFile filePath
                    case result of
                        Left (e :: SomeException) -> do
                            let errMsg = Text.pack $ show e
                            runTracer tracer (FileAttachErrorTrace filePath errMsg)
                            pure $ Left $ SystemInfoError $ "Failed to read file: " <> errMsg
                        Right content -> do
                            let fileSize = BS.length content

                            -- Check file size
                            if fileSize > maxFileSize
                                then do
                                    runTracer tracer (FileAttachErrorTrace filePath "File too large")
                                    pure $ Left $ FileTooLargeError filePath maxFileSize
                                else do
                                    -- Detect media type
                                    case detectMediaType filePath of
                                        Nothing -> do
                                            let errMsg = "Unsupported file type: " <> getFileExtension filePath
                                            runTracer tracer (FileAttachErrorTrace filePath errMsg)
                                            pure $ Left $ UnsupportedFileTypeError filePath (getFileExtension filePath)
                                        Just mediaType -> do
                                            let mimeType = mediaTypeToMime mediaType
                                            let base64Data = Text.decodeUtf8 $ B64.encode content

                                            runTracer tracer (FileAttachSuccessTrace filePath mediaType fileSize)

                                            pure $
                                                Right $
                                                    AttachFileResult
                                                        { attachFilePath = filePath
                                                        , attachMimeType = mimeType
                                                        , attachBase64Data = base64Data
                                                        , attachFileSize = fileSize
                                                        }

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
    executeQueryWithParams tracer toolbox capabilityName Nothing Nothing

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
    IO (Either QueryError QueryResult)
executeQueryWithParams tracer toolbox capabilityName mSessionId mQuery = do
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
                    result <- getCapabilityInfoWithTimeout capability toolbox mSessionId mQuery
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

-- | Default timeout for system info gathering (5 seconds).
defaultTimeoutSeconds :: Int
defaultTimeoutSeconds = 5

{- | Gather system information for a capability with a timeout.

Uses race to implement a timeout, preventing the toolbox from hanging
on slow or blocking system calls.
-}
getCapabilityInfoWithTimeout ::
    SystemToolCapability ->
    Toolbox ->
    Maybe Text ->
    Maybe Text ->
    IO (Either Text (Text, Aeson.Value))
getCapabilityInfoWithTimeout capability toolbox mSessionId mQuery = do
    let timeoutMicros = defaultTimeoutSeconds * 1000000
    result <- race (threadDelay timeoutMicros) (getCapabilityInfoInternal capability toolbox mSessionId mQuery)
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
    IO (Either Text (Text, Aeson.Value))
getCapabilityInfoInternal capability toolbox mSessionId mQuery = do
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
            SystemToolReadSession -> getReadSessionInfo (toolboxSessionIntrospection toolbox) mSessionId
            SystemToolGetSessionStats -> getSessionStatsInfo (toolboxSessionIntrospection toolbox)
        pure (name, value)
    case result of
        Left (e :: SomeException) -> pure $ Left $ Text.pack $ show e
        Right val -> pure $ Right val

-------------------------------------------------------------------------------
-- Session Introspection Capabilities
-------------------------------------------------------------------------------

{- | Check if an IOException indicates a "resource busy" (file locked) condition.
This checks the error message for common patterns indicating a locked file.
-}
isResourceBusyError :: IOException -> Bool
isResourceBusyError e = 
    let errStr = ioeGetErrorString e
        errTxt = Text.toLower $ Text.pack errStr
    in "resource busy" `Text.isInfixOf` errTxt
        || "file is locked" `Text.isInfixOf` errTxt
        || "locked" `Text.isInfixOf` errTxt

{- | List accessible sessions based on scope.

Returns session IDs, timestamps, turn counts, and relationship info.
Handles locked files gracefully by marking sessions that are currently
being written to.
-}
getListSessionsInfo :: Maybe SessionIntrospectionConfig -> IO (Text, Aeson.Value)
getListSessionsInfo Nothing = pure ("list-sessions", String "Session store not configured")
getListSessionsInfo (Just config) = do
    -- List all sessions from the store with error handling for locked files
    allSessionsResult <- try $ SessionStore.listSessions (introspectionStore config)
    
    case allSessionsResult of
        Left (e :: SomeException) -> do
            -- If we can't list sessions at all, return an error
            pure ("list-sessions", Aeson.object ["error" .= Text.pack (show e), "sessions" .= ([] :: [Aeson.Value])])
        Right allSessions -> do
            -- Filter sessions based on scope
            let accessibleSessions = filterSessionsByScope config allSessions

            -- Apply limit
            let limitedSessions = take (introspectionMaxResults config) accessibleSessions

            -- Build session info list, handling locked files gracefully
            sessionInfos <- forM limitedSessions $ \(path, mSession, convId) -> do
                -- Try to get modification time, handling locked files
                mtimeResult <- try $ getSessionModTime (introspectionStore config) convId
                let mtime = case mtimeResult of
                        Left (_ :: SomeException) -> Nothing
                        Right mt -> mt
                
                -- Check if session is locked (currently being edited)
                isLocked <- isSessionFileLocked path
                
                let turnCount = maybe 0 (length . turns) mSession
                let isParent = isParentSession config mSession
                let isChild = isChildSession config mSession
                
                pure $ Aeson.object
                    [ "sessionId" .= conversationIdToText convId
                    , "conversationId" .= conversationIdToText convId
                    , "modificationTime" .= maybe "" (Text.pack . show) mtime
                    , "turnCount" .= turnCount
                    , "isParent" .= isParent
                    , "isChild" .= isChild
                    , "isLocked" .= isLocked
                    , "status" .= if isLocked then ("active" :: Text) else ("idle" :: Text)
                    ]

            let totalAccessible = length accessibleSessions

            pure $
                ( "list-sessions"
                , Aeson.object
                    [ "sessions" .= sessionInfos
                    , "totalAccessible" .= totalAccessible
                    ]
                )

-- | Check if a session file is currently locked (being written to)
isSessionFileLocked :: FilePath -> IO Bool
isSessionFileLocked path = do
    result <- try $ bracket (openBinaryFile path ReadMode) hClose (\_ -> pure ())
    case result of
        Left (ioe :: IOException) -> pure $ isResourceBusyError ioe
        Right _ -> pure False

{- | Search sessions using full-text search.

Returns matching sessions with relevance scores and snippet previews.
-}
getSearchSessionsInfo :: Maybe SessionIntrospectionConfig -> Maybe Text -> IO (Text, Aeson.Value)
getSearchSessionsInfo Nothing _ = pure ("search-sessions", String "Session store not configured")
getSearchSessionsInfo (Just _config) Nothing = 
    pure ("search-sessions", Aeson.object ["error" .= ("Missing 'query' parameter for search-sessions" :: Text)])
getSearchSessionsInfo (Just config) (Just searchQuery) = do
    -- List all sessions from the store
    allSessionsResult <- try $ SessionStore.listSessions (introspectionStore config)
    
    case allSessionsResult of
        Left (e :: SomeException) -> do
            pure ("search-sessions", Aeson.object ["error" .= Text.pack (show e), "results" .= ([] :: [Aeson.Value])])
        Right allSessions -> do
            -- Filter sessions based on scope first
            let accessibleSessions = filterSessionsByScope config allSessions
            
            -- Perform simple text search across accessible sessions
            let searchTerms = Text.words $ Text.toLower searchQuery
            let matchingSessions = filter (sessionMatchesSearch searchTerms) accessibleSessions
            
            -- Apply limit
            let limitedResults = take (introspectionMaxResults config) matchingSessions
            
-- Build result items
            resultItems <- forM limitedResults $ \(_path, mSession, convId) -> do
                let turnCount = maybe 0 (length . turns) mSession
                let preview = generateSessionPreview mSession searchTerms
                
                pure $ Aeson.object
                    [ "sessionId" .= conversationIdToText convId
                    , "conversationId" .= conversationIdToText convId
                    , "turnCount" .= turnCount
                    , "preview" .= preview
                    , "matchType" .= ("content" :: Text)
                    ]
            
            pure $
                ( "search-sessions"
                , Aeson.object
                    [ "query" .= searchQuery
                    , "results" .= resultItems
                    , "totalMatches" .= length matchingSessions
                    , "scope" .= showScope (introspectionScope config)
                    ]
                )

-- | Check if a session matches the search terms
sessionMatchesSearch :: [Text] -> (FilePath, Maybe Session, ConversationId) -> Bool
sessionMatchesSearch searchTerms (_, mSession, _) =
    case mSession of
        Nothing -> False
        Just session -> any (termMatchesSession session) searchTerms

-- | Check if a search term matches any content in the session
termMatchesSession :: Session -> Text -> Bool
termMatchesSession session term =
    any (turnMatchesTerm term) (turns session)

-- | Check if a turn matches the search term
turnMatchesTerm :: Text -> Turn -> Bool
turnMatchesTerm term turn =
    let content = Text.toLower $ turnContentText turn
    in term `Text.isInfixOf` content

-- | Extract text content from a turn for searching
turnContentText :: Turn -> Text
turnContentText (UserTurn content _) =
    let promptText = case userPrompt content of SystemPrompt t -> t
        userQueryText = maybe "" (\(UserQuery t _) -> t) (userQuery content)
     in promptText <> " " <> userQueryText
turnContentText (LlmTurn content _) =
    fromMaybe "" $ responseText $ llmResponse content
turnContentText (PartialUserTurn content _) =
    let promptText = case pUserPrompt content of SystemPrompt t -> t
        userQueryText = maybe "" (\(UserQuery t _) -> t) (pUserQuery content)
     in promptText <> " " <> userQueryText

-- | Generate a preview of matching content from a session
generateSessionPreview :: Maybe Session -> [Text] -> Text
generateSessionPreview mSession searchTerms =
    case mSession of
        Nothing -> ""
        Just session ->
            let allContent = Text.intercalate " " $ map turnContentText (take 3 $ turns session)
                lowerContent = Text.toLower allContent
                -- Find first matching term and extract context around it
                matchPositions = concatMap (\term -> findTermPositions term lowerContent 0) searchTerms
            in case matchPositions of
                [] -> Text.take 200 allContent <> "..."
                (pos:_) ->
                    let start = max 0 (pos - 100)
                        end = min (Text.length allContent) (pos + 100)
                    in "..." <> Text.take (end - start) (Text.drop start allContent) <> "..."

-- | Find all positions of a term in text
findTermPositions :: Text -> Text -> Int -> [Int]
findTermPositions term txt offset =
    case Text.breakOn term txt of
        (_, "") -> [] -- Not found
        (prefix, rest) ->
            let pos = offset + Text.length prefix
            in pos : findTermPositions term (Text.drop (Text.length term) rest) (pos + Text.length term)

{- | Read session content.

Returns: turns, tool calls, responses based on scope
-}
getReadSessionInfo :: Maybe SessionIntrospectionConfig -> Maybe Text -> IO (Text, Aeson.Value)
getReadSessionInfo Nothing _ = pure ("read-session", String "Session store not configured")
getReadSessionInfo (Just _config) Nothing = 
    pure ("read-session", Aeson.object ["error" .= ("Missing 'session_id' parameter for read-session" :: Text)])
getReadSessionInfo (Just config) (Just sessionIdText) = do
    -- Parse the session ID from text
    case parseSessionId sessionIdText of
        Nothing -> 
            pure ("read-session", Aeson.object ["error" .= ("Invalid session_id format: " <> sessionIdText)])
        Just targetSessionId -> do
            -- Convert SessionId to ConversationId for lookup
            let targetConvId = sessionIdToConversationId targetSessionId
            
            -- Try to read the session
            mSession <- SessionStore.readSession (introspectionStore config) targetConvId
            
            case mSession of
                Nothing -> 
                    pure ("read-session", Aeson.object 
                        [ "error" .= ("Session not found or is currently locked: " <> sessionIdText)
                        , "sessionId" .= sessionIdText
                        ])
                Just session -> do
                    -- Check scope access
                    if canAccessSession config session
                        then do
                            -- Build turn summaries
                            let turnSummaries = map turnToSummary (turns session)
                            
                            pure ("read-session", Aeson.object
                                [ "sessionId" .= sessionIdText
                                , "conversationId" .= conversationIdToText targetConvId
                                , "turnCount" .= length (turns session)
                                , "turns" .= turnSummaries
                                , "scope" .= showScope (introspectionScope config)
                                , "access" .= ("granted" :: Text)
                                ])
                        else do
                            pure ("read-session", Aeson.object
                                [ "error" .= ("Access denied: session is outside configured scope" :: Text)
                                , "sessionId" .= sessionIdText
                                , "scope" .= showScope (introspectionScope config)
                                , "access" .= ("denied" :: Text)
                                ])

-- | Parse a SessionId from text
parseSessionId :: Text -> Maybe SessionId
parseSessionId txt =
    case UUID.fromText txt of
        Just uuid -> Just $ SessionId uuid
        Nothing -> Nothing

-- | Convert a SessionId to a ConversationId
sessionIdToConversationId :: SessionId -> ConversationId
sessionIdToConversationId (SessionId uuid) = ConversationId uuid

-- | Check if a session can be accessed based on the configured scope
canAccessSession :: SessionIntrospectionConfig -> Session -> Bool
canAccessSession config session =
    case introspectionScope config of
        ScopeAll -> True
        ScopeParentsOnly -> isParentSession config (Just session)
        ScopeChildrenOnly -> isChildSession config (Just session)
        ScopeSubtree -> 
            isParentSession config (Just session) || 
            isChildSession config (Just session) || 
            isCurrentSession config (Just session)

-- | Convert a turn to a summary for output
turnToSummary :: Turn -> Aeson.Value
turnToSummary (UserTurn content _) =
    Aeson.object
        [ "type" .= ("user" :: Text)
        , "prompt" .= case userPrompt content of SystemPrompt t -> t
        , "query" .= fmap (\(UserQuery t _) -> t) (userQuery content)
        , "toolCount" .= length (userTools content)
        ]
turnToSummary (LlmTurn content _) =
    Aeson.object
        [ "type" .= ("llm" :: Text)
        , "response" .= responseText (llmResponse content)
        , "toolCallCount" .= length (llmToolCalls content)
        ]
turnToSummary (PartialUserTurn content _) =
    Aeson.object
        [ "type" .= ("partial" :: Text)
        , "prompt" .= case pUserPrompt content of SystemPrompt t -> t
        , "completedCount" .= length (pCompletedResponses content)
        , "pendingCount" .= length (pPendingCalls content)
        ]

{- | Get session statistics.

Returns: turn counts, token usage, byte counts, trajectory signals
-}
getSessionStatsInfo :: Maybe SessionIntrospectionConfig -> IO (Text, Aeson.Value)
getSessionStatsInfo Nothing = pure ("get-session-stats", String "Session store not configured")
getSessionStatsInfo (Just config) = do
    -- List all accessible sessions with error handling
    allSessionsResult <- try $ SessionStore.listSessions (introspectionStore config)
    
    case allSessionsResult of
        Left (_ :: SomeException) -> do
            pure $ ("get-session-stats", Aeson.object 
                [ "totalSessions" .= (0 :: Int)
                , "totalTurnsAcrossAllSessions" .= (0 :: Int)
                , "scope" .= showScope (introspectionScope config)
                , "note" .= ("Could not access session store" :: Text)
                ])
        Right allSessions -> do
            let accessibleSessions = filterSessionsByScope config allSessions

            -- Calculate aggregate stats, handling Nothing sessions (locked/unreadable)
            let totalTurns = sum [maybe 0 (length . turns) mSession | (_, mSession, _) <- accessibleSessions]
            let totalSessions = length accessibleSessions

            pure $
                ( "get-session-stats"
                , Aeson.object
                    [ "totalSessions" .= totalSessions
                    , "totalTurnsAcrossAllSessions" .= totalTurns
                    , "scope" .= showScope (introspectionScope config)
                    , "note" .= ("Use SessionPrint.calculateStatistics for detailed per-session stats" :: Text)
                    ]
                )

-- | Helper to convert scope to text
showScope :: SessionIntrospectionScope -> Text
showScope ScopeParentsOnly = "parents-only"
showScope ScopeChildrenOnly = "children-only"
showScope ScopeSubtree = "subtree"
showScope ScopeAll = "all"

-- | Filter sessions based on the configured scope
filterSessionsByScope :: SessionIntrospectionConfig -> [(FilePath, Maybe Session, ConversationId)] -> [(FilePath, Maybe Session, ConversationId)]
filterSessionsByScope config sessions =
    case introspectionScope config of
        ScopeAll -> sessions
        _ -> filter (sessionMatchesScope config) sessions

-- | Check if a session matches the configured scope
sessionMatchesScope :: SessionIntrospectionConfig -> (FilePath, Maybe Session, ConversationId) -> Bool
sessionMatchesScope config (_, mSession, _convId) =
    case introspectionScope config of
        ScopeAll -> True
        ScopeParentsOnly -> isParentSession config mSession
        ScopeChildrenOnly -> isChildSession config mSession
        ScopeSubtree -> isParentSession config mSession || isChildSession config mSession || isCurrentSession config mSession

-- | Check if a session is a parent (ancestor) of the current session
isParentSession :: SessionIntrospectionConfig -> Maybe Session -> Bool
isParentSession config mSession =
    case (introspectionCurrentForkedFrom config, mSession) of
        (Just currentForkedFrom, Just session) ->
            sessionId session == currentForkedFrom
        _ -> False

-- | Check if a session is a child (descendant) of the current session
isChildSession :: SessionIntrospectionConfig -> Maybe Session -> Bool
isChildSession config mSession =
    case (introspectionCurrentSessionId config, mSession) of
        (Just currentId, Just session) ->
            forkedFromSessionId session == Just currentId
        _ -> False

-- | Check if a session is the current session
isCurrentSession :: SessionIntrospectionConfig -> Maybe Session -> Bool
isCurrentSession config mSession =
    case (introspectionCurrentSessionId config, mSession) of
        (Just currentId, Just session) ->
            sessionId session == currentId
        _ -> False

-- | Helper to get session modification time
getSessionModTime :: SessionStore.SessionStore -> ConversationId -> IO (Maybe UTCTime)
getSessionModTime store convId = do
    let path = SessionStore.sessionFilePath store convId
    result <- try $ getModificationTime path
    case result of
        Left (_ :: SomeException) -> pure Nothing
        Right mtime -> pure $ Just mtime

-- | Helper to convert ConversationId to Text
conversationIdToText :: ConversationId -> Text
conversationIdToText (ConversationId uuid) = UUID.toText uuid

-------------------------------------------------------------------------------
-- System Info Capabilities
-------------------------------------------------------------------------------

{- | Gather date/time information.

Returns:
* Current UTC time
* Current local time
* System timezone
-}
getDateInfo :: IO (Text, Aeson.Value)
getDateInfo = do
    utcTime <- Time.getCurrentTime
    tz <- Time.getCurrentTimeZone
    let localTime = Time.utcToLocalTime tz utcTime
    let tzName = Time.timeZoneName tz
    let tzOffset = Time.timeZoneOffsetString tz

    pure $
        ( "date"
        , Aeson.object
            [ "utc" .= Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" utcTime
            , "local" .= Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q" localTime
            , "timezone" .= tzName
            , "timezoneOffset" .= tzOffset
            ]
        )

{- | Gather operating system information.

Reads from /etc/os-release on Linux systems, and gathers kernel
version and architecture information.
-}
getOperatingSystemInfo :: IO (Text, Aeson.Value)
getOperatingSystemInfo = do
    -- Try to read /etc/os-release
    osRelease <- readOsRelease

    -- Get kernel version from uname
    kernelVersion <- safeReadProcess "uname" ["-r"] ""

    -- Get architecture from uname
    arch <- safeReadProcess "uname" ["-m"] ""

    pure $
        ( "operating-system"
        , Aeson.object $
            catMaybes
                [ fmap ("name" .=) (lookup "NAME" osRelease)
                , fmap ("version" .=) (lookup "VERSION" osRelease)
                , fmap ("id" .=) (lookup "ID" osRelease)
                , fmap ("versionId" .=) (lookup "VERSION_ID" osRelease)
                , fmap ("prettyName" .=) (lookup "PRETTY_NAME" osRelease)
                , fmap ("kernel" .=) (safeHead kernelVersion)
                , fmap ("architecture" .=) (safeHead arch)
                ]
        )
  where
    safeHead [] = Nothing
    safeHead (x : _) = Just (Text.strip x)

{- | Parse /etc/os-release file.

Returns a list of key-value pairs from the file.
-}
readOsRelease :: IO [(Text, Text)]
readOsRelease = do
    result <- tryReadFile "/etc/os-release"
    case result of
        Left _ -> do
            -- Try /usr/lib/os-release as fallback
            result2 <- tryReadFile "/usr/lib/os-release"
            case result2 of
                Left _ -> pure []
                Right content -> pure $ parseOsRelease content
        Right content -> pure $ parseOsRelease content
  where
    parseOsRelease :: ByteString -> [(Text, Text)]
    parseOsRelease content =
        let lines' = BS.lines content
            pairs = map parseLine lines'
         in catMaybes pairs

    parseLine :: ByteString -> Maybe (Text, Text)
    parseLine line
        | BS.null line = Nothing
        | BS.head line == '#' = Nothing
        | otherwise =
            case BS.break (== '=') line of
                (key, rest)
                    | BS.null rest -> Nothing
                    | otherwise ->
                        let value = BS.drop 1 rest
                            -- Remove quotes if present
                            value' =
                                if BS.length value >= 2 && BS.head value == '"' && BS.last value == '"'
                                    then BS.drop 1 (BS.init value)
                                    else value
                         in Just (Text.decodeUtf8 key, Text.decodeUtf8 value')

{- | Gather environment variable information.

If a filter pattern is provided, only variables whose names contain
the pattern are included (case-sensitive substring match).
-}
getEnvVarsInfo :: Maybe Text -> IO (Text, Aeson.Value)
getEnvVarsInfo mFilter = do
    env <- getEnvironment
    let filtered = case mFilter of
            Nothing -> env
            Just pattern ->
                let pattern' = Text.unpack pattern
                 in filter (\(k, _) -> pattern' `isInfixOf` k) env

    -- Convert to Text for JSON
    let envMap =
            KeyMap.fromList $
                map (\(k, v) -> (AesonKey.fromText $ Text.pack k, String $ Text.pack v)) filtered

    pure ("env-vars", Object envMap)

{- | Gather running user information.

Returns username, UID, GID, home directory, and shell.
-}
getRunningUserInfo :: IO (Text, Aeson.Value)
getRunningUserInfo = do
    uid <- getEffectiveUserID
    gid <- getEffectiveGroupID

    -- Try to get user info from passwd database
    userInfo <- try $ getUserEntryForID uid

    let (username, home, shell) = case userInfo of
            Left (_ :: SomeException) -> (Text.pack $ show uid, "", "")
            Right entry ->
                ( Text.pack $ userName entry
                , Text.pack $ homeDirectory entry
                , Text.pack $ userShell entry
                )

    pure $
        ( "running-user"
        , Aeson.object
            [ "username" .= username
            , "uid" .= (fromIntegral uid :: Int)
            , "gid" .= (fromIntegral gid :: Int)
            , "homeDirectory" .= home
            , "shell" .= shell
            ]
        )

-- | Gather hostname information.
getHostnameInfo :: IO (Text, Aeson.Value)
getHostnameInfo = do
    hostname <- lookupEnv "HOSTNAME"
    hostname' <- case hostname of
        Just h -> pure $ Text.pack h
        Nothing -> do
            -- Try to read from /proc/sys/kernel/hostname
            result <- tryReadFile "/proc/sys/kernel/hostname"
            case result of
                Left _ -> do
                    -- Fallback to uname -n
                    result2 <- safeReadProcess "uname" ["-n"] ""
                    pure $ maybe "" id (safeHead result2)
                Right content -> pure $ Text.strip $ Text.decodeUtf8 content

    pure ("hostname", Aeson.object ["hostname" .= hostname'])
  where
    safeHead [] = Nothing
    safeHead (x : _) = Just (Text.strip x)

-- | Gather working directory information.
getWorkingDirectoryInfo :: IO (Text, Aeson.Value)
getWorkingDirectoryInfo = do
    cwd <- getCurrentDirectory
    pure ("working-directory", Aeson.object ["path" .= Text.pack cwd])

{- | Gather process information.

Returns process ID and parent process ID.
-}
getProcessInfo :: IO (Text, Aeson.Value)
getProcessInfo = do
    pid <- getProcessID
    ppid <- getParentProcessID

    -- Try to get process name from /proc/self/comm
    commResult <- tryReadFile "/proc/self/comm"
    let procName = case commResult of
            Left _ -> ""
            Right content -> Text.strip $ Text.decodeUtf8 content

    pure $
        ( "process-info"
        , Aeson.object
            [ "pid" .= (fromIntegral pid :: Int)
            , "ppid" .= (fromIntegral ppid :: Int)
            , "name" .= procName
            ]
        )

{- | Gather system uptime information.

Reads from /proc/uptime on Linux systems.
-}
getUptimeInfo :: IO (Text, Aeson.Value)
getUptimeInfo = do
    result <- tryReadFile "/proc/uptime"
    case result of
        Left err ->
            pure ("uptime", Aeson.object ["error" .= Text.pack err])
        Right content -> do
            let text = Text.decodeUtf8 content
            let uptimeStr = Text.takeWhile (/= ' ') text
            let uptimeSec = readMaybe (Text.unpack uptimeStr) :: Maybe Double

            pure $
                ( "uptime"
                , Aeson.object $
                    catMaybes
                        [ Just $ "seconds" .= uptimeSec
                        , fmap (\s -> "formatted" .= formatUptime s) uptimeSec
                        ]
                )
  where
    readMaybe :: (Read a) => String -> Maybe a
    readMaybe s = case reads s of
        [(x, "")] -> Just x
        _ -> Nothing

    formatUptime :: Double -> Text
    formatUptime seconds =
        let days = floor (seconds / 86400) :: Int
            hours = floor ((seconds - fromIntegral days * 86400) / 3600) :: Int
            minutes = floor ((seconds - fromIntegral days * 86400 - fromIntegral hours * 3600) / 60) :: Int
            secs = floor (seconds - fromIntegral days * 86400 - fromIntegral hours * 3600 - fromIntegral minutes * 60) :: Int
         in Text.pack $
                (if days > 0 then show days ++ "d " else "")
                    ++ (if hours > 0 then show hours ++ "h " else "")
                    ++ (if minutes > 0 then show minutes ++ "m " else "")
                    ++ show secs
                    ++ "s"

{- | Get capability info (public version).

This is a convenience function that wraps the internal implementation
with a default toolbox context (no env var filtering).
-}
getCapabilityInfo :: SystemToolCapability -> IO (Text, Aeson.Value)
getCapabilityInfo capability = do
    let toolbox = Toolbox "" "" [capability] Nothing undefined Nothing
    result <- getCapabilityInfoWithTimeout capability toolbox Nothing Nothing
    case result of
        Left err -> pure (capabilityToName capability, String err)
        Right val -> pure val

-------------------------------------------------------------------------------
-- Utility Functions
-------------------------------------------------------------------------------

-- | Try to read a file, returning Left with error message on failure.
tryReadFile :: FilePath -> IO (Either String ByteString)
tryReadFile path = do
    result <- try $ BS.readFile path
    case result of
        Left e
            | isDoesNotExistError e -> pure $ Left $ "File not found: " ++ path
            | otherwise -> pure $ Left $ "Error reading " ++ path ++ ": " ++ show e
        Right content -> pure $ Right content

{- | Safely execute a process and return stdout lines.

Returns empty list on error.
-}
safeReadProcess :: String -> [String] -> String -> IO [Text]
safeReadProcess cmd args input = do
    result <- try $ Process.readProcess cmd args input
    case result of
        Left (_ :: SomeException) -> pure []
        Right output -> pure $ map Text.pack $ lines output

-------------------------------------------------------------------------------
-- Result Formatting
-------------------------------------------------------------------------------

{- | Format query results as JSON.

Returns a ByteString containing a JSON object with:
* "capability": The capability name
* "data": The capability data
* "executionTime": Execution time in seconds

Example output:

> {
>   "capability": "date",
>   "data": {
>     "utc": "2024-01-15T10:30:00.123456Z",
>     "local": "2024-01-15T11:30:00.123456",
>     "timezone": "CET",
>     "timezoneOffset": "+0100"
>   },
>   "executionTime": 0.001
> }
-}
formatResults :: QueryResult -> ByteString
formatResults result =
    LByteString.toStrict $ Aeson.encode jsonObj
  where
    jsonObj =
        Aeson.object
            [ "capability" .= resultCapability result
            , "data" .= resultData result
            , "executionTime" .= formatExecutionTime (resultExecutionTime result)
            ]

-- | Format execution time as seconds with 3 decimal places.
formatExecutionTime :: NominalDiffTime -> Double
formatExecutionTime = realToFrac

