{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Provides a runtime for gathering system information.

This module implements the system toolbox functionality, including:

* System information gathering (date, OS info, environment variables, etc.)
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

    -- * Initialization
    initializeToolbox,

    -- * Query execution
    executeQuery,
    getCapabilityInfo,

    -- * Result formatting
    formatResults,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Exception (SomeException, try)
import Data.Aeson (ToJSON (..), Value (..), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LByteString
import Data.List (isInfixOf)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import qualified Data.Time as Time
import Prod.Tracer (Tracer (..), runTracer)
import System.Directory (getCurrentDirectory)
import System.Environment (getEnvironment, lookupEnv)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Process (getParentProcessID, getProcessID)
import System.Posix.User (getEffectiveGroupID, getEffectiveUserID, getUserEntryForID, homeDirectory, userName, userShell)
import qualified System.Process as Process

import System.Agents.Base (SystemToolCapability (..), SystemToolboxDescription (..))

-------------------------------------------------------------------------------
-- Core Types
-------------------------------------------------------------------------------

{- | Trace events for monitoring system toolbox operations.

These events allow tracking of:
* System info requests
* Query execution progress
* System info retrieval timing
* Capability errors
-}
data Trace
    = -- | System information was requested for a capability
      SystemInfoRequestedTrace !Text
    | -- | System information was retrieved successfully
      SystemInfoRetrievedTrace !Text !NominalDiffTime
    | -- | Error occurred while retrieving system information
      SystemInfoErrorTrace !Text !Text
    deriving (Show)

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
-}
data Toolbox = Toolbox
    { toolboxName :: Text
    , toolboxDescription :: Text
    , toolboxCapabilities :: [SystemToolCapability]
    , toolboxEnvVarFilter :: Maybe Text
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

-- | Errors that can occur during system info queries.
data QueryError
    = -- | The requested capability is not enabled in the toolbox
      CapabilityNotEnabledError !Text
    | -- | Error occurred while gathering system information
      SystemInfoError !Text
    deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Initialization
-------------------------------------------------------------------------------

{- | Initialize a system toolbox from a description.

This function creates a 'Toolbox' value from a 'SystemToolboxDescription',
validating the configuration and preparing the runtime state.

Returns an error if the configuration is invalid.
-}
initializeToolbox ::
    Tracer IO Trace ->
    SystemToolboxDescription ->
    IO (Either String Toolbox)
initializeToolbox _tracer desc = do
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
-}
executeQuery :: Toolbox -> Text -> IO (Either QueryError QueryResult)
executeQuery toolbox capabilityName = do
    runTracer (Tracer (const (pure ()))) (SystemInfoRequestedTrace capabilityName)
    startTime <- getCurrentTime

    -- Find the capability
    case capabilityFromName capabilityName of
        Nothing -> do
            let err = "Unknown capability: " <> capabilityName
            runTracer (Tracer (const (pure ()))) (SystemInfoErrorTrace capabilityName err)
            pure $ Left $ SystemInfoError err
        Just capability -> do
            -- Check if capability is enabled
            if capability `notElem` toolboxCapabilities toolbox
                then pure $ Left $ CapabilityNotEnabledError capabilityName
                else do
                    -- Gather the information
                    result <- getCapabilityInfoWithTimeout capability toolbox
                    endTime <- getCurrentTime
                    let execTime = diffUTCTime endTime startTime

                    case result of
                        Left err -> do
                            runTracer (Tracer (const (pure ()))) (SystemInfoErrorTrace capabilityName err)
                            pure $ Left $ SystemInfoError err
                        Right (_, value) -> do
                            runTracer (Tracer (const (pure ()))) (SystemInfoRetrievedTrace capabilityName execTime)
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
    IO (Either Text (Text, Aeson.Value))
getCapabilityInfoWithTimeout capability toolbox = do
    let timeoutMicros = defaultTimeoutSeconds * 1000000
    result <- race (threadDelay timeoutMicros) (getCapabilityInfoInternal capability toolbox)
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
    IO (Either Text (Text, Aeson.Value))
getCapabilityInfoInternal capability toolbox = do
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
        pure (name, value)
    case result of
        Left (e :: SomeException) -> pure $ Left $ Text.pack $ show e
        Right val -> pure $ Right val

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
    let toolbox = Toolbox "" "" [capability] Nothing
    result <- getCapabilityInfoWithTimeout capability toolbox
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
