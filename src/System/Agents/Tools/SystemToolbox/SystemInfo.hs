{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

{- | System information gathering capabilities.

This module implements the various system info gathering capabilities:
* date - Current date/time information
* operating-system - OS information from /etc/os-release
* env-vars - Environment variables
* running-user - User information (UID, GID, home, shell)
* hostname - System hostname
* working-directory - Current working directory
* process-info - Process ID and parent process ID
* uptime - System uptime from /proc/uptime
-}
module System.Agents.Tools.SystemToolbox.SystemInfo (
    -- * System info gathering
    getDateInfo,
    getOperatingSystemInfo,
    getEnvVarsInfo,
    getRunningUserInfo,
    getHostnameInfo,
    getWorkingDirectoryInfo,
    getProcessInfo,
    getUptimeInfo,

    -- * Utility functions
    tryReadFile,
    safeReadProcess,
) where

import Control.Exception (SomeException, try)
import Data.Aeson (Value (..), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.List (isInfixOf)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Time as Time
import System.Directory (getCurrentDirectory)
import System.Environment (getEnvironment, lookupEnv)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Process (getParentProcessID, getProcessID)
import System.Posix.User (getEffectiveGroupID, getEffectiveUserID, getUserEntryForID, homeDirectory, userName, userShell)
import qualified System.Process as Process

-------------------------------------------------------------------------------
-- Date/Time
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

-------------------------------------------------------------------------------
-- Operating System
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
-- Environment Variables
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
-- Running User
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
-- Hostname
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
-- Working Directory
-------------------------------------------------------------------------------

-- | Gather working directory information.
getWorkingDirectoryInfo :: IO (Text, Aeson.Value)
getWorkingDirectoryInfo = do
    cwd <- getCurrentDirectory
    pure ("working-directory", Aeson.object ["path" .= Text.pack cwd])

-------------------------------------------------------------------------------
-- Process Info
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
-- Uptime
-------------------------------------------------------------------------------

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

