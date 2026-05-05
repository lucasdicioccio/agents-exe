{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{- | Session storage management with multi-location support.

This module provides functionality to store and list sessions
using configurable file prefixes. Sessions can be read from multiple
locations while always writing to a single designated location.

The file naming pattern is @conv.<uuid>.json@.

Multi-location support allows:
- Reading sessions from multiple directories (e.g., project-local, global)
- Writing all new sessions to a single unified location
- Deduplication by ConversationId with priority based on location order
-}
module System.Agents.SessionStore (
    -- * Session Store
    SessionStore (..),
    defaultSessionStore,
    mkSessionStore,
    mkSimpleSessionStore,

    -- * Path resolution
    resolveSessionPath,

    -- * Low-level operations, mostly to implement command-line bypasses
    readSessionFromFile,
    storeSessionToFile,

    -- * File path operations
    sessionFilePath,
    sessionWritePath,

    -- * Session storage operations
    storeSession,
    readSession,

    -- * Listing sessions
    listSessions,
    SessionFileInfo (..),
    findSessionFiles,
    isSessionFile,
) where

import Control.Exception (IOException, try)
import Control.Monad (filterM)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.List (foldl', isInfixOf, isPrefixOf, sortOn)
import Data.Ord (Down (..))
import qualified Data.Text as Text
import Data.Time (UTCTime)
import System.Directory (doesFileExist, getHomeDirectory, getModificationTime, listDirectory)
import System.FilePath (takeFileName, (</>))
import System.IO.Error (ioeGetErrorString)

import System.Agents.Base (ConversationId (..))
import System.Agents.Session.Base (Session (..))

-------------------------------------------------------------------------------
-- Session Store Configuration
-------------------------------------------------------------------------------

{- | The default session file pattern prefix.
Files are named: @conv.<uuid>.json@
-}
defaultSessionPattern :: String
defaultSessionPattern = "conv."

-- | The default session file suffix.
defaultSessionSuffix :: String
defaultSessionSuffix = ".json"

{- | Session store with multi-location support.

The store supports reading from multiple locations while writing
to a single designated location. This enables:

1. Unified view of sessions scattered across directories
2. Project-local sessions overriding global ones
3. Archived or shared sessions in separate directories

Deduplication is done by ConversationId, with the first location
in 'sessionReadPrefixes' having the highest priority.
-}
data SessionStore = SessionStore
    { sessionWritePrefix :: FilePath
    -- ^ Directory where new sessions are written.
    -- This path is used for all write operations.
    , sessionReadPrefixes :: [FilePath]
    -- ^ Directories to search for existing sessions, in priority order.
    -- Earlier locations take precedence when the same session ID exists
    -- in multiple locations.
    }
    deriving (Show, Eq)

-- | Create an ineffective session store (no read or write locations).
defaultSessionStore :: SessionStore
defaultSessionStore = SessionStore "" []

{- | Create a new session store with separate write and read locations.

If the write location is not in the read locations list, it is automatically
prepended to ensure newly written sessions are immediately readable.

Example:

> mkSessionStore "./sessions/" ["./sessions/", "~/.config/agents-exe/sessions/"]
-}
mkSessionStore :: FilePath -> [FilePath] -> SessionStore
mkSessionStore writePrefix readPrefixes =
    SessionStore
        { sessionWritePrefix = writePrefix
        , sessionReadPrefixes =
            if writePrefix `elem` readPrefixes
                then readPrefixes
                else writePrefix : readPrefixes
        }

{- | Create a simple session store with a single location for both reading and writing.

This is the backwards-compatible constructor for single-location stores.

Example:

> mkSimpleSessionStore "./sessions/"
-}
mkSimpleSessionStore :: FilePath -> SessionStore
mkSimpleSessionStore prefix = mkSessionStore prefix [prefix]

-------------------------------------------------------------------------------
-- Path Resolution
-------------------------------------------------------------------------------

{- | Resolve a session path, expanding tilde (~) to the user's home directory.

Examples:

> resolveSessionPath "~/.config/agents-exe/sessions/"
> -- Returns: "/home/user/.config/agents-exe/sessions/"

> resolveSessionPath "./sessions/"
> -- Returns: "./sessions/" (unchanged)
-}
resolveSessionPath :: FilePath -> IO FilePath
resolveSessionPath path =
    if "~" `isPrefixOf` path
        then do
            home <- getHomeDirectory
            pure $ home ++ drop 1 path
        else pure path

{- | Resolve all paths in a SessionStore.

This expands tilde (~) prefixes in both write and read locations.
-}
resolveSessionStorePaths :: SessionStore -> IO SessionStore
resolveSessionStorePaths store = do
    writePath <- resolveSessionPath store.sessionWritePrefix
    readPaths <- mapM resolveSessionPath store.sessionReadPrefixes
    pure $ SessionStore writePath readPaths

-------------------------------------------------------------------------------
-- File Path Operations
-------------------------------------------------------------------------------

{- | Generate the file path for a session given a ConversationId.
The pattern is: @<prefix>conv.<uuid>.json@

Note: This uses the first read prefix to construct the path.
For write operations, use 'sessionWritePath' instead.
-}
sessionFilePath :: SessionStore -> ConversationId -> FilePath
sessionFilePath store convId =
    case sessionReadPrefixes store of
        [] -> conversationIdToFileName convId
        (prefix : _) -> prefix </> conversationIdToFileName convId

{- | Generate the write file path for a session.

This always uses the 'sessionWritePrefix' location, ensuring
new sessions are written to the correct directory.
-}
sessionWritePath :: SessionStore -> ConversationId -> FilePath
sessionWritePath store convId =
    store.sessionWritePrefix </> conversationIdToFileName convId

-- | Convert a ConversationId to a file name using the default pattern.
conversationIdToFileName :: ConversationId -> FilePath
conversationIdToFileName (ConversationId cid) =
    defaultSessionPattern ++ show cid ++ defaultSessionSuffix

{- | Parse a ConversationId from a file name using the default pattern.
Returns 'Nothing' if the file name doesn't match the expected pattern.
-}
parseConversationIdFromFileName :: FilePath -> Maybe ConversationId
parseConversationIdFromFileName name =
    let baseName = takeFileName name
        prefixLen = length defaultSessionPattern
        suffixLen = length defaultSessionSuffix
     in if defaultSessionPattern `isPrefixOf` baseName
            && defaultSessionSuffix `isSuffixOf` baseName
            then
                let uuidStr =
                        take (length baseName - prefixLen - suffixLen) $
                            drop prefixLen baseName
                 in case reads uuidStr of
                        [(uuid, "")] -> Just (ConversationId uuid)
                        _ -> Nothing
            else Nothing
  where
    isSuffixOf suffix str = reverse suffix `isInfixOf` reverse str

-------------------------------------------------------------------------------
-- Low-level File Operations
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

{- | Read a session from a file path.
Returns 'Nothing' if:
- The file doesn't exist
- The file is locked (resource busy)
- The file can't be parsed

Note: This function forces strict evaluation of the file content to ensure
the file handle is closed promptly, preventing "resource busy" errors when
listing sessions concurrently.
-}
readSessionFromFile :: FilePath -> IO (Maybe Session)
readSessionFromFile path = do
    fileExists <- doesFileExist path
    if fileExists
        then do
            -- Try to read the file, handling locked files gracefully
            result <- try $ BSL.readFile path
            case result of
                Left (e :: IOException)
                    | isResourceBusyError e -> pure Nothing -- File is locked, return Nothing
                    | otherwise -> pure Nothing -- Other IO errors, return Nothing
                Right dat ->
                    -- Force strict evaluation by using `seq` on the length.
                    -- This ensures the file handle is closed before we return,
                    -- preventing "resource busy" errors when listing sessions.
                    LByteString.length dat `seq`
                        pure $
                            Aeson.decode =<< lastLine dat
        else do
            pure Nothing
  where
    lastLine :: LByteString.ByteString -> Maybe LByteString.ByteString
    lastLine dat = case BSL.lines dat of [] -> Nothing; rows -> Just (last rows)

-- | Store a session to a file path.
storeSessionToFile :: Session -> FilePath -> IO ()
storeSessionToFile sess path = do
    BSL.writeFile path (Aeson.encode sess <> "\n")

-------------------------------------------------------------------------------
-- Session Storage Operations
-------------------------------------------------------------------------------

-- | Store a session to disk at the appropriate path for the given ConversationId.
-- Always writes to the 'sessionWritePrefix' location.
storeSession :: SessionStore -> ConversationId -> Session -> IO ()
storeSession store convId sess =
    storeSessionToFile sess (sessionWritePath store convId)

{- | Read a session from disk for the given ConversationId.

Searches through all read locations in priority order.
Returns 'Nothing' if the session file doesn't exist in any location,
is locked, or can't be parsed.
-}
readSession :: SessionStore -> ConversationId -> IO (Maybe Session)
readSession store convId = go (sessionReadPrefixes store)
  where
    go [] = pure Nothing
    go (prefix : rest) = do
        let path = prefix </> conversationIdToFileName convId
        mSession <- readSessionFromFile path
        case mSession of
            Just session -> pure (Just session)
            Nothing -> go rest

-------------------------------------------------------------------------------
-- Session File Discovery
-------------------------------------------------------------------------------

-- | Information about a discovered session file.
data SessionFileInfo = SessionFileInfo
    { sessionInfoPath :: FilePath
    -- ^ Full path to the session file
    , sessionInfoModTime :: UTCTime
    -- ^ Last modification time of the file
    , sessionInfoConversationId :: ConversationId
    -- ^ The ConversationId extracted from the file name
    }
    deriving (Show, Eq)

{- | Check if a filename matches the session file pattern.
Pattern: @conv.<uuid>.json@
-}
isSessionFile :: String -> Bool
isSessionFile name =
    let baseName = takeFileName name
     in defaultSessionPattern `isPrefixOf` baseName
            && defaultSessionSuffix `isSuffixOf` baseName
  where
    isSuffixOf suffix str = reverse suffix `isInfixOf` reverse str

{- | Find all session files in a single directory.
Returns files matching the @conv.<uuid>.json@ pattern with their metadata.
-}
findSessionsInDir :: FilePath -> IO [SessionFileInfo]
findSessionsInDir dir = do
    exists <- doesFileExist dir
    if exists
        then -- If the path is a file, return empty list
            pure []
        else do
            entries <- listDirectory dir
            let candidates = [dir </> entry | entry <- entries, isSessionFile entry]
            -- Filter to only existing files and get modification times
            existing <- filterM doesFileExist candidates
            -- Build session info, handling locked files gracefully
            catMaybes <$> mapM mkSessionInfo existing
  where
    mkSessionInfo :: FilePath -> IO (Maybe SessionFileInfo)
    mkSessionInfo path = do
        -- Try to get modification time, handling locked files
        mtimeResult <- try $ getModificationTime path
        case mtimeResult of
            Left (_ :: IOException) -> do
                -- File is locked or inaccessible, skip it
                pure Nothing
            Right mtime ->
                case parseConversationIdFromFileName (takeFileName path) of
                    Just cid -> pure $ Just $ SessionFileInfo path mtime cid
                    Nothing -> error $ "Unexpected: file passed isSessionFile but failed parse: " ++ path

    catMaybes :: [Maybe a] -> [a]
    catMaybes = foldr (maybe id (:)) []

{- | Find all session files across all read locations.

This function:
1. Collects sessions from all read locations
2. Deduplicates by ConversationId (first location wins)
3. Returns sessions sorted by modification time (most recent first)

The deduplication strategy ensures that if the same session exists
in multiple locations, the one from the earliest location in
'sessionReadPrefixes' is kept.
-}
findSessionFiles :: SessionStore -> IO [SessionFileInfo]
findSessionFiles store = do
    -- Resolve all paths first (expand tildes)
    resolvedStore <- resolveSessionStorePaths store

    -- Collect sessions from all read locations, preserving order
    allSessions <- concat <$> mapM findSessionsInDir resolvedStore.sessionReadPrefixes

    -- Deduplicate by ConversationId, keeping first occurrence (highest priority)
    let deduplicated = dedupeBy sessionInfoConversationId allSessions

    -- Sort by modification time (most recent first)
    pure $ sortOn (Down . sessionInfoModTime) deduplicated

{- | Deduplicate a list by a key function, keeping the first occurrence.

The first occurrence of each key is kept, subsequent duplicates are discarded.
This maintains the priority order of the input list.
-}
dedupeBy :: Eq k => (a -> k) -> [a] -> [a]
dedupeBy keyFn = foldl' addItem []
  where
    addItem acc item =
        let k = keyFn item
         in if k `elem` map keyFn acc
                then acc
                else acc ++ [item]

{- | List all sessions from files matching the store's prefix pattern.
Returns a list of @(FilePath, Maybe Session, ConversationId)@ triples,
sorted by file modification time (most recent first).

The 'Maybe Session' is 'Nothing' if:
- The session file couldn't be parsed
- The session file is locked (resource busy)
- The session file is inaccessible

This function aggregates sessions from all read locations and deduplicates
by ConversationId. The first location in 'sessionReadPrefixes' has the
highest priority for resolving duplicates.
-}
listSessions :: SessionStore -> IO [(FilePath, Maybe Session, ConversationId)]
listSessions store = do
    sessionFiles <- findSessionFiles store
    -- Load each session file (locked/inaccessible files will return Nothing)
    mapM (\info -> (sessionInfoPath info,,sessionInfoConversationId info) <$> readSessionFromFile (sessionInfoPath info)) sessionFiles

