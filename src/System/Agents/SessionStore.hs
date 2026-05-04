{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{- | Session storage management with configurable file prefix.

This module provides functionality to store and list sessions
using a configurable file prefix and a default naming pattern
of @conv.<uuid>.json@.
-}
module System.Agents.SessionStore (
    -- * Session Store
    SessionStore (..),
    defaultSessionStore,
    mkSessionStore,

    -- * Low-level operations, mostly to implement command-line bypasses
    readSessionFromFile,
    storeSessionToFile,

    -- * File path operations
    sessionFilePath,

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
import Data.List (isInfixOf, isPrefixOf, sortOn)
import Data.Ord (Down (..))
import qualified Data.Text as Text
import Data.Time (UTCTime)
import System.Directory (doesFileExist, getModificationTime, listDirectory)
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

-- | Session store with configurable prefix filepath.
data SessionStore = SessionStore
    { storePrefix :: FilePath
    {- ^ The prefix for session files. This can be a directory path
    or a path prefix. The directory component is extracted to find
    sessions, and the file component is used as a prefix before
    the default pattern.
    -}
    }
    deriving (Show, Eq)

-- | Create a ineffective session store.
defaultSessionStore :: SessionStore
defaultSessionStore = SessionStore ""

-- | Create a new session store with the given prefix.
mkSessionStore :: FilePath -> SessionStore
mkSessionStore = SessionStore

-------------------------------------------------------------------------------
-- File Path Operations
-------------------------------------------------------------------------------

{- | Generate the file path for a session given a ConversationId.
The pattern is: @<prefix>conv.<uuid>.json@
-}
sessionFilePath :: SessionStore -> ConversationId -> FilePath
sessionFilePath store convId =
    store.storePrefix ++ conversationIdToFileName convId

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
storeSession :: SessionStore -> ConversationId -> Session -> IO ()
storeSession store convId sess =
    storeSessionToFile sess (sessionFilePath store convId)

{- | Read a session from disk for the given ConversationId.
Returns 'Nothing' if the session file doesn't exist, is locked, or can't be parsed.
-}
readSession :: SessionStore -> ConversationId -> IO (Maybe Session)
readSession store convId =
    readSessionFromFile (sessionFilePath store convId)

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

{- | Get the directory component from a prefix path.
If the prefix contains path separators, returns the directory part.
Otherwise, returns the current directory @".@".
-}
getDirectoryFromPrefix :: FilePath -> FilePath
getDirectoryFromPrefix prefix =
    if '/' `elem` prefix || '\\' `elem` prefix
        then case reverse $ dropWhile (\c -> c /= '/' && c /= '\\') $ reverse prefix of
            "" -> "."
            d -> d
        else "."

{- | Find all session files in the directory implied by the store prefix.
Returns files matching the @conv.<uuid>.json@ pattern, sorted by
modification time (most recent first).

Note: This function uses getModificationTime which may fail for locked files.
We handle this by catching exceptions and skipping locked/inaccessible files.
-}
findSessionFiles :: SessionStore -> IO [SessionFileInfo]
findSessionFiles store = do
    let dir = getDirectoryFromPrefix store.storePrefix
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

{- | List all sessions from files matching the store's prefix pattern.
Returns a list of @(FilePath, Maybe Session, ConversationId)@ triples,
sorted by file modification time (most recent first).

The 'Maybe Session' is 'Nothing' if:
- The session file couldn't be parsed
- The session file is locked (resource busy)
- The session file is inaccessible
-}
listSessions :: SessionStore -> IO [(FilePath, Maybe Session, ConversationId)]
listSessions store = do
    sessionFiles <- findSessionFiles store
    -- Sort by modification time (most recent first)
    let sortedFiles = sortOn (Data.Ord.Down . sessionInfoModTime) sessionFiles
    -- Load each session file (locked/inaccessible files will return Nothing)
    mapM (\info -> (sessionInfoPath info,,sessionInfoConversationId info) <$> readSessionFromFile (sessionInfoPath info)) sortedFiles
