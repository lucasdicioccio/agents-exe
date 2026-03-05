{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}

-- | Session storage management with configurable file prefix.
--
-- This module provides functionality to store and list sessions
-- using a configurable file prefix and a default naming pattern
-- of @conv.<uuid>.json@.
module System.Agents.SessionStore (
    -- * Session Store
    SessionStore (..),
    defaultSessionStore,
    mkSessionStore,

    -- * Session storage operations
    storeSession,
    readSession,

    -- * Listing sessions
    listSessions,
    SessionFileInfo (..),
    findSessionFiles,
    isSessionFile,
) where

import Control.Monad (filterM)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.List (isPrefixOf, sortOn)
import Data.Ord (Down (..))
import Data.Time (UTCTime)
import System.Directory (doesFileExist, getModificationTime, listDirectory)
import System.FilePath ((</>), takeFileName)

import System.Agents.Base (ConversationId (..))
import System.Agents.Session.Base (Session (..), newSessionId, newTurnId)

-------------------------------------------------------------------------------
-- Session Store Configuration
-------------------------------------------------------------------------------

-- | The default session file pattern prefix.
-- Files are named: @conv.<uuid>.json@
defaultSessionPattern :: String
defaultSessionPattern = "conv."

-- | The default session file suffix.
defaultSessionSuffix :: String
defaultSessionSuffix = ".json"

-- | Session store with configurable prefix filepath.
data SessionStore = SessionStore
    { storePrefix :: FilePath
    -- ^ The prefix for session files. This can be a directory path
    -- or a path prefix. The directory component is extracted to find
    -- sessions, and the file component is used as a prefix before
    -- the default pattern.
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

-- | Generate the file path for a session given a ConversationId.
-- The pattern is: @<prefix>conv.<uuid>.json@
sessionFilePath :: SessionStore -> ConversationId -> FilePath
sessionFilePath store convId =
    store.storePrefix ++ conversationIdToFileName convId

-- | Convert a ConversationId to a file name using the default pattern.
conversationIdToFileName :: ConversationId -> FilePath
conversationIdToFileName (ConversationId cid) =
    defaultSessionPattern ++ show cid ++ defaultSessionSuffix

-- | Parse a ConversationId from a file name using the default pattern.
-- Returns 'Nothing' if the file name doesn't match the expected pattern.
parseConversationIdFromFileName :: FilePath -> Maybe ConversationId
parseConversationIdFromFileName name =
    let baseName = takeFileName name
        prefixLen = length defaultSessionPattern
        suffixLen = length defaultSessionSuffix
    in if defaultSessionPattern `isPrefixOf` baseName &&
           defaultSessionSuffix `isSuffixOf` baseName
       then
           let uuidStr = take (length baseName - prefixLen - suffixLen) $
                         drop prefixLen baseName
           in case reads uuidStr of
                [(uuid, "")] -> Just (ConversationId uuid)
                _ -> Nothing
       else Nothing
  where
    isSuffixOf suffix str = reverse suffix `isPrefixOf` reverse str

-------------------------------------------------------------------------------
-- Low-level File Operations
-------------------------------------------------------------------------------

-- | Read a session from a file path.
-- Returns 'Nothing' if the file doesn't exist or can't be parsed.
readSessionFromFile :: FilePath -> IO (Maybe Session)
readSessionFromFile path = do
  fileExists <- doesFileExist path
  if fileExists
    then do
       dat <- BSL.readFile path
       pure $ Aeson.decode =<< lastLine dat
    else do
       -- Create a new empty session with all required fields
       sess <- Session [] <$> newSessionId <*> pure Nothing <*> newTurnId
       pure $ Just sess
  where
    lastLine :: LByteString.ByteString -> Maybe LByteString.ByteString
    lastLine dat = case BSL.lines dat of [] -> Nothing ; rows -> Just (last rows)

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

-- | Read a session from disk for the given ConversationId.
-- Returns 'Nothing' if the session file doesn't exist or can't be parsed.
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

-- | Check if a filename matches the session file pattern.
-- Pattern: @conv.<uuid>.json@
isSessionFile :: String -> Bool
isSessionFile name =
    let baseName = takeFileName name
    in defaultSessionPattern `isPrefixOf` baseName &&
       defaultSessionSuffix `isSuffixOf` baseName
  where
    isSuffixOf suffix str = reverse suffix `isPrefixOf` reverse str

-- | Get the directory component from a prefix path.
-- If the prefix contains path separators, returns the directory part.
-- Otherwise, returns the current directory @"."@.
getDirectoryFromPrefix :: FilePath -> FilePath
getDirectoryFromPrefix prefix =
    if '/' `elem` prefix || '\\' `elem` prefix
        then case reverse $ dropWhile (\c -> c /= '/' && c /= '\\') $ reverse prefix of
               "" -> "."
               d -> d
        else "."

-- | Find all session files in the directory implied by the store prefix.
-- Returns files matching the @conv.<uuid>.json@ pattern, sorted by
-- modification time (most recent first).
findSessionFiles :: SessionStore -> IO [SessionFileInfo]
findSessionFiles store = do
    let dir = getDirectoryFromPrefix store.storePrefix
    entries <- listDirectory dir
    let candidates = [dir </> entry | entry <- entries, isSessionFile entry]
    -- Filter to only existing files and get modification times
    existing <- filterM doesFileExist candidates
    mapM mkSessionInfo existing
  where
    mkSessionInfo path = do
        mtime <- getModificationTime path
        case parseConversationIdFromFileName (takeFileName path) of
            Just cid -> pure $ SessionFileInfo path mtime cid
            Nothing -> error $ "Unexpected: file passed isSessionFile but failed parse: " ++ path

-- | List all sessions from files matching the store's prefix pattern.
-- Returns a list of @(FilePath, Maybe Session, ConversationId)@ triples,
-- sorted by file modification time (most recent first).
--
-- The 'Maybe Session' is 'Nothing' if the session file couldn't be parsed.
listSessions :: SessionStore -> IO [(FilePath, Maybe Session, ConversationId)]
listSessions store = do
    sessionFiles <- findSessionFiles store
    -- Sort by modification time (most recent first)
    let sortedFiles = sortOn (Data.Ord.Down . sessionInfoModTime) sessionFiles
    -- Load each session file
    mapM (\info -> (sessionInfoPath info,,sessionInfoConversationId info) <$> readSessionFromFile (sessionInfoPath info)) sortedFiles

