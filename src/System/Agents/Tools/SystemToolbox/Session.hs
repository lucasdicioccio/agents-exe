{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Session introspection capabilities.

This module implements the session introspection functionality:
* list-sessions - List accessible sessions
* search-sessions - Full-text search across sessions
* read-session - Read session content with slicing
* get-session-stats - Get aggregate session statistics
-}
module System.Agents.Tools.SystemToolbox.Session (
    -- * Session introspection capabilities
    getListSessionsInfo,
    getSearchSessionsInfo,
    getReadSessionInfo,
    getSessionStatsInfo,

    -- * Utility functions
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
    formatTurnCondensed,
    truncateText,
    extractToolCallName,
    isSessionFileLocked,
    isResourceBusyError,
    getSessionModTime,
    turnContentText,
    turnMatchesTerm,
    termMatchesSession,
    sessionMatchesSearch,
    findTermPositions,
    generateSessionPreview,
) where

import Control.Exception (IOException, SomeException, bracket, try)
import Control.Monad (forM)
import Data.Aeson (Value (..), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Data.UUID as UUID
import System.Directory (getModificationTime)
import System.IO (IOMode (..), hClose, openBinaryFile)
import System.IO.Error (ioeGetErrorString)

import System.Agents.Base (
    ConversationId (..),
    SessionIntrospectionScope (..),
 )
import System.Agents.Session.Types (
    LlmResponse (..),
    LlmToolCall (..),
    LlmTurnContent (..),
    PartialUserTurnContent (..),
    Session (..),
    SessionId (..),
    SystemPrompt (..),
    Turn (..),
    UserQuery (..),
    UserTurnContent (..),
 )
import qualified System.Agents.SessionStore as SessionStore
import System.Agents.Tools.SystemToolbox.Types (
    ReadSessionParams (..),
    SessionIntrospectionConfig (..),
 )

-------------------------------------------------------------------------------
-- List Sessions
-------------------------------------------------------------------------------

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

                let turnCount = maybe 0 (length . (.turns)) mSession
                let isParent = isParentSession config mSession
                let isChild = isChildSession config mSession

                pure $
                    Aeson.object
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

-------------------------------------------------------------------------------
-- Search Sessions
-------------------------------------------------------------------------------

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
                let turnCount = maybe 0 (length . (.turns)) mSession
                let preview = generateSessionPreview mSession searchTerms

                pure $
                    Aeson.object
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
termMatchesSession sess term =
    any (turnMatchesTerm term) sess.turns

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
        Just sess ->
            let allContent = Text.intercalate " " $ map turnContentText (take 3 sess.turns)
                lowerContent = Text.toLower allContent
                -- Find first matching term and extract context around it
                matchPositions = concatMap (\term -> findTermPositions term lowerContent 0) searchTerms
             in case matchPositions of
                    [] -> Text.take 200 allContent <> "..."
                    (pos : _) ->
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

-------------------------------------------------------------------------------
-- Read Session
-------------------------------------------------------------------------------

{- | Read session content and return condensed text format.

Parameters:
- take_n: Take last N turns
- drop_n: Drop first N turns
- offset: Starting turn index (alternative to drop_n)
- limit: Max turns to return (alternative to take_n)
- include_thinking: Include LLM thinking content (default: false)
- include_tool_responses: Include tool call responses (default: false)

Returns condensed text format suitable for LLM consumption.
-}
getReadSessionInfo :: Maybe SessionIntrospectionConfig -> Maybe Text -> ReadSessionParams -> IO (Text, Aeson.Value)
getReadSessionInfo Nothing _ _ = pure ("read-session", String "Session store not configured")
getReadSessionInfo (Just _config) Nothing _ =
    pure ("read-session", Aeson.object ["error" .= ("Missing 'session_id' parameter for read-session" :: Text)])
getReadSessionInfo (Just config) (Just sessionIdText) params = do
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
                    pure
                        ( "read-session"
                        , Aeson.object
                            [ "error" .= ("Session not found or is currently locked: " <> sessionIdText)
                            , "sessionId" .= sessionIdText
                            ]
                        )
                Just sess -> do
                    -- Check scope access
                    if canAccessSession config sess
                        then do
                            -- Apply slicing to get the desired turns
                            let slicedTurns = applySlicing params sess.turns

                            -- Format turns as condensed text
                            let formattedText = formatSessionAsCondensedText params slicedTurns

                            -- Return as JSON with text content and metadata
                            pure
                                ( "read-session"
                                , Aeson.object
                                    [ "sessionId" .= sessionIdText
                                    , "conversationId" .= conversationIdToText targetConvId
                                    , "totalTurns" .= length sess.turns
                                    , "returnedTurns" .= length slicedTurns
                                    , "content" .= formattedText
                                    , "format" .= ("condensed-text" :: Text)
                                    , "scope" .= showScope (introspectionScope config)
                                    , "access" .= ("granted" :: Text)
                                    ]
                                )
                        else do
                            pure
                                ( "read-session"
                                , Aeson.object
                                    [ "error" .= ("Access denied: session is outside configured scope" :: Text)
                                    , "sessionId" .= sessionIdText
                                    , "scope" .= showScope (introspectionScope config)
                                    , "access" .= ("denied" :: Text)
                                    ]
                                )

{- | Apply slicing parameters to a list of turns.
Sessions store turns newest-first (most recent turn is first in the list).
For display purposes, we want chronological order (oldest first).
-}
applySlicing :: ReadSessionParams -> [Turn] -> [Turn]
applySlicing params turnList =
    let
        -- Reverse to get chronological order (oldest first)
        chronological = reverse turnList

        -- Apply drop_n or offset
        afterDrop = case (rspDropN params, rspOffset params) of
            (Just n, _) -> drop n chronological
            (_, Just off) -> drop off chronological
            _ -> chronological

        -- Apply take_n or limit
        afterTake = case (rspTakeN params, rspLimit params) of
            (Just n, _) -> takeLast n afterDrop
            (_, Just lim) -> take lim afterDrop
            _ -> afterDrop
     in
        afterTake
  where
    -- Take the last n elements from a list
    takeLast :: Int -> [a] -> [a]
    takeLast n xs = drop (max 0 (length xs - n)) xs

-- | Format a list of turns as condensed text suitable for LLM consumption.
formatSessionAsCondensedText :: ReadSessionParams -> [Turn] -> Text
formatSessionAsCondensedText params turnList =
    Text.intercalate "\n\n" $ zipWith (formatTurnCondensed params) [1 ..] turnList

-- | Format a single turn in condensed format.
formatTurnCondensed :: ReadSessionParams -> Int -> Turn -> Text
formatTurnCondensed params turnNum turn = case turn of
    UserTurn content _ ->
        let
            -- User query
            queryPart = case userQuery content of
                Just (UserQuery q _) -> "[User] " <> truncateText 500 q
                Nothing -> "[User] (no query)"

            -- Tool calls made by user (responses to LLM tool calls)
            toolRespPart =
                if rspIncludeToolResponses params && not (null (userToolResponses content))
                    then
                        let toolCount = length (userToolResponses content)
                            toolNames = map (extractToolCallName . fst) (userToolResponses content)
                            toolSummary = Text.intercalate ", " toolNames
                         in "\n  [Tool Responses: " <> Text.pack (show toolCount) <> " tools - " <> toolSummary <> "]"
                    else ""
         in
            "Turn " <> Text.pack (show turnNum) <> ": " <> queryPart <> toolRespPart
    LlmTurn content _ ->
        let
            -- LLM response
            respPart = case responseText (llmResponse content) of
                Just txt -> "[LLM] " <> truncateText 500 txt
                Nothing -> "[LLM] (no response)"

            -- Thinking/reasoning content
            thinkingPart =
                if rspIncludeThinking params
                    then case responseThinking (llmResponse content) of
                        Just thinking -> "\n  [Thinking] " <> truncateText 300 thinking
                        Nothing -> ""
                    else ""

            -- Tool calls requested by LLM
            toolCallPart =
                if not (null (llmToolCalls content))
                    then
                        let toolCount = length (llmToolCalls content)
                            toolNames = map extractToolCallName (llmToolCalls content)
                            toolSummary = Text.intercalate ", " toolNames
                         in "\n  [Tool Calls: " <> Text.pack (show toolCount) <> " - " <> toolSummary <> "]"
                    else ""
         in
            "Turn " <> Text.pack (show turnNum) <> ": " <> respPart <> thinkingPart <> toolCallPart
    PartialUserTurn _ _ ->
        "Turn " <> Text.pack (show turnNum) <> ": [Partial/In Progress]"

-- | Truncate text to a maximum length with ellipsis.
truncateText :: Int -> Text -> Text
truncateText maxLen txt
    | Text.length txt <= maxLen = txt
    | otherwise = Text.take maxLen txt <> "..."

-- | Extract tool name from a tool call.
extractToolCallName :: LlmToolCall -> Text
extractToolCallName (LlmToolCall val) =
    case val of
        Aeson.Object obj ->
            case KeyMap.lookup "function" obj of
                Just (Aeson.Object funcObj) ->
                    case KeyMap.lookup "name" funcObj of
                        Just (Aeson.String toolName) -> toolName
                        _ -> "(unnamed)"
                _ -> case KeyMap.lookup "name" obj of
                    Just (Aeson.String toolName) -> toolName
                    _ -> "(unnamed)"
        _ -> "(unnamed)"

-------------------------------------------------------------------------------
-- Session Stats
-------------------------------------------------------------------------------

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
            pure $
                ( "get-session-stats"
                , Aeson.object
                    [ "totalSessions" .= (0 :: Int)
                    , "totalTurnsAcrossAllSessions" .= (0 :: Int)
                    , "scope" .= showScope (introspectionScope config)
                    , "note" .= ("Could not access session store" :: Text)
                    ]
                )
        Right allSessions -> do
            let accessibleSessions = filterSessionsByScope config allSessions

            -- Calculate aggregate stats, handling Nothing sessions (locked/unreadable)
            let totalTurns = sum [maybe 0 (length . (.turns)) mSession | (_, mSession, _) <- accessibleSessions]
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

-------------------------------------------------------------------------------
-- Utility Functions
-------------------------------------------------------------------------------

-- | Parse a SessionId from text
parseSessionId :: Text -> Maybe SessionId
parseSessionId txt =
    case UUID.fromText txt of
        Just uuid -> Just $ SessionId uuid
        Nothing -> Nothing

-- | Convert a SessionId to a ConversationId
sessionIdToConversationId :: SessionId -> ConversationId
sessionIdToConversationId (SessionId uuid) = ConversationId uuid

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
        (Just currentForkedFrom, Just sess) ->
            sess.sessionId == currentForkedFrom
        _ -> False

-- | Check if a session is a child (descendant) of the current session
isChildSession :: SessionIntrospectionConfig -> Maybe Session -> Bool
isChildSession config mSession =
    case (introspectionCurrentSessionId config, mSession) of
        (Just currentId, Just sess) ->
            sess.forkedFromSessionId == Just currentId
        _ -> False

-- | Check if a session is the current session
isCurrentSession :: SessionIntrospectionConfig -> Maybe Session -> Bool
isCurrentSession config mSession =
    case (introspectionCurrentSessionId config, mSession) of
        (Just currentId, Just sess) ->
            sess.sessionId == currentId
        _ -> False

-- | Helper to get session modification time
getSessionModTime :: SessionStore.SessionStore -> ConversationId -> IO (Maybe Time.UTCTime)
getSessionModTime store convId = do
    let path = SessionStore.sessionFilePath store convId
    result <- try $ getModificationTime path
    case result of
        Left (_ :: SomeException) -> pure Nothing
        Right mtime -> pure $ Just mtime

-- | Helper to convert ConversationId to Text
conversationIdToText :: ConversationId -> Text
conversationIdToText (ConversationId uuid) = UUID.toText uuid

-- | Check if a session can be accessed based on the configured scope
canAccessSession :: SessionIntrospectionConfig -> Session -> Bool
canAccessSession config sess =
    case introspectionScope config of
        ScopeAll -> True
        ScopeParentsOnly -> isParentSession config (Just sess)
        ScopeChildrenOnly -> isChildSession config (Just sess)
        ScopeSubtree ->
            isParentSession config (Just sess)
                || isChildSession config (Just sess)
                || isCurrentSession config (Just sess)

