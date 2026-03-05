{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Module for printing session files in markdown format.
--
-- This module provides functionality to load and format session JSON files
-- as readable markdown output. It is used by the @session-print@ CLI command.
module System.Agents.SessionPrint
    ( -- * Options
      SessionPrintOptions (..)
    , OrderPreference (..)
      -- * Main handler
    , handleSessionPrint
      -- * Formatting functions
    , formatSessionAsMarkdown
    ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.IO (stderr)

import qualified System.Agents.Session.Base as Session

-- | Preference for ordering session steps.
data OrderPreference
    = Chronological       -- ^ Oldest first (earlier steps shown first)
    | Antichronological   -- ^ Newest first (recent steps shown first)
    deriving (Show, Eq)

-- | Options for controlling the session print output.
data SessionPrintOptions = SessionPrintOptions
    { -- | Path to the session JSON file to print
      sessionPrintFile :: FilePath
      -- | Whether to show tool call results in the output
    , showToolCallResults :: Bool
      -- | Optional limit on the number of turns to display
    , nTurns :: Maybe Int
      -- | Whether to repeat the system prompt at each turn
    , repeatSystemPrompt :: Bool
      -- | Whether to repeat the available tools at each turn
    , repeatTools :: Bool
      -- | Order preference for displaying session steps
    , orderPreference :: OrderPreference
    }

-- | Handle the session-print command: load a session file and output it as markdown.
handleSessionPrint :: SessionPrintOptions -> IO ()
handleSessionPrint opts = do
    result <- Aeson.eitherDecodeFileStrict' opts.sessionPrintFile
    case result of
        Left err -> do
            Text.hPutStrLn stderr $ "Error loading session file: " <> Text.pack err
        Right session -> do
            let markdown = formatSessionAsMarkdown opts session
            Text.putStr markdown

-- | Format a Session as markdown text.
--
-- The logic for processing steps is:
-- 1. First, number the steps (zip with [1..]) - preserves original step numbers
-- 2. Then, cut the n-latest steps if n-turns limit is specified
-- 3. Then, reverse or not depending on the order preference
-- 4. Finally, traverse and print
formatSessionAsMarkdown :: SessionPrintOptions -> Session.Session -> Text.Text
formatSessionAsMarkdown opts session =
    let -- Session turns are stored newest-first (last turn is first in the list)
        -- Step 1: Number the steps with their original position
        -- We reverse first to get chronological order for numbering, then zip
        numberedChronological = zip [1 :: Int ..] (reverse session.turns)
        
        -- Step 2: Apply n-turns limit (takes the last N steps in chronological order = most recent)
        limitedTurns = case opts.nTurns of
            Just n -> takeLast n numberedChronological
            Nothing -> numberedChronological
        
        -- Step 3: Apply ordering preference
        orderedTurns = case opts.orderPreference of
            Chronological -> limitedTurns  -- Already in chronological order
            Antichronological -> reverse limitedTurns  -- Newest first
        
        -- Determine the first step number in display order for "is first turn" checks
        firstDisplayStepNum = case orderedTurns of
            [] -> Nothing
            ((n, _) : _) -> Just n
        
        mdHeader = "# Session Report\n\n"
        sessionInfo = formatSessionInfo session
        turnsSection = formatTurns opts firstDisplayStepNum orderedTurns
        limitNotice = case opts.nTurns of
            Just n -> "\n\n_(Showing last " <> Text.pack (show n) <> " turns)_\n"
            Nothing -> ""
    in mdHeader <> sessionInfo <> "\n---\n\n" <> turnsSection <> limitNotice
  where
    -- | Take the last n elements from a list
    takeLast :: Int -> [a] -> [a]
    takeLast n xs = drop (max 0 (length xs - n)) xs

-- | Format session metadata.
formatSessionInfo :: Session.Session -> Text.Text
formatSessionInfo session =
    "**Session ID:** " <> formatSessionId session.sessionId <> "\n" <>
    maybe "" (\sid -> "**Forked from:** " <> formatSessionId sid <> "\n") session.forkedFromSessionId

-- | Extract UUID text from SessionId.
formatSessionId :: Session.SessionId -> Text.Text
formatSessionId (Session.SessionId uuid) = Text.pack $ show uuid

-- | Format all turns (already numbered and ordered).
-- The firstDisplayStepNum indicates which step number appears first in the output
-- (used for determining whether to show system prompt/tools on the first displayed turn).
formatTurns :: SessionPrintOptions -> Maybe Int -> [(Int, Session.Turn)] -> Text.Text
formatTurns opts firstDisplayStepNum turns =
    Text.intercalate "\n\n---\n\n" $ map (formatTurn opts firstDisplayStepNum) turns

-- | Format a single turn with its original step number.
formatTurn :: SessionPrintOptions -> Maybe Int -> (Int, Session.Turn) -> Text.Text
formatTurn opts firstDisplayStepNum (stepNum, turn) = 
    let isFirstTurn = Just stepNum == firstDisplayStepNum
    in case turn of
        Session.UserTurn content ->
            "## Step " <> Text.pack (show stepNum) <> ": User Turn\n\n" <>
            formatUserTurn opts isFirstTurn content
        Session.LlmTurn content ->
            "## Step " <> Text.pack (show stepNum) <> ": LLM Turn\n\n" <>
            formatLlmTurn opts content

-- | Format user turn content.
-- The 'isFirstTurn' parameter ensures that system prompt and tools are shown
-- at least once, even when 'repeatSystemPrompt' or 'repeatTools' is False.
formatUserTurn :: SessionPrintOptions -> Bool -> Session.UserTurnContent -> Text.Text
formatUserTurn opts isFirstTurn content =
    let -- Show system prompt if repeatSystemPrompt is True, or if it's the first turn
        systemPromptSection = case content.userPrompt of
            Session.SystemPrompt sp | opts.repeatSystemPrompt || isFirstTurn ->
                "### System Prompt\n\n```\n" <> sp <> "```\n"
            _ -> ""
        querySection = case content.userQuery of
            Just (Session.UserQuery q) -> "\n### User Query\n\n" <> q <> "\n"
            Nothing -> ""
        -- Show tools if repeatTools is True, or if it's the first turn (and tools exist)
        toolsSection = if null content.userTools
            then ""
            else if opts.repeatTools || isFirstTurn
                then "\n### Available Tools\n\n" <> formatAvailableTools content.userTools
                else ""
        toolResponsesSection = if null content.userToolResponses || not opts.showToolCallResults
            then ""
            else "\n### Tool Responses\n\n" <> formatToolResponses content.userToolResponses
    in systemPromptSection <> querySection <> toolsSection <> toolResponsesSection

-- | Format LLM turn content.
formatLlmTurn :: SessionPrintOptions -> Session.LlmTurnContent -> Text.Text
formatLlmTurn _opts content =
    let responseSection = case content.llmResponse.responseText of
            Just txt -> "### Response\n\n" <> txt <> "\n"
            Nothing -> "### Response\n\n_(No text response)_\n"
        toolCallsSection = if null content.llmToolCalls
            then ""
            else "\n### Tool Calls\n\n" <> formatLlmToolCalls content.llmToolCalls
    in responseSection <> toolCallsSection

-- | Format available tools (just names and descriptions).
formatAvailableTools :: [Session.SystemTool] -> Text.Text
formatAvailableTools tools =
    Text.intercalate "\n" $ map formatSystemTool tools

-- | Format a single system tool.
formatSystemTool :: Session.SystemTool -> Text.Text
formatSystemTool (Session.SystemTool toolDef) = case toolDef of
    Session.V0 val -> "- **Tool (V0):** `" <> formatJsonAsText val <> "`"
    Session.V1 def ->
        "- **" <> def.name <> "** (`" <> def.llmName <> "`)\n" <>
        "  - Description: " <> def.description

-- | Format LLM tool calls (only names).
formatLlmToolCalls :: [Session.LlmToolCall] -> Text.Text
formatLlmToolCalls calls =
    Text.intercalate "\n" $ map formatLlmToolCall calls

-- | Format a single LLM tool call, extracting the name.
formatLlmToolCall :: Session.LlmToolCall -> Text.Text
formatLlmToolCall (Session.LlmToolCall val) =
    case val of
        Aeson.Object obj ->
            -- Try to extract the function name from the tool call structure
            case KeyMap.lookup "function" obj of
                Just (Aeson.Object funcObj) ->
                    case KeyMap.lookup "name" funcObj of
                        Just (Aeson.String toolName) -> "- **" <> toolName <> "**"
                        _ -> "- (unnamed tool call)"
                _ -> case KeyMap.lookup "name" obj of
                    Just (Aeson.String toolName) -> "- **" <> toolName <> "**"
                    _ -> "- (unnamed tool call): `" <> formatJsonAsText val <> "`"
        _ -> "- (unnamed tool call): `" <> formatJsonAsText val <> "`"

-- | Format tool responses.
formatToolResponses :: [(Session.LlmToolCall, Session.UserToolResponse)] -> Text.Text
formatToolResponses responses =
    Text.intercalate "\n\n" $ map formatToolResponse responses

-- | Format a single tool response.
formatToolResponse :: (Session.LlmToolCall, Session.UserToolResponse) -> Text.Text
formatToolResponse (call, Session.UserToolResponse response) =
    let callName = extractToolCallName call
    in "**" <> callName <> "** response:\n```\n" <> formatJsonAsText response <> "\n```"

-- | Extract tool name from a tool call.
extractToolCallName :: Session.LlmToolCall -> Text.Text
extractToolCallName (Session.LlmToolCall val) =
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

-- | Format a JSON value as compact text.
formatJsonAsText :: Aeson.Value -> Text.Text
formatJsonAsText = Text.pack . show . Aeson.encode

