{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Module for printing session files in markdown format.
--
-- This module provides functionality to load and format session JSON files
-- as readable markdown output. It is used by the @session-print@ CLI command.
module System.Agents.SessionPrint
    ( -- * Options
      SessionPrintOptions (..)
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

-- | Options for controlling the session print output.
data SessionPrintOptions = SessionPrintOptions
    { -- | Path to the session JSON file to print
      sessionPrintFile :: FilePath
      -- | Whether to show tool call results in the output
    , showToolCallResults :: Bool
      -- | Optional limit on the number of turns to display
    , nTurns :: Maybe Int
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
formatSessionAsMarkdown :: SessionPrintOptions -> Session.Session -> Text.Text
formatSessionAsMarkdown opts session =
    let -- Turns are stored newest-first, so reverse for chronological order
        -- Apply n-turns limit if specified
        limitedTurns = case opts.nTurns of
            Just n -> take n session.turns
            Nothing -> session.turns
        chronologicalTurns = reverse limitedTurns
        mdHeader = "# Session Report\n\n"
        sessionInfo = formatSessionInfo session
        turnsSection = formatTurns opts chronologicalTurns
        limitNotice = case opts.nTurns of
            Just n -> "\n\n_(Showing first " <> Text.pack (show n) <> " turns)_\n"
            Nothing -> ""
    in mdHeader <> sessionInfo <> "\n---\n\n" <> turnsSection <> limitNotice

-- | Format session metadata.
formatSessionInfo :: Session.Session -> Text.Text
formatSessionInfo session =
    "**Session ID:** " <> formatSessionId session.sessionId <> "\n" <>
    maybe "" (\sid -> "**Forked from:** " <> formatSessionId sid <> "\n") session.forkedFromSessionId

-- | Extract UUID text from SessionId.
formatSessionId :: Session.SessionId -> Text.Text
formatSessionId (Session.SessionId uuid) = Text.pack $ show uuid

-- | Format all turns.
formatTurns :: SessionPrintOptions -> [Session.Turn] -> Text.Text
formatTurns opts turns =
    Text.intercalate "\n\n---\n\n" $ zipWith (formatTurn opts) [1 :: Int ..] turns

-- | Format a single turn with step number.
formatTurn :: SessionPrintOptions -> Int -> Session.Turn -> Text.Text
formatTurn opts stepNum turn = case turn of
    Session.UserTurn content ->
        "## Step " <> Text.pack (show stepNum) <> ": User Turn\n\n" <>
        formatUserTurn opts content
    Session.LlmTurn content ->
        "## Step " <> Text.pack (show stepNum) <> ": LLM Turn\n\n" <>
        formatLlmTurn opts content

-- | Format user turn content.
formatUserTurn :: SessionPrintOptions -> Session.UserTurnContent -> Text.Text
formatUserTurn opts content =
    let systemPromptSection = case content.userPrompt of
            Session.SystemPrompt sp ->
                "### System Prompt\n\n```\n" <> sp <> "\n```\n"
        querySection = case content.userQuery of
            Just (Session.UserQuery q) -> "\n### User Query\n\n" <> q <> "\n"
            Nothing -> ""
        toolsSection = if null content.userTools
            then ""
            else "\n### Available Tools\n\n" <> formatAvailableTools content.userTools
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

