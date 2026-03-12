{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Module for injecting session content into prompts with varying verbosity levels.

This module provides functionality to format sessions at different "t-shirt sizes"
for use as prompt input. The sizes range from minimal (xs) to complete (xl).
-}
module System.Agents.SessionPrint.Inject (
    -- * Verbosity levels
    SessionVerbosity (..),

    -- * Formatting functions
    formatSessionForPrompt,
    isToolOnlyTurn,
) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as Text

import qualified System.Agents.Session.Base as Session

-- | Verbosity levels for session formatting, from minimal to complete.
data SessionVerbosity
    = -- | Only queries and responses, skip tool-only turns
      SessionXS
    | -- | Add thinking process and tool names called
      SessionS
    | -- | Add statistics
      SessionM
    | -- | Add tool-call results
      SessionL
    | -- | Same as L (complete)
      SessionXL
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | Format a session for injection into a prompt at the given verbosity level.
formatSessionForPrompt :: SessionVerbosity -> Session.Session -> Text
formatSessionForPrompt verbosity session =
    let turns = filterTurns verbosity session.turns
        formattedTurns = zipWith (formatTurn verbosity) [1 ..] (reverse turns)
        statsSection =
            if verbosity >= SessionM
                then formatStatisticsSection session
                else ""
        header = case verbosity of
            SessionXS -> "## Session Summary (minimal)\n\n"
            SessionS -> "## Session Summary\n\n"
            SessionM -> "## Session Summary with Statistics\n\n"
            SessionL -> "## Complete Session\n\n"
            SessionXL -> "## Complete Session\n\n"
     in header <> statsSection <> Text.intercalate "\n---\n\n" formattedTurns

{- | Filter turns based on verbosity level.
For XS verbosity, we skip tool-only turns (turns with no user query and no response text).
-}
filterTurns :: SessionVerbosity -> [Session.Turn] -> [Session.Turn]
filterTurns SessionXS = filter (not . isToolOnlyTurn)
filterTurns _ = id

-- | Check if a turn is "tool-only" (no user query, no response text, just tool calls).
isToolOnlyTurn :: Session.Turn -> Bool
isToolOnlyTurn (Session.UserTurn utc _mUsage) =
    isNothing utc.userQuery && null utc.userToolResponses
isToolOnlyTurn (Session.LlmTurn ltc _mUsage) =
    isNothing ltc.llmResponse.responseText && not (null ltc.llmToolCalls)

-- | Format a single turn at the given verbosity level.
formatTurn :: SessionVerbosity -> Int -> Session.Turn -> Text
formatTurn verbosity idx turn =
    case turn of
        Session.UserTurn content _mUsage -> formatUserTurn verbosity idx content
        Session.LlmTurn content _mUsage -> formatLlmTurn verbosity idx content

-- | Format a user turn.
formatUserTurn :: SessionVerbosity -> Int -> Session.UserTurnContent -> Text
formatUserTurn _verbosity idx content =
    let querySection = case content.userQuery of
            Just (Session.UserQuery q) -> "**User:** " <> q <> "\n\n"
            Nothing -> ""
        toolRespSection = case content.userToolResponses of
            [] -> ""
            _responses -> "_(tool responses available at higher verbosity)_\n\n"
     in "### Turn " <> Text.pack (show idx) <> " (User)\n\n" <> querySection <> toolRespSection

-- | Format an LLM turn.
formatLlmTurn :: SessionVerbosity -> Int -> Session.LlmTurnContent -> Text
formatLlmTurn verbosity idx content =
    let thinkingSection =
            if verbosity >= SessionS
                then case content.llmResponse.responseThinking of
                    Just thinking -> "**Thinking:**\n```\n" <> thinking <> "\n```\n\n"
                    Nothing -> ""
                else ""
        responseSection = case content.llmResponse.responseText of
            Just txt -> "**Assistant:** " <> txt <> "\n\n"
            Nothing -> "_(no response text)_\n\n"
        toolCallsSection =
            if verbosity >= SessionS && not (null content.llmToolCalls)
                then "**Tools called:** " <> Text.intercalate ", " (map extractToolName content.llmToolCalls) <> "\n\n"
                else ""
        toolResultsSection =
            if verbosity >= SessionL && not (null content.llmToolCalls)
                then formatToolCallResults content.llmToolCalls
                else ""
     in "### Turn "
            <> Text.pack (show idx)
            <> " (Assistant)\n\n"
            <> thinkingSection
            <> responseSection
            <> toolCallsSection
            <> toolResultsSection

-- | Extract tool name from a tool call.
extractToolName :: Session.LlmToolCall -> Text
extractToolName (Session.LlmToolCall val) =
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

-- | Format tool call results (for L and XL verbosity).
formatToolCallResults :: [Session.LlmToolCall] -> Text
formatToolCallResults calls =
    "**Tool call details:**\n" <> Text.intercalate "\n" (map formatToolCallDetail calls)

-- | Format a single tool call detail.
formatToolCallDetail :: Session.LlmToolCall -> Text
formatToolCallDetail call =
    "- `" <> extractToolName call <> "`: `" <> formatJsonAsText (unwrapToolCall call) <> "`"
  where
    unwrapToolCall (Session.LlmToolCall val) = val

-- | Format a JSON value as compact text.
formatJsonAsText :: Aeson.Value -> Text
formatJsonAsText = Text.pack . show . Aeson.encode

-- | Format statistics section (for M, L, XL verbosity).
formatStatisticsSection :: Session.Session -> Text
formatStatisticsSection session =
    let totalTurns = length session.turns
        userTurns = length [() | Session.UserTurn _ _ <- session.turns]
        llmTurns = length [() | Session.LlmTurn _ _ <- session.turns]
        toolCalls = sum [length ltc.llmToolCalls | Session.LlmTurn ltc _ <- session.turns]
     in "**Statistics:**\n"
            <> "- Total turns: "
            <> Text.pack (show (totalTurns :: Int))
            <> "\n"
            <> "- User turns: "
            <> Text.pack (show (userTurns :: Int))
            <> "\n"
            <> "- Assistant turns: "
            <> Text.pack (show (llmTurns :: Int))
            <> "\n"
            <> "- Total tool calls: "
            <> Text.pack (show (toolCalls :: Int))
            <> "\n\n"
