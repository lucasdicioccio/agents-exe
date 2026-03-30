{-# LANGUAGE OverloadedRecordDot #-}

{- | Compatibility module for session operations.

This module provides utility functions for converting between different
session-related types, particularly for tool call handling.

This module replaces the missing System.Agents.Session.Compat that was
referenced in the codebase.
-}
module System.Agents.Session.Compat (
    -- * Tool Call Parsing
    parseToolCallFromLlmToolCall,
    callResultToUserToolResponse,
) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import System.Agents.Session.Types (
    LlmToolCall (..),
    UserToolResponse (..),
 )
import System.Agents.Tools.Base (CallResult (..))
import System.Agents.Tools.Context (ToolCall (..))

{- | Parse a ToolCall from an LlmToolCall.

This extracts the function name and arguments from the LLM tool call
and creates a ToolCall suitable for execution.
Returns Nothing if the tool call cannot be parsed.
-}
parseToolCallFromLlmToolCall :: LlmToolCall -> Maybe ToolCall
parseToolCallFromLlmToolCall (LlmToolCall val) =
    case val of
        Aeson.Object obj ->
            case KeyMap.lookup "function" obj of
                Just (Aeson.Object funcObj) ->
                    let mToolName = case KeyMap.lookup "name" funcObj of
                            Just (Aeson.String n) -> Just n
                            _ -> Nothing
                        toolArgs = case KeyMap.lookup "arguments" funcObj of
                            Just (Aeson.String s) ->
                                case Aeson.decode (LByteString.fromStrict $ Text.encodeUtf8 s) of
                                    Just (Aeson.Object o) -> Aeson.Object o
                                    _ -> Aeson.object []
                            Just other -> other
                            Nothing -> Aeson.object []
                     in case mToolName of
                            Just toolName -> Just ToolCall{callToolName = toolName, callArgs = toolArgs}
                            Nothing -> Nothing
                _ -> Nothing
        _ -> Nothing

{- | Convert a CallResult to a UserToolResponse.

This serializes the tool execution result into a format suitable
for returning to the LLM in the conversation.
-}
callResultToUserToolResponse :: ToolCall -> CallResult ToolCall -> UserToolResponse
callResultToUserToolResponse _tc result =
    case result of
        BlobToolSuccess _ bs ->
            UserToolResponse $ Aeson.String (Text.decodeUtf8 bs)
        ToolNotFound _ ->
            UserToolResponse $ Aeson.String "Error: Tool not found"
        BashToolError _ err ->
            UserToolResponse $ Aeson.String (Text.pack $ show err)
        IOToolError _ err ->
            UserToolResponse $ Aeson.String (Text.pack $ show err)
        McpToolResult _ mcpResult ->
            UserToolResponse $ Aeson.toJSON mcpResult
        McpToolError _ err ->
            UserToolResponse $ Aeson.String (Text.pack err)
        OpenAPIToolResult _ apiResult ->
            UserToolResponse $ Aeson.toJSON apiResult
        OpenAPIToolError _ err ->
            UserToolResponse $ Aeson.String (Text.pack err)
        PostgRESToolResult _ prResult ->
            UserToolResponse $ Aeson.toJSON prResult
        PostgRESToolError _ err ->
            UserToolResponse $ Aeson.String (Text.pack err)
        SqliteToolResult _ sqlResult ->
            UserToolResponse $ Aeson.toJSON sqlResult
        SqliteToolError _ err ->
            UserToolResponse $ Aeson.String (Text.pack $ show err)
        SystemToolResult _ sysResult ->
            UserToolResponse $ Aeson.toJSON sysResult
        SystemToolError _ err ->
            UserToolResponse $ Aeson.String (Text.pack $ show err)
        DeveloperToolResult _ devResult ->
            UserToolResponse $ Aeson.toJSON devResult
        DeveloperToolScaffoldResult _ devResult ->
            UserToolResponse $ Aeson.toJSON devResult
        DeveloperToolSpecResult _ content ->
            UserToolResponse $ Aeson.String content
        DeveloperToolAgentValidationResult _ devResult ->
            UserToolResponse $ Aeson.toJSON devResult
        DeveloperToolCreateResult _ devResult ->
            UserToolResponse $ Aeson.toJSON devResult
        DeveloperToolError _ err ->
            UserToolResponse $ Aeson.String (Text.pack $ show err)
        LuaToolResult _ luaResult ->
            UserToolResponse luaResult
        LuaToolError _ err ->
            UserToolResponse $ Aeson.String err
