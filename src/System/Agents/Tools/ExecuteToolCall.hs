{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Shared implementation for executing tool calls from LLM responses.

This module provides common functions for parsing LLM tool calls and executing
them against registered tools. It is used by:

* System.Agents.OneShot - One-shot agent execution
* System.Agents.MCP.Server - MCP server agent execution
* System.Agents.AgentTree.OneShotTool - Sub-agent tool execution

The functions here work with OS-native types and handle the conversion between
LLM tool call formats and the internal ToolCall representation.
-}
module System.Agents.Tools.ExecuteToolCall (
    -- * Tool Call Execution
    executeToolCall,
    llmCallTool,
    callResultToUserToolResponse,

    -- * Tool Call Parsing
    parseLlmToolCall,
) where

import Control.Concurrent.STM (TVar, readTVarIO)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as Aeson
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding.Error (lenientDecode)
import Prod.Tracer (Tracer (..))

import System.Agents.Base (AgentId, ConversationId)
import qualified System.Agents.LLMs.OpenAI as OpenAI
import System.Agents.Session.Base (
    LlmToolCall (..),
    UserToolResponse (..),
 )
import System.Agents.Session.Types (newTurnId, newSessionId)
import System.Agents.ToolRegistration (ToolRegistration (..))
import System.Agents.Tools.Base (CallResult (..), Tool (..), mapCallResult)
import System.Agents.Tools.Context (
    CallStackEntry (..),
    ToolExecutionContext (..),
    mkToolExecutionContext,
 )

-- | Parse an LlmToolCall into OpenAI's ToolCall format.
--
-- This function handles multiple input formats:
-- * Direct OpenAI ToolCall JSON format
-- * Our internal LlmToolCall format with id/function structure
parseLlmToolCall :: Aeson.Value -> Maybe OpenAI.ToolCall
parseLlmToolCall val =
    case Aeson.parseMaybe Aeson.parseJSON val of
        Just tc -> Just tc
        Nothing ->
            -- Try to extract from our LlmToolCall format
            case val of
                Aeson.Object obj ->
                    case (KeyMap.lookup "id" obj, KeyMap.lookup "function" obj) of
                        (Just (Aeson.String tid), Just funcVal) ->
                            Just $
                                OpenAI.ToolCall
                                    { OpenAI.rawToolCall = obj
                                    , OpenAI.toolCallId = tid
                                    , OpenAI.toolCallType = KeyMap.lookup "type" obj >>= \v -> case v of Aeson.String t -> Just t; _ -> Nothing
                                    , OpenAI.toolCallFunction = case Aeson.parseMaybe Aeson.parseJSON funcVal of
                                        Just f -> f
                                        Nothing -> OpenAI.ToolCallFunction (OpenAI.ToolName "") "" Nothing
                                    }
                        _ -> Nothing
                _ -> Nothing

{- | Execute a tool call using the node's registered tools.

Constructs a 'ToolExecutionContext' with the provided agent and session identifiers,
then executes the tool with this context. The context gives tools access to:

* 'ctxSessionId' - Generated fresh for this execution
* 'ctxConversationId' - The conversation ID passed from the runtime
* 'ctxTurnId' - Generated fresh for this execution
* 'ctxAgentId' - The agent ID from the node
* 'ctxFullSession' - Nothing (not directly available at this point)
* 'ctxToolPortal' - Inherited from the provided context
* 'ctxCallStack' - Root call stack entry
* 'ctxMaxRecursionDepth' - Nothing (no limit by default)

This function is the primary entry point for executing tool calls from LLM responses.
-}
executeToolCall ::
    -- | Agent ID for context
    AgentId ->
    -- | Conversation ID for context
    ConversationId ->
    -- | Tools TVar from OSAgentNode
    TVar [ToolRegistration] ->
    -- | Context passed from runStepM (used for tool portal access)
    ToolExecutionContext ->
    LlmToolCall ->
    IO UserToolResponse
executeToolCall agentId convId toolsTVar ctx (LlmToolCall callVal) =
    -- Extract the tool call ID and function info from the LlmToolCall
    case parseLlmToolCall callVal of
        Nothing -> pure $ UserToolResponse $ Aeson.String "Failed to parse tool call"
        Just tc -> do
            regs <- readTVarIO toolsTVar
            -- Construct context for this tool execution
            -- We don't have access to the full session here, so we generate a minimal context
            -- with the identifiers we have. In the future, we could pass session through the
            -- Agent type or use a Reader pattern to access it here.
            sessId <- newSessionId
            tId <- newTurnId
            let toolCtx =
                    mkToolExecutionContext
                        sessId
                        convId
                        tId
                        (Just agentId)
                        Nothing -- No full session available at this point
                        (ctxToolPortal ctx)
                        [CallStackEntry "root" convId 0] -- Root call stack entry
                        Nothing -- No max recursion depth by default
            result <- llmCallTool regs toolCtx tc
            pure $ callResultToUserToolResponse tc result

{- | Execute a single tool call against registered tools.

The 'ToolExecutionContext' is passed as a parameter, providing tools with
access to session metadata without exposing these details to the LLM.

Looks up the tool in the registrations and runs it if found, returning
an appropriate 'CallResult'.
-}
llmCallTool ::
    [ToolRegistration] ->
    -- | Context containing session metadata for tools
    ToolExecutionContext ->
    OpenAI.ToolCall ->
    IO (CallResult OpenAI.ToolCall)
llmCallTool registrations ctx call =
    let
        script =
            Maybe.listToMaybe $
                Maybe.mapMaybe (\r -> r.findTool call) registrations
        args = call.toolCallFunction.toolCallFunctionArgs
        spec = (,) <$> script <*> args
     in
        case spec of
            Nothing -> pure $ ToolNotFound call
            Just (t, v) -> do
                ret <- t.toolRun (Tracer $ const $ pure ()) ctx v
                pure $ mapCallResult (const call) ret

{- | Convert a CallResult to UserToolResponse.

This function maps all possible 'CallResult' variants to their corresponding
'UserToolResponse' representations. It handles:

* Tool not found errors
* Various tool-specific errors (Bash, IO, MCP, etc.)
* Successful results from all tool types
* Binary data handling with lenient UTF-8 decoding
-}
callResultToUserToolResponse :: OpenAI.ToolCall -> CallResult OpenAI.ToolCall -> UserToolResponse
callResultToUserToolResponse _ result =
    case result of
        ToolNotFound _ ->
            UserToolResponse $ Aeson.String "Tool not found"
        BashToolError _ err ->
            UserToolResponse $ Aeson.String $ Text.pack $ show err
        IOToolError _ err ->
            UserToolResponse $ Aeson.String $ Text.pack $ show err
        McpToolError _ err ->
            UserToolResponse $ Aeson.String $ Text.unlines ["tool-error", Text.pack $ show err]
        McpToolResult _ res ->
            UserToolResponse $ Aeson.toJSON res
        BlobToolSuccess _ v ->
            -- Use lenient UTF-8 decoding to handle binary data safely.
            -- Invalid bytes are replaced with U+FFFD (replacement character).
            UserToolResponse $ Aeson.String $ Text.decodeUtf8With lenientDecode v
        OpenAPIToolResult _ toolResult ->
            UserToolResponse $ Aeson.toJSON toolResult
        OpenAPIToolError _ err ->
            UserToolResponse $ Aeson.String $ Text.pack $ "OpenAPI tool error: " <> err
        PostgRESToolResult _ toolResult ->
            UserToolResponse $ Aeson.toJSON toolResult
        PostgRESToolError _ err ->
            UserToolResponse $ Aeson.String $ Text.pack $ "PostgREST tool error: " <> err
        SqliteToolResult _ toolResult ->
            UserToolResponse $ Aeson.toJSON toolResult
        SqliteToolError _ err ->
            UserToolResponse $ Aeson.String $ Text.pack $ "SQLite tool error: " <> show err
        SystemToolResult _ toolResult ->
            UserToolResponse $ Aeson.toJSON toolResult
        SystemToolError _ err ->
            UserToolResponse $ Aeson.String $ Text.pack $ "System tool error: " <> show err
        DeveloperToolResult _ valResult ->
            UserToolResponse $ Aeson.toJSON valResult
        DeveloperToolScaffoldResult _ scaffoldResult ->
            UserToolResponse $ Aeson.toJSON scaffoldResult
        DeveloperToolSpecResult _ content ->
            UserToolResponse $ Aeson.String content
        DeveloperToolAgentValidationResult _ validationResult ->
            UserToolResponse $ Aeson.toJSON validationResult
        DeveloperToolCreateResult _ createResult ->
            UserToolResponse $ Aeson.toJSON createResult
        DeveloperToolError _ err ->
            UserToolResponse $ Aeson.String $ Text.pack $ "Developer tool error: " <> show err
        LuaToolResult _ toolResult ->
            UserToolResponse $ Aeson.toJSON toolResult
        LuaToolError _ err ->
            UserToolResponse $ Aeson.String $ "Lua tool error: " <> err

