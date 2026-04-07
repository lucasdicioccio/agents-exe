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
LLM tool call formats and the internal OpenAIToolCall representation.
-}
module System.Agents.Tools.ExecuteToolCall (
    -- * Tool Call Execution
    executeLlmToolCall,
) where

import qualified Data.Aeson as Aeson
import qualified Data.Maybe as Maybe
import Prod.Tracer (Tracer (..))

import System.Agents.Session.Base (
    LlmToolCall (..),
    UserToolResponse (..),
 )
import System.Agents.ToolRegistration (ToolRegistration (..), Trace)
import System.Agents.Tools.Base (CallResult (..), Tool (..), mapCallResult)
import System.Agents.Tools.Context (
    ToolCall (..),
    ToolExecutionContext,
 )

type ToolMappingFunctions r =
    ( LlmToolCall -> Maybe r
    , r -> CallResult r -> UserToolResponse
    )

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
executeLlmToolCall ::
    Tracer IO Trace ->
    -- | Action to retrieve tool registrations
    IO [ToolRegistration] ->
    -- | Context passed from runStepM (used for tool portal access)
    ToolMappingFunctions ToolCall ->
    ToolExecutionContext ->
    LlmToolCall ->
    IO UserToolResponse
executeLlmToolCall tracer getTools (adaptTc, adaptCr) toolCtx llmToolCall =
    case adaptTc llmToolCall of
        Nothing -> pure $ UserToolResponse $ Aeson.String "Failed to parse tool call"
        Just tc -> do
            regs <- getTools
            result <- llmCallTool tracer regs toolCtx tc
            pure $ adaptCr tc result

{- | Execute a single tool call against registered tools.

The 'ToolExecutionContext' is passed as a parameter, providing tools with
access to session metadata without exposing these details to the LLM.

Looks up the tool in the registrations and runs it if found, returning
an appropriate 'CallResult'.
-}
llmCallTool ::
    Tracer IO Trace ->
    [ToolRegistration] ->
    -- | Context containing session metadata for tools
    ToolExecutionContext ->
    ToolCall ->
    IO (CallResult ToolCall)
llmCallTool tracer registrations ctx call =
    let
        script =
            Maybe.listToMaybe $
                Maybe.mapMaybe (\r -> r.findTool call) registrations
        spec = (,) <$> script <*> pure call.callArgs
     in do
        print call
        print [ reg.declareTool | reg <- registrations]
        case spec of
            Nothing -> pure $ ToolNotFound call
            Just (t, v) -> do
                ret <- t.toolRun tracer ctx v
                pure $ mapCallResult (const call) ret
