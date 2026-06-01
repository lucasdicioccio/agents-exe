{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Tool Portal implementation for inter-toolbox communication.

This module provides the bridge between the ToolPortal type (defined in
System.Agents.Tools.Context) and the ToolRegistration system. It allows
tools to invoke other tools through a callback mechanism.

The portal is used by:
* Lua scripts via the @tools@ module (tools.call, tools.bash.run, etc.)
* Any other tool that needs to orchestrate multiple tool calls

Usage:

@
-- Create portal from registered tools
let portal = makeToolPortal registrations tracer

-- Create context with portal
let ctx = mkPortalContext
        sessId convId turnId mAgentId mSession
        stack maxDepth (Just portal) allowedTools

-- Now tools can call other tools through ctxToolPortal
@

Security:
* Tool whitelist is enforced at the portal level
* Portal tools execute with a minimal context (no nested portal to prevent loops)
* Execution time is tracked for each portal invocation

Context Propagation:
When a parent context is provided (Just ctx), the portal propagates the OS
integration fields (ctxWorld, ctxEventQueue) AND the conversation identifiers
(ctxSessionId, ctxConversationId, ctxTurnId, ctxCallStack) to the nested tool
call. This ensures proper conversation tracking when tools call sub-agents.
When called with Nothing, a fresh minimal context is created without OS
integration.
-}
module System.Agents.ToolPortal (
    Trace (..),

    -- * Portal creation
    makeToolPortal,

    -- * Portal execution
    callToolViaPortal,

    -- * Utility types
    PortalError (..),
) where

import Control.Concurrent.STM (TVar, readTVarIO)
import Control.Exception (SomeException, try)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Time (diffUTCTime, getCurrentTime)

import Prod.Tracer (Tracer (..), contramap)

import System.Agents.Base (newConversationId)
import System.Agents.Session.Types (newSessionId, newTurnId)
import System.Agents.ToolRegistration (Tool, ToolRegistration (..))
import qualified System.Agents.ToolRegistration as ToolRegistration
import System.Agents.ToolSchema (ToolDescription (..), ToolName (..))
import System.Agents.Tools.Base (CallResult (..), toolRun)
import System.Agents.Tools.Context (
    ToolCall (..),
    ToolExecutionContext,
    ToolPortal,
    ToolResult (..),
 )
import qualified System.Agents.Tools.Context as Ctx

data Trace = PortalCall !ToolCall !ToolRegistration.Trace
    deriving (Show)

-------------------------------------------------------------------------------
-- Portal Error Type
-------------------------------------------------------------------------------

{- | Errors that can occur during portal tool invocation.

These errors are distinct from tool execution errors - they represent
failures in the portal mechanism itself (tool not found, not allowed, etc.).
-}
data PortalError
    = -- | The requested tool is not registered
      PortalToolNotFound Text
    | -- | The tool is not in the allowed list
      PortalToolNotAllowed Text [Text]
    | -- | Failed to construct tool call
      PortalInvalidArguments Text
    | -- | Execution failed with exception
      PortalExecutionError Text
    deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Portal Creation
-------------------------------------------------------------------------------

{- | Create a ToolPortal from a list of tool registrations.

The portal allows tools to invoke other registered tools. Each invocation:
1. Looks up the tool by name
2. Checks against the allowed tools whitelist
3. Constructs a synthetic OpenAI ToolCall for the lookup mechanism
4. Executes the tool with a minimal context (no nested portal to prevent loops)
5. Returns the result as a ToolResult

When called with a parent context (Just ctx), the portal propagates OS
integration fields (World, EventQueue, ParentConversation) AND conversation
identifiers (session ID, conversation ID, turn ID, call stack) to ensure proper
conversation tracking when tools call sub-agents.

Example:

@
regs <- readTVarIO agentTools
let portal = makeToolPortal myTracer regs
let ctx = mkPortalContext sessId convId turnId mAgentId mSession
                    stack maxDepth (Just portal) ["bash", "sqlite"]
@

This variant allows tracing portal invocations for debugging and monitoring.
-}
makeToolPortal ::
    -- | Tracer for tool execution events
    Tracer IO Trace ->
    -- | Registered tools available through the portal
    TVar [ToolRegistration] ->
    ToolPortal
makeToolPortal tracer registrations = portal
  where
    portal :: ToolPortal
    portal mParentCtx toolCall = do
        print toolCall
        startTime <- getCurrentTime
        result <- callToolViaPortal (contramap (PortalCall toolCall) tracer) portal registrations mParentCtx toolCall -- TODO(lucas): clarify stack recursion here
        endTime <- getCurrentTime
        let duration = diffUTCTime endTime startTime

        -- Generate a trace ID (simplified - could use UUID)
        let traceId = Text.pack $ show startTime

        case result of
            Left err -> do
                -- Return error as JSON in the result data
                let errData =
                        Aeson.object
                            [ "error" .= show err
                            , "toolName" .= callToolName toolCall
                            ]
                pure $ ToolResult errData duration traceId
            Right callResult -> do
                -- Convert CallResult to JSON for ToolResult
                let resData = callResultToJson callResult
                pure $ ToolResult resData duration traceId

-------------------------------------------------------------------------------
-- Portal Execution
-------------------------------------------------------------------------------

{- | Execute a tool call through the portal.

This function handles the core portal logic:
1. Find the tool by name from registrations
2. Check whitelist if parent context has allowed tools
3. Execute with minimal context (no nested portal to prevent loops)
4. Convert the result

When a parent context is provided, its OS integration fields AND conversation
identifiers are propagated to ensure proper conversation tracking.
-}
callToolViaPortal ::
    Tracer IO ToolRegistration.Trace ->
    ToolPortal ->
    TVar [ToolRegistration] ->
    -- | Optional parent context for OS field propagation
    Maybe ToolExecutionContext ->
    ToolCall ->
    IO (Either PortalError (CallResult ToolCall))
callToolViaPortal tracer portal registrations mParentCtx toolCall = do
    regs <- readTVarIO registrations
    -- Find tool by name
    case findToolByName regs (callToolName toolCall) of
        Nothing ->
            pure $ Left $ PortalToolNotFound (callToolName toolCall)
        Just _tool -> do
            -- Find the specific tool with the call context
            case findMatchingTool regs toolCall of
                Nothing ->
                    pure $ Left $ PortalToolNotFound (callToolName toolCall)
                Just matchedTool -> do
                    -- Check whitelist if parent context has allowed tools
                    case mParentCtx of
                        Just parentCtx -> do
                            if not (Ctx.isToolAllowed (callToolName toolCall) parentCtx)
                                then pure $ Left $ PortalToolNotAllowed (callToolName toolCall) (Ctx.ctxAllowedTools parentCtx)
                                else do
                                    -- Execute with OS fields and IDs from parent context
                                    execResult <- executeTool tracer portal mParentCtx matchedTool (callArgs toolCall)
                                    pure $ first PortalExecutionError execResult
                        Nothing -> do
                            -- Execute without parent context (no OS fields, fresh IDs)
                            execResult <- executeTool tracer portal Nothing matchedTool (callArgs toolCall)
                            pure $ first PortalExecutionError execResult

{- | Find a tool registration by tool name.

This looks up the tool by comparing the requested name against the
declared tool names in the registrations.
-}
findToolByName ::
    [ToolRegistration] ->
    -- | Tool name to look up
    Text ->
    Maybe ToolRegistration
findToolByName regs name =
    case filter (matchesName name) regs of
        (r : _) -> Just r
        [] -> Nothing
  where
    matchesName :: Text -> ToolRegistration -> Bool
    matchesName n reg =
        n == reg.declareTool.toolDescriptionName.getToolName

findMatchingTool ::
    [ToolRegistration] ->
    ToolCall ->
    Maybe (Tool ToolCall)
findMatchingTool regs call =
    case concatMap (\r -> maybe [] (: []) (r.findTool call)) regs of
        (t : _) -> Just t
        [] -> Nothing

{- | Execute a tool with a minimal context.

The minimal context has no portal to prevent nested portal calls.
This is a safety measure to avoid potential infinite recursion.

When a parent context is provided, its OS integration fields (World,
EventQueue, ParentConversation) AND its identifiers (session ID, conversation
ID, turn ID, call stack) are propagated to the minimal context.
This ensures proper conversation tracking when tools call sub-agents.

When no parent context is available (Nothing), fresh IDs are generated
for the tool execution.
-}
executeTool ::
    Tracer IO ToolRegistration.Trace ->
    ToolPortal ->
    -- | Optional parent context for OS field propagation
    Maybe ToolExecutionContext ->
    Tool ToolCall ->
    Aeson.Value ->
    IO (Either Text (CallResult ToolCall))
executeTool tracer portal mParentCtx tool args =
    case mParentCtx of
        Just parentCtx -> do
            -- Propagate parent's IDs to maintain conversation continuity
            let sessId = Ctx.ctxSessionId parentCtx
            let convId = Ctx.ctxConversationId parentCtx
            let turnId = Ctx.ctxTurnId parentCtx

            -- Create minimal context with propagated IDs and OS fields
            let minimalCtx =
                    (Ctx.mkMinimalContext sessId convId turnId portal)
                        { Ctx.ctxWorld = Ctx.ctxWorld parentCtx
                        , Ctx.ctxEventQueue = Ctx.ctxEventQueue parentCtx
                        , Ctx.ctxParentConversation = Just (Ctx.ctxConversationId parentCtx)
                        , Ctx.ctxCallStack = Ctx.ctxCallStack parentCtx
                        }

            -- Execute the tool
            result <- try $ tool.toolRun tracer minimalCtx args

            case result of
                Left (e :: SomeException) ->
                    pure $ Left $ Text.pack $ show e
                Right callResult ->
                    pure $ Right callResult
        Nothing -> do
            -- No parent context - generate fresh IDs for standalone execution
            sessId <- newSessionId
            convId <- newConversationId
            turnId <- newTurnId

            let minimalCtx = Ctx.mkMinimalContext sessId convId turnId portal

            -- Execute the tool
            result <- try $ tool.toolRun tracer minimalCtx args

            case result of
                Left (e :: SomeException) ->
                    pure $ Left $ Text.pack $ show e
                Right callResult ->
                    pure $ Right callResult

-------------------------------------------------------------------------------
-- Result Conversion
-------------------------------------------------------------------------------

{- | Convert a CallResult to JSON for the ToolResult.

Different tool types have different result shapes, so we normalize them
to JSON for the portal interface.
-}
callResultToJson :: forall a. CallResult a -> Aeson.Value
callResultToJson (BlobToolSuccess _ bs _) =
    Aeson.object
        [ "type" .= ("blob" :: Text)
        , "data" .= decodeUtf8 bs
        ]
callResultToJson (ToolNotFound _) =
    Aeson.object
        [ "type" .= ("error" :: Text)
        , "error" .= ("Tool not found" :: Text)
        ]
callResultToJson (BashToolError _ err) =
    Aeson.object
        [ "type" .= ("error" :: Text)
        , "error" .= show err
        , "toolType" .= ("bash" :: Text)
        ]
callResultToJson (IOToolError _ err) =
    Aeson.object
        [ "type" .= ("error" :: Text)
        , "error" .= show err
        , "toolType" .= ("io" :: Text)
        ]
callResultToJson (McpToolResult _ result) =
    Aeson.object
        [ "type" .= ("success" :: Text)
        , "data" .= result
        , "toolType" .= ("mcp" :: Text)
        ]
callResultToJson (McpToolError _ err) =
    Aeson.object
        [ "type" .= ("error" :: Text)
        , "error" .= err
        , "toolType" .= ("mcp" :: Text)
        ]
callResultToJson (OpenAPIToolResult _ result) =
    Aeson.object
        [ "type" .= ("success" :: Text)
        , "data" .= result
        , "toolType" .= ("openapi" :: Text)
        ]
callResultToJson (OpenAPIToolError _ err) =
    Aeson.object
        [ "type" .= ("error" :: Text)
        , "error" .= err
        , "toolType" .= ("openapi" :: Text)
        ]
callResultToJson (PostgRESToolResult _ result) =
    Aeson.object
        [ "type" .= ("success" :: Text)
        , "data" .= result
        , "toolType" .= ("postgrest" :: Text)
        ]
callResultToJson (PostgRESToolError _ err) =
    Aeson.object
        [ "type" .= ("error" :: Text)
        , "error" .= err
        , "toolType" .= ("postgrest" :: Text)
        ]
callResultToJson (SqliteToolResult _ result) =
    Aeson.object
        [ "type" .= ("success" :: Text)
        , "data" .= result
        , "toolType" .= ("sqlite" :: Text)
        ]
callResultToJson (SqliteToolError _ err) =
    Aeson.object
        [ "type" .= ("error" :: Text)
        , "error" .= show err
        , "toolType" .= ("sqlite" :: Text)
        ]
callResultToJson (SystemToolResult _ result) =
    Aeson.object
        [ "type" .= ("success" :: Text)
        , "data" .= result
        , "toolType" .= ("system" :: Text)
        ]
callResultToJson (SystemToolListDirectoryResult _ result) = Aeson.toJSON result
callResultToJson (SystemToolError _ err) =
    Aeson.object
        [ "type" .= ("error" :: Text)
        , "error" .= show err
        , "toolType" .= ("system" :: Text)
        ]
callResultToJson (DeveloperToolResult _ result) =
    Aeson.object
        [ "type" .= ("success" :: Text)
        , "data" .= result
        , "toolType" .= ("devtool" :: Text)
        ]
callResultToJson (DeveloperToolScaffoldResult _ result) =
    Aeson.object
        [ "type" .= ("success" :: Text)
        , "data" .= result
        , "toolType" .= ("devtool-scaffold" :: Text)
        ]
callResultToJson (DeveloperToolSpecResult _ result) =
    Aeson.object
        [ "type" .= ("success" :: Text)
        , "data" .= result
        , "toolType" .= ("devtool-spec" :: Text)
        ]
callResultToJson (DeveloperToolAgentValidationResult _ result) =
    Aeson.object
        [ "type" .= ("success" :: Text)
        , "data" .= result
        , "toolType" .= ("devtool-agent-validation" :: Text)
        ]
callResultToJson (DeveloperToolCreateResult _ result) =
    Aeson.object
        [ "type" .= ("success" :: Text)
        , "data" .= result
        , "toolType" .= ("devtool-create" :: Text)
        ]
callResultToJson (DeveloperToolReadFileRangeResult _ result) =
    Aeson.object
        [ "type" .= ("success" :: Text)
        , "data" .= result
        , "toolType" .= ("devtool-read-file-range" :: Text)
        ]
callResultToJson (DeveloperToolWriteFileRangeResult _ result) =
    Aeson.object
        [ "type" .= ("success" :: Text)
        , "data" .= result
        , "toolType" .= ("devtool-write-file-range" :: Text)
        ]
callResultToJson (DeveloperToolPatchResult _ result) =
    Aeson.object
        [ "type" .= ("success" :: Text)
        , "data" .= result
        , "toolType" .= ("devtool-patch" :: Text)
        ]
callResultToJson (DeveloperToolError _ err) =
    Aeson.object
        [ "type" .= ("error" :: Text)
        , "error" .= show err
        , "toolType" .= ("devtool-err" :: Text)
        ]
callResultToJson (LuaToolResult _ result) =
    Aeson.object
        [ "type" .= ("success" :: Text)
        , "data" .= result
        , "toolType" .= ("lua" :: Text)
        ]
callResultToJson (LuaToolError _ err) =
    Aeson.object
        [ "type" .= ("error" :: Text)
        , "error" .= err
        , "toolType" .= ("lua" :: Text)
        ]

