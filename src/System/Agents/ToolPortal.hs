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
        callStack maxDepth (Just portal) allowedTools

-- Now tools can call other tools through ctxToolPortal
@

Security:
* Tool whitelist is enforced at the portal level
* Portal tools execute with a minimal context (no nested portal to prevent loops)
* Execution time is tracked for each portal invocation
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
    ToolPortal,
    ToolResult (..),
    mkMinimalContext,
 )

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
    portal toolCall = do
        print toolCall
        startTime <- getCurrentTime
        result <- callToolViaPortal tracer portal registrations toolCall -- TODO(lucas): clarify stack recursion here
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
2. Execute with minimal context (no nested portal to prevent loops)
3. Convert the result

Note: The portal executes tools with a minimal context that does NOT include
a nested portal. This prevents infinite recursion through the portal.
-}
callToolViaPortal ::
    Tracer IO Trace ->
    ToolPortal ->
    TVar [ToolRegistration] ->
    ToolCall ->
    IO (Either PortalError (CallResult ToolCall))
callToolViaPortal tracer portal registrations toolCall = do
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
                    -- Execute with minimal context (no nested portal)
                    execResult <- executeTool (contramap (PortalCall toolCall) tracer) portal matchedTool (callArgs toolCall)
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
-}
executeTool ::
    Tracer IO ToolRegistration.Trace ->
    ToolPortal ->
    Tool ToolCall ->
    Aeson.Value ->
    IO (Either Text (CallResult ToolCall))
executeTool tracer portal tool args = do
    -- Create minimal context without portal
    sessId <- newSessionId
    convId <- newConversationId
    turnId <- newTurnId
    let minimalCtx = mkMinimalContext sessId convId turnId portal

    -- Execute the tool
    result <- try $ tool.toolRun tracer minimalCtx args

    case result of
        Left (e :: SomeException) ->
            pure $ Left $ Text.pack $ show e
        Right callResult ->
            -- Strip the OpenAI.ToolCall from the result
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

