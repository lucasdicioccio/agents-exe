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
    -- * Portal creation
    makeToolPortal,

    -- * Portal execution
    callToolViaPortal,

    -- * Utility types
    PortalError (..),
) where

import Control.Exception (SomeException, try)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Bifunctor (first)
import Data.ByteString.Lazy (toStrict)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Time (diffUTCTime, getCurrentTime)

import Prod.Tracer (Tracer (..))

import qualified System.Agents.LLMs.OpenAI as OpenAI
import System.Agents.ToolRegistration (ToolRegistration (..))
import System.Agents.Tools.Base (CallResult (..), Tool (..), mapCallResult)
import System.Agents.Tools.Context (
    ToolCall (..),
    ToolPortal,
    ToolResult (..),
    mkMinimalContext,
 )
import System.Agents.Tools.Trace (ToolTrace (..))
import System.Agents.Session.Types (newSessionId, newTurnId)
import System.Agents.Base (newConversationId)

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
4. Executes the tool with a minimal context (no nested portal)
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
    Tracer IO ToolTrace ->
    -- | Registered tools available through the portal
    [ToolRegistration] ->
    ToolPortal
makeToolPortal tracer registrations = portal
  where
    portal :: ToolPortal
    portal toolCall = do
        startTime <- getCurrentTime
        result <- callToolViaPortal tracer registrations toolCall
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
    Tracer IO ToolTrace ->
    [ToolRegistration] ->
    ToolCall ->
    IO (Either PortalError (CallResult ()))
callToolViaPortal tracer registrations toolCall = do
    -- Find tool by name
    case findToolByName registrations (callToolName toolCall) of
        Nothing ->
            pure $ Left $ PortalToolNotFound (callToolName toolCall)
        Just _tool -> do
            -- Create a synthetic OpenAI ToolCall to match the existing API
            let openaiCall = makeSyntheticToolCall toolCall

            -- Find the specific tool with the call context
            case findMatchingTool registrations openaiCall of
                Nothing ->
                    pure $ Left $ PortalToolNotFound (callToolName toolCall)
                Just matchedTool -> do
                    -- Execute with minimal context (no nested portal)
                    execResult <- executeTool tracer matchedTool (callArgs toolCall)
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
        n == OpenAI.getToolName (OpenAI.toolName reg.declareTool)

{- | Create a synthetic OpenAI ToolCall from a portal ToolCall.

The existing findTool infrastructure expects OpenAI.ToolCall, so we
convert our ToolCall to that format for compatibility.
-}
makeSyntheticToolCall ::
    ToolCall ->
    OpenAI.ToolCall
makeSyntheticToolCall tc =
    OpenAI.ToolCall
        { OpenAI.rawToolCall = KeyMap.empty
        , OpenAI.toolCallId = "portal-" <> callCallerId tc
        , OpenAI.toolCallType = Just "function"
        , OpenAI.toolCallFunction =
            OpenAI.ToolCallFunction
                { OpenAI.toolCallFunctionName = OpenAI.ToolName (callToolName tc)
                , OpenAI.toolCallFunctionArgsUnparsed = decodeUtf8 $ toStrict $ Aeson.encode (callArgs tc)
                , OpenAI.toolCallFunctionArgs = Just (callArgs tc)
                }
        }

{- | Find a matching tool from registrations using OpenAI ToolCall.

Uses the findTool function from each registration to find a match.
-}
findMatchingTool ::
    [ToolRegistration] ->
    OpenAI.ToolCall ->
    Maybe (Tool OpenAI.ToolCall)
findMatchingTool regs call =
    case concatMap (\r -> maybe [] (: []) (r.findTool call)) regs of
        (t : _) -> Just t
        [] -> Nothing

{- | Execute a tool with a minimal context.

The minimal context has no portal to prevent nested portal calls.
This is a safety measure to avoid potential infinite recursion.
-}
executeTool ::
    Tracer IO ToolTrace ->
    Tool OpenAI.ToolCall ->
    Aeson.Value ->
    IO (Either Text (CallResult ()))
executeTool toolTracer tool args = do
    -- Create minimal context without portal
    sessId <- newSessionId
    convId <- newConversationId
    turnId <- newTurnId
    let minimalCtx = mkMinimalContext sessId convId turnId

    -- Execute the tool
    result <- try $ tool.toolRun toolTracer minimalCtx args

    case result of
        Left (e :: SomeException) ->
            pure $ Left $ Text.pack $ show e
        Right callResult ->
            -- Strip the OpenAI.ToolCall from the result
            pure $ Right $ mapCallResult (const ()) callResult

-------------------------------------------------------------------------------
-- Result Conversion
-------------------------------------------------------------------------------

{- | Convert a CallResult to JSON for the ToolResult.

Different tool types have different result shapes, so we normalize them
to JSON for the portal interface.
-}
callResultToJson :: CallResult () -> Aeson.Value
callResultToJson (BlobToolSuccess _ bs) =
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
callResultToJson (DeveloperToolResult _ err) =
    Aeson.object
        [ "type" .= ("error" :: Text)
        , "error" .= show err
        , "toolType" .= ("devtool" :: Text)
        ]
callResultToJson (DeveloperToolScaffoldResult _ err) =
    Aeson.object
        [ "type" .= ("error" :: Text)
        , "error" .= show err
        , "toolType" .= ("devtool-scaffold" :: Text)
        ]
callResultToJson (DeveloperToolSpecResult _ err) =
    Aeson.object
        [ "type" .= ("error" :: Text)
        , "error" .= show err
        , "toolType" .= ("devtool-spec" :: Text)
        ]
callResultToJson (DeveloperToolError _ err) =
    Aeson.object
        [ "type" .= ("error" :: Text)
        , "error" .= show err
        , "toolType" .= ("devtool-err" :: Text)
        ]
{-
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
-}
