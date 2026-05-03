{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Tool portal module for LuaToolbox.

This module exposes the ToolPortal to Lua scripts, allowing them to call
other tools (bash, sqlite, http, etc.) directly from within the sandbox.

Security features:
* Tool whitelist enforcement - only allowed tools can be called
* Empty whitelist means NO tools allowed (secure default)
* Portal availability check - returns "not-found" if no portal configured
* All calls are traced for debugging and auditing

Context Propagation:
When a Lua script is executed as part of a tool call, the parent context
(containing OS World and EventQueue for TUI visibility) is captured and
passed through to nested tool calls. This enables subcall conversations
initiated from Lua to be visible in the TUI.

Lua API:

> local tools = require("tools")
> local result = tools.call("bash", {command = "echo hello"})
>
> if result.status == "ok" then
>     local json = require("json")
>     local data = json.decode(result.result_txt)
>     print(data.stdout)
> end
>
> -- List available tools
> local availableTools = tools.list()
> for i, toolName in ipairs(availableTools) do
>     print("Tool: " .. toolName)
> end

The result table from tools.call has the following structure:
* status: "ok" | "error" | "not-found" | "not-allowed"
* result_txt: JSON string with result data (empty string on error)
* duration: execution time in seconds (number, optional)
* trace_id: trace identifier string (optional)

The tools.list() function returns an array of tool name strings.
-}
module System.Agents.Tools.LuaToolbox.Modules.Tools (
    ToolsConfig (..),
    ToolsTrace (..),
    registerToolsModule,
) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Text as AesonText
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LazyText
import qualified HsLua as Lua
import Prod.Tracer (Tracer (..), runTracer)

import System.Agents.Tools.Context (ToolCall (..), ToolExecutionContext, ToolPortal, ToolResult (..))
import System.Agents.Tools.LuaToolbox.Utils (luaToJsonValue)

stackIdxToInt :: Lua.StackIndex -> Int
stackIdxToInt (Lua.StackIndex n) = fromIntegral n

getStackInt :: Lua.StackIndex -> Int
getStackInt = stackIdxToInt

{- | Trace events for tool portal operations.

These events allow tracking of all tool calls through the portal for
debugging, auditing, and monitoring purposes.
-}
data ToolsTrace
    = -- | Tool call initiated
      ToolsCallTrace !Text !Aeson.Value
    | -- | Tool call completed with result
      ToolsResultTrace !Text !Text !Aeson.Value
    | -- | Tool call blocked (not in whitelist)
      ToolsBlockedTrace !Text ![Text]
    deriving (Show, Eq)

{- | Tools module configuration.

Security defaults:
* Empty allowedTools means NO tools allowed (secure default)

Example:
@
ToolsConfig
    { toolsAllowedTools = ["bash", "sqlite_memory_query", "http_get"]
    }
@
-}
data ToolsConfig = ToolsConfig
    { toolsAllowedTools :: [Text]
    -- ^ Whitelist of allowed tool names. If empty, NO tools are allowed (secure default).
    }
    deriving (Show, Eq)

{- | Register the tools module in the Lua state.

The parent context is captured in the closure and passed to nested tool calls,
enabling OS integration fields (World, EventQueue) to propagate for TUI visibility.
-}
registerToolsModule ::
    Tracer IO ToolsTrace ->
    Lua.State ->
    ToolsConfig ->
    -- | Parent execution context for OS field propagation
    ToolExecutionContext ->
    ToolPortal ->
    IO ()
registerToolsModule tracer lstate config parentCtx portal = Lua.runWith lstate $ do
    Lua.newtable

    Lua.pushName "call"
    Lua.pushHaskellFunction (luaCall tracer config parentCtx portal)
    Lua.settable (Lua.nthTop 3)

    Lua.pushName "list"
    Lua.pushHaskellFunction (luaList config)
    Lua.settable (Lua.nthTop 3)

    Lua.setglobal (Lua.Name "tools")

{- | Lua function to list available tools.
Usage: tools.list() -> array of tool name strings
Returns an array containing all tool names in the allowed whitelist.
-}
luaList ::
    ToolsConfig ->
    Lua.LuaE Lua.Exception Lua.NumResults
luaList config = do
    -- Pop any arguments (ignore them)
    top <- Lua.gettop
    Lua.pop (getStackInt top)

    -- Create result array table
    Lua.newtable

    -- Add each allowed tool to the array
    let allowedTools = toolsAllowedTools config
    mapM_ (\(idx, toolName) -> pushToolName (idx + 1) toolName) (zip [0 ..] allowedTools)

    -- Return the array
    pure 1
  where
    pushToolName :: Int -> Text -> Lua.LuaE Lua.Exception ()
    pushToolName idx toolName = do
        Lua.pushinteger (fromIntegral idx)
        Lua.pushstring (Text.encodeUtf8 toolName)
        Lua.settable (Lua.nthTop 3)

{- | Lua function to call a tool through the portal.
Usage: tools.call(toolName, payload) -> result table

The parent context is passed to the portal, enabling OS integration fields
(World, EventQueue) to propagate to nested tool calls for TUI visibility.
-}
luaCall ::
    Tracer IO ToolsTrace ->
    ToolsConfig ->
    -- | Parent execution context for OS field propagation
    ToolExecutionContext ->
    ToolPortal ->
    Lua.LuaE Lua.Exception Lua.NumResults
luaCall tracer config parentCtx portal = do
    top <- Lua.gettop
    let topInt = getStackInt top

    if topInt < 2
        then do
            Lua.pop topInt
            pushErrorResult "error" "Usage: tools.call(toolName, payload)" Nothing Nothing
            pure 1
        else do
            -- Get tool name from first argument
            toolNameBytes <- Lua.tostring' (Lua.nthTop 2)
            Lua.pop 1
            -- Get payload table from second argument
            payloadValue <- luaTableToAeson (Lua.nthTop 1)
            Lua.pop topInt

            let toolName = Text.decodeUtf8 toolNameBytes

            -- Check whitelist (empty list = no tools allowed)
            if not (isToolAllowed config toolName)
                then do
                    liftIO $ runTracer tracer (ToolsBlockedTrace toolName (toolsAllowedTools config))
                    pushErrorResult "not-allowed" ("Tool '" <> toolName <> "' not in allowed whitelist") Nothing Nothing
                    pure 1
                else do
                    -- Execute the tool call through portal with parent context
                    liftIO $ runTracer tracer (ToolsCallTrace toolName payloadValue)
                    result <- liftIO $ callToolThroughPortal portal parentCtx toolName payloadValue
                    liftIO $ runTracer tracer (ToolsResultTrace toolName (resultStatus result) (resultData result))
                    pushToolResult result
                    pure 1

{- | Check if a tool is in the allowed whitelist.
Empty whitelist means NO tools are allowed (secure default).
-}
isToolAllowed :: ToolsConfig -> Text -> Bool
isToolAllowed config toolName =
    -- Empty allowed list means no tools allowed (secure default)
    not (null (toolsAllowedTools config)) && toolName `elem` toolsAllowedTools config

{- | Call a tool through the portal with parent context propagation.

The parent context is passed to the portal, enabling OS integration fields
(World, EventQueue) to propagate to nested tool calls for TUI visibility.
-}
callToolThroughPortal :: ToolPortal -> ToolExecutionContext -> Text -> Aeson.Value -> IO ToolResult
callToolThroughPortal portal parentCtx toolName args = do
    let toolCall =
            ToolCall
                { callToolName = toolName
                , callArgs = args
                }
    -- Pass parent context to portal for OS field propagation
    portal (Just parentCtx) toolCall

-- | Extract result status string from a ToolResult.
resultStatus :: ToolResult -> Text
resultStatus result =
    -- Check if result contains an error indicator
    case resultData result of
        Aeson.Object obj ->
            case KeyMap.lookup "error" obj of
                Just _ -> "error"
                Nothing -> "ok"
        _ -> "ok"

-- | Push a tool result as a Lua table.
pushToolResult :: ToolResult -> Lua.LuaE Lua.Exception ()
pushToolResult result = do
    let status = resultStatus result
    let resultTxt = LazyText.toStrict $ AesonText.encodeToLazyText (resultData result)
    let duration = Just (realToFrac (resultDuration result) :: Double)
    let traceId = Just (resultTraceId result)
    pushOkResult status resultTxt duration traceId

-- | Push a successful result table.
pushOkResult :: Text -> Text -> Maybe Double -> Maybe Text -> Lua.LuaE Lua.Exception ()
pushOkResult status resultTxt mDuration mTraceId = do
    Lua.newtable

    -- status field
    Lua.pushName "status"
    Lua.pushstring (Text.encodeUtf8 status)
    Lua.settable (Lua.nthTop 3)

    -- result_txt field
    Lua.pushName "result_txt"
    Lua.pushstring (Text.encodeUtf8 resultTxt)
    Lua.settable (Lua.nthTop 3)

    -- duration field (optional)
    case mDuration of
        Just dur -> do
            Lua.pushName "duration"
            Lua.pushnumber (realToFrac dur)
            Lua.settable (Lua.nthTop 3)
        Nothing -> pure ()

    -- trace_id field (optional)
    case mTraceId of
        Just tid -> do
            Lua.pushName "trace_id"
            Lua.pushstring (Text.encodeUtf8 tid)
            Lua.settable (Lua.nthTop 3)
        Nothing -> pure ()

-- | Push an error result table.
pushErrorResult :: Text -> Text -> Maybe Double -> Maybe Text -> Lua.LuaE Lua.Exception ()
pushErrorResult status errMsg mDuration mTraceId = do
    Lua.newtable

    -- status field
    Lua.pushName "status"
    Lua.pushstring (Text.encodeUtf8 status)
    Lua.settable (Lua.nthTop 3)

    -- result_txt field (empty or error message)
    Lua.pushName "result_txt"
    Lua.pushstring (Text.encodeUtf8 errMsg)
    Lua.settable (Lua.nthTop 3)

    -- duration field (optional)
    case mDuration of
        Just dur -> do
            Lua.pushName "duration"
            Lua.pushnumber (realToFrac dur)
            Lua.settable (Lua.nthTop 3)
        Nothing -> pure ()

    -- trace_id field (optional)
    case mTraceId of
        Just tid -> do
            Lua.pushName "trace_id"
            Lua.pushstring (Text.encodeUtf8 tid)
            Lua.settable (Lua.nthTop 3)
        Nothing -> pure ()

-- | Convert a Lua table at the given index to an Aeson Value.
luaTableToAeson :: Lua.StackIndex -> Lua.LuaE Lua.Exception Aeson.Value
luaTableToAeson idx = do
    -- Use the standard luaToJsonValue conversion from Utils
    -- First, we need to push a copy of the table/value to the top
    Lua.pushvalue idx
    ret <- luaToJsonValue
    Lua.pop 1
    pure ret
