{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Tools module for LuaToolbox - portal integration for calling other tools with tracing.

This module provides Lua scripts with the ability to call other tools through
the portal mechanism. It implements:

* Generic tool invocation via @tools.call(toolName, args)@
* Convenience wrappers for common tools (bash, sqlite, postgrest)
* Tool whitelist enforcement for security
* Comprehensive tracing for nested tool calls with unique trace IDs
* Proper error handling and result conversion

The module integrates with the ToolPortal from System.Agents.Tools.Context
to enable inter-toolbox communication and uses Prod.Tracer for detailed
trace events.

Example Lua usage:

@
-- Generic tool call
local result = tools.call("bash", {command = "ls -la"})
print(result.data.stdout)

-- Convenience wrapper for bash
local r = tools.bash.run("ls -la", {cwd = "/tmp"})
print(r.data.exitCode, r.data.stdout)

-- SQLite query
local rows = tools.sqlite.query("/path/to/db.sqlite", "SELECT * FROM users")
for _, row in ipairs(rows.data.rows) do
    print(row[1], row[2])
end
@

Tracing:
Each tool call generates unique trace events:
* ToolCallStarted - when the call begins
* ToolCallCompleted - when the call succeeds
* ToolCallFailed - when the call fails

These events include timing information for performance analysis.
-}
module System.Agents.Tools.LuaToolbox.Modules.Tools (
    -- * Configuration
    ToolsConfig (..),

    -- * Tracing
    Trace (..),
    TraceId,

    -- * Module registration
    registerToolsModule,
) where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Scientific as Scientific
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import qualified Data.UUID.V4 as UUID
import qualified Data.Vector as Vector
import qualified HsLua as Lua

import Prod.Tracer (Tracer, runTracer)

import System.Agents.Tools.Context (ToolCall (..), ToolPortal, ToolResult (..))

-------------------------------------------------------------------------------
-- Tracing Types
-------------------------------------------------------------------------------

-- | Unique identifier for trace events.
type TraceId = Text

{- | Trace events for tool invocations.

These events allow detailed tracing of tool calls for debugging,
performance analysis, and call tree reconstruction.
-}
data Trace
    = {- | Tool call has started.
      Arguments: traceId, callerId, toolName, args
      -}
      ToolCallStarted TraceId Text Text Aeson.Value
    | {- | Tool call completed successfully.
      Arguments: traceId, duration, result
      -}
      ToolCallCompleted TraceId NominalDiffTime ToolResult
    | {- | Tool call failed with an error.
      Arguments: traceId, error message
      -}
      ToolCallFailed TraceId Text
    deriving (Show)

-------------------------------------------------------------------------------
-- Helpers for Lua stack operations
-------------------------------------------------------------------------------

-- | Convert StackIndex to Int
stackIdxToInt :: Lua.StackIndex -> Int
stackIdxToInt (Lua.StackIndex n) = fromIntegral n

-------------------------------------------------------------------------------
-- Configuration
-------------------------------------------------------------------------------

{- | Configuration for the tools module.

The configuration includes:
* Optional portal callback for invoking tools
* Whitelist of allowed tool names
* Caller identifier for tracing
* Tracer for emitting trace events

An empty whitelist means all tools are allowed (backward compatibility).
-}
data ToolsConfig = ToolsConfig
    { toolsPortal :: Maybe ToolPortal
    -- ^ The portal callback for invoking tools. If Nothing, tool calls will fail.
    , toolsAllowed :: [Text]
    -- ^ Whitelist of allowed tool names. Empty list means all tools allowed.
    , toolsCallerId :: Text
    -- ^ Identifier for this Lua script (used for tracing nested calls)
    , toolsTracer :: Tracer IO Trace
    -- ^ Tracer for emitting tool call events
    }

-- | Custom Show instance that doesn't try to show the portal function or tracer
instance Show ToolsConfig where
    show cfg =
        "ToolsConfig { toolsPortal = "
            <> portalStr
            <> ", toolsAllowed = "
            <> show cfg.toolsAllowed
            <> ", toolsCallerId = "
            <> show cfg.toolsCallerId
            <> ", toolsTracer = <tracer> }"
      where
        portalStr = case cfg.toolsPortal of
            Nothing -> "Nothing"
            Just _ -> "Just <portal>"

-------------------------------------------------------------------------------
-- Module Registration
-------------------------------------------------------------------------------

{- | Register the tools module in the Lua state.

This creates a global @tools@ table with the following structure:

@
tools = {
    call = function(toolName, args) ... end,
    bash = {
        run = function(command, opts) ... end
    },
    sqlite = {
        query = function(db, sql, params) ... end,
        execute = function(db, sql, params) ... end
    }
}
@

The configuration is stored in the Lua registry for access by the functions.
-}
registerToolsModule :: Lua.State -> ToolsConfig -> IO ()
registerToolsModule lstate config = Lua.runWith lstate $ do
    Lua.newtable

    -- tools.call(toolName, args)
    Lua.pushName "call"
    Lua.pushHaskellFunction (luaCall config)
    Lua.settable (Lua.nthTop 3)

    -- Create convenience wrappers for common tools
    createToolWrappers config

    Lua.setglobal (Lua.Name "tools")

-------------------------------------------------------------------------------
-- Core Tool Call Function with Tracing
-------------------------------------------------------------------------------

{- | Lua function: tools.call(toolName, args)

Calls a tool through the portal with tracing and returns the result.

Tracing flow:
1. Generate unique trace ID
2. Emit ToolCallStarted event
3. Execute tool call
4. Emit ToolCallCompleted or ToolCallFailed event
5. Return result with traceId

Returns one of:
* result table with data, duration, and traceId on success
* nil, error string on failure

The result table has the following structure:
@
{
    data = { ... },         -- Tool-specific result data
    duration = 0.123,       -- Execution time in seconds
    traceId = "uuid-..."    -- Trace identifier for debugging
}
@
-}
luaCall :: ToolsConfig -> Lua.LuaE Lua.Exception Lua.NumResults
luaCall config = do
    top <- Lua.gettop
    let topInt = stackIdxToInt top

    if topInt < 1
        then do
            Lua.pop topInt
            Lua.pushnil
            Lua.pushstring "Usage: tools.call(toolName, [args])"
            pure 2
        else do
            toolNameBs <- Lua.tostring' (Lua.nthTop (fromIntegral topInt))
            let toolName = Text.decodeUtf8 toolNameBs

            -- Get args table if provided, otherwise empty object
            argsValue <-
                if topInt >= 2
                    then do
                        ltype <- Lua.ltype (Lua.nthTop (fromIntegral topInt - 1))
                        if ltype == Lua.TypeTable
                            then do
                                val <- luaTableToAesonValue (Lua.nthTop (fromIntegral topInt - 1))
                                Lua.pop topInt
                                pure val
                            else do
                                Lua.pop topInt
                                pure (Aeson.Object mempty)
                    else do
                        Lua.pop topInt
                        pure (Aeson.Object mempty)

            callToolWithTracing toolName argsValue config

{- | Check if a tool is allowed based on the whitelist.

Empty whitelist means all tools are allowed.
-}
isToolAllowed' :: Text -> ToolsConfig -> Bool
isToolAllowed' toolName config =
    null config.toolsAllowed || toolName `elem` config.toolsAllowed

-------------------------------------------------------------------------------
-- Tool Call with Tracing
-------------------------------------------------------------------------------

{- | Call a tool with full tracing support.

This function:
1. Checks if the tool is allowed
2. Generates a unique trace ID
3. Emits ToolCallStarted event
4. Executes the tool call
5. Emits ToolCallCompleted or ToolCallFailed event
6. Returns the result with traceId
-}
callToolWithTracing :: Text -> Aeson.Value -> ToolsConfig -> Lua.LuaE Lua.Exception Lua.NumResults
callToolWithTracing toolName args config = do
    if not (isToolAllowed' toolName config)
        then do
            Lua.pushnil
            Lua.pushstring (Text.encodeUtf8 $ "Tool not allowed: " <> toolName)
            pure 2
        else case toolsPortal config of
            Nothing -> do
                Lua.pushnil
                Lua.pushstring "Tool portal not available"
                pure 2
            Just portal -> do
                -- Generate unique trace ID
                traceId <- liftIO $ Text.pack . show <$> UUID.nextRandom
                startTime <- liftIO getCurrentTime

                -- Trace start
                liftIO $
                    runTracer (toolsTracer config) $
                        ToolCallStarted traceId (toolsCallerId config) toolName args

                let call =
                        ToolCall
                            { callToolName = toolName
                            , callArgs = args
                            , callCallerId = toolsCallerId config
                            }

                -- Execute through portal (blocking IO)
                result <- liftIO $ try $ portal call

                endTime <- liftIO getCurrentTime
                let duration = diffUTCTime endTime startTime

                case result of
                    Left (e :: SomeException) -> do
                        let errMsg = Text.pack $ show e
                        liftIO $
                            runTracer (toolsTracer config) $
                                ToolCallFailed traceId errMsg
                        Lua.pushnil
                        Lua.pushstring (Text.encodeUtf8 errMsg)
                        pure 2
                    Right toolResult -> do
                        -- Create result with our trace ID for call tree reconstruction
                        let resultWithTrace = toolResult{resultTraceId = traceId}
                        liftIO $
                            runTracer (toolsTracer config) $
                                ToolCallCompleted traceId duration resultWithTrace
                        pushToolResult resultWithTrace
                        pure 1

-------------------------------------------------------------------------------
-- Convenience Wrappers
-------------------------------------------------------------------------------

{- | Create convenience wrappers for common tools.

Creates nested tables like @tools.bash@, @tools.sqlite@ with
higher-level functions for specific tool patterns.
-}
createToolWrappers :: ToolsConfig -> Lua.Lua ()
createToolWrappers config = do
    let prefixes = ["bash", "sqlite", "postgrest", "system", "io"]
    mapM_ (createPrefixWrapper config) prefixes

-- | Create a wrapper table for a tool prefix (e.g., tools.bash)
createPrefixWrapper :: ToolsConfig -> Text -> Lua.Lua ()
createPrefixWrapper config prefix = do
    Lua.pushstring (Text.encodeUtf8 prefix)
    Lua.newtable

    -- Add generic call method: tools.<prefix>.call(args)
    -- This calls <prefix> with the args directly
    Lua.pushName "call"
    Lua.pushHaskellFunction (luaCallWithPrefix prefix config)
    Lua.settable (Lua.nthTop 3)

    -- Add tool-specific convenience methods
    addConvenienceMethods prefix config

    Lua.settable (Lua.nthTop 3)

-- | Add tool-specific convenience methods to a prefix table.
addConvenienceMethods :: Text -> ToolsConfig -> Lua.Lua ()
addConvenienceMethods "bash" config = do
    -- tools.bash.run(command, opts)
    Lua.pushName "run"
    Lua.pushHaskellFunction (luaBashRun config)
    Lua.settable (Lua.nthTop 3)
addConvenienceMethods "sqlite" config = do
    -- tools.sqlite.query(db, sql, params)
    Lua.pushName "query"
    Lua.pushHaskellFunction (luaSqliteQuery config)
    Lua.settable (Lua.nthTop 3)

    -- tools.sqlite.execute(db, sql, params)
    Lua.pushName "execute"
    Lua.pushHaskellFunction (luaSqliteExecute config)
    Lua.settable (Lua.nthTop 3)
addConvenienceMethods "postgrest" config = do
    -- tools.postgrest.get(endpoint, opts)
    Lua.pushName "get"
    Lua.pushHaskellFunction (luaPostgrestGet config)
    Lua.settable (Lua.nthTop 3)

    -- tools.postgrest.post(endpoint, body, opts)
    Lua.pushName "post"
    Lua.pushHaskellFunction (luaPostgrestPost config)
    Lua.settable (Lua.nthTop 3)

    -- tools.postgrest.patch(endpoint, body, opts)
    Lua.pushName "patch"
    Lua.pushHaskellFunction (luaPostgrestPatch config)
    Lua.settable (Lua.nthTop 3)

    -- tools.postgrest.delete(endpoint, opts)
    Lua.pushName "delete"
    Lua.pushHaskellFunction (luaPostgrestDelete config)
    Lua.settable (Lua.nthTop 3)
addConvenienceMethods "system" config = do
    -- tools.system.info(capability)
    Lua.pushName "info"
    Lua.pushHaskellFunction (luaSystemInfo config)
    Lua.settable (Lua.nthTop 3)
addConvenienceMethods _ _ = pure ()

-------------------------------------------------------------------------------
-- Bash Tool Convenience Functions
-------------------------------------------------------------------------------

{- | tools.bash.run(command, opts)

Executes a bash command with optional parameters.

Returns: result table with data containing exitCode, stdout, stderr
-}
luaBashRun :: ToolsConfig -> Lua.LuaE Lua.Exception Lua.NumResults
luaBashRun config = do
    top <- Lua.gettop
    let topInt = stackIdxToInt top

    if topInt < 1
        then do
            Lua.pop topInt
            Lua.pushnil
            Lua.pushstring "Usage: tools.bash.run(command, [opts])"
            pure 2
        else do
            cmdBs <- Lua.tostring' (Lua.nthTop (fromIntegral topInt))
            let cmd = Text.decodeUtf8 cmdBs

            -- Parse options if provided
            opts <-
                if topInt >= 2
                    then luaTableToKeyMap (Lua.nthTop (fromIntegral topInt - 1))
                    else pure mempty

            Lua.pop topInt

            let args =
                    Aeson.object
                        [ "command" .= cmd
                        , "cwd" .= KeyMap.lookup (Aeson.Key.fromText "cwd") opts
                        , "env" .= KeyMap.lookup (Aeson.Key.fromText "env") opts
                        , "timeout" .= KeyMap.lookup (Aeson.Key.fromText "timeout") opts
                        ]

            callToolWithTracing "bash" args config

-------------------------------------------------------------------------------
-- SQLite Tool Convenience Functions
-------------------------------------------------------------------------------

{- | tools.sqlite.query(db, sql, params)

Executes a SELECT query and returns rows.

Returns: result table with data containing columns, rows, rowCount
-}
luaSqliteQuery :: ToolsConfig -> Lua.LuaE Lua.Exception Lua.NumResults
luaSqliteQuery config = do
    top <- Lua.gettop
    let topInt = stackIdxToInt top

    if topInt < 2
        then do
            Lua.pop topInt
            Lua.pushnil
            Lua.pushstring "Usage: tools.sqlite.query(db, sql, [params])"
            pure 2
        else do
            dbBs <- Lua.tostring' (Lua.nthTop (fromIntegral topInt))
            sqlBs <- Lua.tostring' (Lua.nthTop (fromIntegral topInt - 1))
            let db = Text.decodeUtf8 dbBs
            let sql = Text.decodeUtf8 sqlBs

            -- Parse params if provided
            params <-
                if topInt >= 3
                    then do
                        ltype <- Lua.ltype (Lua.nthTop (fromIntegral topInt - 2))
                        if ltype == Lua.TypeTable
                            then luaTableToAesonValue (Lua.nthTop (fromIntegral topInt - 2))
                            else pure (Aeson.Array mempty)
                    else pure (Aeson.Array mempty)

            Lua.pop topInt

            let args =
                    Aeson.object
                        [ "database" .= db
                        , "sql" .= sql
                        , "params" .= params
                        ]

            callToolWithTracing "sqlite_query" args config

{- | tools.sqlite.execute(db, sql, params)

Executes a non-SELECT statement (INSERT, UPDATE, DELETE, etc.).

Returns: result table with data containing affectedRows, lastInsertRowId
-}
luaSqliteExecute :: ToolsConfig -> Lua.LuaE Lua.Exception Lua.NumResults
luaSqliteExecute config = do
    top <- Lua.gettop
    let topInt = stackIdxToInt top

    if topInt < 2
        then do
            Lua.pop topInt
            Lua.pushnil
            Lua.pushstring "Usage: tools.sqlite.execute(db, sql, [params])"
            pure 2
        else do
            dbBs <- Lua.tostring' (Lua.nthTop (fromIntegral topInt))
            sqlBs <- Lua.tostring' (Lua.nthTop (fromIntegral topInt - 1))
            let db = Text.decodeUtf8 dbBs
            let sql = Text.decodeUtf8 sqlBs

            -- Parse params if provided
            params <-
                if topInt >= 3
                    then do
                        ltype <- Lua.ltype (Lua.nthTop (fromIntegral topInt - 2))
                        if ltype == Lua.TypeTable
                            then luaTableToAesonValue (Lua.nthTop (fromIntegral topInt - 2))
                            else pure (Aeson.Array mempty)
                    else pure (Aeson.Array mempty)

            Lua.pop topInt

            let args =
                    Aeson.object
                        [ "database" .= db
                        , "sql" .= sql
                        , "params" .= params
                        ]

            callToolWithTracing "sqlite_execute" args config

-------------------------------------------------------------------------------
-- PostgREST Tool Convenience Functions
-------------------------------------------------------------------------------

{- | tools.postgrest.get(endpoint, opts)

Performs a GET request to a PostgREST endpoint.

Returns: result table with data from the response
-}
luaPostgrestGet :: ToolsConfig -> Lua.LuaE Lua.Exception Lua.NumResults
luaPostgrestGet config = do
    top <- Lua.gettop
    let topInt = stackIdxToInt top

    if topInt < 1
        then do
            Lua.pop topInt
            Lua.pushnil
            Lua.pushstring "Usage: tools.postgrest.get(endpoint, [opts])"
            pure 2
        else do
            endpointBs <- Lua.tostring' (Lua.nthTop (fromIntegral topInt))
            let endpoint = Text.decodeUtf8 endpointBs

            -- Parse options if provided
            opts <-
                if topInt >= 2
                    then luaTableToAesonValue (Lua.nthTop (fromIntegral topInt - 1))
                    else pure (Aeson.Object mempty)

            Lua.pop topInt

            let args =
                    Aeson.object
                        [ "endpoint" .= endpoint
                        , "method" .= ("GET" :: Text)
                        , "options" .= opts
                        ]

            callToolWithTracing "postgrest" args config

{- | tools.postgrest.post(endpoint, body, opts)

Performs a POST request to a PostgREST endpoint.
-}
luaPostgrestPost :: ToolsConfig -> Lua.LuaE Lua.Exception Lua.NumResults
luaPostgrestPost config = do
    top <- Lua.gettop
    let topInt = stackIdxToInt top

    if topInt < 2
        then do
            Lua.pop topInt
            Lua.pushnil
            Lua.pushstring "Usage: tools.postgrest.post(endpoint, body, [opts])"
            pure 2
        else do
            endpointBs <- Lua.tostring' (Lua.nthTop (fromIntegral topInt))
            body <- luaTableToAesonValue (Lua.nthTop (fromIntegral topInt - 1))
            let endpoint = Text.decodeUtf8 endpointBs

            -- Parse options if provided
            opts <-
                if topInt >= 3
                    then luaTableToAesonValue (Lua.nthTop (fromIntegral topInt - 2))
                    else pure (Aeson.Object mempty)

            Lua.pop topInt

            let args =
                    Aeson.object
                        [ "endpoint" .= endpoint
                        , "method" .= ("POST" :: Text)
                        , "body" .= body
                        , "options" .= opts
                        ]

            callToolWithTracing "postgrest" args config

{- | tools.postgrest.patch(endpoint, body, opts)

Performs a PATCH request to a PostgREST endpoint.
-}
luaPostgrestPatch :: ToolsConfig -> Lua.LuaE Lua.Exception Lua.NumResults
luaPostgrestPatch config = do
    top <- Lua.gettop
    let topInt = stackIdxToInt top

    if topInt < 2
        then do
            Lua.pop topInt
            Lua.pushnil
            Lua.pushstring "Usage: tools.postgrest.patch(endpoint, body, [opts])"
            pure 2
        else do
            endpointBs <- Lua.tostring' (Lua.nthTop (fromIntegral topInt))
            body <- luaTableToAesonValue (Lua.nthTop (fromIntegral topInt - 1))
            let endpoint = Text.decodeUtf8 endpointBs

            opts <-
                if topInt >= 3
                    then luaTableToAesonValue (Lua.nthTop (fromIntegral topInt - 2))
                    else pure (Aeson.Object mempty)

            Lua.pop topInt

            let args =
                    Aeson.object
                        [ "endpoint" .= endpoint
                        , "method" .= ("PATCH" :: Text)
                        , "body" .= body
                        , "options" .= opts
                        ]

            callToolWithTracing "postgrest" args config

{- | tools.postgrest.delete(endpoint, opts)

Performs a DELETE request to a PostgREST endpoint.
-}
luaPostgrestDelete :: ToolsConfig -> Lua.LuaE Lua.Exception Lua.NumResults
luaPostgrestDelete config = do
    top <- Lua.gettop
    let topInt = stackIdxToInt top

    if topInt < 1
        then do
            Lua.pop topInt
            Lua.pushnil
            Lua.pushstring "Usage: tools.postgrest.delete(endpoint, [opts])"
            pure 2
        else do
            endpointBs <- Lua.tostring' (Lua.nthTop (fromIntegral topInt))
            let endpoint = Text.decodeUtf8 endpointBs

            opts <-
                if topInt >= 2
                    then luaTableToAesonValue (Lua.nthTop (fromIntegral topInt - 1))
                    else pure (Aeson.Object mempty)

            Lua.pop topInt

            let args =
                    Aeson.object
                        [ "endpoint" .= endpoint
                        , "method" .= ("DELETE" :: Text)
                        , "options" .= opts
                        ]

            callToolWithTracing "postgrest" args config

-------------------------------------------------------------------------------
-- System Tool Convenience Functions
-------------------------------------------------------------------------------

{- | tools.system.info(capability)

Retrieves system information based on capability.

Returns: result table with system information
-}
luaSystemInfo :: ToolsConfig -> Lua.LuaE Lua.Exception Lua.NumResults
luaSystemInfo config = do
    top <- Lua.gettop
    let topInt = stackIdxToInt top

    if topInt < 1
        then do
            Lua.pop topInt
            Lua.pushnil
            Lua.pushstring "Usage: tools.system.info(capability)"
            pure 2
        else do
            capBs <- Lua.tostring' (Lua.nthTop (fromIntegral topInt))
            let capability = Text.decodeUtf8 capBs
            Lua.pop topInt

            let args = Aeson.object ["capability" .= capability]
            callToolWithTracing "system" args config

-------------------------------------------------------------------------------
-- Helper Functions
-------------------------------------------------------------------------------

{- | Call a tool by prefix, passing args directly.

This is used for tools.bash.call({...}) which is equivalent to
tools.call("bash", {...})
-}
luaCallWithPrefix :: Text -> ToolsConfig -> Lua.LuaE Lua.Exception Lua.NumResults
luaCallWithPrefix prefix config = do
    top <- Lua.gettop
    let topInt = stackIdxToInt top

    argsValue <-
        if topInt >= 1
            then do
                ltype <- Lua.ltype (Lua.nthTop (fromIntegral topInt))
                if ltype == Lua.TypeTable
                    then do
                        val <- luaTableToAesonValue (Lua.nthTop (fromIntegral topInt))
                        Lua.pop topInt
                        pure val
                    else do
                        Lua.pop topInt
                        pure (Aeson.Object mempty)
            else do
                Lua.pop topInt
                pure (Aeson.Object mempty)

    callToolWithTracing prefix argsValue config

{- | Convert a ToolResult to a Lua table.

Creates a table with:
* data - the result data (converted from JSON)
* duration - execution time as number
* traceId - trace identifier as string
-}
pushToolResult :: ToolResult -> Lua.Lua ()
pushToolResult tr = do
    Lua.newtable

    -- result.data
    Lua.pushName "data"
    aesonToLuaValue (resultData tr)
    Lua.settable (Lua.nthTop 3)

    -- result.duration
    Lua.pushName "duration"
    Lua.pushnumber (realToFrac $ resultDuration tr)
    Lua.settable (Lua.nthTop 3)

    -- result.traceId
    Lua.pushName "traceId"
    Lua.pushstring (Text.encodeUtf8 $ resultTraceId tr)
    Lua.settable (Lua.nthTop 3)

-------------------------------------------------------------------------------
-- Lua to Aeson Conversion
-------------------------------------------------------------------------------

{- | Convert a Lua table at the given index to an Aeson Value.

This function does NOT pop the table from the stack.
-}
luaTableToAesonValue :: Lua.StackIndex -> Lua.LuaE Lua.Exception Aeson.Value
luaTableToAesonValue idx = do
    isArr <- isArrayAt idx
    if isArr
        then convertArrayAt idx
        else convertObjectAt idx

{- | Check if the table at the given index is an array.

An array is defined as a table with only sequential integer keys starting at 1.
-}
isArrayAt :: Lua.StackIndex -> Lua.LuaE Lua.Exception Bool
isArrayAt idx = do
    Lua.pushnil
    isSequentialAt idx 1
  where
    isSequentialAt :: Lua.StackIndex -> Int -> Lua.LuaE Lua.Exception Bool
    isSequentialAt idx' expectedIdx = do
        hasNext <- Lua.next idx'
        if not hasNext
            then pure True
            else do
                isNum <- Lua.isnumber (Lua.nthTop 1)
                if isNum
                    then do
                        mIdx <- Lua.tointeger (Lua.nthTop 1)
                        Lua.pop 2
                        case mIdx of
                            Just idxVal
                                | idxVal == fromIntegral expectedIdx ->
                                    isSequentialAt idx' (expectedIdx + 1)
                            _ -> pure False
                    else do
                        Lua.pop 2
                        pure False

-- | Convert array table at index to Aeson Array.
convertArrayAt :: Lua.StackIndex -> Lua.LuaE Lua.Exception Aeson.Value
convertArrayAt idx = do
    len' <- Lua.rawlen idx
    vals <-
        mapM
            ( \i -> do
                Lua.pushinteger (fromIntegral i)
                _ <- Lua.gettable idx
                val <- luaValueToAeson
                pure val
            )
            [1 .. fromIntegral len']
    pure $ Aeson.Array $ Vector.fromList vals

-- | Convert object table at index to Aeson Object.
convertObjectAt :: Lua.StackIndex -> Lua.LuaE Lua.Exception Aeson.Value
convertObjectAt idx = do
    Lua.pushnil
    pairs <- go []
    pure $ Aeson.Object $ KeyMap.fromList pairs
  where
    go acc = do
        hasNext <- Lua.next idx
        if not hasNext
            then pure acc
            else do
                keyBs <- Lua.tostring' (Lua.nthTop 2)
                val <- luaValueToAeson
                Lua.pop 1
                go ((Aeson.Key.fromText (Text.decodeUtf8 keyBs), val) : acc)

{- | Convert the value at the top of the stack to Aeson Value.

Pops the value from the stack.
-}
luaValueToAeson :: Lua.LuaE Lua.Exception Aeson.Value
luaValueToAeson = do
    ltype <- Lua.ltype (Lua.nthTop 1)
    case ltype of
        Lua.TypeNil -> do
            Lua.pop 1
            pure Aeson.Null
        Lua.TypeBoolean -> do
            b <- Lua.toboolean (Lua.nthTop 1)
            Lua.pop 1
            pure $ Aeson.Bool b
        Lua.TypeNumber -> do
            mNum <- Lua.tonumber (Lua.nthTop 1)
            Lua.pop 1
            case mNum of
                Nothing -> pure Aeson.Null
                Just n -> pure $ Aeson.Number $ fromRational $ toRational n
        Lua.TypeString -> do
            s <- Lua.tostring' (Lua.nthTop 1)
            Lua.pop 1
            pure $ Aeson.String (Text.decodeUtf8 s)
        Lua.TypeTable -> do
            val <- luaTableToAesonValue (Lua.nthTop 1)
            Lua.pop 1
            pure val
        _ -> do
            Lua.pop 1
            pure Aeson.Null

{- | Convert a Lua table at the given index to a KeyMap of Aeson Values.

This is useful for extracting named parameters from options tables.
-}
luaTableToKeyMap :: Lua.StackIndex -> Lua.LuaE Lua.Exception (KeyMap.KeyMap Aeson.Value)
luaTableToKeyMap idx = do
    Lua.pushnil
    go KeyMap.empty
  where
    go acc = do
        hasNext <- Lua.next idx
        if not hasNext
            then pure acc
            else do
                keyBs <- Lua.tostring' (Lua.nthTop 2)
                val <- luaValueToAeson
                Lua.pop 1
                go (KeyMap.insert (Aeson.Key.fromText (Text.decodeUtf8 keyBs)) val acc)

-------------------------------------------------------------------------------
-- Aeson to Lua Conversion
-------------------------------------------------------------------------------

-- | Convert an Aeson Value to a Lua value on the stack.
aesonToLuaValue :: Aeson.Value -> Lua.Lua ()
aesonToLuaValue Aeson.Null = Lua.pushnil
aesonToLuaValue (Aeson.Bool b) = Lua.pushboolean b
aesonToLuaValue (Aeson.Number n) =
    case Scientific.floatingOrInteger n of
        Left d -> Lua.pushnumber d
        Right i -> Lua.pushinteger (fromIntegral i)
aesonToLuaValue (Aeson.String t) = Lua.pushstring (Text.encodeUtf8 t)
aesonToLuaValue (Aeson.Array arr) = do
    Lua.newtable
    mapM_
        ( \(i, val) -> do
            aesonToLuaValue val
            Lua.pushinteger (fromIntegral (i + 1))
            Lua.insert (Lua.nthTop 2)
            Lua.settable (Lua.nthTop 3)
        )
        (zip [0 ..] (Vector.toList arr))
aesonToLuaValue (Aeson.Object obj) = do
    Lua.newtable
    mapM_
        ( \(k, val) -> do
            Lua.pushstring (Text.encodeUtf8 $ Aeson.Key.toText k)
            aesonToLuaValue val
            Lua.settable (Lua.nthTop 3)
        )
        (KeyMap.toList obj)
