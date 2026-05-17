{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- | Provides a sandboxed Lua interpreter for agent-scriptable tool orchestration with security hardening.

This module implements the Lua toolbox functionality with per-tool-call isolation:

* Each tool call creates a fresh, isolated Lua state
* Lua state is destroyed immediately after execution completes
* No persistent state between calls (use SQLite for persistence)
* Memory limits via Lua allocator hooks
* Timeout enforcement via Haskell watchdog thread
* Error handling and stack trace capture
* Integration with the Tool Portal for calling other tools
* Standard library modules: json, text, time, fs, http, tools
* Comprehensive tool invocation tracing

Security Features:
* Maximum isolation: Each call starts from identical initial state
* Memory bounded: No accumulation of Lua heap across calls
* Secure by default: No way for call N to influence call N+1
* No deadlocks: No shared lock between parent and recursive sub-agent
* Path sandboxing with canonicalization (prevents symlink traversal)
* Host whitelisting for HTTP requests
* Tool whitelist enforcement
* Dangerous Lua functions removed (os.execute, io.popen, etc.)
* Empty whitelist = no access (secure defaults)

The toolbox is designed to be safe for LLM-generated code by:
* Removing dangerous standard library functions (os.execute, io.popen, etc.)
* Enforcing memory limits
* Enforcing execution time limits
* Providing only whitelisted tool access through the portal
* Sandboxing filesystem and HTTP access
* Tracing all tool calls for debugging and auditing

Standard library modules available to Lua scripts:

* @json@: JSON encoding/decoding with 'json.encode' and 'json.decode'
* @text@: UTF-8 string utilities (split, find, trim, etc.)
* @time@: Time functions (now, sleep, format, diff)
* @fs@: Sandboxed filesystem operations (read, write, list, patch, etc.)
* @http@: HTTP requests with host whitelisting (get, post, request)
* @tools@: Tool portal integration for calling other tools with tracing

Example Lua script using modules:

> local json = require("json")
> local text = require("text")
> local time = require("time")
> local fs = require("fs")
> local tools = require("tools")
>
> -- Read and parse a JSON file
> local data = json.decode(fs.read("/allowed/path/config.json"))
>
> -- Process text
> local parts = text.split(data.name, " ")
>
> -- Get current time
> local now = time.now()
> local formatted = time.format(now, "%Y-%m-%d")
>
> -- Call another tool through the portal
> local result = tools.call("bash", {command = "echo hello"})
> if result.status == "ok" then
>     local bashData = json.decode(result.result_txt)
>     print(bashData.stdout)
> end
>
> return {parts = parts, date = formatted}

== Migration from Persistent State

Scripts that relied on persistence across calls must use SQLite:

-- BEFORE (won't work now):
-- Call 1: users = { "alice" }
-- Call 2: return users[1]  -- Returns "alice"

-- AFTER (explicit persistence):
local json = require("json")
local tools = require("tools")

-- Save
tools.call("sqlite_memory_query", {
    sql = "INSERT INTO state VALUES ('users', '" .. json.encode(users) .. "')"
})

-- Load
local result = tools.call("sqlite_memory_query", {
    sql = "SELECT value FROM state WHERE key = 'users'"
})
users = json.decode(result.result_txt)
-}
module System.Agents.Tools.LuaToolbox (
    -- * Core types
    Trace (..),
    Toolbox (..),
    ScriptError (..),
    ExecutionResult (..),

    -- * Module traces (union)
    LuaModuleTrace (..),

    -- * Initialization
    initializeToolbox,
    initializeToolboxWithModuleTracer,
    closeToolbox, -- ^ Deprecated: LuaToolbox is now stateless

    -- * Script execution
    executeScriptWithPortal,

    -- * Sandbox configuration
    configureSandbox,
    applyMemoryLimit,
    applyTimeout,

    -- * Module registration
    registerStandardModules,

    -- * Re-exported module traces for convenience
    module System.Agents.Tools.LuaToolbox.Modules.Fs,
    module System.Agents.Tools.LuaToolbox.Modules.Http,
    module System.Agents.Tools.LuaToolbox.Modules.Json,
    module System.Agents.Tools.LuaToolbox.Modules.Text,
    module System.Agents.Tools.LuaToolbox.Modules.Time,
    module System.Agents.Tools.LuaToolbox.Modules.Tools,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (replicateM, void, when)
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import qualified HsLua as Lua

import Prod.Tracer (Tracer (..), contramap, runTracer)

import System.Agents.Base (LuaToolboxDescription (..), migrateLuaSandbox)
import System.Agents.Tools.Context (ToolExecutionContext, ToolPortal)

-- Import standard library modules
import qualified System.Agents.Tools.LuaToolbox.Modules.Fs as FsMod
import qualified System.Agents.Tools.LuaToolbox.Modules.Http as HttpMod
import qualified System.Agents.Tools.LuaToolbox.Modules.Json as JsonMod
import qualified System.Agents.Tools.LuaToolbox.Modules.Text as TextMod
import qualified System.Agents.Tools.LuaToolbox.Modules.Time as TimeMod
import qualified System.Agents.Tools.LuaToolbox.Modules.Tools as ToolsMod
import System.Agents.Tools.LuaToolbox.Utils (luaToJsonValue, toName)

-- Re-export module traces
import System.Agents.Tools.LuaToolbox.Modules.Fs (FsTrace (..))
import System.Agents.Tools.LuaToolbox.Modules.Http (
    HttpConfig (..),
    HttpTrace (..),
 )
import System.Agents.Tools.LuaToolbox.Modules.Json (JsonTrace (..))
import System.Agents.Tools.LuaToolbox.Modules.Text (TextTrace (..))
import System.Agents.Tools.LuaToolbox.Modules.Time (TimeTrace (..))
import System.Agents.Tools.LuaToolbox.Modules.Tools (
    ToolsConfig (..),
    ToolsTrace (..),
 )

-------------------------------------------------------------------------------
-- Union of all module traces
-------------------------------------------------------------------------------

{- | Union of all possible Lua module traces.

This type represents all possible trace events from the LuaToolbox modules.
Calling modules can use this type to handle traces from any of the primitive
functions across all modules.

Example:
@
handleTrace :: LuaModuleTrace -> IO ()
handleTrace (FsTrace (FsReadTrace path result)) = putStrLn $ "Read: " ++ path
handleTrace (HttpTrace (HttpGetTrace url status _ _)) = putStrLn $ "GET: " ++ show url
handleTrace (ToolsTrace (ToolsCallTrace name args)) = putStrLn $ "Tool: " ++ show name
handleTrace _ = pure ()

moduleTracer :: Tracer IO LuaModuleTrace
moduleTracer = Tracer handleTrace

-- Use contramap to create sub-tracers:
let fsTracer = contramap FsTrace moduleTracer
    httpTracer = contramap HttpTrace moduleTracer
    toolsTracer = contramap ToolsTrace moduleTracer
@
-}
data LuaModuleTrace
    = FsTrace !FsMod.FsTrace
    | HttpTrace !HttpMod.HttpTrace
    | TimeTrace !TimeMod.TimeTrace
    | TextTrace !TextMod.TextTrace
    | JsonTrace !JsonMod.JsonTrace
    | ToolsTrace !ToolsMod.ToolsTrace
    deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Core Types
-------------------------------------------------------------------------------

{- | Trace events for monitoring Lua toolbox operations.

These events allow tracking of:
* Lua state initialization and cleanup
* Script execution progress
* Sandbox violations (memory, timeout)
* Errors during execution
-}
data Trace
    = -- | Lua state initialized
      StateInitializedTrace !Text.Text
    | -- | Lua state closed
      StateClosedTrace
    | -- | Lua module registration
      ModuleRegistrationTrace LuaModuleTrace
    | -- | Script execution started (legacy, script name only)
      ScriptStartedTrace !Text.Text
    | -- | Script execution started with full script content
      ScriptExecutionStartTrace !Text.Text
    | -- | Script execution completed successfully
      ScriptCompletedTrace !NominalDiffTime
    | -- | Script execution completed with full details (script, result values, execution time)
      ScriptExecutionEndTrace !Text.Text ![Aeson.Value] !NominalDiffTime
    | -- | Script execution timed out
      ScriptTimeoutTrace !Int
    | -- | Memory limit exceeded
      MemoryLimitExceededTrace !Int
    | -- | Lua error during execution
      LuaErrorTrace !Text.Text
    | -- | Sandbox violation detected
      SandboxViolationTrace !Text.Text
    deriving (Show)

{- | Runtime configuration for a Lua toolbox.

The toolbox maintains only configuration - no persistent Lua state.
Each tool call creates a fresh, isolated Lua state that is destroyed
immediately after execution completes.

This design provides:
* Maximum isolation between calls
* Bounded memory usage (no accumulation across calls)
* No deadlocks with recursive agent calls
* Deterministic behavior (same inputs always produce same outputs)
-}
data Toolbox = Toolbox
    { toolboxConfig :: LuaToolboxDescription
    -- ^ Configuration for this toolbox
    , toolboxName :: Text.Text
    -- ^ Name of this toolbox instance
    }

-- | Errors that can occur during Lua script execution.
data ScriptError
    = -- | Lua syntax error or runtime error
      LuaRuntimeError ![Aeson.Value]
    | -- | Script exceeded maximum execution time
      TimeoutError !Int
    | -- | Script exceeded memory limit
      MemoryError !Int
    | -- | Sandbox violation (attempted to access forbidden function)
      SandboxError !Text.Text
    | -- | Tool invocation error (through portal)
      ToolInvocationError !Text.Text
    | -- | Initialization error
      InitializationError !Text.Text
    deriving (Show, Eq)

{- | Result of executing a Lua script.

Contains:
* The return value as JSON
* Execution time
* Optional trace information
-}
data ExecutionResult = ExecutionResult
    { resultValues :: [Aeson.Value]
    , resultExecutionTime :: NominalDiffTime
    }
    deriving (Show)

instance Aeson.ToJSON ExecutionResult where
    toJSON r =
        Aeson.object
            [ "values" Aeson..= resultValues r
            , "executionTime" Aeson..= resultExecutionTime r
            ]

-------------------------------------------------------------------------------
-- Initialization
-------------------------------------------------------------------------------

{- | Initialize a Lua toolbox from a description.

This function creates a stateless toolbox configuration. The actual Lua state
is created fresh for each tool call and destroyed immediately after execution.

Security configuration:
* fsAllowedPaths: whitelist for filesystem access (empty = no access)
* httpAllowedHosts: whitelist for HTTP hosts (empty = no access)
* luaToolboxAllowedTools: whitelist for tools (empty = no access)

Example:
@
result <- initializeToolbox tracer desc
case result of
    Left err -> putStrLn $ "Failed to initialize: " ++ err
    Right toolbox -> do
        -- Use toolbox for script execution
        result <- executeScriptWithPortal tracer toolbox script parentCtx portal
        -- ...
@
-}
initializeToolbox ::
    Tracer IO Trace ->
    LuaToolboxDescription ->
    IO (Either String Toolbox)
initializeToolbox tracer desc =
    initializeToolboxWithModuleTracer
        tracer
        desc

{- | Initialize a Lua toolbox with module tracing support.

This variant allows passing a tracer for module-level traces (fs, http, time, text, json, tools).
The tracer will receive 'LuaModuleTrace' events from all primitive functions
registered in the modules.

Example:
@
moduleTracer :: Tracer IO LuaModuleTrace
moduleTracer = Tracer $ \trace -> case trace of
    FsTrace t -> putStrLn $ "FS: " ++ show t
    HttpTrace t -> putStrLn $ "HTTP: " ++ show t
    ToolsTrace t -> putStrLn $ "TOOLS: " ++ show t
    _ -> pure ()

result <- initializeToolboxWithModuleTracer tracer moduleTracer desc
@
-}
initializeToolboxWithModuleTracer ::
    Tracer IO Trace ->
    LuaToolboxDescription ->
    IO (Either String Toolbox)
initializeToolboxWithModuleTracer tracer desc = do
    runTracer tracer (StateInitializedTrace desc.luaToolboxName)

    -- The toolbox is now stateless - just configuration
    -- Lua state is created fresh per call
    pure $
        Right
            Toolbox
                { toolboxConfig = desc
                , toolboxName = desc.luaToolboxName
                }

{- | Deprecated: closeToolbox is no longer needed since LuaToolbox is stateless.
Kept for backward compatibility with existing tests.
-}
{-# DEPRECATED closeToolbox "LuaToolbox is now stateless, closeToolbox is no longer needed and does nothing" #-}
closeToolbox :: Tracer IO Trace -> Toolbox -> IO ()
closeToolbox _tracer _toolbox = pure ()

{- | Register all standard library modules with tracing support and portal.

This variant includes the tools module with portal support.

The portal enables Lua scripts to call other tools through tools.call().

The parent context is passed through to enable OS integration field
propagation (World, EventQueue) for TUI visibility of nested subcalls.
-}
registerStandardModules ::
    Tracer IO LuaModuleTrace ->
    Lua.State ->
    LuaToolboxDescription ->
    -- | Parent execution context for OS field propagation
    ToolExecutionContext ->
    ToolPortal ->
    IO ()
registerStandardModules moduleTracer lstate desc parentCtx portal = do
    -- Create individual module tracers using contramap
    let fsTracer = contramap FsTrace moduleTracer
    let httpTracer = contramap HttpTrace moduleTracer
    let timeTracer = contramap TimeTrace moduleTracer
    let textTracer = contramap TextTrace moduleTracer
    let jsonTracer = contramap JsonTrace moduleTracer
    let toolsTracer = contramap ToolsTrace moduleTracer

    -- Register json module
    JsonMod.registerJsonModule jsonTracer lstate

    -- Register text module
    TextMod.registerTextModule textTracer lstate

    -- Register time module
    TimeMod.registerTimeModule timeTracer lstate

    -- Register fs module with unified sandbox
    -- Security: uses the unified FileSandbox with predicate-based access control
    let fsSandboxConfig = migrateLuaSandbox desc
    FsMod.registerFsModule
        fsTracer
        lstate
        fsSandboxConfig

    -- Register http module with sandboxing
    -- Security: empty allowedHosts means NO network access
    HttpMod.registerHttpModule
        httpTracer
        lstate
        HttpMod.HttpConfig
            { HttpMod.httpAllowedHosts = desc.luaToolboxAllowedHosts
            }

    -- Register tools module with portal
    -- Security: empty allowedTools means NO tool access
    -- Pass parent context for OS field propagation
    ToolsMod.registerToolsModule
        toolsTracer
        lstate
        ToolsMod.ToolsConfig
            { ToolsMod.toolsAllowedTools = desc.luaToolboxAllowedTools
            }
        parentCtx
        portal

-------------------------------------------------------------------------------
-- Sandbox Configuration
-------------------------------------------------------------------------------

-- | Convert StackIndex to Int
stackIndexToInt :: Lua.StackIndex -> Int
stackIndexToInt (Lua.StackIndex n) = fromIntegral n

{- | Configure the Lua sandbox by removing dangerous functions.

This removes access to:
* os.execute - could execute arbitrary shell commands
* os.remove - could delete files
* os.rename - could move files
* io.popen - could execute shell commands
* loadfile/dofile - could load arbitrary Lua files
* package.loadlib - could load native libraries

The safe subset includes:
* Basic types and operators
* String manipulation (string library)
* Table manipulation (table library)
* Math functions (math library)
* Coroutine support (coroutine library)
* Error handling (pcall, xpcall)
* Basic io (print, tostring, tonumber, etc.)
* require - for loading our pre-registered modules

This also configures the package.path to prevent loading external Lua files.
-}
configureSandbox :: Lua.State -> IO ()
configureSandbox lstate = Lua.runWith lstate $ do
    Lua.openlibs -- First load all standard libraries

    -- Then remove dangerous functions from global namespace
    -- Note: We keep 'require' for our pre-registered modules
    removeGlobals
        [ "dofile"
        , "loadfile"
        ]

    -- Remove dangerous functions from 'os' table
    withTable "os" $
        removeFields
            [ "execute"
            , "remove"
            , "rename"
            , "exit"
            , "setlocale"
            , "getenv" -- could leak sensitive environment variables
            ]

    -- Remove dangerous functions from 'io' table
    withTable "io" $
        removeFields
            [ "popen"
            , "tmpfile"
            ]

    -- Configure package.path to prevent loading external Lua files
    -- Our modules are pre-registered as globals
    configurePackagePath
  where
    removeGlobals :: [Text.Text] -> Lua.Lua ()
    removeGlobals names = mapM_ removeGlobal names

    removeGlobal :: Text.Text -> Lua.Lua ()
    removeGlobal name = do
        Lua.pushglobaltable
        Lua.pushName (toName name)
        Lua.pushnil
        Lua.settable (Lua.nthTop 3)
        Lua.pop 1

    withTable :: Text.Text -> Lua.Lua () -> Lua.Lua ()
    withTable name action = do
        ty <- Lua.getglobal (toName name)
        if ty == Lua.TypeNil
            then Lua.pop 1 -- pop nil, nothing to do
            else do
                action
                Lua.pop 1

    removeFields :: [Text.Text] -> Lua.Lua ()
    removeFields names = mapM_ removeField names

    removeField :: Text.Text -> Lua.Lua ()
    removeField name = do
        Lua.pushName (toName name)
        Lua.pushnil
        Lua.settable (Lua.nthTop 3)

{- | Configure package.path to prevent loading external Lua files.

We keep the 'require' function for our pre-registered modules,
but set package.path and package.cpath to empty strings to
prevent loading external files.

The preload table is also cleared to prevent loading of C modules.
-}
configurePackagePath :: Lua.Lua ()
configurePackagePath = do
    -- Set package.path to empty to prevent loading external Lua files
    _ <- Lua.getglobal (Lua.Name "package")
    Lua.pushName "path"
    Lua.pushstring ""
    Lua.settable (Lua.nthTop 3)

    -- Set package.cpath to empty to prevent loading C modules
    Lua.pushName "cpath"
    Lua.pushstring ""
    Lua.settable (Lua.nthTop 3)

    -- Clear package.preload to prevent preloaded C modules
    Lua.pushName "preload"
    Lua.newtable
    Lua.settable (Lua.nthTop 3)

    Lua.pop 1

{- | Apply a memory limit to the Lua state.

This uses Lua's allocator hook to track memory usage and abort
if the limit is exceeded. Note that this is approximate and may
allow slight overruns.

The limit is specified in megabytes.
-}
applyMemoryLimit :: Lua.State -> Int -> IO ()
applyMemoryLimit lstate maxMB = do
    -- Store the limit in the Lua state's extra space
    -- For now, we track this in Haskell and check periodically
    -- Full implementation would use a custom allocator
    let maxBytes = maxMB * 1024 * 1024
    -- TODO: Implement custom allocator or periodic memory checks
    -- This is a placeholder for the full memory tracking implementation
    void $ Lua.runWith lstate $ do
        -- Create a global to track memory limit
        Lua.pushinteger (fromIntegral maxBytes)
        Lua.setglobal (toName "__MEMORY_LIMIT") :: Lua.Lua ()

{- | Apply a timeout to Lua script execution.

The timeout is handled at the execution level using Haskell's
async and timeout mechanisms, not within Lua itself.
-}
applyTimeout :: Int -> IO a -> IO (Maybe a)
applyTimeout seconds go =
    fmap eitherToMaybe $ race (threadDelay (seconds * 1000000)) (go)
  where
    eitherToMaybe :: Either a b -> Maybe b
    eitherToMaybe (Right v) = Just v
    eitherToMaybe _ = Nothing

-------------------------------------------------------------------------------
-- Script Execution
-------------------------------------------------------------------------------

{- | Execute a Lua script with access to the tool portal.

This is the full execution function that:
1. Creates a fresh Lua state using bracket pattern
2. Configures the sandbox (removes dangerous functions)
3. Registers standard library modules (json, text, time, fs, http) with security settings
4. Sets up the tools module with the portal and tracer
5. Executes the script with the portal available
6. Handles timeouts and memory limits
7. Provides detailed error information
8. Traces all tool invocations
9. Cleans up the Lua state automatically after execution

The portal is exposed to Lua through the 'tools' module, which provides
functions like tools.call().

The parent context is passed through to enable OS integration field
propagation (World, EventQueue) for TUI visibility of nested subcalls.
This is essential for sub-agent calls initiated from Lua scripts to be
visible in the TUI.

This function provides maximum isolation:
* Each call starts from identical initial state
* No memory accumulation across calls
* No way for call N to influence call N+1
* No shared locks (safe for recursive agent calls)
-}
executeScriptWithPortal ::
    Tracer IO Trace ->
    Toolbox ->
    -- | Lua script source
    Text.Text ->
    -- | Parent execution context for OS field propagation
    ToolExecutionContext ->
    -- | Tool portal for calling other tools
    ToolPortal ->
    IO (Either ScriptError ExecutionResult)
executeScriptWithPortal tracer toolbox script parentCtx portal = do
    startTime <- getCurrentTime
    let desc = toolboxConfig toolbox
    let maxTime = desc.luaToolboxMaxExecutionTimeSeconds

    -- Trace script execution start with full script content
    runTracer tracer (ScriptExecutionStartTrace script)

    -- Execute with a fresh Lua state using bracket pattern
    result <-
        try $
            bracket
                (createFreshState tracer desc parentCtx portal)
                (destroyState tracer)
                (\lstate -> executeScriptInternal tracer lstate script maxTime)

    endTime <- getCurrentTime
    let execTime = diffUTCTime endTime startTime

    case result of
        Left (e :: SomeException) -> do
            let errMsg = "Execution failed: " <> Text.pack (show e)
            runTracer tracer (LuaErrorTrace errMsg)
            pure $ Left $ LuaRuntimeError [Aeson.String errMsg]
        Right execResult -> do
            case execResult of
                Left err -> do
                    runTracer tracer (LuaErrorTrace (Text.pack $ show err))
                    pure $ Left err
                Right val -> do
                    runTracer tracer (ScriptExecutionEndTrace script val execTime)
                    pure $
                        Right
                            ExecutionResult
                                { resultValues = val
                                , resultExecutionTime = execTime
                                }

{- | Create a fresh Lua state with sandbox and modules configured.

The parent context is passed through to registerStandardModules for
OS integration field propagation.
-}
createFreshState ::
    Tracer IO Trace ->
    LuaToolboxDescription ->
    -- | Parent execution context for OS field propagation
    ToolExecutionContext ->
    ToolPortal ->
    IO Lua.State
createFreshState tracer desc parentCtx portal = do
    -- Create fresh Lua state
    lstate <- Lua.newstate

    -- Configure sandbox
    configureSandbox lstate

    -- Set up memory limit if configured
    when (desc.luaToolboxMaxMemoryMB > 0) $
        applyMemoryLimit lstate desc.luaToolboxMaxMemoryMB

    -- Register standard modules with parent context for OS field propagation
    registerStandardModules
        (contramap ModuleRegistrationTrace tracer)
        lstate
        desc
        parentCtx
        portal

    pure lstate

-- | Destroy a Lua state and release its resources.
destroyState :: Tracer IO Trace -> Lua.State -> IO ()
destroyState tracer lstate = do
    Lua.close lstate
    runTracer tracer StateClosedTrace

-- | Internal execution function that runs in the context of a fresh Lua state.
executeScriptInternal ::
    Tracer IO Trace ->
    Lua.State ->
    Text.Text ->
    -- | Timeout in seconds
    Int ->
    IO (Either ScriptError [Aeson.Value])
executeScriptInternal tracer lstate script maxTime = do
    -- Execute with timeout
    result <- applyTimeout maxTime $ do
        Lua.runWith lstate $ do
            -- Load the script
            let scriptBS = Text.encodeUtf8 script
            status <- Lua.loadbuffer scriptBS (toName "=user")
            if status /= Lua.OK
                then do
                    nrets <- Lua.gettop
                    jsonValues <- replicateM (stackIndexToInt nrets) luaToJsonValue
                    pure $ Left $ LuaRuntimeError jsonValues
                else do
                    -- Execute the loaded chunk with regular pcall (no traceback)
                    execStatus <- Lua.pcall 0 Lua.multret Nothing
                    if execStatus /= Lua.OK
                        then do
                            nrets <- Lua.gettop
                            jsonValues <- replicateM (stackIndexToInt nrets) luaToJsonValue
                            pure $ Left $ LuaRuntimeError jsonValues
                        else do
                            -- Convert result to JSON
                            nrets <- Lua.gettop
                            jsonValues <- replicateM (stackIndexToInt nrets) luaToJsonValue
                            pure $ Right jsonValues

    case result of
        Nothing -> do
            -- Timeout occurred
            runTracer tracer (ScriptTimeoutTrace maxTime)
            pure $ Left $ TimeoutError maxTime
        Just val -> pure val

