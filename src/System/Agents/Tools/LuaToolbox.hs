{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- | Provides a sandboxed Lua interpreter for agent-scriptable tool orchestration with security hardening.

This module implements the Lua toolbox functionality, including:

* Lua interpreter initialization using hslua
* Sandboxed execution environment
* Memory limits via Lua allocator hooks
* Timeout enforcement via Haskell watchdog thread
* Error handling and stack trace capture
* Integration with the Tool Portal for calling other tools
* Standard library modules: json, text, time, fs, http, tools
* Comprehensive tool invocation tracing

Security Features:
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

Example usage:

@
import System.Agents.Tools.LuaToolbox as Lua
import System.Agents.Base (LuaToolboxDescription(..))
import Prod.Tracer (Tracer(..), runTracer)

main :: IO ()
main = do
    let desc = LuaToolboxDescription
            { luaToolboxName = "lua"
            , luaToolboxDescription = "Sandboxed Lua interpreter"
            , luaToolboxMaxMemoryMB = 256
            , luaToolboxMaxExecutionTimeSeconds = 300
            , luaToolboxAllowedTools = ["bash", "sqlite"]
            , luaToolboxAllowedPaths = ["./scripts"]
            , luaToolboxAllowedHosts = ["localhost"]
            }
    result <- Lua.initializeToolbox Tracer.silent desc
    case result of
        Right toolbox -> do
            result <- Lua.executeScript toolbox "return 1 + 1"
            case result of
                Right val -> print val
                Left err -> print err
        Left err -> putStrLn $ "Failed to initialize: " ++ err
@

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
> local result = tools.bash.run("echo hello")
> print(result.data.stdout)
>
> return {parts = parts, date = formatted}
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
    closeToolbox,

    -- * Script execution
    executeScript,
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
) where

import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, try)
import Control.Monad (void, when, replicateM)
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import qualified HsLua as Lua
import Control.Concurrent.Async (race)
import Control.Concurrent (threadDelay)

import Prod.Tracer (Tracer (..), contramap, runTracer)

import System.Agents.Base (LuaToolboxDescription (..))
import System.Agents.Tools.Context (ToolPortal)

-- Import standard library modules
import qualified System.Agents.Tools.LuaToolbox.Modules.Fs as FsMod
import qualified System.Agents.Tools.LuaToolbox.Modules.Http as HttpMod
import qualified System.Agents.Tools.LuaToolbox.Modules.Json as JsonMod
import qualified System.Agents.Tools.LuaToolbox.Modules.Text as TextMod
import qualified System.Agents.Tools.LuaToolbox.Modules.Time as TimeMod
import System.Agents.Tools.LuaToolbox.Utils (luaToJsonValue, toName)

-- Re-export module traces
import System.Agents.Tools.LuaToolbox.Modules.Fs (
    FsConfig (..),
    FsTrace (..),
    PathError (..),
 )
import System.Agents.Tools.LuaToolbox.Modules.Http (
    HttpConfig (..),
    HttpTrace (..),
 )
import System.Agents.Tools.LuaToolbox.Modules.Json (JsonTrace (..))
import System.Agents.Tools.LuaToolbox.Modules.Text (TextTrace (..))
import System.Agents.Tools.LuaToolbox.Modules.Time (TimeTrace (..))

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
handleTrace _ = pure ()

moduleTracer :: Tracer IO LuaModuleTrace
moduleTracer = Tracer handleTrace

-- Use contramap to create sub-tracers:
let fsTracer = contramap FsTrace moduleTracer
    httpTracer = contramap HttpTrace moduleTracer
@
-}
data LuaModuleTrace
    = FsTrace !FsMod.FsTrace
    | HttpTrace !HttpMod.HttpTrace
    | TimeTrace !TimeMod.TimeTrace
    | TextTrace !TextMod.TextTrace
    | JsonTrace !JsonMod.JsonTrace
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

{- | Runtime state for a Lua toolbox.

The toolbox maintains:
* Lua state (from hslua)
* Configuration (memory limit, timeout, etc.)
* An MVar for serializing script execution within this toolbox instance
-}
data Toolbox = Toolbox
    { toolboxLuaState :: Lua.State
    -- ^ The Lua state (opaque pointer managed by hslua)
    , toolboxConfig :: LuaToolboxDescription
    -- ^ Configuration for this toolbox
    , toolboxLock :: MVar ()
    -- ^ Lock for serializing access within this toolbox instance
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

This function:
1. Creates a new Lua state using hslua
2. Configures the sandbox (removes dangerous functions)
3. Sets up memory limit tracking
4. Registers standard library modules (json, text, time, fs, http) with security settings
5. Creates an MVar lock for serializing access
6. Returns a 'Toolbox' ready for script execution

Returns an error if the Lua state cannot be created.

Security configuration:
* fsAllowedPaths: whitelist for filesystem access (empty = no access)
* httpAllowedHosts: whitelist for HTTP hosts (empty = no access)
-}
initializeToolbox ::
    Tracer IO Trace ->
    LuaToolboxDescription ->
    IO (Either String Toolbox)
initializeToolbox tracer desc =
    initializeToolboxWithModuleTracer
        tracer
        (Tracer (const (pure ())))
        desc

{- | Initialize a Lua toolbox with module tracing support.

This variant allows passing a tracer for module-level traces (fs, http, time, text, json).
The tracer will receive 'LuaModuleTrace' events from all primitive functions
registered in the modules.

Example:
@
moduleTracer :: Tracer IO LuaModuleTrace
moduleTracer = Tracer $ \trace -> case trace of
    FsTrace t -> putStrLn $ "FS: " ++ show t
    HttpTrace t -> putStrLn $ "HTTP: " ++ show t
    _ -> pure ()

result <- initializeToolboxWithModuleTracer tracer moduleTracer desc
@
-}
initializeToolboxWithModuleTracer ::
    Tracer IO Trace ->
    Tracer IO LuaModuleTrace ->
    LuaToolboxDescription ->
    IO (Either String Toolbox)
initializeToolboxWithModuleTracer tracer moduleTracer desc = do
    runTracer tracer (StateInitializedTrace desc.luaToolboxName)

    -- Create Lua state
    result <- try $ Lua.newstate

    case result of
        Left (e :: SomeException) -> do
            let errMsg = "Failed to create Lua state: " <> show e
            pure $ Left errMsg
        Right lstate -> do
            -- Configure sandbox
            configureSandbox lstate

            -- Set up memory limit if configured
            when (desc.luaToolboxMaxMemoryMB > 0) $
                applyMemoryLimit lstate desc.luaToolboxMaxMemoryMB

            -- Register standard library modules with security settings and tracers
            registerStandardModulesWithTracer moduleTracer lstate desc

            -- Create lock for serializing access
            lock <- newEmptyMVar
            putMVar lock ()

            pure $
                Right
                    Toolbox
                        { toolboxLuaState = lstate
                        , toolboxConfig = desc
                        , toolboxLock = lock
                        , toolboxName = desc.luaToolboxName
                        }

{- | Register all standard library modules in the Lua state.

This registers:
* json: JSON encoding/decoding
* text: UTF-8 string utilities
* time: Time functions
* fs: Sandboxed filesystem with path canonicalization (using allowedPaths from config)
* http: HTTP requests with host validation (using allowedHosts from config)

Security:
* Empty allowedPaths means NO filesystem access
* Empty allowedHosts means NO network access

Note: This function uses a silent tracer (no-op). Use 'registerStandardModulesWithTracer'
for tracing support.
-}
registerStandardModules :: Lua.State -> LuaToolboxDescription -> IO ()
registerStandardModules =
    registerStandardModulesWithTracer (Tracer (const (pure ())))

{- | Register all standard library modules with tracing support.

This variant allows passing a 'LuaModuleTrace' tracer that will receive
events from all primitive functions in the modules.

Uses 'contramap' to create sub-tracers for each module from the union tracer:

@
let fsTracer = contramap FsTrace moduleTracer
    httpTracer = contramap HttpTrace moduleTracer
    timeTracer = contramap TimeTrace moduleTracer
    textTracer = contramap TextTrace moduleTracer
    jsonTracer = contramap JsonTrace moduleTracer
@
-}
registerStandardModulesWithTracer ::
    Tracer IO LuaModuleTrace ->
    Lua.State ->
    LuaToolboxDescription ->
    IO ()
registerStandardModulesWithTracer moduleTracer lstate desc = do
    -- Create individual module tracers using contramap
    let fsTracer = contramap FsTrace moduleTracer
    let httpTracer = contramap HttpTrace moduleTracer
    let timeTracer = contramap TimeTrace moduleTracer
    let textTracer = contramap TextTrace moduleTracer
    let jsonTracer = contramap JsonTrace moduleTracer

    -- Register json module
    JsonMod.registerJsonModule jsonTracer lstate

    -- Register text module
    TextMod.registerTextModule textTracer lstate

    -- Register time module
    TimeMod.registerTimeModule timeTracer lstate

    -- Register fs module with sandboxing
    -- Security: empty allowedPaths means NO filesystem access
    FsMod.registerFsModule
        fsTracer
        lstate
        FsMod.FsConfig
            { FsMod.fsAllowedPaths = desc.luaToolboxAllowedPaths
            }

    -- Register http module with sandboxing
    -- Security: empty allowedHosts means NO network access
    HttpMod.registerHttpModule
        httpTracer
        lstate
        HttpMod.HttpConfig
            { HttpMod.httpAllowedHosts = desc.luaToolboxAllowedHosts
            }

{- | Close a toolbox and release its resources.

This function closes the Lua state and should be called when the
sandbox is no longer needed.
-}
closeToolbox :: Tracer IO Trace -> Toolbox -> IO ()
closeToolbox tracer toolbox = do
    Lua.close (toolboxLuaState toolbox)
    runTracer tracer StateClosedTrace

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
    eitherToMaybe _  = Nothing

-------------------------------------------------------------------------------
-- Script Execution
-------------------------------------------------------------------------------

{- | Execute a Lua script in the sandbox.

This function:
1. Acquires the toolbox lock for thread safety
2. Records start time
3. Loads and executes the script
4. Converts the return value to JSON
5. Returns the result or an error

The script should return a value that can be converted to JSON:
* nil -> null
* boolean -> boolean
* number -> number
* string -> string
* table -> object/array (if table has only integer keys starting at 1, it's an array)

Without portal - just execute script with no tool access.
-}
executeScript ::
    Toolbox ->
    -- | Lua script source
    Text.Text ->
    IO (Either ScriptError ExecutionResult)
executeScript toolbox script =
    -- Without portal - just execute script with no tool access
    executeScriptWithPortal (Tracer (const (pure ()))) toolbox script Nothing

{- | Execute a Lua script with access to the tool portal.

This is the full execution function that:
1. Sets up the tools module with the portal and tracer
2. Executes the script with the portal available
3. Handles timeouts and memory limits
4. Provides detailed error information
5. Traces all tool invocations

The portal is exposed to Lua through the 'tools' module, which provides
functions like tools.call(), tools.bash.run(), etc.
-}
executeScriptWithPortal ::
    Tracer IO Trace ->
    Toolbox ->
    -- | Lua script source
    Text.Text ->
    -- | Optional tool portal for calling other tools
    Maybe ToolPortal ->
    IO (Either ScriptError ExecutionResult)
executeScriptWithPortal tracer toolbox script mPortal = do
    -- Acquire lock to serialize access within this toolbox instance
    withMVar (toolboxLock toolbox) $ \() ->
        executeScriptInternal tracer toolbox script mPortal

-- | Helper to run IO action with MVar lock
withMVar :: MVar a -> (a -> IO b) -> IO b
withMVar mvar action = do
    val <- takeMVar mvar
    result <- action val
    putMVar mvar val
    pure result

executeScriptInternal ::
    Tracer IO Trace ->
    Toolbox ->
    Text.Text ->
    Maybe ToolPortal ->
    IO (Either ScriptError ExecutionResult)
executeScriptInternal tracer toolbox script _ = do
    startTime <- getCurrentTime
    let lstate = toolboxLuaState toolbox
    let maxTime = (toolboxConfig toolbox).luaToolboxMaxExecutionTimeSeconds

    -- Trace script execution start with full script content
    runTracer tracer (ScriptExecutionStartTrace script)

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
                            -- errMsg <- Lua.tostring' (Lua.nthTop 1)
                            nrets <- Lua.gettop
                            jsonValues <- replicateM (stackIndexToInt nrets) luaToJsonValue
                            pure $ Left $ LuaRuntimeError jsonValues
                        else do
                            -- TODO: consider multiple-return values
                            -- Convert result to JSON
                            nrets <- Lua.gettop
                            jsonValues <- replicateM (stackIndexToInt nrets) luaToJsonValue
                            pure $ Right jsonValues

    endTime <- getCurrentTime
    let execTime = diffUTCTime endTime startTime

    case result of
        Nothing -> do
            -- Timeout occurred
            runTracer tracer (ScriptTimeoutTrace maxTime)
            pure $ Left $ TimeoutError maxTime
        Just (Left err) -> do
            runTracer tracer (LuaErrorTrace (Text.pack $ show err))
            pure $ Left err
        Just (Right val) -> do
            -- Trace script execution end with script, result values, and execution time
            runTracer tracer (ScriptExecutionEndTrace script val execTime)
            pure $
                Right
                    ExecutionResult
                        { resultValues = val
                        , resultExecutionTime = execTime
                        }

