{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- |
Compatibility runtime layer providing backward compatibility with
the old Runtime-per-agent architecture.

This module implements the Runtime interface using the new OS backend,
allowing existing code to continue working while gradually migrating
to the OS model.
-}
module System.Agents.OS.Compat.Runtime (
    -- * Migration State and Configuration
    MigrationState (..),
    MigrationPhase (..),
    MigrationConfig (..),
    defaultMigrationConfig,
    initializeWithMigration,

    -- * Runtime Bridge
    RuntimeBridge (..),
    AgentRuntime (..),

    -- * Bridge Operations
    newRuntimeBridge,
    runWithBridge,

    -- * Conversion Utilities
    callResultToUserToolResponse,

    -- * Placeholder OS types (until fully implemented)
    OS,
    OSM,
    runOSM,
) where

import Control.Concurrent.STM (atomically)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding.Error (lenientDecode)
import Prod.Tracer (Tracer (..))
import qualified System.Agents.Base as Base
import System.Agents.OS.Conversation.Types (
    defaultMaxToolCallDepth,
 )
import System.Agents.OS.Core.World (World, newWorld)
import System.Agents.Runtime.Runtime (Runtime)
import System.Agents.Runtime.Trace (Trace)
import System.Agents.Session.Base (UserToolResponse (..))
import qualified System.Agents.Session.Base as Session
import System.Agents.ToolRegistration (ToolRegistration)
import System.Agents.Tools.Base (CallResult (..))
import System.Agents.Tools.Context (CallStackEntry (..), ToolExecutionContext, mkToolExecutionContext)

import qualified System.Agents.LLMs.OpenAI as OpenAI

-------------------------------------------------------------------------------
-- Placeholder OS types (until fully implemented)
-------------------------------------------------------------------------------

{- | Placeholder OS type.

This is a placeholder until the full OS type is implemented in
System.Agents.OS.Resources. The OS will contain the World and
resource registry.
-}
newtype OS = OS
    { _osWorld :: World
    }

-- | Monad for OS operations.
newtype OSM a = OSM {unOSM :: IO a}
    deriving (Functor, Applicative, Monad, MonadIO)

-- | Run an OSM computation.
runOSM :: OS -> OSM a -> IO a
runOSM _os = unOSM

-------------------------------------------------------------------------------
-- Migration Configuration and State
-------------------------------------------------------------------------------

{- | Configuration for the migration process.

Controls which runtime system to use and how to handle the transition.
-}
data MigrationConfig = MigrationConfig
    { migrationEnableOS :: Bool
    -- ^ Enable the new OS system
    , migrationEnableCompat :: Bool
    -- ^ Enable compatibility layer
    , migrationLogPath :: Maybe FilePath
    -- ^ Optional path for migration logging
    }
    deriving (Show, Eq)

{- | Default migration configuration.

Defaults to old-only mode for maximum backward compatibility.
-}
defaultMigrationConfig :: MigrationConfig
defaultMigrationConfig =
    MigrationConfig
        { migrationEnableOS = False
        , migrationEnableCompat = True
        , migrationLogPath = Nothing
        }

{- | Current phase of the migration.

Tracks which runtime systems are active.
-}
data MigrationPhase
    = -- | Only old Runtime is used
      PhaseOldOnly
    | -- | Both Runtime and OS are available
      PhaseDual
    | -- | Only OS is used, Runtime is deprecated
      PhaseNewOnly
    deriving (Show, Eq, Ord, Enum, Bounded)

{- | State tracking for the migration process.

Maintains a registry of runtime bridges for agents in dual mode.
-}
data MigrationState = MigrationState
    { migrationPhase :: MigrationPhase
    , migrationCompatRuntimes :: Map Base.AgentId RuntimeBridge
    }

{- | Initialize the runtime system with migration support.

Returns Left Runtime if using the old system, Right OS if using the new.

Based on the MigrationConfig, this function:
- PhaseOldOnly: Creates and returns a traditional Runtime
- PhaseDual: Creates both systems, returns Runtime with bridge
- PhaseNewOnly: Creates only OS, returns OS

Example usage:

result <- initializeWithMigration config agentConfig

case result of
    Left runtime -> do
        -- Use traditional Runtime interface
        tools <- atomically $ listTools runtime
    Right os -> do
        -- Use new OS interface
        agent <- runOSM os $ createAgent agentConfig
-}
initializeWithMigration ::
    MigrationConfig ->
    IO (Either String (Either Runtime OS))
initializeWithMigration config
    | not (migrationEnableOS config) = do
        -- PhaseOldOnly: Just return empty indication to use old Runtime
        pure $ Right $ Left $ error "Use traditional newRuntime in PhaseOldOnly"
    | migrationEnableCompat config = do
        -- PhaseDual: Create bridge infrastructure
        os <- initializeOS
        pure $ Right $ Right os
    | otherwise = do
        -- PhaseNewOnly: Create OS only
        os <- initializeOS
        pure $ Right $ Right os

{- | Initialize a minimal OS instance.
This is a placeholder until the full OS initialization is implemented.
-}
initializeOS :: IO OS
initializeOS = do
    -- This would be replaced with actual OS initialization
    -- For now, we create an empty OS
    world <- atomically newWorld
    pure $ OS world

-------------------------------------------------------------------------------
-- Runtime Bridge
-------------------------------------------------------------------------------

{- | Bridge from old Runtime interface to new OS.

Wraps an AgentId and OS to provide Runtime-compatible operations.
-}
data RuntimeBridge = RuntimeBridge
    { bridgeAgentId :: Base.AgentId
    , bridgeOS :: OS
    }

-- | Create a new runtime bridge for an agent.
newRuntimeBridge :: Base.AgentId -> OS -> RuntimeBridge
newRuntimeBridge = RuntimeBridge

-- | Run an operation with a RuntimeBridge.
runWithBridge :: RuntimeBridge -> ReaderT RuntimeBridge IO a -> IO a
runWithBridge bridge action = runReaderT action bridge

-------------------------------------------------------------------------------
-- AgentRuntime Typeclass
-------------------------------------------------------------------------------

{- | Typeclass for agent runtime operations.

Provides the old Runtime interface for compatibility.
-}
class (Monad m) => AgentRuntime m where
    -- | List all available tools for the agent.
    listTools :: m [ToolRegistration]

    -- | Call a tool by name with the given input.
    callTool :: Text -> Value -> m UserToolResponse

    -- | Get the tracer for this runtime.
    getTracer :: m (Tracer IO Trace)

-- | ReaderT instance for RuntimeBridge operations.
instance AgentRuntime (ReaderT RuntimeBridge IO) where
    listTools = do
        _bridge <- ask
        -- In the OS model, tools are accessed via toolbox bindings
        -- This is a simplified implementation
        liftIO $ pure []

    callTool name input = do
        bridge <- ask
        liftIO $ do
            -- Generate identifiers for the tool call context
            sessionId <- Session.newSessionId
            convId <- Base.newConversationId
            turnId <- Session.newTurnId

            -- Create execution context
            let toolCtx =
                    mkToolExecutionContext
                        sessionId
                        convId
                        turnId
                        (Just $ bridgeAgentId bridge)
                        Nothing
                        [CallStackEntry name convId 0]
                        (Just defaultMaxToolCallDepth)

            -- Execute the tool call through the OS
            -- This is a placeholder - actual implementation would use OS tool execution
            result <- executeToolCallInOS bridge name input toolCtx
            pure $ callResultToUserToolResponse (mockToolCall name input) result

    getTracer = do
        -- Return a no-op tracer by default
        pure $ Tracer $ const $ pure ()

-- | Mock ToolCall for compatibility.
mockToolCall :: Text -> Value -> OpenAI.ToolCall
mockToolCall name input =
    OpenAI.ToolCall
        { OpenAI.rawToolCall = KeyMap.empty
        , OpenAI.toolCallId = "mock-call-id"
        , OpenAI.toolCallType = Just "function"
        , OpenAI.toolCallFunction =
            OpenAI.ToolCallFunction
                { OpenAI.toolCallFunctionName = OpenAI.ToolName name
                , OpenAI.toolCallFunctionArgsUnparsed = Text.decodeUtf8With lenientDecode $ LBS.toStrict $ Aeson.encode input
                , OpenAI.toolCallFunctionArgs = Nothing
                }
        }

{- | Execute a tool call through the OS backend.
This is a placeholder implementation.
-}
executeToolCallInOS ::
    RuntimeBridge ->
    Text ->
    Value ->
    ToolExecutionContext ->
    IO (CallResult OpenAI.ToolCall)
executeToolCallInOS _bridge name _input _ctx = do
    -- Placeholder: In actual implementation, this would:
    -- 1. Look up the tool in the OS toolbox bindings
    -- 2. Create a proper ToolCallConfig and ToolCallState
    -- 3. Execute through the OS resource layer
    -- 4. Return the result
    pure $ ToolNotFound $ mockToolCall name Aeson.Null

-------------------------------------------------------------------------------
-- Conversion Utilities
-------------------------------------------------------------------------------

{- | Convert a CallResult to a UserToolResponse.

This function centralizes the conversion logic used in both the old
Runtime and the new OS compatibility layer.
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
        DeveloperToolError _ err ->
            UserToolResponse $ Aeson.String $ Text.pack $ "Developer tool error: " <> show err
        LuaToolResult _ toolResult ->
            UserToolResponse $ Aeson.toJSON toolResult
        LuaToolError _ err ->
            UserToolResponse $ Aeson.String $ "Lua tool error: " <> err
