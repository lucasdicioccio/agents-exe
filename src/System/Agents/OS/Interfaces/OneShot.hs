{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | OneShot-specific interface for the OS model.

This module provides OneShot-specific operations built on top of the common
OS interface. It handles single-conversation batch execution with minimal
overhead and no interactive features.

== Features

1. Single-agent batch execution
2. Session persistence options
3. Minimal resource overhead
4. Compatibility with RuntimeBridge

== Migration Notes

Currently, this module provides a compatibility layer that works with
RuntimeBridge. As the OS model matures, these operations will be
implemented natively using OS primitives.
-}
module System.Agents.OS.Interfaces.OneShot (
    -- * OneShot Interface Handle
    OneShotInterfaceHandle (..),
    OneShotInterfaceConfig (..),

    -- * Initialization
    initOneShotInterface,
    runOneShotInterface,

    -- * Execution
    OneShotResult (..),
    executeOneShot,
    executeOneShotWithSession,

    -- * Configuration
    OneShotPersistence (..),
    defaultOneShotInterfaceConfig,

    -- * Utility
    extractResultText,
) where

import Control.Concurrent.Async (Async)
import Control.Concurrent.STM (TVar, newTVarIO)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import System.Agents.Base (AgentId, newAgentId, newConversationId)
import System.Agents.OS.Compat.Runtime (RuntimeBridge, newRuntimeBridge, initializeOS)
import System.Agents.OS.Interfaces (
    InterfaceConfig (..),
    InterfaceHandle (..),
    InterfaceMode (..),
    defaultInterfaceConfig,
 )
import System.Agents.Session.Base (Session (..), newSessionId, newTurnId)
import System.Agents.SessionStore (SessionStore)

-------------------------------------------------------------------------------
-- OneShot Interface Configuration
-------------------------------------------------------------------------------

-- | Persistence configuration for OneShot execution.
data OneShotPersistence
    = Persistence_None
    -- ^ No session persistence
    | Persistence_SessionStore SessionStore
    -- ^ Persist to SessionStore
    | Persistence_FilePath FilePath
    -- ^ Persist to file path
    | Persistence_Both SessionStore FilePath
    -- ^ Persist to both SessionStore and file
    deriving (Show)

-- | Manual Eq instance for OneShotPersistence (SessionStore doesn't have Eq).
instance Eq OneShotPersistence where
    Persistence_None == Persistence_None = True
    Persistence_SessionStore _ == Persistence_SessionStore _ = True
    Persistence_FilePath p1 == Persistence_FilePath p2 = p1 == p2
    Persistence_Both _ p1 == Persistence_Both _ p2 = p1 == p2
    _ == _ = False

-- | Configuration specific to OneShot interface.
data OneShotInterfaceConfig = OneShotInterfaceConfig
    { oscBaseConfig :: InterfaceConfig
    -- ^ Base interface configuration
    , oscPersistence :: OneShotPersistence
    -- ^ Session persistence configuration
    , oscEnableThinking :: Bool
    -- ^ Whether to capture thinking output
    , oscTimeoutSeconds :: Maybe Int
    -- ^ Optional timeout for execution
    }
    deriving (Show, Eq)

-- | Default OneShot interface configuration.
defaultOneShotInterfaceConfig :: OneShotInterfaceConfig
defaultOneShotInterfaceConfig =
    OneShotInterfaceConfig
        { oscBaseConfig = (defaultInterfaceConfig){ifcMode = ModeOneShot}
        , oscPersistence = Persistence_None
        , oscEnableThinking = False
        , oscTimeoutSeconds = Nothing
        }

-------------------------------------------------------------------------------
-- OneShot Interface Handle
-------------------------------------------------------------------------------

-- | Result of a OneShot execution.
data OneShotResult = OneShotResult
    { osrText :: Text
    -- ^ Response text from the agent
    , osrThinking :: Maybe Text
    -- ^ Thinking/reasoning content if captured
    , osrSession :: Maybe Session
    -- ^ Final session state if persistence enabled
    , osrSuccess :: Bool
    -- ^ Whether execution completed successfully
    , osrError :: Maybe Text
    -- ^ Error message if failed
    }
    deriving (Show)

-- | Handle for OneShot-specific interface operations.
data OneShotInterfaceHandle = OneShotInterfaceHandle
    { oscBaseHandle :: InterfaceHandle
    -- ^ Base interface handle
    , oscConfig :: OneShotInterfaceConfig
    -- ^ OneShot-specific configuration
    , oscAgentBridge :: Maybe RuntimeBridge
    -- ^ RuntimeBridge for the single agent
    , oscExecutionAsync :: TVar (Maybe (Async OneShotResult))
    -- ^ Current execution async if running
    }

-------------------------------------------------------------------------------
-- Initialization
-------------------------------------------------------------------------------

-- | Initialize the OneShot interface.
initOneShotInterface :: OneShotInterfaceConfig -> IO OneShotInterfaceHandle
initOneShotInterface config = do
    -- Initialize OS
    os <- initializeOS

    -- Create base interface components
    shutdownVar <- newTVarIO False
    agentsVar <- newTVarIO Map.empty
    conversationsVar <- newTVarIO Map.empty

    -- Create a fake event queue - in real implementation would use STM
    let fakeQueue = error "Event queue requires STM implementation"

    let baseHandle =
            InterfaceHandle
                { ihOS = os
                , ihConfig = oscBaseConfig config
                , ihEventSub = fakeQueue
                , ihShutdown = shutdownVar
                , ihAgents = agentsVar
                , ihConversations = conversationsVar
                }

    executionVar <- newTVarIO Nothing

    pure $
        OneShotInterfaceHandle
            { oscBaseHandle = baseHandle
            , oscConfig = config
            , oscAgentBridge = Nothing
            , oscExecutionAsync = executionVar
                }

-- | Run the OneShot interface (minimal for batch mode).
runOneShotInterface :: OneShotInterfaceHandle -> IO ()
runOneShotInterface _handle = do
    -- OneShot interface doesn't need a running event loop
    -- Execution happens synchronously in executeOneShot
    pure ()

-------------------------------------------------------------------------------
-- Agent Registration
-------------------------------------------------------------------------------

-- | Register an agent for OneShot execution.
registerOneShotAgent :: OneShotInterfaceHandle -> IO (AgentId, RuntimeBridge)
registerOneShotAgent handle = do
    -- Generate a new agent ID
    agentId <- newAgentId

    -- Create a RuntimeBridge for this agent
    let os = ihOS (oscBaseHandle handle)
    let bridge = newRuntimeBridge agentId os

    pure (agentId, bridge)

-------------------------------------------------------------------------------
-- Execution
-------------------------------------------------------------------------------

{- | Execute a one-shot conversation with an agent.

This is the main entry point for OneShot execution. It:
1. Creates/uses the agent
2. Executes the conversation
3. Returns the result with optional persistence
-}
executeOneShot ::
    OneShotInterfaceHandle ->
    -- | Agent identifier (or create new if empty)
    Maybe AgentId ->
    -- | User query
    Text ->
    -- | Result
    IO OneShotResult
executeOneShot handle mAgentId _query = do
    -- Create session
    session <- Session [] <$> newSessionId <*> pure Nothing <*> newTurnId
    executeOneShotWithSession handle mAgentId session

{- | Execute a one-shot conversation with an existing session.

This allows resuming from a previous session.
-}
executeOneShotWithSession ::
    OneShotInterfaceHandle ->
    -- | Agent identifier (or create new if empty)
    Maybe AgentId ->
    -- | Initial session
    Session ->
    -- | Result
    IO OneShotResult
executeOneShotWithSession handle mAgentId session = do
    -- Get or create agent
    _ <- case mAgentId of
        Just _aid -> do
            -- Try to use existing agent bridge
            case oscAgentBridge handle of
                Just _b -> pure ()
                Nothing -> void $ registerOneShotAgent handle
        Nothing -> void $ registerOneShotAgent handle

    -- Generate conversation ID (unused for now)
    _ <- newConversationId

    -- For now, return a placeholder result
    -- In the full implementation, this would:
    -- 1. Create the agent using the bridge
    -- 2. Run the conversation
    -- 3. Capture results and thinking
    -- 4. Handle persistence

    pure $
        OneShotResult
            { osrText = "OneShot execution not yet fully implemented - use legacy mainOneShotText"
            , osrThinking = Nothing
            , osrSession = Just session
            , osrSuccess = False
            , osrError = Just "OS-native OneShot not yet implemented"
            }
  where
    void = (>> pure ())

-------------------------------------------------------------------------------
-- Utility
-------------------------------------------------------------------------------

-- | Extract just the result text from a OneShotResult.
extractResultText :: OneShotResult -> Text
extractResultText = osrText

