{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | OS-native MCP (Model Context Protocol) interface - SKELETON IMPLEMENTATION.

This module provides the structure for a complete MCP server implementation
using OS-native ECS primitives. This is currently a skeleton that defines
the interface structure - full implementation to be completed.

For now, the legacy System.Agents.MCP.Server is still used for actual
MCP server functionality.
-}
module System.Agents.OS.Interfaces.MCP (
    -- * MCP Interface Handle
    MCPInterfaceHandle (..),
    MCPInterfaceConfig (..),

    -- * Initialization
    initMCPInterface,
    runMCPInterface,
    shutdownMCPInterface,

    -- * Configuration
    defaultMCPInterfaceConfig,
    MCPPersistence (..),

    -- * Tracing
    MCPTrace (..),
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (TQueue, TVar, atomically, newTQueueIO, newTVarIO, readTVarIO, writeTVar)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Prod.Tracer (Tracer (..))

import System.Agents.OS.Compat.Runtime (initializeOS)
import System.Agents.OS.Interfaces (InterfaceConfig (..), InterfaceHandle (..), OSEvent, defaultInterfaceConfig)
import System.Agents.Session.Base (OnSessionProgress, ignoreSessionProgress)

-------------------------------------------------------------------------------
-- Tracing
-------------------------------------------------------------------------------

-- | Trace events for MCP interface debugging.
data MCPTrace
    = MCPTrace_Initialized
    | MCPTrace_Error Text
    deriving (Show)

-------------------------------------------------------------------------------
-- MCP Interface Configuration
-------------------------------------------------------------------------------

-- | Persistence configuration for MCP sessions.
data MCPPersistence
    = MCPPersistence_None
    | MCPPersistence_Sqlite FilePath
    deriving (Show)

-- | Configuration for the MCP interface.
data MCPInterfaceConfig = MCPInterfaceConfig
    { mcpBaseConfig :: InterfaceConfig
    , mcpProtocolVersion :: Text
    , mcpServerName :: Text
    , mcpServerVersion :: Text
    , mcpPersistence :: MCPPersistence
    , mcpOnSessionProgress :: OnSessionProgress
    , mcpTracer :: Tracer IO MCPTrace
    }

-- | Default MCP interface configuration.
defaultMCPInterfaceConfig :: IO MCPInterfaceConfig
defaultMCPInterfaceConfig = do
    let tracer = Tracer $ \_ -> pure ()
    pure $
        MCPInterfaceConfig
            { mcpBaseConfig = defaultInterfaceConfig
            , mcpProtocolVersion = "2024-11-05"
            , mcpServerName = "agents-exe-mcp-server"
            , mcpServerVersion = "0.1.0"
            , mcpPersistence = MCPPersistence_None
            , mcpOnSessionProgress = ignoreSessionProgress
            , mcpTracer = tracer
            }

-------------------------------------------------------------------------------
-- MCP Interface Handle
-------------------------------------------------------------------------------

-- | Handle for MCP interface operations.
data MCPInterfaceHandle = MCPInterfaceHandle
    { mcpBaseHandle :: InterfaceHandle
    , mcpShutdown :: TVar Bool
    }

-------------------------------------------------------------------------------
-- Initialization
-------------------------------------------------------------------------------

-- | Initialize the MCP interface (skeleton implementation).
initMCPInterface :: MCPInterfaceConfig -> IO MCPInterfaceHandle
initMCPInterface config = do
    os <- initializeOS
    shutdownVar <- newTVarIO False
    agentsVar <- newTVarIO Map.empty
    conversationsVar <- newTVarIO Map.empty
    eventQueue <- newTQueueIO :: IO (TQueue OSEvent)

    let baseHandle =
            InterfaceHandle
                { ihOS = os
                , ihConfig = mcpBaseConfig config
                , ihEventSub = eventQueue
                , ihShutdown = shutdownVar
                , ihAgents = agentsVar
                , ihConversations = conversationsVar
                }

    -- Use the tracer
    let Tracer tracerFn = mcpTracer config
    tracerFn MCPTrace_Initialized

    pure $
        MCPInterfaceHandle
            { mcpBaseHandle = baseHandle
            , mcpShutdown = shutdownVar
            }

-- | Run the MCP server (skeleton - does nothing yet).
runMCPInterface :: MCPInterfaceHandle -> IO ()
runMCPInterface handle = do
    -- Skeleton implementation - just wait for shutdown
    let loop = do
            shutdown <- readTVarIO (mcpShutdown handle)
            if shutdown
                then pure ()
                else do
                    threadDelay 1000000
                    loop
    loop

-- | Shutdown the MCP interface.
shutdownMCPInterface :: MCPInterfaceHandle -> IO ()
shutdownMCPInterface handle = do
    atomically $ writeTVar (mcpShutdown handle) True
