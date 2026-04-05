{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | OS-native OneShot interface - SKELETON IMPLEMENTATION.

This module provides the structure for complete one-shot (batch) execution
using OS-native ECS primitives. This is currently a skeleton that defines
the interface structure - full implementation to be completed.

For now, the legacy System.Agents.OneShot is still used for actual
one-shot execution.
-}
module System.Agents.OS.Interfaces.OneShot (
    -- * OneShot Interface Handle
    OneShotInterfaceHandle (..),
    OneShotInterfaceConfig (..),

    -- * Initialization
    initOneShotInterface,
    runOneShotInterface,
    shutdownOneShotInterface,

    -- * Execution
    OneShotResult (..),
    executeOneShot,

    -- * Utility
    extractResultText,

    -- * Configuration
    OneShotPersistence (..),
    defaultOneShotInterfaceConfig,

    -- * Tracing
    OneShotTrace (..),
) where

import Control.Concurrent.STM (TQueue, TVar, atomically, newTQueueIO, newTVarIO, writeTVar)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Prod.Tracer (Tracer (..))

import System.Agents.Base (AgentId)
import System.Agents.OS.Compat.Runtime (initializeOS)
import System.Agents.OS.Interfaces (InterfaceConfig (..), InterfaceHandle (..), OSEvent, defaultInterfaceConfig)
import System.Agents.Session.Base (Session)
import System.Agents.SessionStore (SessionStore)

-------------------------------------------------------------------------------
-- Tracing
-------------------------------------------------------------------------------

-- | Trace events for OneShot interface debugging.
data OneShotTrace
    = OneShotTrace_Initialized
    | OneShotTrace_Error Text
    deriving (Show)

-------------------------------------------------------------------------------
-- Persistence Configuration
-------------------------------------------------------------------------------

-- | Persistence configuration for OneShot execution.
data OneShotPersistence
    = Persistence_None
    | Persistence_SessionStore SessionStore
    | Persistence_FilePath FilePath
    deriving (Show)

instance Eq OneShotPersistence where
    Persistence_None == Persistence_None = True
    Persistence_SessionStore _ == Persistence_SessionStore _ = True
    Persistence_FilePath p1 == Persistence_FilePath p2 = p1 == p2
    _ == _ = False

-------------------------------------------------------------------------------
-- OneShot Interface Configuration
-------------------------------------------------------------------------------

-- | Configuration specific to OneShot interface.
data OneShotInterfaceConfig = OneShotInterfaceConfig
    { oscBaseConfig :: InterfaceConfig
    , oscPersistence :: OneShotPersistence
    , oscEnableThinking :: Bool
    , oscTimeoutSeconds :: Maybe Int
    , oscTracer :: Tracer IO OneShotTrace
    }

-- | Default OneShot interface configuration.
defaultOneShotInterfaceConfig :: IO OneShotInterfaceConfig
defaultOneShotInterfaceConfig = do
    let tracer = Tracer $ \_ -> pure ()
    pure $
        OneShotInterfaceConfig
            { oscBaseConfig = defaultInterfaceConfig
            , oscPersistence = Persistence_None
            , oscEnableThinking = False
            , oscTimeoutSeconds = Nothing
            , oscTracer = tracer
            }

-------------------------------------------------------------------------------
-- OneShot Result
-------------------------------------------------------------------------------

-- | Result of a OneShot execution.
data OneShotResult = OneShotResult
    { osrText :: Text
    , osrThinking :: Maybe Text
    , osrSession :: Maybe Session
    , osrSuccess :: Bool
    , osrError :: Maybe Text
    }
    deriving (Show)

-- | Extract just the result text from a OneShotResult.
extractResultText :: OneShotResult -> Text
extractResultText = osrText

-------------------------------------------------------------------------------
-- OneShot Interface Handle
-------------------------------------------------------------------------------

-- | Handle for OneShot-specific interface operations.
data OneShotInterfaceHandle = OneShotInterfaceHandle
    { oscBaseHandle :: InterfaceHandle
    , oscConfig :: OneShotInterfaceConfig
    , oscShutdown :: TVar Bool
    }

-------------------------------------------------------------------------------
-- Initialization
-------------------------------------------------------------------------------

-- | Initialize the OneShot interface (skeleton implementation).
initOneShotInterface :: OneShotInterfaceConfig -> IO OneShotInterfaceHandle
initOneShotInterface config = do
    os <- initializeOS
    shutdownVar <- newTVarIO False
    agentsVar <- newTVarIO Map.empty
    conversationsVar <- newTVarIO Map.empty
    eventQueue <- newTQueueIO :: IO (TQueue OSEvent)

    let baseHandle =
            InterfaceHandle
                { ihOS = os
                , ihConfig = oscBaseConfig config
                , ihEventSub = eventQueue
                , ihShutdown = shutdownVar
                , ihAgents = agentsVar
                , ihConversations = conversationsVar
                }

    -- Use the tracer
    let Tracer tracerFn = oscTracer config
    tracerFn OneShotTrace_Initialized

    pure $
        OneShotInterfaceHandle
            { oscBaseHandle = baseHandle
            , oscConfig = config
            , oscShutdown = shutdownVar
            }

-- | Run the OneShot interface (skeleton - does nothing).
runOneShotInterface :: OneShotInterfaceHandle -> IO ()
runOneShotInterface _ = pure ()

-- | Execute a one-shot conversation (skeleton - returns placeholder).
executeOneShot :: OneShotInterfaceHandle -> Maybe AgentId -> Text -> IO OneShotResult
executeOneShot _ _ query =
    pure $
        OneShotResult
            { osrText = "OS-native OneShot not yet fully implemented: " <> query
            , osrThinking = Nothing
            , osrSession = Nothing
            , osrSuccess = False
            , osrError = Just "Skeleton implementation"
            }

-- | Shutdown the OneShot interface.
shutdownOneShotInterface :: OneShotInterfaceHandle -> IO ()
shutdownOneShotInterface handle = do
    atomically $ writeTVar (oscShutdown handle) True
