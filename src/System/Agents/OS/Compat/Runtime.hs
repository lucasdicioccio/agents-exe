{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- |
Compatibility runtime layer - minimal exports for transitional compatibility.
This module is deprecated and will be removed in a future release.
-}
module System.Agents.OS.Compat.Runtime (
    -- * OS Type (re-exported from OS.Core for compatibility)
    OS (..),
    initializeOS,

    -- * Runtime Bridge (deprecated)
    RuntimeBridge (..),
    newRuntimeBridge,
    runWithBridge,

    -- * AgentRuntime class (deprecated)
    AgentRuntime (..),
) where

import Control.Concurrent.STM (atomically)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Aeson (Value (..))
import Data.Text (Text)
import Prod.Tracer (Tracer (..))

import System.Agents.Base (AgentId)
import System.Agents.OS.Core.World (World, newWorld)
import System.Agents.Runtime.Trace (Trace)
import System.Agents.Session.Base (UserToolResponse (..))
import System.Agents.ToolRegistration (ToolRegistration)

-------------------------------------------------------------------------------
-- OS Type (re-exported for compatibility)
-------------------------------------------------------------------------------

-- | Placeholder OS type.
newtype OS = OS
    { _osWorld :: World
    }

-- | Manual Eq instance for OS
instance Eq OS where
    _ == _ = True -- All OS instances are equal for compatibility

-- | Manual Show instance for OS
instance Show OS where
    show _ = "OS {<world>}"

-- | Initialize a minimal OS instance.
initializeOS :: IO OS
initializeOS = do
    world <- atomically newWorld
    pure $ OS world

-------------------------------------------------------------------------------
-- Runtime Bridge (deprecated)
-------------------------------------------------------------------------------

{- | Bridge from old Runtime interface to new OS (deprecated).
This type is kept for backward compatibility but should not be used in new code.
-}
data RuntimeBridge = RuntimeBridge
    { bridgeAgentId :: AgentId
    , bridgeOS :: OS
    }
    deriving (Show, Eq)

-- | Create a new runtime bridge for an agent (deprecated).
newRuntimeBridge :: AgentId -> OS -> RuntimeBridge
newRuntimeBridge agentId os =
    RuntimeBridge
        { bridgeAgentId = agentId
        , bridgeOS = os
        }

-- | Run an operation with a RuntimeBridge (deprecated).
runWithBridge :: RuntimeBridge -> ReaderT RuntimeBridge IO a -> IO a
runWithBridge bridge action = runReaderT action bridge

-------------------------------------------------------------------------------
-- AgentRuntime Typeclass (deprecated)
-------------------------------------------------------------------------------

{- | Typeclass for agent runtime operations (deprecated).
This is kept for backward compatibility but should not be used in new code.
-}
class (Monad m) => AgentRuntime m where
    -- | List all available tools for the agent.
    listTools :: m [ToolRegistration]

    -- | Call a tool by name with the given input.
    callTool :: Text -> Value -> m UserToolResponse

    -- | Get the tracer for this runtime.
    getTracer :: m (Tracer IO Trace)

-- | ReaderT instance for RuntimeBridge operations (deprecated).
instance AgentRuntime (ReaderT RuntimeBridge IO) where
    listTools = do
        -- Placeholder: return empty list
        pure []

    callTool name _input = do
        -- Placeholder: return not found response
        pure $ UserToolResponse $ String $ "Tool not found: " <> name

    getTracer = do
        -- Return a no-op tracer by default
        pure $ Tracer $ const $ pure ()
