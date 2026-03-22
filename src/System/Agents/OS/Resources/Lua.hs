{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Lua resource component for the OS resource management system.

This module provides:
* Component storage for Lua interpreter states
* Resource accessor type for type-safe access
* Integration with the ECS component system

== Example Usage

@
import System.Agents.OS.Resources.Lua
import qualified HsLua as Lua

-- Access a Lua resource through the handle
handleAccess handle $ \accessor -> do
    let lstate = luaState accessor
    Lua.runWith lstate $ do
        Lua.dostring "return 1 + 1"
@
-}
module System.Agents.OS.Resources.Lua (
    -- * Component
    LuaResourceData (..),
    LuaAccessor (..),
    luaResourceComponentId,

    -- * Operations
    newLuaAccessor,
    closeLuaAccessor,
    withLuaAccessor,
) where

import Control.Exception (bracket, try)
import Control.Monad (void)
import qualified HsLua as Lua

import System.Agents.OS.Core.Types (Component (..), ComponentTypeId (..))
import System.Agents.OS.Resources.Types (LuaConfig (..))

-------------------------------------------------------------------------------
-- Component Definition
-------------------------------------------------------------------------------

{- | Lua resource data stored as a Component.

This holds the actual Lua interpreter state and sandbox configuration.
It is stored in the ECS World as a component attached to a ResourceId entity.
-}
data LuaResourceData = LuaResourceData
    { luaResourceState :: Lua.State
    -- ^ The Lua interpreter state (opaque pointer managed by hslua)
    , luaResourceSandboxed :: Bool
    -- ^ Whether this Lua state runs in sandboxed mode
    }

instance Show LuaResourceData where
    show _ = "LuaResourceData{..}"

-- | Component ID for LuaResourceData (allocated as ComponentTypeId 12 per spec)
luaResourceComponentId :: ComponentTypeId
luaResourceComponentId = ComponentTypeId 12

instance Component LuaResourceData where
    componentId _ = luaResourceComponentId

{- | Accessor type for Lua resources.

This provides a type-safe way to access Lua resources through the
resource handle's access function.
-}
newtype LuaAccessor = LuaAccessor
    { luaInterpreterState :: Lua.State
    }

-- | Create a new Lua accessor from resource data.
newLuaAccessor :: LuaResourceData -> LuaAccessor
newLuaAccessor data_ = LuaAccessor{luaInterpreterState = luaResourceState data_}

-- | Close a Lua accessor (and its underlying state).
closeLuaAccessor :: LuaAccessor -> IO ()
closeLuaAccessor accessor = do
    Lua.close (luaInterpreterState accessor)

{- | Execute an action with a Lua accessor.

This is a helper function that handles creating and closing the Lua state
based on the configuration. In practice, the accessor is created once
and reused through the resource handle.

Example:
@
let config = LuaConfig
        { luaMaxMemoryMB = 256
        , luaMaxExecutionTimeSeconds = 300
        , luaSandboxed = True
        }
withLuaAccessor config $ \accessor -> do
    Lua.runWith (luaInterpreterState accessor) $ do
        Lua.dostring "return 1 + 1"
@
-}
withLuaAccessor ::
    LuaConfig ->
    (LuaAccessor -> IO a) ->
    IO (Either String a)
withLuaAccessor config action = do
    result <- try $ bracket open close (action . newLuaAccessor)
    case result of
        Left (e :: Lua.Exception) -> pure $ Left $ show e
        Right val -> pure $ Right val
  where
    open = do
        lstate <- Lua.newstate
        -- In a full implementation, we would:
        -- 1. Configure the sandbox (remove dangerous functions)
        -- 2. Apply memory limits
        -- 3. Register standard library modules
        void $ Lua.runWith lstate Lua.openlibs
        pure $
            LuaResourceData
                { luaResourceState = lstate
                , luaResourceSandboxed = luaSandboxed config
                }
    close data_ = do
        Lua.close (luaResourceState data_)
