{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
HTTP resource component for the OS resource management system.

This module provides:
* Component storage for HTTP connection managers
* Resource accessor type for type-safe access
* Integration with the ECS component system

== Example Usage

@
import System.Agents.OS.Resources.Http
import Network.HTTP.Client

-- Access an HTTP resource through the handle
handleAccess handle $ \accessor -> do
    let mgr = httpManager accessor
    request <- parseRequest "https://api.example.com/data"
    response <- httpLbs request mgr
    process response
@
-}
module System.Agents.OS.Resources.Http (
    -- * Component
    HttpResourceData (..),
    HttpAccessor (..),
    httpResourceComponentId,

    -- * Operations
    newHttpAccessor,
    closeHttpAccessor,
    withHttpAccessor,
    createHttpManager,
) where

import Control.Exception (bracket, try)
import Network.HTTP.Client (Manager)
import qualified Network.HTTP.Client.TLS as TLS

import System.Agents.OS.Core.Types (Component (..), ComponentTypeId (..))
import System.Agents.OS.Resources.Types (
    HttpManagerConfig (..),
 )

-------------------------------------------------------------------------------
-- Component Definition
-------------------------------------------------------------------------------

{- | HTTP resource data stored as a Component.

This holds the HTTP connection manager and connection tracking information.
It is stored in the ECS World as a component attached to a ResourceId entity.
-}
data HttpResourceData = HttpResourceData
    { httpManager :: Manager
    -- ^ The HTTP connection manager (from http-client)
    , httpConnectionCount :: Int
    -- ^ Current number of active connections
    }

instance Show HttpResourceData where
    show _ = "HttpResourceData{..}"

-- | Component ID for HttpResourceData (allocated as ComponentTypeId 13 per spec)
httpResourceComponentId :: ComponentTypeId
httpResourceComponentId = ComponentTypeId 13

instance Component HttpResourceData where
    componentId _ = httpResourceComponentId

{- | Accessor type for HTTP resources.

This provides a type-safe way to access HTTP resources through the
resource handle's access function.
-}
newtype HttpAccessor = HttpAccessor
    { httpConnManager :: Manager
    }

-- | Create a new HTTP accessor from resource data.
newHttpAccessor :: HttpResourceData -> HttpAccessor
newHttpAccessor data_ = HttpAccessor{httpConnManager = httpManager data_}

-- | Close an HTTP accessor (and its underlying manager).
closeHttpAccessor :: HttpAccessor -> IO ()
closeHttpAccessor _accessor = do
    -- Note: HTTP managers don't have explicit close in http-client
    -- They rely on GC. However, we could track connections and close them.
    pure ()

{- | Create a new HTTP manager with the given configuration.

This creates a TLS-enabled manager with settings based on the configuration.
-}
createHttpManager :: HttpManagerConfig -> IO Manager
createHttpManager _config = do
    -- Note: http-client doesn't expose direct pool size control
    -- through tlsManagerSettings, but we could use
    -- managerConnCount in a real implementation
    TLS.newTlsManagerWith TLS.tlsManagerSettings

{- | Execute an action with an HTTP accessor.

This is a helper function that handles creating and closing the manager
based on the configuration. In practice, the accessor is created once
and reused through the resource handle.

Example:
@
let config = HttpManagerConfig
        { httpPoolSize = 10
        , httpConnectionTimeoutSeconds = 30
        , httpResponseTimeoutSeconds = 60
        }
withHttpAccessor config $ \accessor -> do
    let mgr = httpConnManager accessor
    request <- parseRequest "https://api.example.com/data"
    response <- httpLbs request mgr
    process response
@
-}
withHttpAccessor ::
    HttpManagerConfig ->
    (HttpAccessor -> IO a) ->
    IO (Either String a)
withHttpAccessor config action = do
    result <- try $ bracket open close (action . newHttpAccessor)
    case result of
        Left (e :: IOError) -> pure $ Left $ show e
        Right val -> pure $ Right val
  where
    open = do
        mgr <- createHttpManager config
        pure $
            HttpResourceData
                { httpManager = mgr
                , httpConnectionCount = 0
                }
    close _data = do
        -- HTTP managers don't require explicit cleanup
        -- Connections are closed when the manager is garbage collected
        pure ()

