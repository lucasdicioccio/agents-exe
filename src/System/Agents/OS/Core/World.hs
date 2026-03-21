{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
World storage for the ECS architecture.

This module provides the top-level world container that holds all
component stores. It uses STM for thread-safe concurrent access and
heterogeneous storage via type erasure to @Any@.

== Storage Model

The world contains a map from 'ComponentTypeId' to 'TVar Any'. Each
component type has its own TVar containing a 'ComponentStore' wrapped
in 'Any' for type erasure.

== Safety Considerations

The use of 'Any' requires careful casting. The API provided here ensures
type safety by only exposing typed operations. Internal unsafeCoerce
operations are isolated and verified through the Component typeclass.

== Example Usage

@
import System.Agents.OS.Core.World
import System.Agents.OS.Core.Types

-- Create a new world
world <- newWorld

-- Register a component store
registerComponentStore world (Proxy @MyComponent)

-- In an STM transaction:
atomically $ do
    modifyComponent world entityId myComponent
    mComp <- getComponent world entityId
    ...
@
-}
module System.Agents.OS.Core.World (
    -- * World
    World (..),
    newWorld,

    -- * Component store management
    registerComponentStore,
    getComponentStore,

    -- * Entity operations
    createEntity,
    entityExists,

    -- * Component operations
    getComponent,
    setComponent,
    modifyComponent,
    removeComponent,
    hasComponent,

    -- * Query operations
    allEntitiesWithComponent,
) where

import Control.Concurrent.STM (STM, TVar, newTVar, readTVar, writeTVar)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (isJust)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)

import System.Agents.OS.Core.Types (
    Component (..),
    ComponentStore (..),
    ComponentTypeId,
    EntityId,
    deleteComponent,
    emptyComponentStore,
    insertComponent,
    lookupComponent,
    newEntityId,
 )

-------------------------------------------------------------------------------
-- World
-------------------------------------------------------------------------------

{- | The world contains all component stores.

Each component type is stored in its own TVar, allowing for
fine-grained concurrency control. Components are stored as @Any@
to allow heterogeneous storage.
-}
newtype World = World
    { componentStores :: HashMap ComponentTypeId (TVar Any)
    }

-- | Create an empty world.
newWorld :: STM World
newWorld = pure $ World HashMap.empty

-------------------------------------------------------------------------------
-- Component Store Management
-------------------------------------------------------------------------------

{- | Register a new component store for a component type.

This creates an empty 'ComponentStore' wrapped in a 'TVar'.
If a store already exists for this component type, it is replaced.
-}
registerComponentStore ::
    forall a.
    (Component a, Typeable a) =>
    World ->
    Proxy a ->
    STM World
registerComponentStore (World stores) _proxy = do
    let cid = componentId (Proxy @a)
    tvar <- newTVar $ toAny (emptyComponentStore @a)
    pure $ World $ HashMap.insert cid tvar stores
  where
    toAny :: ComponentStore a -> Any
    toAny = unsafeCoerce

-- | Get the raw TVar for a component store (internal use).
getComponentStoreVar ::
    forall a.
    (Component a) =>
    World ->
    Proxy a ->
    Maybe (TVar Any)
getComponentStoreVar (World stores) _proxy =
    HashMap.lookup (componentId (Proxy @a)) stores

{- | Get the component store for a specific component type.
Returns Nothing if the store hasn't been registered.
-}
getComponentStore ::
    forall a.
    (Component a, Typeable a) =>
    World ->
    Proxy a ->
    STM (Maybe (ComponentStore a))
getComponentStore world proxy =
    case getComponentStoreVar world proxy of
        Nothing -> pure Nothing
        Just tvar -> do
            anyVal <- readTVar tvar
            pure $ fromAny anyVal
  where
    fromAny :: Any -> Maybe (ComponentStore a)
    fromAny anyVal = do
        let compStore = unsafeCoerce anyVal :: ComponentStore a
        pure compStore

-------------------------------------------------------------------------------
-- Entity Operations
-------------------------------------------------------------------------------

{- | Create a new entity by generating a fresh EntityId.

Note: This just generates the ID. Use 'setComponent' to attach
components to the entity.
-}
createEntity :: IO EntityId
createEntity = newEntityId

{- | Check if an entity has at least one component in the world.

This is a potentially expensive operation as it checks all component stores.
-}
entityExists :: World -> EntityId -> STM Bool
entityExists (World stores) eid = do
    anyHasComponent <- mapM (checkStore eid) (HashMap.elems stores)
    pure $ or anyHasComponent
  where
    checkStore :: EntityId -> TVar Any -> STM Bool
    checkStore entity tvar = do
        anyVal <- readTVar tvar
        -- We need to check if the entity exists in this store
        -- Since we don't know the type, we use a generic check
        -- This is safe because ComponentStore has a consistent layout
        let compStore = unsafeCoerce anyVal :: ComponentStore ()
        pure $ isJust $ lookupComponent entity compStore

-------------------------------------------------------------------------------
-- Component Operations
-------------------------------------------------------------------------------

{- | Get a component for an entity.
Returns Nothing if the entity doesn't have this component
or if the component store isn't registered.
-}
getComponent ::
    forall a.
    (Component a, Typeable a) =>
    World ->
    EntityId ->
    STM (Maybe a)
getComponent world eid =
    case getComponentStoreVar world (Proxy @a) of
        Nothing -> pure Nothing
        Just tvar -> do
            anyVal <- readTVar tvar
            let compStore = unsafeCoerce anyVal :: ComponentStore a
            pure $ lookupComponent eid compStore

{- | Set a component for an entity.
Creates the component if it doesn't exist, updates it otherwise.
Does nothing if the component store isn't registered.
-}
setComponent ::
    forall a.
    (Component a, Typeable a) =>
    World ->
    EntityId ->
    a ->
    STM ()
setComponent world eid comp =
    case getComponentStoreVar world (Proxy @a) of
        Nothing -> pure ()
        Just tvar -> do
            anyVal <- readTVar tvar
            let compStore = unsafeCoerce anyVal :: ComponentStore a
            let newStore = insertComponent eid comp compStore
            writeTVar tvar (unsafeCoerce newStore)

{- | Modify a component for an entity.
Does nothing if the entity doesn't have this component
or if the component store isn't registered.
-}
modifyComponent ::
    forall a.
    (Component a, Typeable a) =>
    World ->
    EntityId ->
    (a -> a) ->
    STM ()
modifyComponent world eid f =
    case getComponentStoreVar world (Proxy @a) of
        Nothing -> pure ()
        Just tvar -> do
            anyVal <- readTVar tvar
            let compStore = unsafeCoerce anyVal :: ComponentStore a
            case lookupComponent eid compStore of
                Nothing -> pure ()
                Just comp -> do
                    let newStore = insertComponent eid (f comp) compStore
                    writeTVar tvar (unsafeCoerce newStore)

{- | Remove a component from an entity.
Does nothing if the entity doesn't have this component.

Use with TypeApplications: @removeComponent @MyComponent world entityId@
-}
removeComponent ::
    forall a.
    (Component a, Typeable a) =>
    World ->
    EntityId ->
    STM ()
removeComponent world eid =
    case getComponentStoreVar world (Proxy @a) of
        Nothing -> pure ()
        Just tvar -> do
            anyVal <- readTVar tvar
            let compStore = unsafeCoerce anyVal :: ComponentStore a
            let newStore = deleteComponent eid compStore
            writeTVar tvar (unsafeCoerce newStore)

{- | Check if an entity has a specific component.

Use with TypeApplications: @hasComponent @MyComponent world entityId@
-}
hasComponent ::
    forall a.
    (Component a, Typeable a) =>
    World ->
    EntityId ->
    STM Bool
hasComponent world eid = do
    mComp <- getComponent @a world eid
    pure $ isJust mComp

-------------------------------------------------------------------------------
-- Query Operations
-------------------------------------------------------------------------------

{- | Get all entities that have a specific component.

Use with TypeApplications: @allEntitiesWithComponent @MyComponent world@
-}
allEntitiesWithComponent ::
    forall a.
    (Component a, Typeable a) =>
    World ->
    STM [EntityId]
allEntitiesWithComponent world =
    case getComponentStoreVar world (Proxy @a) of
        Nothing -> pure []
        Just tvar -> do
            anyVal <- readTVar tvar
            let ComponentStore storeMap = unsafeCoerce anyVal :: ComponentStore a
            pure $ HashMap.keys storeMap
