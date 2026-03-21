{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Core types for the Entity-Component-System (ECS) architecture.

This module defines the foundational types for the new OS architecture,
inspired by <https://dicioccio.fr/how-i-write-purescript-apps.html#the-cheap-entity-component-system>.

In ECS:
- Entities are unique identifiers (just IDs)
- Components are pure data attached to entities
- Systems are functions that process entities with specific components

== Tradeoffs

1. Type Erasure (Any) vs Type Safety: Using TVar Any requires careful
casting but enables heterogeneous storage. Alternative would be a closed-world
sum type which limits extensibility.

2. STM vs MVar: STM allows composable transactions, preferred for complex
resource coordination across multiple entities.

3. Component ID Assignment: Manual ID assignment currently used.
Template Haskell could auto-generate IDs to prevent collisions when adding
new component types.

== Migration Notes

This module is foundational and does not interact with existing code. It
establishes the new data model that will eventually replace parts of:
- System.Agents.Runtime.Runtime
- System.Agents.Base.Agent (description part)
- Parts of System.Agents.Tools.Base
-}
module System.Agents.OS.Core.Types (
    -- * Core Identifiers
    EntityId (..),
    newEntityId,

    -- * Phantom-typed entity IDs
    AgentId (..),
    ToolboxId (..),
    ConversationId (..),
    TurnId (..),
    ToolCallId (..),
    ResourceId (..),

    -- * Component system
    ComponentTypeId (..),
    Component (..),

    -- * Component storage
    ComponentStore (..),
    emptyComponentStore,
    insertComponent,
    lookupComponent,
    deleteComponent,

    -- * Type aliases for documentation
    ToolboxBindingSpec,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.Proxy (Proxy)
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID
import GHC.Generics (Generic)

-------------------------------------------------------------------------------
-- Core Identifiers
-------------------------------------------------------------------------------

-- | Unique identifier for any entity in the system.
newtype EntityId = EntityId {unEntityId :: UUID}
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (Hashable, FromJSON, ToJSON)

-- | Generate a new unique EntityId.
newEntityId :: IO EntityId
newEntityId = EntityId <$> UUID.nextRandom

-------------------------------------------------------------------------------
-- Phantom-typed Entity IDs
-------------------------------------------------------------------------------

{- | Phantom-typed entity ID for agents.

The phantom type ensures type safety when working with agent entities.
-}
newtype AgentId = AgentId {unAgentId :: EntityId}
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (Hashable, FromJSON, ToJSON)

-- | Phantom-typed entity ID for toolboxes.
newtype ToolboxId = ToolboxId {unToolboxId :: EntityId}
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (Hashable, FromJSON, ToJSON)

-- | Phantom-typed entity ID for conversations.
newtype ConversationId = ConversationId {unConversationId :: EntityId}
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (Hashable, FromJSON, ToJSON)

-- | Phantom-typed entity ID for turns.
newtype TurnId = TurnId {unTurnId :: EntityId}
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (Hashable, FromJSON, ToJSON)

-- | Phantom-typed entity ID for tool calls.
newtype ToolCallId = ToolCallId {unToolCallId :: EntityId}
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (Hashable, FromJSON, ToJSON)

-- | Phantom-typed entity ID for resources.
newtype ResourceId = ResourceId {unResourceId :: EntityId}
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (Hashable, FromJSON, ToJSON)

-------------------------------------------------------------------------------
-- Component System
-------------------------------------------------------------------------------

{- | Type-level identifier for component types.

Each component type must have a unique ID to prevent collisions
in the heterogeneous storage. These IDs are assigned manually for now.
-}
newtype ComponentTypeId = ComponentTypeId {unComponentTypeId :: Int}
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (Hashable, FromJSON, ToJSON)

{- | A component is any data type that can be attached to an entity.

The 'componentId' function provides a type-level identifier used
for routing components to their appropriate storage.
-}
class Component a where
    componentId :: Proxy a -> ComponentTypeId

{- | Type alias for toolbox binding specifications.
Placeholder for the full binding spec type.
-}
type ToolboxBindingSpec = Text

-------------------------------------------------------------------------------
-- Component Storage
-------------------------------------------------------------------------------

{- | Storage for all components of a specific type.

Uses a HashMap for O(1) lookups by EntityId.
-}
newtype ComponentStore a = ComponentStore
    { store :: HashMap EntityId a
    }
    deriving stock (Show, Generic)

-- | Create an empty component store.
emptyComponentStore :: ComponentStore a
emptyComponentStore = ComponentStore HashMap.empty

-- | Insert or update a component for an entity.
insertComponent :: EntityId -> a -> ComponentStore a -> ComponentStore a
insertComponent eid comp (ComponentStore s) =
    ComponentStore $ HashMap.insert eid comp s

-- | Lookup a component for an entity.
lookupComponent :: EntityId -> ComponentStore a -> Maybe a
lookupComponent eid (ComponentStore s) = HashMap.lookup eid s

-- | Delete a component for an entity.
deleteComponent :: EntityId -> ComponentStore a -> ComponentStore a
deleteComponent eid (ComponentStore s) =
    ComponentStore $ HashMap.delete eid s
