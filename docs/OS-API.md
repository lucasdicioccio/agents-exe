# OS Model API Reference

Complete API reference for the Entity-Component-System (ECS) based OS architecture.

## Table of Contents

1. [Core ECS Types](#core-ecs-types)
2. [World Operations](#world-operations)
3. [Agent Operations](#agent-operations)
4. [Toolbox Operations](#toolbox-operations)
5. [Resource Management](#resource-management)
6. [Concurrent Access](#concurrent-access)
7. [Conversation Tracking](#conversation-tracking)
8. [Persistence Layer](#persistence-layer)
9. [Compatibility Layer](#compatibility-layer)

---

## Core ECS Types

### EntityId

```haskell
-- | Unique identifier for any entity in the system.
newtype EntityId = EntityId { unEntityId :: UUID }
    deriving (Eq, Ord, Show, Hashable, FromJSON, ToJSON)

-- | Generate a new unique EntityId.
newEntityId :: IO EntityId
```

**Example:**
```haskell
eid1 <- newEntityId  -- Unique ID
eid2 <- newEntityId  -- Different unique ID
assert (eid1 /= eid2)  -- True
```

### Phantom-Typed Entity IDs

Phantom types provide compile-time safety for entity operations:

```haskell
newtype AgentId = AgentId { unAgentId :: EntityId }
newtype ToolboxId = ToolboxId { unToolboxId :: EntityId }
newtype ConversationId = ConversationId { unConversationId :: EntityId }
newtype TurnId = TurnId { unTurnId :: EntityId }
newtype ToolCallId = ToolCallId { unToolCallId :: EntityId }
newtype ResourceId = ResourceId { unResourceId :: EntityId }
```

**Example:**
```haskell
eid <- newEntityId
let agentId = AgentId eid
let toolboxId = ToolboxId eid  -- Same underlying ID, different type

-- Type-safe: Can't mix up IDs
useAgent agentId       -- OK
useAgent toolboxId     -- Compile error!
```

### Component Typeclass

```haskell
-- | Type-level identifier for component types.
newtype ComponentTypeId = ComponentTypeId { unComponentTypeId :: Int }

-- | A component is any data type that can be attached to an entity.
class Component a where
    componentId :: Proxy a -> ComponentTypeId
```

**Creating a custom component:**
```haskell
data MyComponent = MyComponent
    { mcValue :: Int
    , mcName :: Text
    } deriving (Show, Eq, Generic)

instance FromJSON MyComponent
instance ToJSON MyComponent

-- Choose a unique ID (check existing IDs to avoid collisions)
instance Component MyComponent where
    componentId _ = ComponentTypeId 100
```

---

## World Operations

### Creating and Managing Worlds

```haskell
-- | The world contains all component stores.
newtype World = World { componentStores :: HashMap ComponentTypeId (TVar Any) }

-- | Create an empty world.
newWorld :: STM World

-- | Register a component store for a component type.
registerComponentStore :: (Component a, Typeable a) => World -> Proxy a -> STM World
```

**Example:**
```haskell
import Data.Proxy (Proxy(..))

-- Create world with agent components
world <- atomically $ do
    w <- newWorld
    w' <- registerComponentStore w (Proxy @AgentConfig)
    w'' <- registerComponentStore w' (Proxy @AgentState)
    pure w''
```

### Entity Operations

```haskell
-- | Create a new entity.
createEntity :: IO EntityId

-- | Check if an entity exists (has any component).
entityExists :: World -> EntityId -> STM Bool

-- | Get all entities with a specific component.
allEntitiesWithComponent :: (Component a, Typeable a) => World -> STM [EntityId]
```

**Example:**
```haskell
eid <- createEntity
exists <- atomically $ entityExists world eid  -- False

atomically $ setComponent world eid (AgentConfig "test" ...)
exists' <- atomically $ entityExists world eid  -- True

-- Find all agents
agentIds <- atomically $ allEntitiesWithComponent @AgentConfig world
```

### Component Operations

```haskell
-- | Get a component for an entity.
getComponent :: (Component a, Typeable a) => World -> EntityId -> STM (Maybe a)

-- | Set a component for an entity.
setComponent :: (Component a, Typeable a) => World -> EntityId -> a -> STM ()

-- | Modify a component for an entity.
modifyComponent :: (Component a, Typeable a) => World -> EntityId -> (a -> a) -> STM ()

-- | Remove a component from an entity.
removeComponent :: (Component a, Typeable a) => World -> EntityId -> STM ()

-- | Check if an entity has a specific component.
hasComponent :: (Component a, Typeable a) => World -> EntityId -> STM Bool
```

**Example:**
```haskell
-- Set component
atomically $ setComponent world eid AgentConfig
    { agentName = "my-agent"
    , agentModel = ModelConfig "openai" "..." "gpt-4" "key1"
    , agentSystemPrompt = "You are helpful"
    , agentToolboxBindings = []
    }

-- Get component
mConfig <- atomically $ getComponent @AgentConfig world eid
case mConfig of
    Nothing -> putStrLn "No agent config"
    Just config -> putStrLn $ "Agent: " ++ show config.agentName

-- Modify component
atomically $ modifyComponent @AgentConfig world eid $
    \c -> c { agentName = "renamed-agent" }

-- Check and remove
hasIt <- atomically $ hasComponent @AgentConfig world eid
when hasIt $ atomically $ removeComponent @AgentConfig world eid
```

---

## Agent Operations

### Agent Configuration

```haskell
data AgentConfig = AgentConfig
    { agentName :: Text                    -- ^ Human-readable name
    , agentModel :: ModelConfig            -- ^ LLM configuration
    , agentSystemPrompt :: Text            -- ^ System instructions
    , agentToolboxBindings :: [ToolboxBindingSpec]  -- ^ Bound toolboxes
    } deriving (Show, Eq, Generic)

data ModelConfig = ModelConfig
    { modelFlavor :: Text      -- ^ Provider (openai, mistral, etc.)
    , modelUrl :: Text         -- ^ API endpoint
    , modelName :: Text        -- ^ Model identifier
    , modelApiKeyId :: Text    -- ^ Key reference
    } deriving (Show, Eq, Generic)
```

### Agent State

```haskell
data AgentState = AgentState
    { agentStatus :: AgentStatus           -- ^ Current status
    , agentCurrentConversation :: Maybe ConversationId
    , agentCreatedAt :: UTCTime
    } deriving (Show, Eq, Generic)

data AgentStatus
    = AgentIdle                           -- ^ Available for work
    | AgentBusy TurnId                    -- ^ Executing a turn
    | AgentError Text                     -- ^ Error state
    deriving (Show, Eq, Generic)
```

**Example: Creating an Agent**
```haskell
createAgent :: OS -> AgentConfig -> IO AgentId
createAgent os config = do
    eid <- createEntity
    now <- getCurrentTime
    
    atomically $ do
        setComponent (osWorld os) eid config
        setComponent (osWorld os) eid AgentState
            { agentStatus = AgentIdle
            , agentCurrentConversation = Nothing
            , agentCreatedAt = now
            }
    
    pure $ AgentId eid

-- Usage
let config = AgentConfig
    { agentName = "coder-agent"
    , agentModel = ModelConfig "openai" "https://api.openai.com/v1" "gpt-4" "openai-key"
    , agentSystemPrompt = "You are a helpful coding assistant"
    , agentToolboxBindings = []
    }
agentId <- createAgent os config
```

---

## Toolbox Operations

### Toolbox Configuration

```haskell
data ToolboxConfig = ToolboxConfig
    { toolboxName :: Text           -- ^ Human-readable name
    , toolboxType :: ToolboxType    -- ^ Type of toolbox
    , toolboxSettings :: Value      -- ^ Type-specific settings
    } deriving (Show, Eq, Generic)

data ToolboxType
    = ToolboxTypeBash
    | ToolboxTypeMCP
    | ToolboxTypeOpenAPI
    | ToolboxTypePostgREST
    | ToolboxTypeSqlite
    | ToolboxTypeSystem
    | ToolboxTypeDeveloper
    | ToolboxTypeLua
    | ToolboxTypeSkills
    deriving (Show, Eq, Generic)
```

### Toolbox State

```haskell
data ToolboxState = ToolboxState
    { toolboxScope :: ResourceScope     -- ^ Resource lifetime scope
    , toolboxStatus :: ToolboxStatus    -- ^ Current status
    , toolboxResourceRef :: Maybe ResourceId  -- ^ Associated resource
    } deriving (Show, Eq, Generic)

data ToolboxStatus
    = ToolboxInitializing
    | ToolboxReady
    | ToolboxError Text
    | ToolboxDisposed
    deriving (Show, Eq, Generic)
```

### Resource Scope

```haskell
data ResourceScope
    = ScopeGlobal                       -- ^ Global/program scope
    | ScopeAgent AgentId                -- ^ Per-agent scope
    | ScopeConversation ConversationId  -- ^ Per-conversation scope
    deriving (Show, Eq, Generic)
```

**Example: Creating Toolboxes**
```haskell
-- Create a bash toolbox
let bashConfig = ToolboxConfig
    { toolboxName = "bash-tools"
    , toolboxType = ToolboxTypeBash
    , toolboxSettings = object ["directory" .= "./tools"]
    }
bashId <- createToolbox os bashConfig

-- Create a SQLite toolbox
let sqliteConfig = ToolboxConfig
    { toolboxName = "memory-db"
    , toolboxType = ToolboxTypeSqlite
    , toolboxSettings = object 
        [ "path" .= "./memory.db"
        , "access" .= "readwrite"
        ]
    }
sqliteId <- createToolbox os sqliteConfig

-- Bind toolboxes to agent
let agentConfig = AgentConfig
    { ...
    , agentToolboxBindings = 
        [ unToolboxId bashId
        , unToolboxId sqliteId
        ]
    }
```

---

## Resource Management

### Resource Types

```haskell
data ResourceType
    = SqliteResource SqliteConfig
    | LuaResource LuaConfig
    | HttpResource HttpConfig
    | CustomResource Text Value
    deriving (Show, Eq)

data ResourceHandle = ResourceHandle
    { handleId :: ResourceId
    , handleCleanup :: IO ()
    , handleAccess :: (ResourceAccessor -> IO a) -> IO a
    }

data ResourceInfo = ResourceInfo
    { resourceId :: ResourceId
    , resourceScope :: ResourceScope
    , resourceType :: ResourceType
    , resourceCreatedAt :: UTCTime
    }
```

### Resource Context

```haskell
data ResourceContext = ResourceContext
    { contextScope :: [ScopeLevel]      -- ^ Active scope path
    , contextRegistry :: ResourceRegistry
    }

data ScopeLevel
    = ProgramScope
    | AgentScope AgentId
    | ToolboxScope ToolboxId
    | ConversationScope ConversationId
    | TurnScope TurnId
    | ToolCallScope ToolCallId
    deriving (Show, Eq)
```

### Resource Operations

```haskell
-- | Create a new resource.
createResource :: 
    ResourceContext -> 
    ResourceType -> 
    (ResourceId -> IO ResourceHandle) -> 
    IO ResourceId

-- | Cleanup all resources in a scope.
cleanupScope :: ResourceRegistry -> ScopeLevel -> IO ()

-- | Check if a resource is valid in given scopes.
isResourceValid :: ResourceScope -> [ScopeLevel] -> Bool

-- | Access a resource with a function.
withResource :: ResourceRegistry -> ResourceId -> (ResourceAccessor -> IO a) -> IO (Maybe a)

-- | Get total resource count.
getResourceCount :: ResourceRegistry -> IO Int
```

**Example: Resource Lifecycle**
```haskell
import System.Agents.OS.Resources

-- Create registry and context
registry <- atomically newResourceRegistry
let ctx = ResourceContext [ProgramScope] registry

-- Create SQLite resource
rid <- createResource ctx (SqliteResource config) $ \rid -> do
    conn <- openConnection config
    pure ResourceHandle
        { handleId = rid
        , handleCleanup = do
            putStrLn "Closing SQLite connection"
            closeConnection conn
        , handleAccess = \f -> f (SqliteAccessor conn)
        }

-- Use the resource
result <- withResource registry rid $ \accessor -> do
    case accessor of
        SqliteAccessor conn -> queryDatabase conn "SELECT * FROM table"

-- Later, cleanup all agent resources
cleanupScope registry (AgentScope agentId)
```

---

## Concurrent Access

### Access Patterns

```haskell
data AccessPattern
    = ExclusiveAccess      -- ^ Single accessor (TMVar)
    | ReadWriteAccess      -- ^ Multiple readers, single writer (RWLock)
    | PoolAccess Int       -- ^ Bounded pool (TBQueue)
    | StatelessAccess      -- ^ No synchronization needed
    deriving (Show, Eq)

data AccessControl = AccessControl
    { accessPattern :: AccessPattern
    , accessTimeout :: Maybe NominalDiffTime
    }
```

### Resource Monad

```haskell
-- | Monad for resource operations.
newtype ResourceM a = ResourceM { unResourceM :: ReaderT ResourceContext (ExceptT ResourceError IO) a }
    deriving (Functor, Applicative, Monad, MonadIO)

-- | Run a ResourceM computation.
runResourceM :: ResourceContext -> ResourceM a -> IO (Either ResourceError a)

-- | Resource errors.
data ResourceError
    = ResourceNotFound ResourceId
    | ResourceBusy ResourceId
    | ResourceClosed ResourceId
    | ResourceAccessTimeout ResourceId NominalDiffTime
    | ResourceInvalidAccess ResourceId Text
    deriving (Show, Eq)
```

### Access Operations

```haskell
-- | Execute with exclusive access.
withExclusive :: ResourceId -> ResourceM a -> ResourceM a

-- | Execute with read access.
withRead :: ResourceId -> ResourceM a -> ResourceM a

-- | Execute with write access.
withWrite :: ResourceId -> ResourceM a -> ResourceM a

-- | Execute with pooled resource.
withPooled :: ResourceId -> ResourceM a -> ResourceM a

-- | Execute without synchronization.
withStateless :: ResourceId -> ResourceM a -> ResourceM a
```

**Example: Concurrent Access Patterns**
```haskell
import System.Agents.OS.Concurrent

-- SQLite with WAL mode (supports concurrent reads)
sqliteRid <- createSqliteResource ctx config True  -- WAL mode enabled

-- Multiple concurrent reads (safe)
forConcurrently_ [1..10] $ \_ -> do
    result <- runResourceM ctx $ withRead sqliteRid $ do
        queryData
    print result

-- Exclusive write (blocks readers)
runResourceM ctx $ withWrite sqliteRid $ do
    modifyData

-- Lua interpreter (requires exclusive access)
luaRid <- createLuaResource ctx config
result <- runResourceM ctx $ withExclusive luaRid $ do
    runScript "return 1 + 1"

-- HTTP connection pool
httpRid <- createHttpPool ctx 10  -- 10 connections
results <- forConcurrently urls $ \url ->
    runResourceM ctx $ withPooled httpRid $ do
        fetchUrl url
```

### Initialization Helpers

```haskell
-- | Initialize access control.
initAccessControl :: AccessPattern -> Maybe NominalDiffTime -> AccessControl

-- | Initialize SQLite access (WAL mode flag).
initSqliteAccess :: Bool -> IO SqliteAccess

-- | Initialize Lua access.
initLuaAccess :: IO LuaAccess

-- | Initialize HTTP access.
initHttpAccess :: Int -> ResourceId -> IO HttpAccess
```

---

## Conversation Tracking

### Conversation Components

```haskell
data ConversationConfig = ConversationConfig
    { conversationTitle :: Maybe Text
    , conversationMetadata :: Map Text Value
    } deriving (Show, Eq, Generic)

data ConversationState = ConversationState
    { conversationAgentId :: AgentId
    , conversationStatus :: ConversationStatus
    , conversationStartedAt :: UTCTime
    , conversationLastActivity :: TVar UTCTime
    } deriving (Generic)

data ConversationStatus
    = ConversationActive
    | ConversationPaused
    | ConversationCompleted
    | ConversationError Text
    deriving (Show, Eq, Generic)
```

### Turn Components

```haskell
data TurnConfig = TurnConfig
    { turnConversationId :: ConversationId
    , turnParentTurnId :: Maybe TurnId
    } deriving (Show, Eq, Generic)

data TurnState = TurnState
    { turnStatus :: TurnStatus
    , turnStartedAt :: UTCTime
    , turnCompletedAt :: Maybe UTCTime
    } deriving (Show, Eq, Generic)

data TurnStatus
    = TurnStarting
    | TurnProcessing
    | TurnWaitingForTools
    | TurnCompleted
    | TurnError Text
    deriving (Show, Eq, Generic)
```

### Tool Call Components

```haskell
data ToolCallConfig = ToolCallConfig
    { toolCallTurnId :: TurnId
    , toolCallParentId :: Maybe ToolCallId
    , toolCallName :: Text
    , toolCallInput :: Value
    } deriving (Show, Eq, Generic)

data ToolCallState = ToolCallState
    { toolCallStatus :: ToolCallStatus
    , toolCallResult :: Maybe Value
    , toolCallStartedAt :: UTCTime
    , toolCallCompletedAt :: Maybe UTCTime
    } deriving (Show, Eq, Generic)

data ToolCallStatus
    = ToolCallPending
    | ToolCallExecuting
    | ToolCallCompleted
    | ToolCallFailed Text
    deriving (Show, Eq, Generic)
```

### Lineage Tracking

```haskell
data Lineage = Lineage { unLineage :: [LineageFrame] }
    deriving (Show, Eq)

data LineageFrame = LineageFrame
    { frameType :: FrameType
    , frameEntityId :: EntityId
    , frameTimestamp :: UTCTime
    } deriving (Show, Eq)

data FrameType
    = ProgramFrame
    | AgentFrame
    | ToolboxFrame
    | ConversationFrame
    | TurnFrame
    | ToolCallFrame
    deriving (Show, Eq, Enum, Bounded)
```

### Lineage Operations

```haskell
-- | Empty lineage.
emptyLineage :: Lineage

-- | Push a frame onto the lineage.
pushLineage :: FrameType -> EntityId -> UTCTime -> Lineage -> Lineage

-- | Get lineage depth.
lineageDepth :: Lineage -> Int

-- | Get the most recent frame.
lineageHead :: Lineage -> Maybe LineageFrame

-- | Get the oldest frame.
lineageRoot :: Lineage -> Maybe LineageFrame

-- | Build context from lineage.
buildLineageContext :: Lineage -> LineageContext

-- | Find frames by type.
findFramesByType :: FrameType -> Lineage -> [LineageFrame]

-- | Check if in specific context.
isInConversation :: Lineage -> Bool
isInTurn :: Lineage -> Bool
currentFrameType :: Lineage -> Maybe FrameType
```

**Example: Conversation and Lineage**
```haskell
import System.Agents.OS.Conversation

-- Create conversation
convId <- createEntity
now <- getCurrentTime
lastActivity <- newTVarIO now

atomically $ do
    setComponent world convId ConversationConfig
        { conversationTitle = Just "My Chat"
        , conversationMetadata = Map.empty
        }
    setComponent world convId ConversationState
        { conversationAgentId = agentId
        , conversationStatus = ConversationActive
        , conversationStartedAt = now
        , conversationLastActivity = lastActivity
        }

-- Create turn
turnId <- createEntity
atomically $ do
    setComponent world turnId TurnConfig
        { turnConversationId = convId
        , turnParentTurnId = Nothing
        }
    setComponent world turnId TurnState
        { turnStatus = TurnStarting
        , turnStartedAt = now
        , turnCompletedAt = Nothing
        }

-- Build lineage
let lineage = pushLineage ConversationFrame (unConversationId convId) now $
              pushLineage TurnFrame (unTurnId turnId) now $
              emptyLineage

-- Check depth
print $ lineageDepth lineage  -- 2

-- Find conversation frames
let convFrames = findFramesByType ConversationFrame lineage
```

---

## Persistence Layer

### Backend Types

```haskell
data PersistenceHandle
    = InMemoryHandle
    | FileHandle FilePersistenceBackend FilePersistence
    | SqliteHandle SqliteBackend
    deriving (Show)

data PersistenceBackendType
    = InMemory                           -- ^ No persistence
    | FileBackendType FilePath FileFormat -- ^ File-based
    | SqliteBackendType FilePath          -- ^ SQLite
    | PostgresBackendType Text            -- ^ PostgreSQL
    deriving (Show, Eq)

data FileFormat = JSONFormat | YAMLFormat
    deriving (Show, Eq)
```

### Core Operations

```haskell
-- | Create a persistence backend.
createPersistenceBackend :: PersistenceBackendType -> IO PersistenceHandle

-- | Close a persistence backend.
closePersistenceBackend :: PersistenceHandle -> IO ()

-- | Persist a component.
persist :: (Persistable a, ToJSON a) => PersistenceHandle -> EntityId -> a -> IO ()

-- | Load a component.
load :: (Persistable a, FromJSON a) => PersistenceHandle -> EntityId -> IO (Maybe a)

-- | Query entities by component type.
query :: (Persistable a) => PersistenceHandle -> EntityQuery a -> IO [EntityId]

-- | Delete an entity and all components.
delete :: PersistenceHandle -> EntityId -> IO ()
```

### Persistable Typeclass

```haskell
class Persistable a where
    persistenceTable :: Proxy a -> Text
    persistenceKey :: Proxy a -> EntityId -> Text
    persistenceComponentType :: Proxy a -> ComponentType

-- Example instance
instance Persistable AgentConfig where
    persistenceTable _ = "agents"
    persistenceKey _ eid = "agent:" <> entityIdToText eid
    persistenceComponentType _ = componentTypeIdToType (ComponentTypeId 1)
```

### Event Logging

```haskell
-- | Persist an OS event.
persistOSEvent :: PersistenceHandle -> Text -> Value -> Maybe EntityId -> IO ()

-- | Get events for an entity.
getEvents :: PersistenceHandle -> EntityId -> IO [(UTCTime, Text, Value)]
```

### Specialized Queries

```haskell
-- | Get messages for a conversation.
getConversationMessages :: PersistenceHandle -> EntityId -> IO [(UTCTime, Text, Text, Maybe Value)]

-- | Get tool calls for a turn.
getTurnToolCalls :: PersistenceHandle -> EntityId -> IO [(Text, Text, Value, Maybe Value, Text)]

-- | Get nested tool calls.
getNestedToolCalls :: PersistenceHandle -> EntityId -> IO [(Text, Text, Value, Maybe Value, Text)]
```

**Example: Persistence Usage**
```haskell
import System.Agents.OS.Persistence

-- Create SQLite backend
backend <- createPersistenceBackend (SqliteBackendType "./agents.db")

-- Persist agent configuration
eid <- createEntity
let config = AgentConfig "my-agent" ...
persist backend eid config

-- Load it back later
mConfig <- load backend eid :: IO (Maybe AgentConfig)
case mConfig of
    Nothing -> putStrLn "Agent not found"
    Just cfg -> putStrLn $ "Loaded: " ++ show cfg.agentName

-- Query all agents
agentIds <- query backend (mkEntityQuery @AgentConfig)
putStrLn $ "Found " ++ show (length agentIds) ++ " agents"

-- Log an event
persistOSEvent backend "agent_created" 
    (object ["name" .= "my-agent"]) 
    (Just eid)

-- Get events
events <- getEvents backend eid
forM_ events $ \(time, eventType, data_) -> do
    putStrLn $ show time ++ ": " ++ show eventType

-- Cleanup
closePersistenceBackend backend
```

---

## Compatibility Layer

### Migration Types

```haskell
data MigrationConfig = MigrationConfig
    { migrationEnableOS :: Bool
    , migrationEnableCompat :: Bool
    , migrationLogPath :: Maybe FilePath
    } deriving (Show, Eq)

data MigrationPhase
    = PhaseOldOnly      -- ^ Only old Runtime
    | PhaseDual         -- ^ Both Runtime and OS
    | PhaseNewOnly      -- ^ Only new OS
    deriving (Show, Eq, Ord, Enum, Bounded)

data MigrationState = MigrationState
    { migrationPhase :: MigrationPhase
    , migrationCompatRuntimes :: Map Base.AgentId RuntimeBridge
    }
```

### Initialization

```haskell
-- | Default migration config (old-only for compatibility).
defaultMigrationConfig :: MigrationConfig
defaultMigrationConfig = MigrationConfig
    { migrationEnableOS = False
    , migrationEnableCompat = True
    , migrationLogPath = Nothing
    }

-- | Initialize with migration support.
initializeWithMigration :: MigrationConfig -> IO (Either String (Either Runtime OS))
```

### Runtime Bridge

```haskell
-- | Bridge from old Runtime to new OS.
data RuntimeBridge = RuntimeBridge
    { bridgeAgentId :: Base.AgentId
    , bridgeOS :: OS
    }

-- | Create a new runtime bridge.
newRuntimeBridge :: Base.AgentId -> OS -> RuntimeBridge

-- | Run an operation with the bridge.
runWithBridge :: RuntimeBridge -> ReaderT RuntimeBridge IO a -> IO a
```

### AgentRuntime Typeclass

```haskell
class (Monad m) => AgentRuntime m where
    listTools :: m [ToolRegistration]
    callTool :: Text -> Value -> m UserToolResponse
    getTracer :: m (Tracer IO Trace)

-- Instance for compatibility
instance AgentRuntime (ReaderT RuntimeBridge IO) where
    listTools = ...
    callTool = ...
    getTracer = ...
```

**Example: Using the Compatibility Layer**
```haskell
import System.Agents.OS.Compat

main :: IO ()
main = do
    -- Initialize with dual-mode support
    let config = defaultMigrationConfig
        { migrationEnableOS = True
        , migrationEnableCompat = True
        }
    
    result <- initializeWithMigration config
    case result of
        Left err -> error err
        Right (Left runtime) -> do
            -- Old Runtime path
            putStrLn "Using legacy Runtime"
            runWithRuntime runtime $ do
                tools <- listTools
                callTool "my-tool" args
                
        Right (Right os) -> do
            -- New OS path with compatibility
            putStrLn "Using new OS with compatibility"
            bridge <- newRuntimeBridge agentId os
            runWithBridge bridge $ do
                -- Same interface as old Runtime!
                tools <- listTools
                callTool "my-tool" args
```

---

## Common Patterns

### Pattern 1: Agent with Multiple Toolboxes

```haskell
createFullAgent :: OS -> Text -> [ToolboxId] -> IO AgentId
createFullAgent os name toolboxes = do
    let config = AgentConfig
        { agentName = name
        , agentModel = ModelConfig "openai" "..." "gpt-4" "key"
        , agentSystemPrompt = "You are helpful"
        , agentToolboxBindings = map unToolboxId toolboxes
        }
    createAgent os config

-- Usage
bashTb <- createToolbox os bashConfig
sqlTb <- createToolbox os sqliteConfig
httpTb <- createToolbox os httpConfig

agentId <- createFullAgent os "multi-tool-agent" [bashTb, sqlTb, httpTb]
```

### Pattern 2: Forking a Conversation

```haskell
forkConversation :: World -> ConversationId -> IO ConversationId
forkConversation world convId = do
    -- Get original config
    Just config <- atomically $ getComponent @ConversationConfig world (unConversationId convId)
    
    -- Create new conversation
    newConvId <- createEntity
    now <- getCurrentTime
    lastActivity <- newTVarIO now
    
    atomically $ do
        setComponent world newConvId config
        setComponent world newConvId ConversationState
            { conversationAgentId = ...
            , conversationStatus = ConversationActive
            , conversationStartedAt = now
            , conversationLastActivity = lastActivity
            }
    
    -- Copy turn history
    turns <- getConversationTurns world convId
    forM_ turns $ \turnId -> do
        Just turnConfig <- atomically $ getComponent @TurnConfig world (unTurnId turnId)
        newTurnId <- createEntity
        atomically $ setComponent world newTurnId turnConfig
            { turnConversationId = ConversationId newConvId
            }
    
    pure $ ConversationId newConvId
```

### Pattern 3: Resource Pool with Timeout

```haskell
withPooledTimeout :: ResourceContext -> ResourceId -> NominalDiffTime -> ResourceM a -> ResourceM a
withPooledTimeout ctx rid timeout action = do
    currentTimeout <- getResourceTimeout rid
    -- Temporarily set timeout
    updateResourceTimeout rid (Just timeout)
    result <- withPooled rid action
    -- Restore original timeout
    updateResourceTimeout rid currentTimeout
    pure result
```

### Pattern 4: Transaction with Multiple Components

```haskell
updateAgentAndToolbox :: World -> AgentId -> ToolboxId -> AgentConfig -> ToolboxConfig -> STM ()
updateAgentAndToolbox world agentId toolboxId agentConfig toolboxConfig = do
    -- Both updates happen atomically
    setComponent world (unAgentId agentId) agentConfig
    setComponent world (unToolboxId toolboxId) toolboxConfig

-- Usage
atomically $ updateAgentAndToolbox world agentId toolboxId newAgentConfig newToolboxConfig
```

---

## Type Index

### Core Types
- `EntityId` - Base entity identifier
- `AgentId`, `ToolboxId`, `ConversationId`, `TurnId`, `ToolCallId`, `ResourceId` - Phantom-typed IDs
- `ComponentTypeId` - Component type identifier
- `World` - ECS world container

### Component Types
- `AgentConfig`, `AgentState`, `AgentStatus` - Agent components
- `ToolboxConfig`, `ToolboxState`, `ToolboxBinding` - Toolbox components
- `ConversationConfig`, `ConversationState`, `ConversationStatus` - Conversation components
- `TurnConfig`, `TurnState`, `TurnStatus` - Turn components
- `ToolCallConfig`, `ToolCallState`, `ToolCallStatus` - Tool call components
- `Message`, `MessageRole` - Message components
- `Lineage`, `LineageFrame`, `FrameType` - Lineage components

### Resource Types
- `ResourceType`, `ResourceHandle`, `ResourceInfo` - Resource definitions
- `ResourceContext`, `ScopeLevel`, `ResourceScope` - Scope management
- `ResourceRegistry` - Resource storage

### Concurrent Types
- `AccessPattern`, `AccessControl` - Access patterns
- `ResourceM` - Resource monad
- `ResourceError` - Error types
- `SyncPrimitive`, `ExclusiveLock`, `ReadWriteLock`, `PoolLock` - Lock types

### Persistence Types
- `PersistenceHandle`, `PersistenceBackendType` - Backend types
- `Persistable` - Persistence typeclass
- `EntityQuery` - Query specification
- `MigrationConfig`, `MigrationPhase`, `MigrationState` - Migration types
- `RuntimeBridge` - Compatibility bridge

