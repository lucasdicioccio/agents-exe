# OS Model Migration Guide

This guide helps you migrate from the old Runtime-per-agent architecture to the new Entity-Component-System (ECS) based OS model.

## Overview

The OS model replaces the `Runtime` abstraction with a centralized operating system that manages agents, toolboxes, and resources using an ECS pattern. This enables:

- **Shared toolboxes** between agents (e.g., same SQLite database)
- **HTTP connection pooling** across all agents
- **Better resource lifecycle management** with explicit scopes
- **Durable persistence** with multiple backend options
- **Complete lineage tracking** for debugging and accounting

## Migration Phases

```
┌─────────────────┐     ┌─────────────────┐     ┌─────────────────┐
│   Phase 1       │────>│   Phase 2       │────>│   Phase 3       │
│   Old Only      │     │   Dual Mode     │     │   New Only      │
│                 │     │                 │     │                 │
│ Use existing    │     │ Gradual         │     │ Fully migrated  │
│ Runtime code    │     │ migration       │     │ to OS model     │
└─────────────────┘     └─────────────────┘     └─────────────────┘
       │                       │                       │
       │                       │                       │
       ▼                       ▼                       ▼
  No changes needed      Use compat layer         New OS interface
```

## Quick Start for New Projects

If you're starting a new project, use the OS model directly:

```haskell
import System.Agents.OS

main :: IO ()
main = do
    -- Initialize the OS
    os <- initializeOS defaultConfig
    
    -- Create a shared toolbox (SQLite)
    tbConfig <- createToolboxConfig SqliteToolboxType "shared-db" 
        (object ["path" .= "./data.db"])
    tbId <- createToolbox os tbConfig
    
    -- Create agents that share the toolbox
    agent1 <- createAgent os (testAgentConfig 
        { agentToolboxBindings = [tbId] })
    agent2 <- createAgent os (testAgentConfig 
        { agentToolboxBindings = [tbId] })
    
    -- Run conversations
    runOSM os $ do
        conv1 <- startConversation agent1 defaultConvConfig
        result1 <- runConversationTurn conv1 "Hello, agent 1!"
        
        conv2 <- startConversation agent2 defaultConvConfig
        result2 <- runConversationTurn conv2 "Hello, agent 2!"
        
        -- Both agents can access the same database!
        pure (result1, result2)
```

## Step-by-Step Migration

### Step 1: Identify Runtime Usage

First, find where you use the old Runtime:

```bash
# Search for Runtime imports and usage
grep -r "newRuntime" src/
grep -r "runWithRuntime" src/
grep -r "Runtime" --include="*.hs" src/ | grep -v "OS"
```

Common patterns to look for:

```haskell
-- Pattern 1: Runtime creation
runtime <- newRuntime props agent tracer

-- Pattern 2: Runtime usage in IO
result <- runWithRuntime runtime $ do
    tools <- listTools
    response <- callTool name args
    
-- Pattern 3: Multiple runtimes
runtimes <- mapM (newRuntime props) agents
```

### Step 2: Use the Compatibility Layer

The compatibility layer provides the old Runtime interface using the new OS backend:

```haskell
-- Old code
import System.Agents.Runtime

main = do
    runtime <- newRuntime props agent tracer
    result <- runWithRuntime runtime $ do
        tools <- listTools
        callTool "my-tool" args

-- With compatibility layer
import System.Agents.OS.Compat

main = do
    -- Initialize with migration support
    result <- initializeWithMigration defaultMigrationConfig
    case result of
        Left runtime -> do
            -- Use traditional Runtime
            result <- runWithRuntime runtime $ do
                tools <- listTools
                callTool "my-tool" args
        Right os -> do
            -- Use new OS (via compatibility bridge)
            bridge <- newRuntimeBridge agentId os
            result <- runWithBridge bridge $ do
                tools <- listTools
                callTool "my-tool" args
```

### Step 3: Gradually Migrate Components

#### Migrating Agent Configuration

```haskell
-- OLD: Runtime-based agent
let agent = Agent
    { slug = "my-agent"
    , apiKeyId = "openai"
    , flavor = "openai"
    , modelUrl = "https://api.openai.com/v1"
    , modelName = "gpt-4"
    , systemPrompt = ["You are helpful"]
    , toolDirectory = "./tools"
    , ...
    }

-- NEW: OS-based agent configuration
let agentConfig = AgentConfig
    { agentName = "my-agent"
    , agentModel = ModelConfig
        { modelFlavor = "openai"
        , modelUrl = "https://api.openai.com/v1"
        , modelName = "gpt-4"
        , modelApiKeyId = "openai"
        }
    , agentSystemPrompt = "You are helpful"
    , agentToolboxBindings = [bashToolboxId, sqliteToolboxId]
    }

-- Create the agent in the OS
agentId <- createAgent os agentConfig
```

#### Migrating Toolbox Configuration

```haskell
-- OLD: Tool directories
let agent = agent 
    { toolDirectory = "./tools"
    , builtinToolboxes = Just 
        [ SqliteToolbox SqliteToolboxDescription
            { sqliteToolboxName = "memory"
            , sqliteToolboxPath = "./memory.db"
            }
        ]
    }

-- NEW: Explicit toolbox entities
-- Create bash toolbox
bashConfig <- createToolboxConfig BashToolboxType "bash-tools"
    (object ["directory" .= "./tools"])
bashId <- createToolbox os bashConfig

-- Create SQLite toolbox
sqliteConfig <- createToolboxConfig SqliteToolboxType "memory-db"
    (object ["path" .= "./memory.db", "access" .= "readwrite"])
sqliteId <- createToolbox os sqliteConfig

-- Bind toolboxes to agent
let agentConfig = AgentConfig
    { ...
    , agentToolboxBindings = [unToolboxId bashId, unToolboxId sqliteId]
    }
```

#### Migrating Conversation Handling

```haskell
-- OLD: Session-based conversations
import System.Agents.Session.Types

session <- newSession agentId
let turn = UserTurn userContent Nothing
let session' = session { turns = turns session ++ [turn] }
saveSession sessionFile session'

-- NEW: ECS-based conversations
import System.Agents.OS.Conversation

-- Conversation is an entity with components
convId <- createEntity
setComponent world convId ConversationConfig
    { conversationTitle = Just "My Chat"
    , conversationMetadata = Map.empty
    }

-- Turns are separate entities
turnId <- createEntity
setComponent world turnId TurnConfig
    { turnConversationId = convId
    , turnParentTurnId = Nothing
    }

-- Messages are attached to turns
msgId <- createEntity
setComponent world msgId Message
    { messageRole = UserRole
    , messageContent = "Hello!"
    , messageToolCalls = Nothing
    }
```

### Step 4: Update Resource Management

```haskell
-- OLD: Implicit resource cleanup (GC-based)
withBashToolbox dir $ \tb -> do
    -- toolbox automatically cleaned up

-- NEW: Explicit resource scopes
import System.Agents.OS.Resources

-- Create a resource context
let ctx = ResourceContext [ProgramScope] registry

-- Create resource with explicit cleanup
rid <- createResource ctx (SqliteResource config) $ \rid -> do
    conn <- openConnection config
    pure ResourceHandle
        { handleId = rid
        , handleCleanup = closeConnection conn  -- Explicit!
        , handleAccess = \f -> f (SqliteAccessor conn)
        }

-- Later, explicitly cleanup
cleanupScope registry (AgentScope agentId)
```

### Step 5: Update Concurrent Access

```haskell
-- OLD: Direct resource access (may have race conditions)
result <- queryDatabase conn sql

-- NEW: Explicit access patterns
import System.Agents.OS.Concurrent

-- For SQLite with WAL mode
result <- runResourceM ctx $ withRead rid $ do
    queryDatabase sql

-- For exclusive access (Lua interpreter)
result <- runResourceM ctx $ withExclusive luaRid $ do
    runLuaScript script

-- For pooled resources (HTTP connections)
result <- runResourceM ctx $ withPooled httpRid $ do
    makeHttpRequest req
```

### Step 6: Enable Persistence

```haskell
-- NEW: Add persistence layer
import System.Agents.OS.Persistence

-- Create persistence backend
backend <- createPersistenceBackend (SqliteBackendType "./agents.db")

-- Persist agent configuration
persist backend (unAgentId agentId) agentConfig

-- Load agent configuration later
mConfig <- load backend (unAgentId agentId) :: IO (Maybe AgentConfig)

-- Query all agents
agentIds <- query backend (mkEntityQuery @AgentConfig)

-- Close backend when done
closePersistenceBackend backend
```

## API Changes Summary

| Old API | New API | Notes |
|---------|---------|-------|
| `newRuntime` | `initializeOS` | Creates OS instead of Runtime |
| `runWithRuntime` | `runOSM` | Runs OS monad instead of Runtime |
| `Agent` | `AgentConfig` + `AgentState` | Split into ECS components |
| `toolDirectory` | `createToolbox` | Toolboxes are explicit entities |
| `builtinToolboxes` | `createToolbox` + bindings | Toolboxes created separately |
| `Session` | `Conversation` + `Turn` + `Message` | Split into multiple entities |
| Implicit cleanup | `cleanupScope` | Explicit resource lifecycle |
| Direct access | `withRead`/`withWrite`/`withExclusive` | Controlled concurrent access |
| File-based sessions | `PersistenceHandle` | Pluggable persistence backends |

## Common Pitfalls

### Pitfall 1: Forgetting to Initialize Component Stores

```haskell
-- WRONG: World created but component stores not registered
world <- atomically newWorld
setComponent world eid myComponent  -- Does nothing!

-- CORRECT: Register component stores first
world <- atomically newWorld
world' <- atomically $ registerComponentStore world (Proxy @MyComponent)
setComponent world' eid myComponent
```

### Pitfall 2: Mixing Old and New IDs

```haskell
-- WRONG: Mixing old and new AgentId types
import System.Agents.Base (AgentId(..))  -- OLD
import System.Agents.OS.Core (AgentId(..))  -- NEW

-- These are different types! Be careful with imports.

-- CORRECT: Use qualified imports
import qualified System.Agents.Base as Base
import qualified System.Agents.OS.Core as OS

let oldId :: Base.AgentId = ...
let newId :: OS.AgentId = ...
```

### Pitfall 3: Not Handling Resource Cleanup

```haskell
-- WRONG: Resources not cleaned up
main = do
    os <- initializeOS defaultConfig
    agentId <- createAgent os config
    -- Agent resources never cleaned up!

-- CORRECT: Explicit cleanup
main = bracket
    (initializeOS defaultConfig)
    (\os -> cleanupOS os)  -- Or use runOSM with bracket
    (\os -> do
        agentId <- createAgent os config
        -- Use agent...
        destroyAgent os agentId  -- Cleanup resources
    )
```

### Pitfall 4: Concurrent Access Without Locks

```haskell
-- WRONG: Direct SQLite access from multiple threads
forkIO $ queryDatabase conn "SELECT ..."
forkIO $ queryDatabase conn "SELECT ..."
-- May corrupt database!

-- CORRECT: Use withRead for concurrent reads
forkIO $ runResourceM ctx $ withRead rid $ queryDatabase "SELECT ..."
forkIO $ runResourceM ctx $ withRead rid $ queryDatabase "SELECT ..."
-- Safe concurrent reads with WAL mode
```

### Pitfall 5: Forgetting STM for World Operations

```haskell
-- WRONG: Direct world access without STM
world <- atomically newWorld
world' <- registerComponentStore world (Proxy @AgentConfig)  -- Needs atomically
setComponent world' eid config  -- Needs atomically

-- CORRECT: All world operations in STM
world <- atomically $ do
    w <- newWorld
    w' <- registerComponentStore w (Proxy @AgentConfig)
    setComponent w' eid config
    pure w'
```

## Performance Considerations

### Memory Usage

- **Component Storage**: Uses more memory than direct fields due to HashMap overhead
- **ECS Overhead**: ~50-100 bytes per entity for storage structure
- **Mitigation**: Use strict fields, consider compacting for long-running processes

### Concurrency

- **STM Performance**: Generally fast for low contention
- **Lock Granularity**: Each component type has its own TVar
- **Best Practice**: Keep transactions short, avoid long computations in STM

### Query Performance

- **Component Lookup**: O(1) via HashMap
- **Entity Queries**: O(n) where n = number of entities with component
- **Index Usage**: Persistence layer creates indexes for common queries

### Migration Performance

- **Dual Mode Overhead**: ~5-10% performance penalty from compatibility layer
- **Recommendation**: Migrate critical paths first, keep dual mode only as needed

## Testing During Migration

### Unit Tests

```haskell
-- Test both old and new implementations
prop_runtimeParity :: Property
prop_runtimeParity = property $ do
    -- Run same operation through both
    oldResult <- runOldImplementation input
    newResult <- runNewImplementation input
    oldResult === newResult
```

### Integration Tests

```haskell
-- Test resource cleanup
testResourceCleanup :: IO ()
testResourceCleanup = do
    os <- initializeOS defaultConfig
    initialCount <- getResourceCount (osRegistry os)
    
    agentId <- createAgent os testAgentConfig
    -- Create resources...
    
    destroyAgent os agentId
    finalCount <- getResourceCount (osRegistry os)
    
    finalCount @?= initialCount  -- Resources cleaned up
```

### Compatibility Tests

```haskell
-- Test compatibility layer matches old behavior
test_compatLayerParity :: IO ()
test_compatLayerParity = do
    -- Create old Runtime
    oldRuntime <- newRuntime props agent tracer
    oldResult <- runWithRuntime oldRuntime testOperation
    
    -- Create new OS with compatibility bridge
    os <- initializeOS defaultConfig
    bridge <- newRuntimeBridge agentId os
    newResult <- runWithBridge bridge testOperation
    
    oldResult @?= newResult
```

## Troubleshooting

### Issue: Components Not Persisting

**Symptom**: Data disappears after restart

**Solution**: 
1. Check persistence backend is initialized
2. Verify `Persistable` instance exists for your type
3. Call `persist` after component changes

### Issue: Deadlocks in Concurrent Access

**Symptom**: Program hangs during resource access

**Solution**:
1. Check for nested `withRead`/`withWrite` calls on same resource
2. Ensure consistent lock ordering across threads
3. Add timeouts: `withTimeout 30 rid action`

### Issue: High Memory Usage

**Symptom**: Memory grows unbounded

**Solution**:
1. Check resource cleanup is happening
2. Verify no circular references in lineage
3. Use `cleanupScope` regularly for temporary resources

### Issue: Slow Queries

**Symptom**: Component queries are slow

**Solution**:
1. Use `allEntitiesWithComponent` for simple queries
2. Add custom indexes in persistence layer
3. Cache frequently accessed components

## Getting Help

- **Documentation**: See `docs/OS-API.md` for complete API reference
- **Examples**: Check `test/OS/IntegrationTests.hs` for usage examples
- **Issues**: File issues with "migration" label for migration-specific problems

## Migration Checklist

- [ ] Identify all Runtime usage in codebase
- [ ] Add compatibility layer imports
- [ ] Migrate agent configurations
- [ ] Migrate toolbox configurations
- [ ] Update resource management code
- [ ] Update concurrent access patterns
- [ ] Add persistence layer (optional)
- [ ] Update tests for new architecture
- [ ] Performance benchmark before/after
- [ ] Remove compatibility layer (final step)

