# OS Model Migration Guide

Complete migration guide for transitioning from the legacy Runtime-based architecture to the new ECS-based OS Model.

## Overview

The Agents framework has migrated from a **Runtime-per-agent** model to an **Entity-Component-System (ECS)** architecture. This change provides:

- **Better concurrency control** via STM-based component access
- **Resource lifecycle management** with scoped cleanup
- **Unified agent/toolbox/conversation tracking** in a single World
- **Subcall visibility** for multi-agent hierarchies
- **Persistence layer** with pluggable backends

## Architecture Comparison

### Legacy Runtime Model (Pre-OS)

```
┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐
│   Agent A       │  │   Agent B       │  │   Agent C       │
│ ┌─────────────┐ │  │ ┌─────────────┐ │  │ ┌─────────────┐ │
│ │   Runtime   │ │  │ │   Runtime   │ │  │ │   Runtime   │ │
│ │ ┌─────────┐ │ │  │ │ ┌─────────┐ │ │  │ │ ┌─────────┐ │ │
│ │ │ Session │ │ │  │ │ │ Session │ │ │  │ │ │ Session │ │ │
│ │ │ Tools   │ │ │  │ │ │ Tools   │ │ │  │ │ │ Tools   │ │ │
│ │ │ LLM     │ │ │  │ │ │ LLM     │ │ │  │ │ │ LLM     │ │ │
│ │ └─────────┘ │ │  │ │ └─────────┘ │ │  │ │ └─────────┘ │ │
│ └─────────────┘ │  │ └─────────────┘ │  │ └─────────────┘ │
└─────────────────┘  └─────────────────┘  └─────────────────┘
       │                    │                    │
       └────────────────────┼────────────────────┘
                            │
                    ┌───────▼───────┐
                    │  AgentTree    │
                    │  (references) │
                    └───────────────┘
```

### New OS Model (ECS)

```
┌─────────────────────────────────────────────────────────────┐
│                           OS                                │
│  ┌───────────────────────────────────────────────────────┐  │
│  │                        World                          │  │
│  │  ┌─────────────┐ ┌─────────────┐ ┌─────────────────┐  │  │
│  │  │ AgentStore  │ │ToolboxStore │ │ ConversationStore│  │  │
│  │  │  (TVar)     │ │  (TVar)     │ │    (TVar)       │  │  │
│  │  ├─────────────┤ ├─────────────┤ ├─────────────────┤  │  │
│  │  │ AgentConfig │ │ToolboxConfig│ │ ConversationCfg │  │  │
│  │  │ AgentState  │ │ToolboxState │ │ ConversationState│  │  │
│  │  │ ...         │ │ ...         │ │ ...             │  │  │
│  │  └─────────────┘ └─────────────┘ └─────────────────┘  │  │
│  └───────────────────────────────────────────────────────┘  │
│  ┌───────────────────────────────────────────────────────┐  │
│  │                   ResourceRegistry                    │  │
│  │    (SQLite, Lua, HTTP pools with scoped cleanup)      │  │
│  └───────────────────────────────────────────────────────┘  │
│  ┌───────────────────────────────────────────────────────┐  │
│  │                   EventQueue (TQueue)                 │  │
│  │          (OSEvents for TUI visibility)                │  │
│  └───────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────┘
```

## Migration Phases

The migration was implemented in three phases:

```
PhaseOldOnly ──► PhaseDual ──► PhaseNewOnly
(removed)        (default)     (available)
```

- **PhaseOldOnly**: Only legacy Runtime (removed as of March 2026)
- **PhaseDual**: Both Runtime and OS available (current default)
- **PhaseNewOnly**: Only OS Model (available for testing)

## Key Changes

### 1. Agent Creation

**Legacy:**
```haskell
import System.Agents.Runtime

-- Create runtime directly
runtime <- newRuntime tracer config tools
```

**New (OS Model):**
```haskell
import System.Agents.OS

-- Initialize OS
os <- initializeOS defaultOSConfig

-- Create agent in OS
agentId <- createAgent os AgentConfig
    { agentName = "my-agent"
    , agentModel = ModelConfig "openai" "..." "gpt-4" "key1"
    , agentSystemPrompt = "You are helpful"
    , agentToolboxBindings = []
    }
```

### 2. Toolbox Registration

**Legacy:**
```haskell
-- Tools passed to Runtime constructor
let tools = bashTools ++ mcpTools ++ ioTools
runtime <- newRuntime tracer config tools
```

**New:**
```haskell
-- Toolboxes created as entities in the World
bashId <- createToolbox os bashConfig
sqlId <- createToolbox os sqliteConfig

-- Bound to agent via agentToolboxBindings
let agentConfig = AgentConfig
    { ...
    , agentToolboxBindings = 
        [ unToolboxId bashId
        , unToolboxId sqlId
        ]
    }
```

### 3. Resource Management

**Legacy:**
```haskell
-- Manual resource tracking
connections <- newTVarIO []
-- Cleanup handled per-runtime
```

**New:**
```haskell
import System.Agents.OS.Resources

-- Create registry
registry <- atomically newResourceRegistry
let ctx = ResourceContext [ProgramScope] registry

-- Create scoped resource
rid <- createResource ctx (SqliteResource config) $ \rid -> do
    conn <- openConnection config
    pure ResourceHandle
        { handleId = rid
        , handleCleanup = closeConnection conn
        , handleAccess = \f -> f (SqliteAccessor conn)
        }

-- Automatic cleanup
cleanupScope registry (AgentScope agentId)
```

### 4. Conversation Tracking

**Legacy:**
```haskell
-- Session managed separately
session <- loadSession sessionPath
-- Conversation ID tracked in Session type
```

**New:**
```haskell
-- Conversation is a first-class entity
convId <- createEntity
atomically $ do
    setComponent world convId ConversationConfig{...}
    setComponent world convId ConversationState{...}

-- Link to agent
atomically $ modifyComponent @AgentState world (unAgentId agentId) $
    \s -> s { agentCurrentConversation = Just (ConversationId convId) }
```

### 5. Subcall Visibility

**Legacy:**
```haskell
-- Subcalls invisible to TUI
result <- turnAgentRuntimeIntoIOTool agent prompt
-- No visibility into subcall conversation
```

**New:**
```haskell
-- Subcalls visible as OSEvents
result <- turnAgentRuntimeIntoIOTool agent prompt
-- OSEvent_SubcallStarted, OSEvent_SubcallProgress, 
-- OSEvent_SubcallCompleted emitted
-- TUI displays subcall as separate conversation
```

## Compatibility Layer

The `System.Agents.OS.Compat.Runtime` module provides a compatibility bridge:

```haskell
import System.Agents.OS.Compat.Runtime

-- Run existing Runtime code with OS backend
withCompatOS $ \compat -> do
    -- Use compatRuntime for legacy code
    let runtime = compatRuntime compat
    
    -- Or use OS directly
    let os = compatOS compat
    
    -- Both are kept in sync
```

### Compatibility Patterns

**Pattern 1: Runtime Bridge**
```haskell
bridge <- newRuntimeBridge agentId os
runWithBridge bridge $ do
    -- Legacy Runtime code
    tools <- listTools
    callTool "my-tool" args
```

**Pattern 2: Dual Initialization**
```haskell
-- Initialize both
runtime <- newRuntime tracer config tools
os <- runtimeToOS runtime

-- Use whichever is appropriate
useRuntime runtime  -- Legacy code
useOS os            -- New code
```

**Pattern 3: Migration Wrapper**
```haskell
-- Wrap Runtime operations with OS event emission
wrappedCallTool ctx tool args = do
    -- Emit subcall events if in OS context
    case ctxEventQueue ctx of
        Just q -> emitSubcallStart q
        Nothing -> pure ()
    
    -- Execute tool
    result <- callTool tool args
    
    -- Emit completion
    case ctxEventQueue ctx of
        Just q -> emitSubcallComplete q result
        Nothing -> pure ()
    
    pure result
```

## Module Mapping

| Legacy Module | New OS Module | Purpose |
|---------------|---------------|---------|
| `System.Agents.Runtime` | `System.Agents.OS` | Main entry point |
| `System.Agents.Runtime.Runtime` | `System.Agents.OS.Agents` | Agent operations |
| `System.Agents.Session.Base` | `System.Agents.OS.Conversation` | Conversation tracking |
| `System.Agents.Session.Types` | `System.Agents.OS.Conversation.Types` | Conversation types |
| `System.Agents.AgentTree` | `System.Agents.OS.AgentTree` | Multi-agent hierarchy |
| (new) | `System.Agents.OS.Core` | ECS core types |
| (new) | `System.Agents.OS.Resources` | Resource management |
| (new) | `System.Agents.OS.Concurrent` | Concurrent access |
| (new) | `System.Agents.OS.Events` | OS event system |
| (new) | `System.Agents.OS.Persistence` | Persistence layer |

## Type Mapping

| Legacy Type | New Type | Notes |
|-------------|----------|-------|
| `Runtime` | `OS` | Contains World, not just config |
| `Session` | `Conversation` + `Turn` + `ToolCall` | Split into components |
| `AgentId` | `AgentId` | Now wraps `EntityId` |
| `SessionId` | `ConversationId` | Now wraps `EntityId` |
| (new) | `EntityId` | Base identifier type |
| (new) | `ToolboxId` | Phantom-typed EntityId |
| (new) | `TurnId` | Phantom-typed EntityId |
| (new) | `ToolCallId` | Phantom-typed EntityId |
| (new) | `World` | ECS world container |

## Step-by-Step Migration

### Step 1: Update Imports

```haskell
-- Before
import System.Agents.Runtime
import qualified System.Agents.Runtime.Runtime as Runtime

-- After
import System.Agents.OS
import qualified System.Agents.OS.Agents as OSAgents
```

### Step 2: Initialize OS Instead of Runtime

```haskell
-- Before
runtime <- newRuntime tracer config tools

-- After
os <- initializeOS defaultOSConfig

-- Create agent
agentId <- createAgent os agentConfig

-- Create toolboxes and bind to agent
toolboxId <- createToolbox os toolboxConfig
```

### Step 3: Update Agent Configuration

```haskell
-- Before
config = AgentConfig
    { slug = "my-agent"
    , toolDirectory = "./tools"
    , ...
    }

-- After
config = AgentConfig
    { agentName = "my-agent"
    , agentModel = ...
    , agentSystemPrompt = ...
    , agentToolboxBindings = [unToolboxId bashId, unToolboxId sqliteId]
    }
```

### Step 4: Use Resource Context

```haskell
-- Before
conn <- openConnection config
-- Cleanup handled manually

-- After
rid <- createResource ctx (SqliteResource config) $ \rid -> do
    conn <- openConnection config
    pure ResourceHandle
        { handleId = rid
        , handleCleanup = closeConnection conn
        , handleAccess = \f -> f (SqliteAccessor conn)
        }

-- Automatic cleanup on scope exit
cleanupScope registry scope
```

### Step 5: Update Subcall Handling

```haskell
-- Before
result <- turnAgentRuntimeIntoIOTool agent prompt

-- After
-- Ensure ToolExecutionContext has event queue
let ctx' = ctx { ctxEventQueue = Just eventQueue }
result <- turnAgentRuntimeIntoIOTool agent prompt
-- Events automatically emitted
```

## Testing Migration

### Unit Tests

```haskell
-- Test OS operations directly
spec :: Spec
spec = describe "OS Core" $ do
    it "creates agents" $ do
        os <- initializeOS testConfig
        agentId <- createAgent os testAgentConfig
        
        mConfig <- atomically $ 
            getComponent @AgentConfig (osWorld os) (unAgentId agentId)
        
        mConfig `shouldSatisfy` isJust
```

### Integration Tests

```haskell
-- Test full agent lifecycle
spec :: Spec
spec = describe "Agent Lifecycle" $ do
    it "runs conversation" $ do
        os <- initializeOS testConfig
        agentId <- createAgent os testAgentConfig
        
        -- Create conversation
        convId <- startConversation os agentId
        
        -- Add user message
        addMessage os convId UserRole "Hello!"
        
        -- Run agent turn
        result <- runAgentTurn os agentId convId
        
        -- Verify response
        messages <- getConversationMessages os convId
        length messages `shouldBe` 2
```

## Troubleshooting

### Issue: "Entity not found" errors

**Cause:** Trying to access entity that doesn't exist in World.

**Solution:** Check entity creation and component registration:
```haskell
-- Verify entity exists
exists <- atomically $ entityExists world eid
when (not exists) $ error "Entity not found"

-- Check component store registered
hasStore <- atomically $ hasComponentStore @MyComponent world
when (not hasStore) $ error "Component store not registered"
```

### Issue: STM deadlock

**Cause:** Nested STM transactions with external IO.

**Solution:** Use `atomically` only for pure STM, run IO separately:
```haskell
-- Bad: IO inside atomically
atomically $ do
    val <- getComponent world eid
    result <- liftIO $ expensiveOp val  -- DON'T DO THIS
    setComponent world eid result

-- Good: Separate IO
mVal <- atomically $ getComponent world eid
case mVal of
    Just val -> do
        result <- expensiveOp val
        atomically $ setComponent world eid result
    Nothing -> pure ()
```

### Issue: Resource leaks

**Cause:** Not cleaning up resources properly.

**Solution:** Use `bracket` or `ResourceT`:
```haskell
import Control.Exception (bracket)

-- Using bracket
bracket 
    (createResource ctx config mkHandle)
    (\rid -> cleanupResource registry rid)
    (\rid -> do
        withResource registry rid $ \accessor ->
            useResource accessor
    )

-- Or use cleanupScope for batch cleanup
cleanupScope registry (AgentScope agentId)
```

### Issue: Subcall not visible in TUI

**Cause:** Event queue not configured in ToolExecutionContext.

**Solution:** Ensure context includes event queue:
```haskell
-- Check if event queue exists
case ctxEventQueue ctx of
    Nothing -> putStrLn "Warning: No event queue, subcall won't be visible"
    Just q -> putStrLn "Event queue configured"

-- When creating context, include event queue
ctx = ToolExecutionContext
    { ...
    , ctxEventQueue = Just eventQueue  -- Required for visibility
    }
```

## Performance Considerations

### STM Contention

**Problem:** Many threads competing for same TVar.

**Solution:** Use granular component stores:
```haskell
-- Good: Separate TVars per component type
agentStore :: TVar (Map EntityId AgentConfig)
toolboxStore :: TVar (Map EntityId ToolboxConfig)

-- Bad: Single TVar for everything
allComponents :: TVar (Map EntityId (AgentConfig, ToolboxConfig, ...))
```

### Resource Pool Sizing

**Problem:** Too few connections in pool causes contention.

**Solution:** Size pools based on expected concurrency:
```haskell
-- HTTP pool: size = expected concurrent requests
httpRid <- createHttpPool ctx 20

-- SQLite with WAL: can have many readers
sqliteRid <- createSqliteResource ctx config True  -- WAL mode
```

### Snapshot Frequency

**Problem:** Too frequent snapshots impact performance.

**Solution:** Configure snapshot intervals:
```haskell
config = defaultOSConfig
    { osSnapshotInterval = Just (secondsToNominalDiffTime 60)
    , osMaxSnapshotsPerEntity = 10
    }
```

## Future Directions

### Query System

Planned: ECS-style queries for efficient entity filtering:
```haskell
-- Query all busy agents
busyAgents <- query world $ 
    select @AgentState 
    where_ (\s -> agentStatus s == AgentBusy)

-- Query conversations with recent activity
recentConvs <- query world $
    select @ConversationState
    where_ (\s -> conversationLastActivity s > cutoffTime)
```

### Distributed OS

Planned: Multi-process OS with shared state:
```haskell
-- Initialize distributed OS
os <- initializeDistributedOS 
    { osNodeId = "node-1"
    , osCluster = ["node-1", "node-2", "node-3"]
    , osConsensus = RaftConsensus
    }
```

### Persistence Evolution

Planned: Event sourcing for complete history:
```haskell
-- Replay from event log
events <- loadEvents backend
world <- foldM applyEvent emptyWorld events

-- Where applyEvent applies each event to world state
```

## References

- [OS API Reference](OS-API.md) - Complete API documentation
- [Architecture Overview](architecture.md) - System architecture
- [CHANGELOG-OS-MIGRATION.md](CHANGELOG-OS-MIGRATION.md) - Detailed change log

## Summary

The OS Model migration provides:

1. **Better concurrency** via STM-based ECS
2. **Resource safety** via scoped lifecycle management
3. **Visibility** via OS events for subcalls
4. **Flexibility** via pluggable persistence
5. **Future-proofing** via clean separation of concerns

For new code, use the OS Model directly. For legacy code, use the compatibility layer during migration.

