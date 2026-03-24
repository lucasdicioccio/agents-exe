# Changelog: OS Model Migration

**Date:** March 2026  
**Commits:** `0eb5263` to `cb52af7`

## Summary

This release introduces a major architectural refactoring of the agents-exe core, migrating from a Runtime-per-agent model to a centralized **Entity-Component-System (ECS) based OS model**.

## Major Changes

### New Architecture: ECS-Based OS Model

The framework now uses an Entity-Component-System (ECS) pattern at its core:

- **Entities**: Lightweight UUID-based identifiers (AgentId, ToolboxId, ConversationId, etc.)
- **Components**: Pure, serializable data attached to entities (AgentConfig, AgentState, ToolboxConfig)
- **Systems**: Functions that operate on entities with specific component combinations

### New Modules (27 OS Modules Added)

| Module | Purpose |
|--------|---------|
| `System.Agents.OS` | Main OS module, exports all OS functionality |
| `System.Agents.OS.Core` | Core ECS types and World operations |
| `System.Agents.OS.Core.Types` | Component typeclass and entity ID types |
| `System.Agents.OS.Core.World` | World storage with STM-based component stores |
| `System.Agents.OS.Agents` | OS-native agent creation and management |
| `System.Agents.OS.AgentTree` | OS-native agent tree initialization |
| `System.Agents.OS.Concurrent` | Concurrent access patterns |
| `System.Agents.OS.Concurrent.Types` | Access pattern types (Exclusive, Read-Write, Pool) |
| `System.Agents.OS.Concurrent.Locks` | STM-based locking primitives |
| `System.Agents.OS.Resources` | Resource lifecycle management |
| `System.Agents.OS.Resources.Types` | Resource scope definitions |
| `System.Agents.OS.Resources.Sqlite` | SQLite resource management |
| `System.Agents.OS.Resources.Lua` | Lua interpreter resources |
| `System.Agents.OS.Resources.Http` | HTTP connection pool resources |
| `System.Agents.OS.Conversation` | Conversation and turn management |
| `System.Agents.OS.Conversation.Types` | Turn, Message, Conversation components |
| `System.Agents.OS.Conversation.Lineage` | Call chain tracking |
| `System.Agents.OS.Persistence` | Component persistence layer |
| `System.Agents.OS.Persistence.Types` | Backend abstractions |
| `System.Agents.OS.Persistence.Sqlite` | SQLite persistence backend |
| `System.Agents.OS.Persistence.File` | File-based persistence |
| `System.Agents.OS.Persistence.Schema` | Database schema |
| `System.Agents.OS.Compat` | Compatibility layer exports |
| `System.Agents.OS.Compat.Runtime` | Runtime-to-OS bridge |
| `System.Agents.OS.Interfaces` | Interface layer |
| `System.Agents.OS.Interfaces.TUI` | TUI adaptation for OS |
| `System.Agents.OS.Interfaces.OneShot` | OneShot adaptation for OS |

### Migration Phases

The migration follows a phased approach:

```
PhaseOldOnly (REMOVED) ──> PhaseDual ──> PhaseNewOnly
    (Legacy only)          (Both)        (OS only)
```

**March 2026 Update:** PhaseOldOnly has been removed. The system now operates in:
- **PhaseDual** (default): Both Runtime and OS available
- **PhaseNewOnly**: OS only, Runtime deprecated

### Key New Capabilities

1. **Shared Toolboxes**: Multiple agents can share the same SQLite database, HTTP connection pool, or other resources
2. **Better Resource Management**: Explicit lifecycle scopes (Program, Agent, Toolbox, Conversation, Turn, ToolCall)
3. **Concurrent Access**: STM-based synchronization with multiple patterns:
   - `ExclusiveAccess`: Single accessor (TMVar) - for Lua interpreters
   - `ReadWriteAccess`: Multiple readers/single writer (RWLock) - for SQLite
   - `PoolAccess`: Bounded pool (TBQueue) - for HTTP connections
   - `StatelessAccess`: No synchronization needed
4. **Durable Persistence**: Pluggable backends (SQLite, PostgreSQL, file-based)
5. **Complete Lineage Tracking**: Full call chains for debugging and accounting
6. **Foundation for Web API**: Centralized state enables HTTP server interface

### Component Type IDs

| ID | Component |
|----|-----------|
| 1 | AgentConfig |
| 2 | AgentState |
| 3 | ToolboxConfig |
| 4 | ToolboxState |
| 5 | ToolboxBinding |
| 30 | ConversationConfig |
| 31 | ConversationState |
| 32 | AgentConversation |
| 33 | TurnConfig |
| 34 | TurnState |
| 35 | ToolCallConfig |
| 36 | ToolCallState |
| 38 | Message |

### New CLI Modules

- `System.Agents.CLI.Export` - Export tools and agents
- `System.Agents.CLI.Import` - Import tools and agents

### New Documentation

| Document | Purpose |
|----------|---------|
| `docs/architecture.md` | Updated with OS model architecture |
| `docs/OS-API.md` | Complete API reference for OS model |
| `docs/MIGRATION-OS.md` | Migration guide from Runtime to OS |
| `docs/MIGRATION-GUIDE.md` | General migration guidance |
| `docs/advanced-configuration.md` | Advanced configuration options |

### New Tests

| Test Module | Purpose |
|-------------|---------|
| `test/OS/IntegrationTests.hs` | End-to-end OS scenarios |
| `test/OS/CompatibilityTests.hs` | Runtime/OS compatibility tests |
| `test/OS/CoreTests.hs` | ECS core functionality |
| `test/OS/ConcurrentTests.hs` | Concurrent access patterns |
| `test/OS/ResourcesTests.hs` | Resource management |
| `test/OS/ConversationTests.hs` | Conversation/lineage |
| `test/OS/PersistenceTests.hs` | Persistence layer |
| `test/OS/CompatTests.hs` | Compatibility layer |
| `test/OS/InterfaceTests.hs` | Interface layer |

### New Benchmarks

| Benchmark | Purpose |
|-----------|---------|
| `bench/OSBenchmarks.hs` | OS model performance benchmarks |

## API Changes

### Old (Still Supported via Compatibility Layer)

```haskell
-- Legacy Runtime approach
runtime <- newRuntime props agent tracer
result <- runWithRuntime runtime $ do
    tools <- listTools
    callTool "my-tool" args
```

### New (OS Model)

```haskell
-- OS model approach
import System.Agents.OS

-- Initialize world
world <- atomically $ do
    w <- newWorld
    w' <- registerComponentStore w (Proxy @AgentConfig)
    registerComponentStore w' (Proxy @AgentState)

-- Create agent
let config = AgentConfig
    { agentName = "my-agent"
    , agentModel = ModelConfig "openai" "url" "gpt-4" "key"
    , agentSystemPrompt = "You are helpful"
    , agentToolboxBindings = []
    }
agentId <- createAgent world config
```

## Migration Path

1. **Current (PhaseDual)**: Use `System.Agents.OS.Compat.Runtime` for gradual migration
2. **Future (PhaseNewOnly)**: Direct OS model usage

See `docs/MIGRATION-OS.md` for detailed migration instructions.

## Build Changes

### New Dependencies

- `deepseq` - For benchmark strictness
- `criterion` - Benchmarking framework
- `async` - Concurrent test execution
- `mtl` - Monad transformers

### Cabal Updates

- Added 27 new OS modules to library
- Added new test modules
- Added benchmark section

## Benefits

1. **Shared Resources**: Multiple agents sharing toolboxes
2. **Resource Pooling**: HTTP connections pooled across agents
3. **Better Lifecycle**: Explicit cleanup with scopes
4. **Web API Ready**: Centralized state for HTTP interface
5. **Durable Persistence**: Multiple backend options
6. **Lineage Tracking**: Complete call chains

## Tradeoffs

1. **ECS Complexity**: Adds indirection but enables flexible composition
2. **STM Overhead**: Slight performance cost for composability
3. **Dual Mode**: Maintenance burden during transition
4. **Storage Overhead**: More memory than direct fields

## Backward Compatibility

- Full compatibility layer provided
- Old Runtime interface still works
- Gradual migration supported
- Breaking changes documented in migration guide

## Related Issues

- #348 - Core Entity and Component Types
- #349 - Resource Management
- #350 - Concurrent Access
- #351 - Conversation and Lineage Tracking
- #352 - OS Monad
- #353 - Runtime Compatibility
- #354 - TUI and OneShot Adaptation
- #355 - Persistence Layer
- #356 - Documentation and Integration (this release)

## Contributors

- Lucas DiCioccio

---

For questions about the migration, see `docs/MIGRATION-OS.md` or file an issue with the "migration" label.

