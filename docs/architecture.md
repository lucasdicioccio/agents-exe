# Architecture

This document describes the runtime architecture and core components of the Agents framework.

## Core Architecture

The framework is built around a layered architecture that separates concerns between agent definition, runtime execution, and user interfaces.

### Layer Overview

```
┌────────────────────────────────────────────────────────────────┐
│                    Interface Layer                              │
│  (CLI commands, TUI, MCP server, HTTP endpoints)                │
├────────────────────────────────────────────────────────────────┤
│                    OS Model Layer                               │
│  (Entity-Component-System, Resource Management,                 │
│   Conversation Tracking, Concurrent Access)                     │
├────────────────────────────────────────────────────────────────┤
│                    Agent Tree Layer                             │
│  (multi-agent hierarchy, reference validation, cycle detection) │
├────────────────────────────────────────────────────────────────┤
│                    Foundation Layer                             │
│  (sessions, tools, LLM integration, file loading)               │
└────────────────────────────────────────────────────────────────┘
```

## OS Model Architecture

The OS Model provides a centralized, ECS-based architecture for managing agents, toolboxes, and resources. It replaces the previous Runtime-per-agent model.

### Entity-Component-System (ECS) Pattern

```
┌─────────────────────────────────────────────────────────────────┐
│                         World                                    │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐             │
│  │  Component  │  │  Component  │  │  Component  │             │
│  │   Store 1   │  │   Store 2   │  │   Store N   │             │
│  │ (TVar Any)  │  │ (TVar Any)  │  │ (TVar Any)  │             │
│  └──────┬──────┘  └──────┬──────┘  └──────┬──────┘             │
│         │                │                │                     │
│         └────────────────┼────────────────┘                     │
│                          │                                      │
│                   HashMap ComponentTypeId                        │
└──────────────────────────┬──────────────────────────────────────┘
                           │
         ┌─────────────────┼─────────────────┐
         │                 │                 │
         ▼                 ▼                 ▼
    ┌─────────┐      ┌─────────┐      ┌─────────┐
    │ Entity  │      │ Entity  │      │ Entity  │
    │   1     │      │   2     │      │   N     │
    └────┬────┘      └────┬────┘      └────┬────┘
         │                │                │
    ┌────┴────┐      ┌────┴────┐      ┌────┴────┐
    │ Agent   │      │Toolbox  │      │ Conv    │
    │ Config  │      │ Config  │      │ Config  │
    │ Agent   │      │Toolbox  │      │ Conv    │
    │ State   │      │ State   │      │ State   │
    └─────────┘      └─────────┘      └─────────┘
```

**Key Design Principles:**

1. **Entities are just IDs**: Lightweight identifiers with phantom types for type safety
2. **Components are pure data**: Serializable, immutable data structures
3. **Systems are functions**: Operate on entities with specific component combinations
4. **Storage is heterogeneous**: Uses `TVar Any` for type erasure with safe casting

### Component Types

#### Agent Components

| Component | ID | Purpose |
|-----------|-----|---------|
| `AgentConfig` | 1 | Static agent configuration (name, model, system prompt) |
| `AgentState` | 2 | Runtime state (status, current conversation) |

#### Toolbox Components

| Component | ID | Purpose |
|-----------|-----|---------|
| `ToolboxConfig` | 3 | Toolbox type and settings |
| `ToolboxState` | 4 | Runtime state and resource reference |
| `ToolboxBinding` | 5 | Agent-to-toolbox relationship |

#### Conversation Components

| Component | ID | Purpose |
|-----------|-----|---------|
| `ConversationConfig` | 30 | Conversation metadata |
| `ConversationState` | 31 | Runtime status and timestamps |
| `AgentConversation` | 32 | Agent-conversation relationship |
| `TurnConfig` | 33 | Turn structure (parent, conversation) |
| `TurnState` | 34 | Turn execution state |
| `ToolCallConfig` | 35 | Tool call specification |
| `ToolCallState` | 36 | Tool call execution state |
| `Message` | 38 | Chat messages |

### Resource Lifecycle Flow

```
Program Startup
      │
      ▼
┌─────────────┐
│  Initialize │
│    World    │
└──────┬──────┘
       │
       ▼
┌─────────────┐     ┌─────────────┐
│   Create    │────>│  Register   │
│   Agents    │     │  Component  │
└──────┬──────┘     │   Stores    │
       │            └─────────────┘
       ▼
┌─────────────┐     ┌─────────────┐
│   Create    │────>│  Register   │
│  Toolboxes  │     │  Resources  │
└──────┬──────┘     └─────────────┘
       │
       ▼
┌─────────────┐
│    Bind     │
│   Agents    │
│  to Toolboxes│
└──────┬──────┘
       │
       ▼
    ┌────────┐
    │ RUNTIME │
    └────┬───┘
         │
    ┌────┴────┬──────────┬──────────┐
    │         │          │          │
    ▼         ▼          ▼          ▼
┌───────┐ ┌───────┐ ┌───────┐ ┌───────┐
│ Agent │ │ Agent │ │Shared │ │ Shared│
│   1   │ │   2   │ │SQLite │ │  HTTP │
│       │ │       │ │  DB   │ │ Pool  │
└───┬───┘ └───┬───┘ └───┬───┘ └───┬───┘
    │         │         │         │
    └─────────┴─────────┴─────────┘
              │
              ▼
       ┌─────────────┐
       │  Cleanup    │
       │   Scope     │
       │ (on destroy)│
       └─────────────┘
```

**Resource Scopes:**

1. **Program Scope**: Global resources (HTTP connection pools, shared caches)
2. **Agent Scope**: Per-agent resources (sandbox directories, agent-specific state)
3. **Toolbox Scope**: Per-toolbox resources (SQLite connections, MCP clients)
4. **Conversation Scope**: Per-conversation resources (isolated Lua states, temp files)
5. **Turn Scope**: Temporary resources (single turn execution context)
6. **ToolCall Scope**: Single-use resources (tool call arguments, results)

### Concurrent Access Patterns

```
┌──────────────────────────────────────────────────────────────┐
│                    Access Patterns                            │
├──────────────────────────────────────────────────────────────┤
│                                                               │
│  Exclusive Access (TMVar)                                     │
│  ┌─────────┐     ┌─────────┐     ┌─────────┐                 │
│  │  Lock   │────>│ Execute │────>│ Release │                 │
│  └─────────┘     └─────────┘     └─────────┘                 │
│  Use: Lua interpreters, process handles                      │
│                                                               │
├──────────────────────────────────────────────────────────────┤
│                                                               │
│  Read-Write Access (RWLock)                                   │
│  ┌─────┐ ┌─────┐       ┌─────────┐      ┌─────┐ ┌─────┐      │
│  │Read │ │Read │──────>│  Data   │<─────│Write│     │      │
│  │  1  │ │  2  │       │         │      │     │     │      │
│  └─────┘ └─────┘       └─────────┘      └─────┘     │      │
│  Use: SQLite databases (especially WAL mode)                 │
│                                                               │
├──────────────────────────────────────────────────────────────┤
│                                                               │
│  Pool Access (TBQueue)                                        │
│  ┌─────────────────────────────────────────────────────┐     │
│  │  Pool: [Token] [Token] [Token] ... [Token]          │     │
│  └─────────────────────────────────────────────────────┘     │
│       ▲    │         ▲    │                                  │
│       │    └─────────┘    │                                  │
│    Acquire             Release                               │
│  Use: HTTP connection pools, DB connection pools             │
│                                                               │
├──────────────────────────────────────────────────────────────┤
│                                                               │
│  Stateless Access (No Lock)                                   │
│  ┌─────────┐ ┌─────────┐ ┌─────────┐                        │
│  │ Access  │ │ Access  │ │ Access  │  (Concurrent, no sync) │
│  └─────────┘ └─────────┘ └─────────┘                        │
│  Use: Immutable data, thread-safe resources                  │
│                                                               │
└──────────────────────────────────────────────────────────────┘
```

### Conversation and Lineage Tracking

```
Conversation Tree Structure

┌─────────────────┐
│ Conversation 1  │
│   (Entity)      │
└────────┬────────┘
         │
    ┌────┴────┐
    ▼         ▼
┌───────┐ ┌───────┐
│ Turn 1│ │ Turn 2│
│(Entity)│ │(Fork) │
└───┬───┘ └───┬───┘
    │         │
    ▼         ▼
┌───────┐ ┌───────┐
│ Call 1│ │ Call 1│
│       │ │       │
└───┬───┘ └───┬───┘
    │         │
    ▼         ▼
┌───────┐ ┌───────┐
│ Call 2│ │ Call 2│
│(Nested)│ │(Nested)│
└───────┘ └───────┘

Lineage Stack
┌─────────────────────────────────────┐
│ LineageFrame                        │
│ ├─ frameType: ToolCallFrame         │
│ ├─ frameEntityId: <tool-call-id>    │
│ └─ frameTimestamp: <time>           │
├─────────────────────────────────────┤
│ LineageFrame                        │
│ ├─ frameType: TurnFrame             │
│ ├─ frameEntityId: <turn-id>         │
│ └─ frameTimestamp: <time>           │
├─────────────────────────────────────┤
│ LineageFrame                        │
│ ├─ frameType: ConversationFrame     │
│ ├─ frameEntityId: <conversation-id> │
│ └─ frameTimestamp: <time>           │
└─────────────────────────────────────┘
```

**Lineage provides:**
- Complete call chain for debugging
- Recursion depth tracking
- Audit trail for accounting
- Context for subagent calls

### Persistence Layer

```
┌──────────────────────────────────────────────────────────────┐
│                    Persistence Backends                       │
├──────────────────────────────────────────────────────────────┤
│                                                               │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐     │
│  │ In-Memory│  │   File   │  │  SQLite  │  │PostgreSQL│     │
│  │ (Dev/Test)│  │(Compat) │  │ (Local)  │  │(Production)    │
│  └────┬─────┘  └────┬─────┘  └────┬─────┘  └────┬─────┘     │
│       │             │             │             │            │
│       └─────────────┴──────┬──────┴─────────────┘            │
│                            │                                 │
│                            ▼                                 │
│                   ┌─────────────────┐                        │
│                   │  Unified API    │                        │
│                   │ (persist, load) │                        │
│                   └─────────────────┘                        │
│                                                               │
└──────────────────────────────────────────────────────────────┘

SQLite Schema (simplified)
┌──────────────────────────────────────────────────────────────┐
│  entities (id, entity_type, created_at)                      │
│  components (entity_id, component_type, data)                │
│  events (id, timestamp, type, data, entity_id)               │
│  messages (conversation_id, timestamp, role, content)        │
│  tool_calls (turn_id, timestamp, name, input, output)        │
└──────────────────────────────────────────────────────────────┘
```

## Migration Path from Old Runtime

### Architecture Comparison

```
OLD: Runtime-per-Agent                    NEW: Centralized OS
┌─────────────────────┐                  ┌─────────────────────┐
│ ┌─────────────────┐ │                  │                     │
│ │   Runtime A     │ │                  │ ┌─────────────────┐ │
│ │ ┌─────┐┌─────┐  │ │                  │ │      World      │ │
│ │ │Tools││HTTP │  │ │                  │ │  ┌───┐┌───┐┌───┐ │ │
│ │ │     ││Pool │  │ │                  │ │  │ C ││ C ││ C │ │ │
│ │ └─────┘└─────┘  │ │                  │ │  └───┘└───┘└───┘ │ │
│ └─────────────────┘ │                  │ └─────────────────┘ │
│ ┌─────────────────┐ │                  │                     │
│ │   Runtime B     │ │                  │ ┌─────────────────┐ │
│ │ ┌─────┐┌─────┐  │ │                  │ │ Resource Registry│ │
│ │ │Tools││HTTP │  │ │                  │ │  ┌───┐┌───┐┌───┐ │ │
│ │ │     ││Pool │  │ │                  │ │  │ R ││ R ││ R │ │ │
│ │ └─────┘└─────┘  │ │                  │ │  └───┘└───┘└───┘ │ │
│ └─────────────────┘ │                  │ └─────────────────┘ │
│ ┌─────────────────┐ │                  │                     │
│ │   Runtime C     │ │                  │ ┌─────────────────┐ │
│ │ ┌─────┐┌─────┐  │ │                  │ │  Persistence    │ │
│ │ │Tools││HTTP │  │ │                  │ │  ┌───┐┌───┐┌───┐ │ │
│ │ │     ││Pool │  │ │                  │ │  │ P ││ P ││ P │ │ │
│ │ └─────┘└─────┘  │ │                  │ │  └───┘└───┘└───┘ │ │
│ └─────────────────┘ │                  │ └─────────────────┘ │
└─────────────────────┘                  └─────────────────────┘
                                                         │
                              ┌──────────────────────────┘
                              │
         ┌────────────────────┼────────────────────┐
         │                    │                    │
         ▼                    ▼                    ▼
    ┌─────────┐          ┌─────────┐          ┌─────────┐
    │ Agent A │          │ Agent B │          │ Agent C │
    └─────────┘          └─────────┘          └─────────┘
         │                    │                    │
         └────────────────────┴────────────────────┘
                              │
                              ▼
                    ┌─────────────────┐
                    │  Shared HTTP    │
                    │  Connection Pool│
                    └─────────────────┘
```

### Benefits of the New Architecture

1. **Shared Resources**: Multiple agents can share toolboxes (e.g., same SQLite database)
2. **Resource Pooling**: HTTP connections pooled across all agents
3. **Better Lifecycle Management**: Explicit scopes with predictable cleanup
4. **Foundation for Web API**: Centralized state enables HTTP server interface
5. **Durable Persistence**: Built-in persistence layer with multiple backends
6. **Thorough Lineage Tracking**: Complete call chains for debugging

### Compatibility Layer

The `System.Agents.OS.Compat` module provides a bridge from the old Runtime interface to the new OS backend:

```haskell
-- Old code continues to work
runtime <- newRuntime config
result <- runWithRuntime runtime $ do
    tools <- listTools
    callTool "my-tool" args

-- New OS interface (when ready)
os <- initializeOS defaultConfig
result <- runOSM os $ do
    agent <- createAgent agentConfig
    conv <- startConversation agent convConfig
    turn <- startTurn conv
    executeToolCall turn "my-tool" args
```

## Core Types

### Base Types (`System.Agents.Base`)

```haskell
-- Unique identifiers
newtype AgentId = AgentId UUID
newtype ConversationId = ConversationId UUID
newtype StepId = StepId UUID

-- Agent configuration
data Agent = Agent
    { slug :: Text                    -- Unique identifier
    , apiKeyId :: Text                -- Reference to API key
    , flavor :: Text                  -- LLM provider (openai, etc.)
    , modelUrl :: Text                -- API endpoint
    , modelName :: Text               -- Model identifier
    , announce :: Text                -- User-facing description
    , systemPrompt :: [Text]          -- System instructions
    , toolDirectory :: FilePath       -- Path to tools
    , mcpServers :: Maybe [McpServerDescription]
    , extraAgents :: Maybe [ExtraAgentRef]
    , builtinToolboxes :: Maybe [BuiltinToolboxDescription]
    }
```

### OS Types (`System.Agents.OS.Core`)

```haskell
-- Phantom-typed entity IDs
newtype AgentId = AgentId EntityId
newtype ToolboxId = ToolboxId EntityId
newtype ConversationId = ConversationId EntityId

-- Agent components
data AgentConfig = AgentConfig
    { agentName :: Text
    , agentModel :: ModelConfig
    , agentSystemPrompt :: Text
    , agentToolboxBindings :: [ToolboxBindingSpec]
    }

data AgentState = AgentState
    { agentStatus :: AgentStatus
    , agentCurrentConversation :: Maybe ConversationId
    , agentCreatedAt :: UTCTime
    }
```

## Agent Tree System

The `AgentTree` module manages multi-agent hierarchies and handles agent discovery, reference validation, and cycle detection.

### Tree Structure

Agents form a directed graph where:
- **Parent-child edges**: Discovered from tool directory hierarchy
- **Extra reference edges**: Explicit references via `extraAgents`

```
┌─────────────┐
│  root-agent │
└──────┬──────┘
       │
   ┌───┴───┐
   ▼       ▼
┌──────┐ ┌──────┐
│tool-a│ │tool-b│
└──┬───┘ └──────┘
   │
   ▼
┌──────┐
│sub-1 │
└──────┘
```

### Subagent Wiring

The Agent Tree system supports dynamic tool registration via STM TVars:

```haskell
-- Runtime now uses STM TVar for mutable tool storage
type AgentTools = TVar [ToolRegistration]

-- Wiring process appends helper agent tools to parent
tireAgentTools :: Props -> AgentConfigGraph -> (AgentSlug, AgentConfigNode) -> IO ()
```

## Conversation Flow

```
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│   User      │────>│   Session   │────>│    LLM      │
│   Input     │     │   (turns)   │     │   (tools)   │
└─────────────┘     └──────┬──────┘     └──────┬──────┘
                           │                     │
                           │    ┌─────────────┐  │
                           └───>│ Tool Call   │<─┘
                                │ Execution   │
                                └──────┬──────┘
                                       │
                           ┌───────────┴───────────┐
                           ▼                       ▼
                    ┌─────────────┐        ┌─────────────┐
                    │  Bash Tool  │        │  MCP Tool   │
                    └─────────────┘        └─────────────┘
```

## Tool Registration

Tools are registered with the LLM via the `ToolRegistration` type:

```haskell
data ToolRegistration = ToolRegistration
    { toolName :: Text
    , toolDescription :: Text
    , toolParameters :: Value  -- JSON Schema
    , toolExecutor :: Value -> IO ToolResult
    }
```

### Tool Sources

1. **BashToolbox**: Executable scripts in the tool directory
2. **McpToolbox**: MCP servers providing dynamic tool lists
3. **OpenAPIToolbox**: REST API operations from OpenAPI specs
4. **IOTools**: Haskell functions embedded in the runtime
5. **SystemToolbox**: Builtin system information tools
6. **Subagent Tools**: Other agents exposed as callable tools

## Module Dependencies

```
Main
  ├── AgentTree
  │     ├── Base
  │     ├── FileLoader
  │     └── Runtime
  ├── CLI.*
  │     └── AgentTree
  ├── TUI
  │     ├── AgentTree
  │     └── Session
  ├── MCP.Server
  │     └── AgentTree
  └── ExportImport.*
  
OS Layer
  ├── OS.Core
  │     ├── OS.Core.Types
  │     └── OS.Core.World
  ├── OS.Resources
  │     ├── OS.Resources.Types
  │     ├── OS.Resources.Sqlite
  │     ├── OS.Resources.Lua
  │     └── OS.Resources.Http
  ├── OS.Concurrent
  │     ├── OS.Concurrent.Types
  │     └── OS.Concurrent.Locks
  ├── OS.Conversation
  │     ├── OS.Conversation.Types
  │     └── OS.Conversation.Lineage
  ├── OS.Persistence
  │     ├── OS.Persistence.Types
  │     ├── OS.Persistence.Sqlite
  │     └── OS.Persistence.File
  └── OS.Compat
        └── OS.Compat.Runtime
```

## Key Design Decisions

1. **STM for Concurrency**: Tool reloading and subagent wiring use STM for thread-safe updates
2. **ECS Pattern**: Enables flexible composition and powerful queries
3. **Explicit Resource Management**: Predictable cleanup with explicit scopes
4. **Phantom Types**: Type safety for entity IDs without runtime overhead
5. **Type Erasure**: `Any` for heterogeneous storage with safe casting via Component typeclass
6. **Tracer Pattern**: All side effects are traced for observability
7. **Two-Phase Initialization**: Runtime shells created first, then wired together to support cycles
8. **Migration Compatibility**: Full compatibility layer for gradual migration

## Tradeoffs

1. **ECS Complexity**: Adds indirection but enables powerful queries and flexible composition
2. **STM Overhead**: Slight performance cost for composability
3. **Migration Duration**: Dual-mode operation adds maintenance burden but ensures smooth transition
4. **Storage Overhead**: Component storage uses more memory than direct fields but enables dynamic extension
5. **Type Erasure**: Using `Any` requires careful casting but enables heterogeneous storage

