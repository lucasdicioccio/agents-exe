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
│                    Agent Tree Layer                             │
│  (multi-agent hierarchy, reference validation, cycle detection) │
├────────────────────────────────────────────────────────────────┤
│                    Runtime Layer                                │
│  (agent execution, tool registration, conversation loop)        │
├────────────────────────────────────────────────────────────────┤
│                    Foundation Layer                             │
│  (sessions, tools, LLM integration, file loading)               │
└────────────────────────────────────────────────────────────────┘
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
    , builtinToolboxes :: Maybe [BuiltinToolboxDescription]  -- SQLite, System toolboxes
    }
```

### Agent Reference Types

```haskell
-- Reference to an external agent
data ExtraAgentRef = ExtraAgentRef
    { extraAgentSlug :: AgentSlug     -- How to refer to this agent
    , extraAgentPath :: FilePath      -- Path to agent JSON file
    }

-- MCP server configuration
data McpServerDescription 
    = McpSimpleBinary McpSimpleBinaryConfiguration
    
data McpSimpleBinaryConfiguration = McpSimpleBinaryConfiguration
    { name :: Text
    , executable :: FilePath
    , args :: [Text]
    }

-- Builtin toolbox types
data BuiltinToolboxDescription
    = SqliteToolbox SqliteToolboxDescription
    | SystemToolbox SystemToolboxDescription
```

## Agent Tree System

The `AgentTree` module manages multi-agent hierarchies and handles agent discovery, reference validation, and cycle detection.

### Agent Config Graph

```haskell
data AgentConfigGraph = AgentConfigGraph
    { graphNodes :: Map AgentSlug AgentConfigNode
    , graphEdges :: Map AgentSlug [AgentSlug]
    }

data AgentConfigNode = AgentConfigNode
    { nodeFile :: FilePath
    , nodeConfig :: Agent
    , nodeChildren :: [AgentSlug]    -- From toolDirectory
    , nodeExtraRefs :: [AgentSlug]   -- From extraAgents
    }
```

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

### Validation

The tree system performs:

1. **Reference Validation**: Ensures all referenced agents exist
2. **Cycle Detection**: Prevents circular agent dependencies
3. **Duplicate Detection**: Identifies agents with the same slug

### Subagent Wiring (Dynamic Tool Registration)

The Agent Tree system supports dynamic tool registration via STM TVars. This enables:

- **Subagent tools**: Parent agents can call child agents as tools
- **Cross-agent references**: Agents can reference agents outside their tool directory
- **Runtime tool updates**: Tools can be added after runtime initialization

```haskell
-- Runtime now uses STM TVar for mutable tool storage
type AgentTools = TVar [ToolRegistration]

data Runtime = Runtime
    { agentTools :: AgentTools  -- Mutable tool registrations
    , ...
    }

-- Wiring process appends helper agent tools to parent
tireAgentTools :: Props -> AgentConfigGraph -> (AgentSlug, AgentConfigNode) -> IO ()
wireAgentTools props _graph _runtimes (slug, node) = do
    -- Look up this agent's runtime from registry
    mRt <- lookupRuntime props.runtimeRegistry slug
    case mRt of
        Nothing -> pure ()
        Just rt -> do
            -- Look up child and extra agent runtimes
            childRuntimes <- mapM (lookupRuntime props.runtimeRegistry) node.nodeChildren
            extraRuntimes <- mapM (lookupRuntime props.runtimeRegistry) node.nodeExtraRefs
            
            let allHelpers = Maybe.catMaybes (childRuntimes ++ extraRuntimes)
            let helperTools = [props.agentToTool helperRt ... | helperRt <- allHelpers]
            
            -- Atomically append helper tools to runtime's agentTools TVar
            atomically $ modifyTVar' rt.agentTools (\existing -> existing ++ helperTools)
```

This design allows:
- Circular agent references (A calls B, B calls A)
- Self-referential agents (agent calls itself)
- Late binding of agent tools (resolved after all runtimes are created)

## Runtime System

The `Runtime` module provides the execution environment for agents.

### Runtime Structure

```haskell
-- Type alias for mutable tool storage
type AgentTools = TVar [ToolRegistration]

data Runtime = Runtime
    { agentSlug :: AgentSlug
    , agentId :: AgentId
    , agentAnnounce :: AgentAnnounce
    , agentTracer :: Tracer IO Trace
    , agentAuthenticatedHttpClientRuntime :: HttpClient.Runtime
    , agentModel :: OpenAI.Model
    , agentTools :: AgentTools      -- STM TVar for dynamic updates
    , agentTriggerRefreshTools :: STM Bool
    }
```

The use of `TVar` for `agentTools` enables:
- **Dynamic tool registration**: Subagent tools added after initialization
- **Thread-safe updates**: STM ensures consistency across threads
- **Hot reloading**: Tools can be refreshed without restarting

### Runtime Lifecycle

```
1. Initialize
   └─> Load agent configuration
       └─> Discover child agents from toolDirectory
       └─> Load extra agent references
           └─> Build agent config graph
           └─> Validate references
           └─> Detect cycles

2. Create Runtime
   └─> Generate AgentId
   └─> Initialize HTTP client with API key
   └─> Initialize bash toolbox (background thread)
   └─> Initialize MCP toolboxes
   └─> Initialize builtin toolboxes (SQLite, System)
   └─> Create TVar for tools and populate with initial set
   └─> Combine all tool registrations

3. Wire Subagent Tools
   └─> For each agent, look up helper runtimes in registry
   └─> Create tool registrations for helpers
   └─> Atomically append to agent's TVar

4. Execute
   └─> Run conversation loop
       └─> Collect user input
       └─> Call LLM with tools
       └─> Execute tool calls
       └─> Stream responses
```

### Tracing

The runtime uses the `Prod.Tracer` library for observability:

```haskell
data Trace
    = AgentTrace_Loading AgentSlug AgentId BashToolbox.Trace
    | AgentTrace_Conversation AgentSlug AgentId ConversationId ConversationTrace
    | ConfigLoadedTrace AgentDescription
    | DataLoadingTrace FileLoader.Trace
    | ReferenceValidationTrace (Either [ReferenceError] ())
    | CyclicReferencesWarning [[AgentSlug]]
    | BuiltinToolboxTrace Text SqliteToolbox.Trace
    | BuiltinToolboxInitError Text String
    | SystemToolboxTrace Text SystemToolbox.Trace
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

### Session Types

```haskell
data Session = Session
    { sessionId :: SessionId
    , conversationId :: ConversationId
    , agentSlug :: AgentSlug
    , turns :: [Turn]
    }

data Turn = Turn
    { turnId :: TurnId
    , userMessage :: Message
    , assistantResponse :: Message
    , toolCalls :: [ToolCall]
    }
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

## HTTP Client

The framework uses a custom HTTP client with authentication:

```haskell
data Runtime = Runtime
    { manager :: Manager
    , baseUrl :: Text
    , auth :: Auth
    }

data Auth
    = NoToken
    | BearerToken Text
```

The client supports:
- Bearer token authentication
- Request/response logging
- JSON serialization via Aeson

## Recursion Control

The framework includes recursion depth limiting to prevent infinite loops:

```haskell
data ToolExecutionContext = ToolExecutionContext
    { ctxCallStack :: [CallStackEntry]
    , ctxMaxDepth :: Maybe Int
    , ...
    }

data CallStackEntry = CallStackEntry
    { callAgentSlug :: AgentSlug
    , callConversationId :: ConversationId
    , callDepth :: Int
    }
```

When an agent calls another agent as a tool, the depth increases. If `maxDepth` is exceeded, the call fails with `MaxRecursionDepthExceeded`.

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
```

## Key Design Decisions

1. **STM for Concurrency**: Tool reloading and subagent wiring use STM for thread-safe updates
2. **TVar for Dynamic Tools**: Runtime tools are stored in a TVar to support late-binding of subagent tools
3. **Tracer Pattern**: All side effects are traced for observability
4. **Background Thread**: File watching for hot-reloading of bash tools
5. **JSON Configuration**: Human-readable agent definitions
6. **Type Safety**: Heavy use of newtypes for IDs prevents mixing up identifiers
7. **Two-Phase Initialization**: Runtime shells created first, then wired together to support cycles

