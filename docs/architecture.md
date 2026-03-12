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

## Runtime System

The `Runtime` module provides the execution environment for agents.

### Runtime Structure

```haskell
data Runtime = Runtime
    { agentSlug :: AgentSlug
    , agentId :: AgentId
    , agentAnnounce :: AgentAnnounce
    , agentTracer :: Tracer IO Trace
    , agentAuthenticatedHttpClientRuntime :: HttpClient.Runtime
    , agentModel :: OpenAI.Model
    , agentTools :: IO [ToolRegistration]
    , agentTriggerRefreshTools :: STM Bool
    }
```

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
   └─> Combine all tool registrations

3. Execute
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

1. **STM for Concurrency**: Tool reloading uses STM for thread-safe updates
2. **Tracer Pattern**: All side effects are traced for observability
3. **Background Thread**: File watching for hot-reloading of bash tools
4. **JSON Configuration**: Human-readable agent definitions
5. **Type Safety**: Heavy use of newtypes for IDs prevents mixing up identifiers

