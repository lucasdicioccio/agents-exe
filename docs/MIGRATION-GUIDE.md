# OS Model Migration Guide

This guide helps you migrate from the old Runtime-per-agent architecture to the new OS (Operating System) model.

## Overview

The OS model replaces the Runtime-per-agent architecture with an Entity-Component-System (ECS) pattern that provides:

- Better resource sharing between agents
- Unified conversation and turn tracking
- Improved audit trails and lineage
- More flexible toolbox management

## Migration Phases

The migration is designed to happen in three phases:

### Phase 1: Old-Only (Current)

In this phase, you continue using the existing `Runtime` API without changes.

```haskell
import System.Agents.Runtime.Runtime

-- Your existing code continues to work
result <- newRuntime slug announce tracer apiKey model ...
```

Deprecation warnings will guide you toward the new API.

### Phase 2: Dual Mode (Recommended for Migration)

Enable both the old Runtime and new OS systems. This allows gradual migration:

```haskell
import System.Agents.OS.Compat

let config = defaultMigrationConfig
        { migrationEnableOS = True
        , migrationEnableCompat = True
        }

result <- initializeWithMigration config

case result of
    Left runtime -> do
        -- Continue with existing Runtime code
        useRuntime runtime
    Right os -> do
        -- Start using the new OS API
        useOS os
```

### Phase 3: New-Only

After full migration, disable the old Runtime entirely:

```haskell
let config = defaultMigrationConfig
        { migrationEnableOS = True
        , migrationEnableCompat = False
        }

Right os <- initializeWithMigration config
-- All code uses OS directly
```

## API Mappings

### Old Runtime API to New OS API

| Old API | New OS API | Notes |
|---------|------------|-------|
| `Runtime` | `OS` + `AgentConfig` | OS manages multiple agents |
| `newRuntime` | `initializeOS` + `createAgent` | Separate OS and agent creation |
| `agentTools` | Toolbox bindings | OS uses toolbox bindings per agent |
| `Runtime -> IO [ToolRegistration]` | `getAgentTools :: AgentId -> OSM [ToolRegistration]` | Use AgentId to query tools |
| `AgentTools` (TVar) | Component store | ECS pattern replaces TVars |
| `agentTracer` | Tracer component | Still supported via OS |

### Example: Creating an Agent

**Old Way:**

```haskell
import System.Agents.Runtime.Runtime

runtime <- newRuntime
    slug
    announce
    tracer
    apiKey
    model
    bashSources
    mcpToolboxes
    openApiToolRegs
    builtinDescriptions
    skillSources
    baseIORegistrations
```

**New Way:**

```haskell
import System.Agents.OS.Core
import System.Agents.OS

-- Create OS instance
os <- initializeOS config

-- Create agent configuration
let agentConfig = AgentConfig
        { agentName = "my-agent"
        , agentModel = modelConfig
        , agentSystemPrompt = "You are a helpful assistant"
        , agentToolboxBindings = toolboxBindings
        }

-- Create the agent
agentId <- runOSM os $ createAgent agentConfig
```

### Example: Tool Execution

**Old Way:**

```haskell
import System.Agents.Runtime.Runtime

tools <- atomically $ readTVar (agentTools runtime)
result <- callTool tools toolName input
```

**New Way:**

```haskell
import System.Agents.OS.Core

-- Using RuntimeBridge for compatibility
result <- runWithBridge bridge $ do
    callTool toolName input

-- Or using OS directly for better performance
result <- runOSM os $ do
    -- Find conversation and turn
    convId <- getOrCreateConversation agentId
    turnId <- startTurn convId
    -- Execute tool
    executeToolCall turnId toolName input
```

## Deprecation Timeline

1. **Version 0.2.0**: Deprecation warnings added to Runtime API
2. **Version 0.3.0**: OS model becomes default, Runtime still supported
3. **Version 0.4.0**: Runtime removed, only OS model available

## Escape Hatches

For performance-critical code paths, you can bypass the compatibility layer:

```haskell
import System.Agents.OS.Compat.Runtime

-- Get underlying OS from bridge
let os = bridgeOS bridge

-- Use OS directly (no overhead)
result <- runOSM os $ do
    -- Your OS operations here
    getAgentTools agentId
```

## Testing During Migration

Use the `MigrationPhase` to control which runtime is active in tests:

```haskell
import System.Agents.OS.Compat.Runtime

-- Test with both runtimes
forM_ [PhaseOldOnly, PhaseDual, PhaseNewOnly] $ \phase -> do
    let config = defaultMigrationConfig
            { migrationPhase = phase
            }
    -- Run your tests
    testWithConfig config
```

## Troubleshooting

### Performance Concerns

The compatibility layer adds minimal overhead (~1-2% in benchmarks). If you need maximum performance:

1. Use `runOSM` with the underlying `OS` directly
2. Avoid frequent conversions between old and new types
3. Cache `RuntimeBridge` references rather than recreating them

### Type Conversion Issues

Some types have changed between Runtime and OS models:

- `AgentId` is now a phantom type wrapper around `EntityId`
- `ConversationId` is also a phantom type
- Tool calls use the `ToolCallId` phantom type

Use the conversion functions provided in `System.Agents.OS.Compat.Runtime`:

```haskell
callResultToUserToolResponse :: CallResult -> UserToolResponse
```

### Missing Features

The OS model is still evolving. Some Runtime features may not have OS equivalents yet:

- **Skill management**: OS model handles skills differently via toolbox bindings
- **Tool refresh**: OS uses component updates rather than TVar writes
- **MCP server lifecycle**: OS has explicit toolbox lifecycle management

## Further Reading

- `System.Agents.OS.Core` - Core ECS types
- `System.Agents.OS.Conversation` - Conversation and turn tracking
- `System.Agents.OS.Resources` - Resource management
- `docs/architecture.md` - Full architecture documentation

## Getting Help

If you encounter issues during migration:

1. Check the deprecation warnings for guidance
2. Review this migration guide
3. Examine the compatibility layer source in `System.Agents.OS.Compat.Runtime`
4. Open an issue with the migration tag

