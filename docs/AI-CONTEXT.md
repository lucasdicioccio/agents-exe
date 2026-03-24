# AI Context - Working on the Agents Framework

> **READ THIS FIRST** when invoked on this codebase.

## Immediate First Steps

When you're invoked on this project, run these queries immediately:

```sql
-- 1. What is this project?
SELECT * FROM project_overview;

-- 2. What docs exist?
SELECT doc_path, title FROM docs_index ORDER BY doc_path;

-- 3. What's the module structure?
SELECT module_name, purpose, documented_in 
FROM code_index 
ORDER BY filepath;

-- 4. Check for recent changes
SELECT * FROM code_index 
WHERE module_name LIKE '%OS%' 
ORDER BY filepath;
```

## Project Snapshot

**Agents** is a Haskell framework for AI agent orchestration with:
- **NEW (2026-03)**: ECS-based OS Model for agent management
- Multi-agent hierarchies (agents can call other agents as tools)
- Tool system: bash scripts, MCP servers, OpenAPI specs
- Session persistence
- TUI + CLI + MCP server interfaces
- OpenAI-compatible LLM integration

### Architecture Overview

```
┌────────────────────────────────────────────────────────────────┐
│                    Interface Layer                              │
│  (CLI commands, TUI, MCP server, HTTP endpoints)                │
├────────────────────────────────────────────────────────────────┤
│                    OS Model Layer     [NEW]                     │
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

**Key architectural pattern**: The **OS Model** (Entity-Component-System) now manages agents, toolboxes, and resources, replacing the legacy Runtime-per-agent model.

## Critical Files

| File | Why It Matters |
|------|----------------|
| `src/System/Agents/OS.hs` | **NEW**: Main OS module, exports ECS-based agent management |
| `src/System/Agents/OS/Core.hs` | **NEW**: Core ECS types: `World`, `EntityId`, `Component` |
| `src/System/Agents/OS/Agents.hs` | **NEW**: OS-native agent creation and management |
| `src/System/Agents/OS/Compat/Runtime.hs` | **NEW**: Migration bridge from old Runtime to new OS |
| `src/System/Agents/Base.hs` | Core types: `Agent`, `AgentId`, `ConversationId`, `ExtraAgentRef` |
| `src/System/Agents/AgentTree.hs` | Multi-agent discovery, cycle detection, reference validation |
| `src/System/Agents/Runtime/Runtime.hs` | Legacy Runtime (deprecated, use OS model) |
| `app/Main.hs` | CLI entry point - all commands defined here |
| `agents.cabal` | Dependencies, build config |

## Documentation Map

```
docs/
├── README.md              # Start here for overview
├── architecture.md        # System design, data flow, module relationships
├── OS-API.md              # [NEW] OS Model API reference
├── MIGRATION-OS.md        # [NEW] Migration guide: Runtime -> OS
├── CHANGELOG-OS-MIGRATION.md # [NEW] Recent changes summary
├── tools.md               # Tool registration (bash/MCP/OpenAPI/IO)
├── mcp.md                 # Model Context Protocol integration
├── sessions.md            # Session persistence and lifecycle
├── tui.md                 # Terminal UI (Brick library)
├── cli-commands.md        # All CLI commands reference
├── export-import.md       # Tool sharing (tar.gz/git)
├── file-loader.md         # JSON loading, section extraction
├── AI-CONTEXT.md          # This file
└── ai-assistant-guidelines.md # Detailed reference
```

## The OS Model (Entity-Component-System)

The **OS Model** is the new architectural foundation (Phase 2+):

### Core Concepts

```haskell
-- Entity: Just a unique ID
newtype EntityId = EntityId UUID
newtype AgentId = AgentId EntityId
newtype ToolboxId = ToolboxId EntityId

-- Component: Pure data attached to entities
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

-- World: Container for all component stores
newtype World = World 
    { componentStores :: HashMap ComponentTypeId (TVar Any) 
    }
```

### Basic Usage

```haskell
import System.Agents.OS

-- Initialize world with component stores
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
result <- createAgent world config
```

### Migration Phases

```
PhaseOldOnly (REMOVED) ──> PhaseDual ──> PhaseNewOnly
    (Legacy only)          (Both)        (OS only)
```

**Current**: PhaseDual is the default, with PhaseNewOnly available.

### Resource Scopes

Resources have explicit lifetimes:

1. **Program Scope**: Global resources (HTTP pools)
2. **Agent Scope**: Per-agent resources
3. **Toolbox Scope**: Per-toolbox resources (SQLite)
4. **Conversation Scope**: Per-conversation resources
5. **Turn Scope**: Temporary turn resources
6. **ToolCall Scope**: Single-use resources

### Concurrent Access Patterns

| Pattern | Use Case | Implementation |
|---------|----------|----------------|
| `ExclusiveAccess` | Lua interpreters | TMVar |
| `ReadWriteAccess` | SQLite (WAL mode) | RWLock |
| `PoolAccess` | HTTP connections | TBQueue |
| `StatelessAccess` | Immutable data | None |

## Knowledge Base Schema

The SQLite knowledge base (`agents_kb.sqlite`) tracks state:

```sql
-- Key tables:
--   project_overview  - project metadata
--   code_index        - 108 Haskell modules with docs linkage
--   docs_index        - 15+ documentation files
--   commits_seen      - git commits already processed

-- Common queries:
-- Find OS modules:
SELECT * FROM code_index WHERE filepath LIKE '%OS%';

-- Find where a module is documented:
SELECT documented_in FROM code_index 
WHERE module_name = 'System.Agents.OS.Core';

-- Check documentation coverage:
SELECT 
    COUNT(*) as total_modules,
    SUM(CASE WHEN documented_in IS NOT NULL THEN 1 ELSE 0 END) as documented
FROM code_index;
```

## Module-to-Doc Mapping

| If working on... | Update... |
|------------------|-----------|
| `System.Agents.OS.*` | `docs/OS-API.md` |
| `System.Agents.OS.Compat*` | `docs/MIGRATION-OS.md` |
| `System.Agents.Base` | `docs/architecture.md`, `docs/README.md` |
| `System.Agents.Runtime*` | `docs/architecture.md` (legacy) |
| `System.Agents.AgentTree*` | `docs/architecture.md` |
| `System.Agents.Tools.*` | `docs/tools.md` |
| `System.Agents.MCP.*` | `docs/mcp.md` |
| `System.Agents.Session*` | `docs/sessions.md` |
| `System.Agents.TUI.*` | `docs/tui.md` |
| `System.Agents.CLI.*` | `docs/cli-commands.md` |
| `app/Main.hs` | `docs/cli-commands.md` |
| `System.Agents.ExportImport*` | `docs/export-import.md` |
| `System.Agents.FileLoader*` | `docs/file-loader.md` |

## Code Patterns to Recognize

### Pattern: ECS Component Definition

```haskell
-- Define component type
newtype ComponentTypeId = ComponentTypeId { unComponentTypeId :: Int }

class Component a where
    componentId :: Proxy a -> ComponentTypeId

-- Create a component
instance Component MyComponent where
    componentId _ = ComponentTypeId 100  -- Unique ID

-- Use with world
atomically $ do
    setComponent world eid myComponent
    mComp <- getComponent @MyComponent world eid
```

### Pattern: OS Agent Creation

```haskell
-- OS model approach
import System.Agents.OS

result <- createAgent world $ AgentConfig
    { agentName = "my-agent"
    , agentModel = ModelConfig "openai" "url" "gpt-4" "key"
    , agentSystemPrompt = "You are helpful"
    , agentToolboxBindings = []
    }

case result of
    Left err -> handleError err
    Right agentId -> do
        -- Agent created successfully
```

### Pattern: Resource Management

```haskell
import System.Agents.OS.Resources

-- Create registry
registry <- atomically newResourceRegistry
let ctx = ResourceContext [ProgramScope] registry

-- Create resource
rid <- createResource ctx (SqliteResource config) $ \rid -> do
    conn <- openConnection config
    pure ResourceHandle
        { handleId = rid
        , handleCleanup = closeConnection conn
        , handleAccess = \f -> f (SqliteAccessor conn)
        }

-- Cleanup
cleanupScope registry (AgentScope agentId)
```

### Pattern: Migration Compatibility

```haskell
-- During migration, use compatibility layer
import System.Agents.OS.Compat.Runtime

result <- initializeWithMigration defaultMigrationConfig
case result of
    Right os -> do
        -- Use new OS with compatibility bridge
        bridge <- newRuntimeBridge agentId os
        runWithBridge bridge $ do
            tools <- listTools
            callTool "my-tool" args
    _ -> error "Migration not configured"
```

### Pattern: Tracer Usage

```haskell
-- All operations are traced
import Prod.Tracer

data MyTrace = Started | Completed | Failed String

myFunction :: Tracer IO MyTrace -> IO ()
myFunction tracer = do
    runTracer tracer Started
    result <- try operation
    case result of
        Right _ -> runTracer tracer Completed
        Left e  -> runTracer tracer (Failed $ show e)
```

### Pattern: Concurrent Access with STM

```haskell
-- SQLite with concurrent reads (WAL mode)
result <- runResourceM ctx $ withRead rid $ do
    queryDatabase sql

-- Lua with exclusive access
result <- runResourceM ctx $ withExclusive luaRid $ do
    runLuaScript script

-- HTTP with connection pool
result <- runResourceM ctx $ withPooled httpRid $ do
    makeHttpRequest req
```

## Key Types Reference

### OS Model Types

```haskell
-- ECS Core
newtype EntityId = EntityId UUID
newtype AgentId = AgentId EntityId
newtype ToolboxId = ToolboxId EntityId
newtype ConversationId = ConversationId EntityId

-- Agent Components
data AgentConfig = AgentConfig
    { agentName :: Text
    , agentModel :: ModelConfig
    , agentSystemPrompt :: Text
    , agentToolboxBindings :: [Text]
    }

data AgentState = AgentState
    { agentStatus :: AgentStatus
    , agentCurrentConversation :: Maybe ConversationId
    , agentCreatedAt :: UTCTime
    }

-- Resource Management
data ResourceScope
    = ScopeGlobal
    | ScopeAgent AgentId
    | ScopeConversation ConversationId

data AccessPattern
    = ExclusiveAccess      -- TMVar
    | ReadWriteAccess      -- RWLock
    | PoolAccess Int       -- TBQueue
    | StatelessAccess      -- No lock
```

### Legacy Types (Still Supported)

```haskell
-- Original Runtime types
data Agent = Agent
    { slug :: Text
    , apiKeyId :: Text
    , flavor :: Text
    , modelUrl :: Text
    , modelName :: Text
    , systemPrompt :: [Text]
    , toolDirectory :: FilePath
    , mcpServers :: Maybe [McpServerDescription]
    , extraAgents :: Maybe [ExtraAgentRef]
    }

data ExtraAgentRef = ExtraAgentRef
    { extraAgentSlug :: Text
    , extraAgentPath :: FilePath
    }
```

## Git Workflow

When documentation work spans multiple invocations:

1. **First invocation**: Plan, gather info, create initial docs
2. **Intermediate**: Continue where left off, check `commits_seen`
3. **Final**: Update all indices, ensure consistency

Always include a summary line as the **first line of response**.

## Testing Documentation

Verify docs are correct by checking:

1. Type definitions match source code
2. File paths in examples exist
3. JSON examples are valid
4. Module names are correct (use `bash_grep-files` to verify)

### Running Tests

```console
# All tests
cabal test

# OS-specific tests
cabal test --test-option=--pattern="OS"

# Benchmarks
cabal bench
```

## Red Flags

Watch for these issues:

| Issue | Action |
|-------|--------|
| Undocumented OS modules | Add to `code_index`, update `docs/OS-API.md` |
| Docs referencing wrong modules | Update `docs_index.related_modules` |
| Missing migration guidance | Update `docs/MIGRATION-OS.md` |
| Broken code examples | Fix or mark as "illustrative" |
| New CLI commands not in `cli-commands.md` | Add with examples |
| Commits not tracked | Process and insert to `commits_seen` |

## Quick Queries

```sql
-- Find all OS-related modules:
SELECT module_name, filepath 
FROM code_index 
WHERE module_name LIKE '%OS%' OR filepath LIKE '%OS%'
ORDER BY filepath;

-- Find all documentation:
SELECT doc_path, title FROM docs_index ORDER BY doc_path;

-- Check documentation coverage:
SELECT 
    COUNT(*) as total,
    SUM(CASE WHEN documented_in IS NOT NULL THEN 1 ELSE 0 END) as documented,
    ROUND(100.0 * SUM(CASE WHEN documented_in IS NOT NULL THEN 1 ELSE 0 END) / COUNT(*), 1) as pct
FROM code_index;

-- Find modules by documentation file:
SELECT module_name, filepath 
FROM code_index 
WHERE documented_in = 'docs/OS-API.md'
ORDER BY module_name;
```

## Working with Cabal

The project uses `agents.cabal`. Key stanzas:

```cabal
-- Library exports modules under System.Agents
library
    exposed-modules:
        System.Agents
        System.Agents.OS           -- NEW
        System.Agents.OS.Core      -- NEW
        System.Agents.OS.Agents    -- NEW
        System.Agents.Base
        System.Agents.Runtime
        ...

-- Executable is app/Main.hs
executable agents-exe
    main-is: Main.hs
    hs-source-dirs: app
    build-depends: agents, ...

-- Test suite
test-suite agents-tests
    other-modules:
        OS.CoreTests
        OS.IntegrationTests
        OS.CompatibilityTests
        ...

-- Benchmarks
benchmark os-benchmarks
    main-is: OSBenchmarks.hs
    hs-source-dirs: bench
```

## Common Import Patterns

```haskell
-- OS Model (New)
import System.Agents.OS
import System.Agents.OS.Core
import System.Agents.OS.Agents
import System.Agents.OS.Compat.Runtime

-- Core types
import System.Agents.Base

-- Legacy Runtime (being deprecated)
import System.Agents.Runtime
import qualified System.Agents.Runtime.Runtime as Runtime

-- Agent tree
import qualified System.Agents.AgentTree as AgentTree
import qualified System.Agents.OS.AgentTree as OSAgentTree

-- Tools
import qualified System.Agents.Tools.Bash as Bash
import qualified System.Agents.Tools.BashToolbox as BashToolbox

-- Sessions
import qualified System.Agents.Session.Base as Session

-- Tracing
import Prod.Tracer

-- STM for concurrency
import Control.Concurrent.STM
```

## Summary Checklist

Before finishing work:

- [ ] Summary line included (first line of response)
- [ ] Knowledge base tables updated if needed
- [ ] New OS modules added to `code_index`
- [ ] New documentation files added to `docs_index`
- [ ] Documentation created/updated for changes
- [ ] Cross-references checked
- [ ] Code examples verified

## Emergency Contacts

If completely lost:

1. Read `docs/README.md` for overview
2. Check `docs/CHANGELOG-OS-MIGRATION.md` for recent changes
3. Read `docs/MIGRATION-OS.md` for migration guidance
4. Look at `src/System/Agents/OS.hs` for OS entry point
5. Check `app/Main.hs` for current command structure
6. Query `code_index` for module purposes

Remember: This is a well-structured Haskell project in transition from Runtime-per-agent to ECS-based OS model. Follow the types, follow the imports, and you'll find your way.

## Recent Changes (March 2026)

See `docs/CHANGELOG-OS-MIGRATION.md` for detailed change log.

Key highlights:
- **27 new OS modules** added for ECS-based architecture
- **PhaseOldOnly removed** - now only PhaseDual and PhaseNewOnly
- **Full compatibility layer** for gradual migration
- **New documentation**: OS-API.md, MIGRATION-OS.md
- **New tests**: Integration, Compatibility, benchmarks

