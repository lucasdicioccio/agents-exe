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

-- 4. Any new commits since last documentation?
SELECT * FROM commits_seen ORDER BY commit_date DESC LIMIT 5;
```

## Project Snapshot

**Agents** is a Haskell framework for AI agent orchestration with:
- Multi-agent hierarchies (agents can call other agents as tools)
- Tool system: bash scripts, MCP servers, OpenAPI specs
- Session persistence
- TUI + CLI + MCP server interfaces
- OpenAI-compatible LLM integration

Key architectural pattern: **AgentTree** manages agent hierarchies, **Runtime** executes conversations.

## Critical Files

| File | Why It Matters |
|------|----------------|
| `src/System/Agents/Base.hs` | Core types: `Agent`, `AgentId`, `ConversationId`, `ExtraAgentRef` |
| `src/System/Agents/AgentTree.hs` | Multi-agent discovery, cycle detection, reference validation |
| `src/System/Agents/Runtime/Runtime.hs` | Agent execution, tool registration |
| `app/Main.hs` | CLI entry point - all commands defined here |
| `agents.cabal` | Dependencies, build config |

## Documentation Map

```
docs/
├── README.md              # Start here for overview
├── architecture.md        # System design, data flow, module relationships
├── tools.md               # Tool registration (bash/MCP/OpenAPI/IO)
├── mcp.md                 # Model Context Protocol integration
├── sessions.md            # Session persistence and lifecycle
├── tui.md                 # Terminal UI (Brick library)
├── cli-commands.md        # All CLI commands reference
├── export-import.md       # Tool sharing (tar.gz/git)
├── file-loader.md         # JSON loading, section extraction
└── AI-CONTEXT.md          # This file
```

## Knowledge Base Schema

The SQLite knowledge base (`agents_kb.sqlite`) tracks state:

```sql
-- Key tables:
--   project_overview  - project metadata
--   code_index        - 79 Haskell modules with docs linkage
--   docs_index        - 10 documentation files
--   commits_seen      - git commits already processed

-- Common queries:
-- Find where a module is documented:
SELECT documented_in FROM code_index WHERE module_name = 'System.Agents.Tools.Bash';

-- Find undocumented modules:
SELECT filepath FROM code_index WHERE documented_in IS NULL;

-- Check if a commit was processed:
SELECT 1 FROM commits_seen WHERE commit_hash = 'abc123';
```

## Common Tasks

### Task: Document a New Module

1. Read the module source
2. Determine which doc it belongs to (see table below)
3. Update the doc file
4. Update `code_index`:
   ```sql
   UPDATE code_index 
   SET documented_in = 'docs/tools.md',
       last_updated = datetime('now')
   WHERE module_name = 'System.Agents.Tools.NewThing';
   ```

### Task: Add New CLI Command

1. Edit `app/Main.hs`:
   - Add to `Command` data type
   - Add parser in parse section
   - Add handler in `prog` function
2. Update `docs/cli-commands.md`
3. Update `code_index` for any new modules

### Task: Update for Breaking Changes

1. Check `commits_seen` for what was processed
2. Read new commits with `bash_get_git_commit`
3. Update affected documentation
4. Insert into `commits_seen`:
   ```sql
   INSERT INTO commits_seen (commit_hash, commit_date, commit_message)
   VALUES ('hash', '2024-01-15', 'description');
   ```

## Module-to-Doc Mapping

| If working on... | Update... |
|------------------|-----------|
| `System.Agents.Base` | `docs/architecture.md`, `docs/README.md` |
| `System.Agents.Runtime*` | `docs/architecture.md` |
| `System.Agents.AgentTree*` | `docs/architecture.md` |
| `System.Agents.Tools.*` | `docs/tools.md` |
| `System.Agents.Tools.Bash` | `docs/tools.md` (bash section) |
| `System.Agents.Tools.McpToolbox` | `docs/mcp.md`, `docs/tools.md` |
| `System.Agents.Tools.OpenAPI*` | `docs/tools.md` (OpenAPI section) |
| `System.Agents.MCP.*` | `docs/mcp.md` |
| `System.Agents.Session*` | `docs/sessions.md` |
| `System.Agents.SessionStore` | `docs/sessions.md` |
| `System.Agents.TUI.*` | `docs/tui.md` |
| `System.Agents.CLI.*` | `docs/cli-commands.md` |
| `app/Main.hs` | `docs/cli-commands.md` |
| `System.Agents.ExportImport.*` | `docs/export-import.md` |
| `System.Agents.FileLoader*` | `docs/file-loader.md` |

## Code Patterns to Recognize

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

### Pattern: Background Thread with STM

```haskell
-- Tools reload in background
import Control.Concurrent.STM

data Toolbox = Toolbox
    { tools :: BackgroundVal [Tool]
    , triggerReload :: STM Bool
    }

-- Trigger reload
atomically $ triggerReload toolbox
```

### Pattern: JSON with Tagged Unions

```haskell
-- MCP server description uses tagged unions
data McpServerDescription
    = McpSimpleBinary McpSimpleBinaryConfiguration

instance ToJSON McpServerDescription where
    toJSON (McpSimpleBinary val) = object
        [ "tag" .= ("McpSimpleBinary" :: Text)
        , "contents" .= val
        ]
```

### Pattern: Tool Registration

```haskell
-- All tools convert to ToolRegistration
data ToolRegistration = ToolRegistration
    { toolName :: Text
    , toolDescription :: Text
    , toolParameters :: Value  -- JSON Schema
    , toolExecutor :: Value -> IO ToolResult
    }
```

## Key Types Reference

```haskell
-- Identity types (all UUID-based)
newtype AgentId = AgentId UUID
newtype ConversationId = ConversationId UUID
newtype SessionId = SessionId UUID
newtype TurnId = TurnId UUID

-- Core agent configuration
data Agent = Agent
    { slug :: Text
    , apiKeyId :: Text
    , flavor :: Text          -- "openai", "openrouter", etc.
    , modelUrl :: Text
    , modelName :: Text
    , announce :: Text        -- User-facing description
    , systemPrompt :: [Text]
    , toolDirectory :: FilePath
    , mcpServers :: Maybe [McpServerDescription]
    , extraAgents :: Maybe [ExtraAgentRef]  -- Cross-references
    }

-- Cross-agent reference
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

## Red Flags

Watch for these issues:

| Issue | Action |
|-------|--------|
| Undocumented modules | Add to `code_index`, determine doc home |
| Docs referencing wrong modules | Update `docs_index.related_modules` |
| Broken code examples | Fix or mark as "illustrative" |
| New CLI commands not in `cli-commands.md` | Add with examples |
| Commits not in `commits_seen` | Process and insert |

## Quick Queries

```sql
-- Find all bash-related modules:
SELECT module_name FROM code_index 
WHERE module_name LIKE '%Bash%';

-- Find all MCP-related modules:
SELECT module_name FROM code_index 
WHERE module_name LIKE '%Mcp%';

-- Check documentation coverage:
SELECT 
    COUNT(*) as total_modules,
    SUM(CASE WHEN documented_in IS NOT NULL THEN 1 ELSE 0 END) as documented,
    SUM(CASE WHEN documented_in IS NULL THEN 1 ELSE 0 END) as undocumented
FROM code_index;

-- Find modules with their docs:
SELECT module_name, documented_in 
FROM code_index 
WHERE documented_in IS NOT NULL
ORDER BY documented_in;
```

## Working with Cabal

The project uses `agents.cabal`. Key stanzas:

```cabal
-- Library exports modules under System.Agents
library
    exposed-modules:
        System.Agents
        System.Agents.Base
        System.Agents.Runtime
        ...

-- Executable is app/Main.hs
executable agents-exe
    main-is: Main.hs
    hs-source-dirs: app
    build-depends: agents, ...
```

## Common Import Patterns

```haskell
-- Core types
import System.Agents.Base

-- Runtime
import System.Agents.Runtime
import qualified System.Agents.Runtime.Runtime as Runtime

-- Agent tree
import qualified System.Agents.AgentTree as AgentTree

-- Tools
import qualified System.Agents.Tools.Bash as Bash
import qualified System.Agents.Tools.BashToolbox as BashToolbox
import qualified System.Agents.Tools.McpToolbox as McpTools

-- Sessions
import qualified System.Agents.Session.Base as Session
import qualified System.Agents.SessionStore as SessionStore

-- CLI
import System.Agents.CLI.Base
import qualified System.Agents.CLI.OneShot as OneShot

-- Tracing
import Prod.Tracer
```

## Summary Checklist

Before finishing work:

- [ ] Summary line included (first line of response)
- [ ] Knowledge base tables updated if needed
- [ ] New modules added to `code_index`
- [ ] Documentation created/updated for changes
- [ ] `docs_index` updated if new docs added
- [ ] Cross-references checked
- [ ] Code examples verified

## Emergency Contacts

If completely lost:

1. Read `docs/README.md` for overview
2. Check `app/Main.hs` for current command structure
3. Look at `src/System/Agents/Base.hs` for core types
4. Query `code_index` for module purposes

Remember: This is a well-structured Haskell project. Follow the types, follow the imports, and you'll find your way.

