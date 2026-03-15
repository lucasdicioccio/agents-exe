# Tool System

The tool system provides agents with the ability to execute external commands, call APIs, and interact with other agents. Tools are dynamically registered and exposed to the LLM via the OpenAI function calling API.

## Overview

```
┌────────────────────────────────────────────────────────────────┐
│                       Tool System                               │
├────────────────────────────────────────────────────────────────┤
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐            │
│  │ Bash Tools  │  │  MCP Tools  │  │ OpenAPI     │            │
│  │ (scripts)   │  │ (servers)   │  │ (REST APIs) │            │
│  └──────┬──────┘  └──────┬──────┘  └──────┬──────┘            │
│         │                │                │                    │
│         └────────────────┴────────────────┘                    │
│                          │                                      │
│                   ┌──────▼──────┐                              │
│                   │  Toolbox    │                              │
│                   │  (merging)  │                              │
│                   └──────┬──────┘                              │
│                          │                                      │
│                   ┌──────▼──────┐                              │
│                   │ ToolRegistration│                           │
│                   │ (LLM schema) │                              │
│                   └─────────────┘                              │
└────────────────────────────────────────────────────────────────┘
```

## Tool Types

The framework supports multiple tool types:

| Tool Type | Description | Use Case |
|-----------|-------------|----------|
| **Bash Tools** | Executable scripts | External commands, system integration |
| **MCP Tools** | MCP servers | Standardized tool protocols |
| **OpenAPI Tools** | REST APIs | API integrations |
| **PostgREST Tools** | Database endpoints | Database queries |
| **SQLite Tools** | SQLite databases | Local SQL queries |
| **System Tools** | System information | Runtime context |
| **Developer Tools** | Development utilities | Agent/tool scaffolding |
| **IO Tools** | Haskell functions | In-process operations |

## Tool Registration

All tools are registered using the `ToolRegistration` type:

```haskell
data ToolRegistration = ToolRegistration
    { innerTool :: Tool ()
    , declareTool :: OpenAI.Tool
    , findTool :: OpenAI.ToolCall -> Maybe (Tool OpenAI.ToolCall)
    }
```

### Registration Flow

1. Tool sources (bash, MCP, OpenAPI, System, Developer) generate `ToolRegistration` values
2. Registrations are combined into a single list
3. The list is passed to the LLM API as available functions
4. When the LLM calls a function, the executor is invoked

## Bash Tools

Bash tools are executable scripts stored in the agent's tool directory.

### Script Requirements

Scripts must support a `describe` subcommand that outputs JSON:

```bash
#!/bin/bash

if [ "$1" == "describe" ]; then
    echo '{
        "slug": "my-tool",
        "description": "What this tool does",
        "args": [
            {
                "name": "input",
                "description": "Input parameter",
                "type": "string",
                "backing_type": "string",
                "arity": "single",
                "mode": "dashdashspace"
            }
        ],
        "empty-result": {"tag": "AddMessage", "contents": "No results"}
    }'
    exit 0
fi

# Main execution
input="$1"
echo "Result for: $input"
```

### ScriptInfo Schema

```haskell
data ScriptInfo = ScriptInfo
    { scriptArgs :: [ScriptArg]
    , scriptSlug :: Text
    , scriptDescription :: Text
    , scriptEmptyResultBehavior :: Maybe EmptyResultBehavior
    }

data ScriptArg = ScriptArg
    { argName :: Text
    , argDescription :: Text
    , argType :: Text
    , argBackingType :: Text
    , argArity :: Text  -- "single", "optional", "multiple"
    , argMode :: Text   -- "dashdashspace", "space", etc.
    }
```

### Environment Variables

When a bash tool runs, it receives context via environment variables:

| Variable | Description |
|----------|-------------|
| `AGENT_SESSION_ID` | Current session UUID |
| `AGENT_CONVERSATION_ID` | Conversation UUID |
| `AGENT_TURN_ID` | Current turn UUID |
| `AGENT_AGENT_ID` | Agent UUID (if available) |
| `AGENT_SESSION_JSON` | Full session as JSON |

### Bash Toolbox

The `BashToolbox` module manages script discovery and execution:

```haskell
data Toolbox = Toolbox
    { tools :: BackgroundVal [ToolRegistration]
    , triggerReload :: STM Bool
    }

initializeBackroundToolbox :: 
    Tracer IO Trace -> 
    FilePath -> 
    IO (Either ToolboxError Toolbox)
```

Features:
- **Hot reloading**: File changes trigger automatic reload
- **Background thread**: Non-blocking tool discovery
- **Error isolation**: Failed scripts don't break other tools

## MCP Tools

Model Context Protocol (MCP) tools connect to external servers that provide dynamic tool listings.

### MCP Server Types

```haskell
data McpServerDescription
    = McpSimpleBinary McpSimpleBinaryConfiguration
    
data McpSimpleBinaryConfiguration = McpSimpleBinaryConfiguration
    { name :: Text
    , executable :: FilePath
    , args :: [Text]
    }
```

### MCP Client Runtime

```haskell
data Runtime = Runtime
    { procHandle :: ProcessHandle
    , stdinHandle :: Handle
    , stdoutHandle :: Handle
    , toolsList :: TVar [ToolDescription]
    , callResults :: TVar (Map CallId Value)
    }
```

### MCP Tool Flow

```
1. Start MCP server process
2. Initialize connection
3. Query available tools
4. Register tools with LLM
5. On LLM call:
   a. Send tool_call message to MCP server
   b. Wait for response
   c. Return result to LLM
```

### MCP Protocol Messages

```json
// Tool list request
{"jsonrpc": "2.0", "method": "tools/list", "id": 1}

// Tool list response
{"jsonrpc": "2.0", "result": 
  {"tools": [
    {"name": "read_file", 
     "description": "Read a file", 
     "inputSchema": {...}}
  ]}, 
  "id": 1}

// Tool call
{"jsonrpc": "2.0", 
 "method": "tools/call", 
 "params": {"name": "read_file", "arguments": {"path": "/tmp/foo"}},
 "id": 2}
```

## OpenAPI Tools

OpenAPI tools convert REST API specifications into LLM-callable tools.

### OpenAPI Server Configuration

```json
{
  "mcpServers": [...],
  "openApiToolboxes": [
    {
      "tag": "OpenAPIServer",
      "contents": {
        "specUrl": "https://api.example.com/openapi.json",
        "baseUrl": "https://api.example.com",
        "headers": {"X-API-Version": "v1"},
        "token": "${API_TOKEN}"
      }
    }
  ]
}
```

### Conversion Process

```haskell
-- Load and parse OpenAPI spec
convertOpenAPIToTools :: OpenAPISpec -> [OpenAPITool]

-- Convert operation to tool
convertOperation :: Path -> Method -> Operation -> OpenAPITool

-- Build tool parameters from OpenAPI parameters
buildToolParameters :: Operation -> ToolParameters

-- Convert to OpenAI tool format
toOpenAITool :: OpenAPITool -> OpenAI.Tool
```

### Schema Resolution

The OpenAPI module handles `$ref` references:

```haskell
resolveSchema :: Schema -> Components -> Schema
dereferenceSpec :: OpenAPISpec -> OpenAPISpec
```

Supports:
- Internal references (`#/components/schemas/Foo`)
- Nested references
- Array item references
- anyOf/allOf compositions
- Circular reference detection

### Name Normalization

OpenAPI operation IDs are normalized for LLM compatibility:

```haskell
-- Original: "pets.getById"
-- Normalized: "pets_getById"

-- Original: "/users/{id}/posts"
-- Normalized: "_users__id__posts"
```

The `NameMapping` type tracks bidirectional mapping:

```haskell
data NameMapping = NameMapping
    { nmOriginal :: Text
    , nmNormalized :: Text
    }
```

## PostgREST Tools

PostgREST tools generate database query tools from PostgREST APIs.

### Configuration

```json
{
  "postgrestToolboxes": [
    {
      "tag": "PostgRESTServer",
      "contents": {
        "specUrl": "http://localhost:3000/",
        "name": "mydb",
        "description": "Main database"
      }
    }
  ]
}
```

### Generated Tools

For each table endpoint, the following tools are generated:

| HTTP Method | Tool Name Pattern | Purpose |
|-------------|-------------------|---------|
| GET | `postgrest_{name}_get_{table}` | Query with filters |
| POST | `postgrest_{name}_post_{table}` | Insert rows |
| PUT | `postgrest_{name}_put_{table}` | Update rows |
| PATCH | `postgrest_{name}_patch_{table}` | Partial update |
| DELETE | `postgrest_{name}_delete_{table}` | Delete rows |

### Tool Parameters

PostgREST tools use structured parameter groups:

```json
{
  "filters": {
    "column_name": "filter_value"
  },
  "subset": {
    "limit": 10,
    "offset": 0,
    "columns": "id,name,email"
  },
  "ranking": {
    "order": "created_at.desc"
  },
  "body": {
    "name": "New Item",
    "value": 42
  }
}
```

## SQLite Tools

SQLite tools provide SQL query capabilities against SQLite databases.

### Configuration

```json
{
  "builtinToolboxes": [
    {
      "tag": "SqliteToolbox",
      "contents": {
        "name": "analytics",
        "description": "Analytics database",
        "path": "./analytics.db"
      }
    }
  ]
}
```

### Tool Interface

Each SQLite toolbox exposes a single `sqlite_{name}_query` tool:

```json
{
  "sql": "SELECT * FROM users WHERE active = 1 LIMIT 10"
}
```

**Parameters:**
- `sql` (string, required): SQL query to execute

**Security:**
- Read-only queries are encouraged
- Write operations are allowed but logged
- No DDL by default (configurable)

## System Toolbox (Builtin)

The System Toolbox provides agents with contextual information about the running system through a configurable set of capabilities.

### Capabilities

| Capability | Description |
|------------|-------------|
| `date` | Current UTC/local time and timezone info |
| `operating-system` | OS name, version, kernel, architecture |
| `env-vars` | Filtered environment variables |
| `running-user` | Username, UID, GID, home, shell |
| `hostname` | Machine hostname |
| `working-directory` | Current working directory |
| `process-info` | Process ID, parent PID, process name |
| `uptime` | System uptime |

### Configuration

```json
{
  "builtinToolboxes": [
    {
      "tag": "SystemToolbox",
      "contents": {
        "name": "system",
        "description": "System context and information",
        "capabilities": ["date", "operating-system", "running-user", "hostname"],
        "envVarFilter": null
      }
    }
  ]
}
```

### Configuration Fields

| Field | Type | Description |
|-------|------|-------------|
| `name` | string | Unique name for this toolbox instance |
| `description` | string | Human-readable description |
| `capabilities` | [string] | List of enabled capabilities |
| `envVarFilter` | string? | Optional substring filter for env vars |

### Security Considerations

- **Capability-based access**: Only enabled capabilities are exposed
- **Env var filtering**: Use `envVarFilter` to limit variable exposure
- **Read-only**: System tools gather information but cannot modify the system
- **Linux-focused**: Initial implementation targets Linux systems

### LLM Tool Interface

The system toolbox exposes a single tool named `system_{name}_system_info` with:

- **Parameter**: `capability` (string) - Which system info to retrieve
- **Returns**: JSON object with the requested information

Example tool call:
```json
{
  "capability": "date"
}
```

Example response:
```json
{
  "capability": "date",
  "data": {
    "utc": "2024-01-15T10:30:00.123456Z",
    "local": "2024-01-15T11:30:00.123456",
    "timezone": "CET",
    "timezoneOffset": "+0100"
  },
  "executionTime": 0.001
}
```

## Developer Toolbox

The Developer Toolbox provides utilities for writing, validating, and scaffolding agents and tools.

### Capabilities

| Capability | Description |
|------------|-------------|
| `validate-tool` | Validates a bash tool script |
| `scaffold-agent` | Generates agent scaffolding from template |
| `scaffold-tool` | Generates tool scaffolding in multiple languages |
| `show-spec` | Displays specification documentation |

### Configuration

```json
{
  "builtinToolboxes": [
    {
      "tag": "DeveloperToolbox",
      "contents": {
        "name": "dev",
        "description": "Development utilities",
        "capabilities": ["validate-tool", "scaffold-agent", "scaffold-tool", "show-spec"]
      }
    }
  ]
}
```

### Tool Interface

The developer toolbox exposes a single tool named `developer_{name}_developer_tools` with:

- **Parameter**: `capability` (string) - Which operation to perform
- **Additional parameters** vary by capability

#### validate-tool

```json
{
  "capability": "validate-tool",
  "tool_path": "./tools/my-tool.sh"
}
```

**Response:**
```json
{
  "path": "./tools/my-tool.sh",
  "valid": true,
  "slug": "my-tool",
  "error": null
}
```

#### scaffold-agent

```json
{
  "capability": "scaffold-agent",
  "template": "openai",
  "slug": "my-new-agent",
  "file_path": "./my-new-agent.json",
  "force": false
}
```

**Templates:** `openai`, `mistral`, `ollama`

**Response:**
```json
{
  "success": true,
  "path": "./my-new-agent.json",
  "error": null
}
```

#### scaffold-tool

```json
{
  "capability": "scaffold-tool",
  "language": "bash",
  "slug": "my-new-tool",
  "file_path": "./tools/my-new-tool.sh",
  "force": false
}
```

**Languages:** `bash`, `python`, `haskell`, `node`

**Response:**
```json
{
  "success": true,
  "path": "./tools/my-new-tool.sh",
  "error": null
}
```

#### show-spec

```json
{
  "capability": "show-spec",
  "spec_name": "bash-tools"
}
```

**Specs:** `bash-tools`

**Response:** Returns the embedded specification documentation as text.

### Generated Templates

#### Bash Tool Template

```bash
#!/bin/bash
# my-tool - A bash tool for agents-exe

set -euo pipefail

case "${1:-}" in
  describe)
    cat <<'DESCRIBE_EOF'
{
  "slug": "my-tool",
  "description": "Tool my-tool - describe what this tool does",
  "args": [],
  "empty-result": {
    "tag": "AddMessage",
    "contents": "--no output--"
  }
}
DESCRIBE_EOF
    ;;
  run)
    # TODO: Implement tool logic
    echo "Tool my-tool executed"
    ;;
  *)
    echo "Usage: my-tool <describe|run>" >&2
    exit 1
    ;;
esac
```

#### Python Tool Template

```python
#!/usr/bin/env python3
import json
import sys
import os

def describe():
    return {
        "slug": "my-tool",
        "description": "Tool my-tool - describe what this tool does",
        "args": [],
        "empty-result": {
            "tag": "AddMessage",
            "contents": "--no output--"
        }
    }

def run():
    # TODO: Implement tool logic
    print("Tool my-tool executed")

def main():
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <describe|run>", file=sys.stderr)
        return 1
    
    command = sys.argv[1]
    if command == "describe":
        print(json.dumps(describe()))
        return 0
    elif command == "run":
        run()
        return 0
    else:
        print(f"Unknown command: {command}", file=sys.stderr)
        return 1

if __name__ == "__main__":
    sys.exit(main())
```

## IO Tools

IO tools are Haskell functions that run within the agent process.

### Definition

```haskell
type IOToolBuilder = AgentSlug -> AgentId -> ToolRegistration

exampleTool :: IOToolBuilder
exampleTool slug agentId = ToolRegistration
    { innerTool = ...
    , declareTool = OpenAI.Tool
        { toolName = "example"
        , toolDescription = "An example tool"
        , toolParamProperties = [...]
        }
    , findTool = \call -> ...
    }
```

### Use Cases

- Agent-to-agent calls (via `turnAgentRuntimeIntoIOTool`)
- Direct system integration
- Performance-critical operations
- Stateful operations

### IO Tool with Context

IO tools receive full execution context:

```haskell
ioTool ::
    (Aeson.FromJSON llmArg) =>
    IOTools.IOScript llmArg ByteString ->
    Tool ()

data IOScript arg result = IOScript
    { description :: IOScriptDescription
    , ioRun :: ToolExecutionContext -> arg -> IO result
    }
```

## Tool Execution Context

Tools receive execution context for recursion tracking:

```haskell
data ToolExecutionContext = ToolExecutionContext
    { ctxSessionId :: SessionId
    , ctxConversationId :: ConversationId
    , ctxTurnId :: TurnId
    , ctxAgentId :: Maybe AgentId
    , ctxFullSession :: Maybe Session
    , ctxCallStack :: [CallStackEntry]
    , ctxMaxDepth :: Maybe Int
    }

data CallStackEntry = CallStackEntry
    { callAgentSlug :: AgentSlug
    , callConversationId :: ConversationId
    , callDepth :: Int
    }
```

### Recursion Control

```haskell
pushAgentContext :: 
    AgentSlug -> 
    ConversationId -> 
    ToolExecutionContext -> 
    Either RecursionError ToolExecutionContext
```

Prevents infinite loops by tracking call depth and failing when `maxDepth` is exceeded.

## Tool Result Types

```haskell
data CallResult call
    = BlobToolSuccess call ByteString
    | JsonToolSuccess call Aeson.Value
    | ToolSkipped call
    | BashToolError call BashError
    | IOToolError call IOToolError
    | McpToolError call Text
    | OpenAPIToolError call String
    | PostgRESToolError call String
    | SqliteToolError call SqliteError
    | SystemToolError call SystemInfoError
    | DeveloperToolError call ValidationError
    | McpToolResult call McpToolResult
    | OpenAPIToolResult call OpenAI.ToolResult
    | PostgRESToolResult call OpenAI.ToolResult
    | SqliteToolResult call SqliteQueryResult
    | SystemToolResult call SystemQueryResult
    | DeveloperToolResult call ValidationResult
    | DeveloperToolScaffoldResult call ScaffoldResult
    | DeveloperToolSpecResult call Text
```

## Tool Schema

Tools expose JSON Schema for LLM function calling:

```haskell
data ParamProperty = ParamProperty
    { propertyKey :: Text
    , propertyType :: ParamType
    , propertyDescription :: Text
    , propertyRequired :: Bool
    }

data ParamType
    = NullParamType
    | StringParamType
    | BoolParamType
    | NumberParamType
    | EnumParamType [Text]
    | OpaqueParamType Text
    | MultipleParamType Text
    | ObjectParamType [ParamProperty]
```

Example JSON Schema:

```json
{
  "type": "object",
  "properties": {
    "filename": {
      "type": "string",
      "description": "Name of the file to read"
    },
    "lines": {
      "type": "integer",
      "description": "Number of lines to read"
    }
  },
  "required": ["filename"]
}
```

## Naming Conventions

Tools are named according to their type and toolbox:

| Tool Type | Naming Pattern | Example |
|-----------|---------------|---------|
| Bash | `bash_{slug}` | `bash_read_file` |
| MCP | `mcp_{toolbox}_{name}` | `mcp_filesystem_read_file` |
| OpenAPI | `openapi_{toolbox}_{operation}` | `openapi_pets_getById` |
| PostgREST | `postgrest_{toolbox}_{method}_{table}` | `postgrest_mydb_get_users` |
| SQLite | `sqlite_{toolbox}_query` | `sqlite_analytics_query` |
| System | `system_{toolbox}_system_info` | `system_system_system_info` |
| Developer | `developer_{toolbox}_developer_tools` | `developer_dev_developer_tools` |
| IO | `io_{slug}` | `io_calculator` |

## Combining Tool Sources

The runtime merges tools from all sources:

```haskell
newRuntime :: ... -> IO (Either String Runtime)
newRuntime ... = do
    -- Bash tools from tool directory
    bashTools <- BashToolbox.initializeBackroundToolbox ...
    
    -- IO tools from code
    let ioTools = [mk slug uid | mk <- mkIoTools]
    
    -- MCP tools from configured servers
    mcpTools <- mapM initializeMcpToolbox mcpConfigs
    
    -- OpenAPI tools from specs
    openApiTools <- mapM loadOpenApiTools openApiConfigs
    
    -- PostgREST tools
    postgrestTools <- mapM loadPostgRESTools prConfigs
    
    -- SQLite tools from builtin toolboxes
    sqliteTools <- readSqliteToolsRegistrations tracer sqliteToolboxes
    
    -- System tools from builtin toolboxes
    systemTools <- readSystemToolsRegistrations tracer systemToolboxes
    
    -- Developer tools from builtin toolboxes
    devTools <- readDeveloperToolsRegistrations tracer devToolboxes
    
    -- Combine all
    let allTools = ioTools ++ bashTools ++ mcpTools ++ openApiTools ++ 
                   postgrestTools ++ sqliteTools ++ systemTools ++ devTools
```

## Error Handling

### Bash Tool Errors

```haskell
data ToolboxError
    = ToolboxDirectoryNotFound FilePath
    | ScriptParseError FilePath String
    | ScriptExecutionError FilePath Int String
```

### MCP Errors

```haskell
data McpError
    = ProcessStartError Text
    | ProtocolError Text
    | ToolCallError Text
```

### OpenAPI Errors

```haskell
data OpenAPIError
    = SpecParseError String
    | SchemaResolutionError Text
    | ReferenceError RefPath
```

### System Toolbox Errors

```haskell
data QueryError
    = CapabilityNotEnabledError Text
    | SystemInfoError Text
```

### Developer Toolbox Errors

```haskell
data DeveloperToolError
    = CapabilityNotEnabledError Text
    | ValidationError Text
    | ScaffoldError Text
    | FileExistsError FilePath
    | InvalidTemplateError Text
```

## Best Practices

1. **Idempotency**: Tools should be safe to call multiple times
2. **Clear descriptions**: Help the LLM understand when to use each tool
3. **Validation**: Validate inputs before execution
4. **Timeouts**: Set reasonable timeouts for external calls
5. **Logging**: Use the tracer for observability
6. **Error messages**: Return clear error messages for LLM consumption
7. **Parameter naming**: Use descriptive parameter names
8. **Required vs Optional**: Mark truly required parameters as required

## Example: Complete Tool Configuration

```json
{
  "slug": "file-agent",
  "apiKeyId": "openai",
  "flavor": "openai",
  "modelUrl": "https://api.openai.com/v1",
  "modelName": "gpt-4",
  "announce": "A file management assistant",
  "systemPrompt": ["You help users manage files."],
  "toolDirectory": "tools",
  "mcpServers": [
    {
      "tag": "McpSimpleBinary",
      "contents": {
        "name": "filesystem",
        "executable": "/usr/bin/mcp-filesystem",
        "args": ["--root", "/home/user"]
      }
    }
  ],
  "openApiToolboxes": [
    {
      "tag": "OpenAPIServer",
      "contents": {
        "specUrl": "https://api.github.com/openapi.json",
        "baseUrl": "https://api.github.com",
        "token": "${GITHUB_TOKEN}"
      }
    }
  ],
  "postgrestToolboxes": [
    {
      "tag": "PostgRESTServer",
      "contents": {
        "specUrl": "http://localhost:3000/",
        "name": "maindb",
        "description": "Main application database"
      }
    }
  ],
  "builtinToolboxes": [
    {
      "tag": "SqliteToolbox",
      "contents": {
        "name": "analytics",
        "description": "Analytics database",
        "path": "./analytics.db"
      }
    },
    {
      "tag": "SystemToolbox",
      "contents": {
        "name": "system",
        "description": "System context",
        "capabilities": ["date", "hostname", "working-directory"],
        "envVarFilter": null
      }
    },
    {
      "tag": "DeveloperToolbox",
      "contents": {
        "name": "dev",
        "description": "Development utilities",
        "capabilities": ["validate-tool", "scaffold-agent", "scaffold-tool"]
      }
    }
  ],
  "extraAgents": [
    {"slug": "helper", "path": "./helper.json"}
  ]
}
```

