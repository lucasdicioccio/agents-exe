# Internal Tool Mapping

This document describes how tool names are mapped between different systems in `agents-exe`.

## Overview

Tools in `agents-exe` come from various sources and may need their names transformed for different contexts:

- LLM context - Names visible to the language model
- MCP context - Names used in Model Context Protocol
- Bash context - Names used in bash tool invocations
- User context - Names displayed to users

## Tool Naming Conventions

### LLM Tool Names

Tools exposed to LLMs follow a prefixed naming convention to avoid collisions:

```
{prefix}_{original_name}
```

Prefixes:
- `bash_` - Bash script tools
- `mcp_{server}_` - MCP tools from a specific server
- `openapi_` - OpenAPI-derived tools
- `postgrest_{toolbox}_` - PostgREST-derived tools
- `io_` - In-process Haskell IO tools

Examples:
- `bash_hello` - Bash tool named "hello"
- `mcp_filesystem_readFile` - MCP tool from "filesystem" server
- `openapi_getPetById` - OpenAPI tool for getPetById operation
- `postgrest_api_get_users` - PostgREST tool for GET /users

### MCP Tool Names

When exposing tools via MCP, names can be transformed using the `NameMappingStrategy`:

#### KeepPrefixedNames

Preserves the LLM prefixes:
```
bash_hello -> bash_hello
mcp_filesystem_readFile -> mcp_filesystem_readFile
```

#### StripPrefixes (Default)

Removes LLM-specific prefixes:
```
bash_hello -> hello
mcp_filesystem_readFile -> filesystem_readFile
openapi_getPetById -> getPetById
```

#### UseOriginalNames

Uses original names where available:
```
bash_hello -> hello
openapi_getPetById -> getPetById
```

## Tool Registration

The `System.Agents.ToolRegistration` module handles tool registration with proper name mapping.

### Registration Functions

```haskell
-- Register a bash tool
registerBashToolInLLM :: ScriptDescription -> ToolRegistration

-- Register an MCP tool
registerMcpToolInLLM :: Toolbox -> ToolDescription -> Either String ToolRegistration

-- Register an OpenAPI tool
registerOpenAPIToolInLLM :: Toolbox -> OpenAPITool -> Either String ToolRegistration

-- Register a PostgREST tool
registerPostgRESToolInLLM :: Toolbox -> PostgRESTool -> Either String ToolRegistration
```

### Naming Policy Functions

```haskell
-- Bash tool naming
bash2LLMName :: ScriptDescription -> ToolName
-- Result: bash_{scriptSlug}

-- MCP tool naming
mcp2LLMName :: Toolbox -> ToolDescription -> ToolName
-- Result: mcp_{serverName}_{toolName}

-- OpenAPI tool naming
openapi2LLMName :: Toolbox -> Text -> ToolName
-- Result: openapi_{normalizedName}

-- PostgREST tool naming
postgrest2LLMName :: Toolbox -> PostgRESTool -> ToolName
-- Result: postgrest_{toolbox}_{method}_{table}
```

## Toolbox Nest Command

The `toolbox nest` command creates a single bash-compatible tool from multiple tools. The nested tool:

1. Preserves original tool configurations in embedded JSON
2. Dispatches to sub-tools based on the `--tool` argument
3. Maintains the bash-tool interface (`describe` and `run` commands)

### Nested Tool Dispatch

```bash
./nested-tool run --tool=<name> --arg='<json>'
```

The tool name is matched against registered tools and dispatched appropriately:

- `BashToolType` - Execute the bash script directly
- `McpToolType` - Call via `agents-exe toolbox call-mcp`
- `IOToolType` - Call via `agents-exe toolbox call-io`
- `OpenAPIToolType` - Call via `agents-exe toolbox call-openapi`
- `PostgRESToolType` - Call via `agents-exe toolbox call-postgrest`

### Nest Configuration Format

The embedded configuration in nested tools:

```json
{
  "name": "nested-tool-name",
  "tools": [
    {
      "name": "tool1",
      "type": "bash",
      "config": {
        "path": "/path/to/tool1",
        "info": { ... }
      }
    },
    {
      "name": "tool2",
      "type": {"mcp": "server-name"},
      "config": {
        "server": "server-name"
      }
    }
  ]
}
```

## Toolbox MCP Command

The `toolbox mcp` command exposes tools directly via MCP without agent indirection. This provides:

1. Direct tool execution without LLM latency
2. Simplified testing of MCP clients
3. Composition of tools from multiple sources

### Name Mapping in MCP Server

When running `toolbox mcp`, tool names are mapped according to the selected strategy:

```bash
# Strip prefixes (default)
agents-exe toolbox mcp --agent-file agent.json --strip-prefixes

# Keep prefixes
agents-exe toolbox mcp --agent-file agent.json --keep-prefixes

# Use original names
agents-exe toolbox mcp --agent-file agent.json --use-original-names
```

The mapping is applied when:
1. Tools are listed via `tools/list`
2. Tools are called via `tools/call`

### Tool Lookup

When a tool is called, the MCP server:

1. Receives the MCP tool name
2. Applies reverse mapping to find the internal tool registration
3. Executes the tool with the provided arguments
4. Returns results in MCP format

## OpenAPI Name Mapping

OpenAPI operation IDs may contain characters invalid for LLM tool names. The `NameMapping` system maintains bidirectional mapping:

```haskell
data NameMapping = NameMapping
  { nmOriginal :: Text    -- Original OpenAPI operationId
  , nmNormalized :: Text  -- LLM-safe normalized name
  }
```

Normalization rules:
1. Replace invalid characters with underscores
2. Ensure name starts with a letter
3. Collapse consecutive underscores

Example:
```
Original: "pets.getById"
Normalized: "pets_getById"
LLM Name: "openapi_pets_getById"
```

## PostgREST Name Mapping

PostgREST tools use a structured naming scheme:

```
postgrest_{toolbox}_{method}_{table}
```

Examples:
```
postgrest_api_get_users    -> GET /users
postgrest_api_post_users   -> POST /users
postgrest_api_patch_users  -> PATCH /users
postgrest_api_delete_users -> DELETE /users
```

The table name is extracted from the path and normalized for LLM compatibility.

## Best Practices

### For Tool Authors

1. Use descriptive, unique script/tool names
2. Avoid special characters that require normalization
3. Keep names concise but meaningful

### For Agent Developers

1. Use `toolbox list` to verify tool names
2. Use `toolbox nest` to create portable tool packages
3. Use `toolbox mcp` for testing tool integrations

### For Client Developers

1. Handle both prefixed and unprefixed names
2. Use the MCP introspection to discover available tools
3. Cache tool schemas for better performance

## Related Documentation

- `docs/cli-reference.md` - CLI command reference
- `System.Agents.ToolRegistration` - Tool registration API
- `System.Agents.Tools.Nest` - Tool nesting implementation
- `System.Agents.Tools.McpServer` - Direct MCP server implementation

