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

## Tool Registration

All tools are registered using the `ToolRegistration` type:

```haskell
data ToolRegistration = ToolRegistration
    { toolName :: Text
    , toolDescription :: Text
    , toolParameters :: Value  -- JSON Schema object
    , toolExecutor :: Value -> IO ToolResult
    }
```

### Registration Flow

1. Tool sources (bash, MCP, OpenAPI) generate `ToolRegistration` values
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
                "required": true
            }
        ],
        "emptyResult": "message"
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
    , argRequired :: Bool
    }
```

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

## IO Tools

IO tools are Haskell functions that run within the agent process.

### Definition

```haskell
type IOToolBuilder = AgentSlug -> AgentId -> ToolRegistration

exampleTool :: IOToolBuilder
exampleTool slug agentId = ToolRegistration
    { toolName = "example"
    , toolDescription = "An example tool"
    , toolParameters = object 
        [ "type" .= ("object" :: Text)
        , "properties" .= object []
        , "required" .= ([] :: [Text])
        ]
    , toolExecutor = \_ -> return $ ToolSuccess $ object ["result" .= ("ok" :: Text)]
    }
```

### Use Cases

- Agent-to-agent calls (via `turnAgentRuntimeIntoIOTool`)
- Direct system integration
- Performance-critical operations
- Stateful operations

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
data ToolResult
    = ToolSuccess Value
    | ToolError Text
    | ToolEmptyResult EmptyResultBehavior

data EmptyResultBehavior
    = AddMessage Text  -- Add a message when output is empty
    | NoAction         -- Do nothing
```

## Tool Schema

Tools expose JSON Schema for LLM function calling:

```haskell
data ToolParameters = ToolParameters
    { paramsType :: Text           -- Always "object"
    , paramsProperties :: Map Text Property
    , paramsRequired :: [Text]
    }

data Property = Property
    { propType :: Maybe Text
    , propDescription :: Text
    , propEnum :: Maybe [Text]
    , propItems :: Maybe Property
    }
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
    
    -- Combine all
    let allTools = ioTools ++ bashTools ++ mcpTools ++ openApiTools
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

## Best Practices

1. **Idempotency**: Tools should be safe to call multiple times
2. **Clear descriptions**: Help the LLM understand when to use each tool
3. **Validation**: Validate inputs before execution
4. **Timeouts**: Set reasonable timeouts for external calls
5. **Logging**: Use the tracer for observability
6. **Error messages**: Return clear error messages for LLM consumption

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
  "extraAgents": [
    {"slug": "helper", "path": "./helper.json"}
  ]
}
```

