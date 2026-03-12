# MCP Protocol

The Model Context Protocol (MCP) integration allows agents to connect to external servers that provide dynamic tool listings and execution capabilities.

## Overview

MCP is a protocol for model context exchange that enables:

- **Dynamic tool discovery**: Servers advertise available tools at runtime
- **Structured tool calls**: JSON-RPC based communication
- **Process isolation**: MCP servers run in separate processes
- **Standardized interface**: Common protocol across different tool providers

```
┌─────────────┐      JSON-RPC       ┌─────────────┐
│   Agent     │<------------------->│ MCP Server  │
│  Runtime    │   (stdio/stdin)     │  Process    │
└─────────────┘                     └─────────────┘
       │                                   │
       │ Tool descriptions                 │ Tool execution
       │                                   │
       ▼                                   ▼
┌─────────────┐                     ┌─────────────┐
│     LLM     │                     │  External   │
│  (tool use) │                     │   Systems   │
└─────────────┘                     └─────────────┘
```

## Architecture

### Client Architecture (`System.Agents.MCP.Client`)

```haskell
-- Client runtime managing MCP server process
data Runtime = Runtime
    { procHandle :: ProcessHandle
    , stdinHandle :: Handle
    , stdoutHandle :: Handle
    , toolsList :: TVar [ToolDescription]
    , callResults :: TVar (Map CallId Value)
    , serverName :: Text
    }
```

### Server Architecture (`System.Agents.MCP.Server`)

The framework can also act as an MCP server, exposing agents as tools to other MCP clients:

```haskell
multiAgentsServer :: 
    McpServerConfig -> 
    [AgentTree.Props] -> 
    IO ()
```

## MCP Server Configuration

### Simple Binary Configuration

```json
{
  "mcpServers": [
    {
      "tag": "McpSimpleBinary",
      "contents": {
        "name": "filesystem",
        "executable": "/usr/bin/mcp-server-filesystem",
        "args": ["--root", "/home/user/projects"]
      }
    }
  ]
}
```

### Haskell Configuration Type

```haskell
data McpServerDescription
    = McpSimpleBinary McpSimpleBinaryConfiguration

data McpSimpleBinaryConfiguration = McpSimpleBinaryConfiguration
    { name :: Text           -- Display name
    , executable :: FilePath -- Path to server binary
    , args :: [Text]         -- Command line arguments
    }
```

## Protocol Flow

### 1. Server Initialization

```
Agent Runtime                    MCP Server
     │                                │
     │── Spawn process ──────────────>│
     │   (executable with args)       │
     │                                │
     │<─ Initialize request ──────────│
     │   (protocol version, etc.)     │
     │                                │
     │── Initialize response ────────>│
     │   (server capabilities)        │
     │                                │
```

### 2. Tool Discovery

```haskell
-- Request tools list
sendRequest :: Runtime -> Method -> Value -> IO CallId

-- Receive tool descriptions
receiveTools :: Runtime -> IO [ToolDescription]
```

### 3. Tool Execution Loop

```
Agent Runtime                    MCP Server        LLM
     │                                │             │
     │<───────────────────────────────│<── Tool call│
     │   (Tool call from server)      │   request   │
     │                                │             │
     │── Execute tool locally ────────│             │
     │   (if agent-as-tool)           │             │
     │                                │             │
     │── Tool result ────────────────>│             │
     │                                │             │
     │<───────────────────────────────│─── Forward ─>│
     │                                │    result   │
```

## JSON-RPC Protocol

### Message Format

```haskell
data JsonRpcMessage
    = JsonRpcRequest 
        { jsonrpc :: Text
        , method :: Text
        , params :: Maybe Value
        , id :: Maybe CallId
        }
    | JsonRpcResponse
        { jsonrpc :: Text
        , result :: Maybe Value
        , error :: Maybe JsonRpcError
        , id :: CallId
        }
    | JsonRpcNotification
        { jsonrpc :: Text
        , method :: Text
        , params :: Maybe Value
        }
```

### Tool List Method

Request:
```json
{
  "jsonrpc": "2.0",
  "method": "tools/list",
  "id": 1
}
```

Response:
```json
{
  "jsonrpc": "2.0",
  "result": {
    "tools": [
      {
        "name": "read_file",
        "description": "Read contents of a file",
        "inputSchema": {
          "type": "object",
          "properties": {
            "path": {
              "type": "string",
              "description": "Path to the file"
            }
          },
          "required": ["path"]
        }
      }
    ]
  },
  "id": 1
}
```

### Tool Call Method

Request:
```json
{
  "jsonrpc": "2.0",
  "method": "tools/call",
  "params": {
    "name": "read_file",
    "arguments": {
      "path": "/home/user/README.md"
    }
  },
  "id": 2
}
```

Response:
```json
{
  "jsonrpc": "2.0",
  "result": {
    "content": [
      {
        "type": "text",
        "text": "# Project README\n\nThis is the content..."
      }
    ],
    "isError": false
  },
  "id": 2
}
```

## Client Runtime

### Starting a Client

```haskell
startMcpClient :: 
    Tracer IO Trace ->
    McpSimpleBinaryConfiguration -> 
    IO (Either McpError Runtime)
startMcpClient tracer config = do
    -- Spawn process
    let procConfig = (proc (unpack config.executable) 
                            (map unpack config.args))
           { std_in = CreatePipe
           , std_out = CreatePipe
           , std_err = Inherit
           }
    (stdinH, stdoutH, _, procH) <- createProcess_ config.name procConfig
    
    -- Initialize communication
    runtime <- Runtime procH stdinH stdoutH 
                <$> newTVarIO []
                <*> newTVarIO Map.empty
                <*> pure config.name
    
    -- Query tools
    tools <- queryTools runtime
    atomically $ writeTVar (toolsList runtime) tools
    
    return $ Right runtime
```

### Tool Execution

```haskell
callTool :: 
    Runtime -> 
    Text ->       -- Tool name
    Value ->      -- Arguments
    IO (Either McpError Value)
callTool rt toolName args = do
    callId <- nextCallId
    
    -- Send call request
    sendRequest rt "tools/call" $ object
        [ "name" .= toolName
        , "arguments" .= args
        ]
    
    -- Wait for response
    waitForResponse rt callId
```

## Server Mode

The framework can expose agents as MCP servers:

### Starting MCP Server Mode

```bash
agents-exe mcp-server --agent-file agent.json
```

### Server Capabilities

```haskell
data McpServerConfig = McpServerConfig
    { serverName :: Text
    , serverVersion :: Text
    , supportedProtocols :: [ProtocolVersion]
    }

defaultMcpServerConfig :: McpServerConfig
defaultMcpServerConfig = McpServerConfig
    { serverName = "agents-mcp-server"
    , serverVersion = "1.0.0"
    , supportedProtocols = ["2024-11-05"]
    }
```

### Agent-as-Tool Mapping

Each loaded agent is exposed as an MCP tool:

```haskell
agentToMcpTool :: AgentTree.AgentTree -> ToolDescription
agentToMcpTool tree = ToolDescription
    { toolName = agentSlug tree
    , toolDescription = agentAnnounce tree
    , toolInputSchema = object
        [ "type" .= ("object" :: Text)
        , "properties" .= object
            [ "prompt" .= object
                [ "type" .= ("string" :: Text)
                , "description" .= ("Prompt to send to the agent" :: Text)
                ]
            ]
        , "required" .= (["prompt"] :: [Text])
        ]
    }
```

## Error Handling

### MCP Errors

```haskell
data McpError
    = ProcessStartError Text
    | ProtocolError Text
    | ToolNotFound Text
    | ToolCallError Text
    | ParseError Text
    | TimeoutError
```

### Error Response Format

```json
{
  "jsonrpc": "2.0",
  "error": {
    "code": -32600,
    "message": "Invalid Request",
    "data": "Additional error details"
  },
  "id": null
}
```

## Tracing

MCP operations are traced for debugging:

```haskell
data Trace
    = McpClientClientTrace ClientTrace
    | McpClientRunTrace RunTrace
    | McpClientLoopTrace LoopTrace
    
data ClientTrace
    = SendingRequest CallId Method Value
    | ReceivedResponse CallId Value
    | ReceivedError CallId JsonRpcError

data RunTrace
    = RunCommandStart Text
    | RunCommandStopped Text ExitCode
    | RunBufferMoved ByteString ByteString

data LoopTrace
    = ToolsRefreshed [ToolDescription]
    | StartToolCall Text Value
    | EndToolCall Text Value Value
    | ExitingToolCallLoop
```

## Best Practices

### Server Implementation

1. **Idempotency**: Tools should be safe to call multiple times
2. **Timeouts**: Set reasonable timeouts for operations
3. **Validation**: Validate all inputs before execution
4. **Error messages**: Return clear, actionable error messages
5. **Resource cleanup**: Properly cleanup on process exit

### Client Usage

1. **Reconnect logic**: Handle server crashes gracefully
2. **Tool caching**: Cache tool lists but refresh periodically
3. **Concurrent calls**: Be aware of server concurrency limits
4. **Input validation**: Validate arguments before sending

## Example: Filesystem MCP Server

```json
{
  "tag": "McpSimpleBinary",
  "contents": {
    "name": "filesystem",
    "executable": "npx",
    "args": ["-y", "@modelcontextprotocol/server-filesystem", "/home/user/docs"]
  }
}
```

## Example: Custom MCP Server

A minimal MCP server in Python:

```python
#!/usr/bin/env python3
import json
import sys

def send_message(msg):
    print(json.dumps(msg))
    sys.stdout.flush()

def handle_request(request):
    method = request.get("method")
    
    if method == "initialize":
        return {
            "protocolVersion": "2024-11-05",
            "capabilities": {},
            "serverInfo": {"name": "example", "version": "1.0.0"}
        }
    
    elif method == "tools/list":
        return {
            "tools": [
                {
                    "name": "echo",
                    "description": "Echo back the input",
                    "inputSchema": {
                        "type": "object",
                        "properties": {
                            "message": {"type": "string"}
                        },
                        "required": ["message"]
                    }
                }
            ]
        }
    
    elif method == "tools/call":
        params = request.get("params", {})
        name = params.get("name")
        args = params.get("arguments", {})
        
        if name == "echo":
            return {
                "content": [{"type": "text", "text": args.get("message", "")}],
                "isError": False
            }
    
    return None

def main():
    while True:
        line = sys.stdin.readline()
        if not line:
            break
        
        request = json.loads(line)
        result = handle_request(request)
        
        if "id" in request:
            send_message({
                "jsonrpc": "2.0",
                "result": result,
                "id": request["id"]
            })

if __name__ == "__main__":
    main()
```

## Debugging

Enable MCP tracing with verbose logging:

```bash
agents-exe run --agent-file agent.json --log-http http://localhost:8080/log
```

View MCP communications in the trace output:

```json
{
  "e": {
    "server": "filesystem",
    "val": {"x": "tool-call-start", "name": "read_file"}
  }
}
```

