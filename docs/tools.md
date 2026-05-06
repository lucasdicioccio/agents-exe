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
| **System Tools** | System information | Runtime context and session introspection |
| **Developer Tools** | Development utilities | Agent/tool scaffolding, file editing |
| **IO Tools** | Haskell functions | In-process operations |
| **Lua Tools** | Lua scripts | Embedded scripting |
| **Skills** | Progressive disclosure | Procedural knowledge |

## Media Types and Multi-Modal Support

The framework supports media attachments and multi-modal responses for LLM interactions.

### Media Type Classification

```haskell
-- System.Agents.Media.Types
data MediaType
    = MediaImage ImageType
    | MediaAudio AudioType
    | MediaVideo VideoType
    | MediaApplication ApplicationType
    | MediaText TextSubtype

data ImageType = ImagePNG | ImageJPEG | ImageGIF | ImageWebP | ImageSVG
data AudioType = AudioMPEG | AudioWAV | AudioOGG | AudioMP3 | AudioAAC | AudioFLAC
data VideoType = VideoMP4 | VideoWebM | VideoOGG | VideoAVI | VideoMOV
data ApplicationType = AppPDF | AppJSON | AppXML | AppOctetStream | AppZip
data TextSubtype = TextPlain | TextHTML | TextCSS | TextCSV | TextMarkdown
```

### Media Attachments

```haskell
data MediaAttachment = MediaAttachment
    { mediaMimeType :: Text        -- e.g., "image/png"
    , mediaBase64Data :: Text      -- Base64-encoded content
    , mediaFilename :: Maybe Text  -- Optional filename
    }
```

### Content Parts for Mixed Responses

```haskell
data ContentPart
    = TextPart Text
    | MediaPart MediaAttachment
```

### Declaring Media Output in Scripts

Tools can declare their output media type in the describe output:

```bash
#!/bin/bash

if [ "$1" == "describe" ]; then
    echo '{
        "slug": "generate_chart",
        "description": "Generates a chart image",
        "args": [...],
        "output-media-type": "image/png"
    }'
    exit 0
fi

# Generate and output PNG image to stdout
./generate-chart "$@"
```

Supported media types:
- **Images**: `image/png`, `image/jpeg`, `image/gif`, `image/webp`, `image/svg+xml`
- **Audio**: `audio/mpeg`, `audio/wav`, `audio/ogg`, `audio/mp3`, `audio/aac`, `audio/flac`
- **Video**: `video/mp4`, `video/webm`, `video/ogg`, `video/avi`, `video/quicktime`
- **Documents**: `application/pdf`, `application/json`, `application/xml`, `application/zip`
- **Generic**: `application/octet-stream`

### Tool Result Types with Media

```haskell
data CallResult call
    = -- | Successful execution with optional media type hint
      BlobToolSuccess call ByteString (Maybe MediaType)
    | -- ... other constructors

data UserToolResponse
    = TextResponse Text                    -- Plain UTF-8 text
    | JsonResponse Aeson.Value             -- Structured JSON data
    | MediaResponse MediaAttachment        -- Single binary media
    | MixedResponse [ContentPart]          -- Multi-modal content
```

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
    , scriptOutputMediaType :: Maybe Text  -- NEW: Media type for binary output
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
| `attach-file` | Attach a file to the conversation |
| `list-sessions` | List accessible sessions (requires session introspection config) |
| `search-sessions` | Full-text search across sessions (requires session introspection config) |
| `read-session` | Read session content (requires session introspection config) |
| `get-session-stats` | Get session statistics (requires session introspection config) |

### Configuration

```json
{
  "builtinToolboxes": [
    {
      "tag": "SystemToolbox",
      "contents": {
        "name": "system",
        "description": "System context and information",
        "capabilities": ["date", "operating-system", "running-user", "hostname", "attach-file"],
        "envVarFilter": null
      }
    }
  ]
}
```

### Attach-File Capability

The `attach-file` capability allows the agent to attach files to the conversation for multi-modal LLM interactions:

```haskell
-- Tool accepts:
{
  "capability": "attach-file",
  "filepath": "/path/to/image.png"
}

-- Returns:
-- MediaAttachment with base64-encoded content
```

**Supported file types:**
- **Images**: PNG, JPEG, GIF, WEBP, SVG
- **Audio**: MP3, WAV, OGG, AAC, FLAC
- **Video**: MP4, WEBM, MOV, AVI
- **Documents**: PDF, JSON, XML
- **Generic**: Any file (as octet-stream)

**Limits:**
- Maximum file size: 50MB

### Session Introspection Capabilities

The System Toolbox supports session introspection capabilities that allow agents to query, search, and read other sessions from the session store. This enables cross-session analysis and context sharing.

#### Session Introspection Scope

Access control is managed through `SessionIntrospectionScope`:

| Scope | Description |
|-------|-------------|
| `parents-only` | Can only see parent sessions (ancestors via forkedFromSessionId) |
| `children-only` | Can only see child sessions (descendants) |
| `subtree` | Parents + current + children (default) |
| `all` | All sessions (requires explicit opt-in) |

#### Configuration with Session Introspection

```json
{
  "tag": "SystemToolbox",
  "contents": {
    "name": "system",
    "description": "System information and session memory",
    "capabilities": [
      "date",
      "hostname",
      "list-sessions",
      "search-sessions",
      "read-session",
      "get-session-stats"
    ],
    "sessionIntrospectionScope": "subtree",
    "sessionIntrospectionMaxResults": 50,
    "sessionIntrospectionIncludeToolOutputs": false
  }
}
```

#### list-sessions

Lists accessible sessions based on the configured scope.

**Input:**
```json
{
  "capability": "list-sessions"
}
```

**Output:**
```json
{
  "sessions": [
    {
      "sessionId": "uuid",
      "conversationId": "uuid",
      "modificationTime": "2024-01-15T10:30:00Z",
      "turnCount": 15,
      "isParent": false,
      "isChild": true,
      "isLocked": false,
      "status": "idle"
    }
  ],
  "totalAccessible": 42
}
```

#### search-sessions

Performs full-text search across accessible sessions.

**Input:**
```json
{
  "capability": "search-sessions",
  "query": "error handling pattern"
}
```

**Output:**
```json
{
  "query": "error handling pattern",
  "results": [
    {
      "sessionId": "uuid",
      "conversationId": "uuid",
      "turnCount": 15,
      "preview": "...context around match...",
      "matchType": "content"
    }
  ],
  "totalMatches": 5,
  "scope": "subtree"
}
```

#### read-session

Reads session content with optional slicing and filtering.

**Input:**
```json
{
  "capability": "read-session",
  "session_id": "target-session-uuid",
  "take_n": 10,
  "include_thinking": false,
  "include_tool_responses": true
}
```

**Parameters:**
- `session_id` (string, required): Session UUID to read
- `take_n` (number, optional): Take last N turns (alternative to offset/limit)
- `drop_n` (number, optional): Drop first N turns
- `offset` (number, optional): Starting turn index (0-based)
- `limit` (number, optional): Max turns to return
- `include_thinking` (boolean, optional): Include LLM thinking/reasoning (default: false)
- `include_tool_responses` (boolean, optional): Include tool call responses (default: false)

**Output:**
```json
{
  "sessionId": "target-session-uuid",
  "conversationId": "uuid",
  "totalTurns": 25,
  "returnedTurns": 10,
  "content": "Turn 1: [User] ...\nTurn 2: [LLM] ...",
  "format": "condensed-text",
  "scope": "subtree",
  "access": "granted"
}
```

#### get-session-stats

Returns aggregate statistics about accessible sessions.

**Input:**
```json
{
  "capability": "get-session-stats"
}
```

**Output:**
```json
{
  "totalSessions": 42,
  "totalTurnsAcrossAllSessions": 850,
  "scope": "subtree",
  "note": "Use SessionPrint.calculateStatistics for detailed per-session stats"
}
```

### Configuration Fields

| Field | Type | Description |
|-------|------|-------------|
| `name` | string | Unique name for this toolbox instance |
| `description` | string | Human-readable description |
| `capabilities` | [string] | List of enabled capabilities |
| `envVarFilter` | string? | Optional substring filter for env vars |
| `sessionIntrospectionScope` | string? | Scope of accessible sessions (default: "subtree") |
| `sessionIntrospectionMaxResults` | number? | Max sessions to return (default: 50) |
| `sessionIntrospectionIncludeToolOutputs` | boolean? | Include tool outputs in read operations (default: true) |

### Security Considerations

- **Capability-based access**: Only enabled capabilities are exposed
- **Session scope enforcement**: Strict access control via `SessionIntrospectionScope`
- **ScopeAll requires explicit opt-in**: Must be explicitly configured, never default
- **Env var filtering**: Use `envVarFilter` to limit variable exposure
- **Read-only**: System tools gather information but cannot modify the system
- **Linux-focused**: Initial implementation targets Linux systems

### LLM Tool Interface

The system toolbox exposes a single tool named `system_{name}_system_info` with:

- **Parameter**: `capability` (string) - Which system info to retrieve
- **Additional parameters**: Vary by capability (see individual capability documentation)
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
| `read-file-range` | Reads specific line ranges from a file |
| `write-file-range` | Replaces line ranges in a file with new content |
| `patch-file` | Applies a unified diff patch to a file |

### Configuration

```json
{
  "builtinToolboxes": [
    {
      "tag": "DeveloperToolbox",
      "contents": {
        "name": "dev",
        "description": "Development utilities",
        "capabilities": ["validate-tool", "scaffold-agent", "scaffold-tool", "read-file-range", "write-file-range", "patch-file"]
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

#### read-file-range

Reads specific line ranges from a file and returns them with line numbers.

**Parameters:**
```json
{
  "capability": "read-file-range",
  "path": "/path/to/file",
  "ranges": "1-10,20-30"
}
```

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `path` | string | Yes | Path to the file to read |
| `ranges` | string | No | Line ranges (e.g., `"1-10"`, `"5"`, `"head"`, `"tail"`, `"1-5,20-30"`). Omit to read entire file. |

**Range Formats:**
- Single line: `"5"` - Reads line 5
- Line range: `"1-10"` - Reads lines 1 through 10
- Multiple ranges: `"1-5,20-30"` - Reads lines 1-5 and 20-30
- Head: `"head"` - Reads from beginning (no-op for read)
- Tail: `"tail"` - Reads to end (no-op for read)

**Returns:**
```json
{
  "path": "/path/to/file",
  "content": "1\tdef hello():\n2\t    print('Hello, World!')\n3\t    return True\n",
  "linesRead": 3
}
```

The content includes line numbers prepended with a tab separator in the format `{line_num}\t{line_content}`.

**Example Output:**
```
1	def hello():
2	    print("Hello, World!")
3	    return True
```

**Error Responses:**
```json
{
  "error": "File not found: /path/to/file"
}
```

#### write-file-range

Replaces specific lines in a file with new content. Supports multiple ranges processed sequentially with position tracking.

**Parameters:**
```json
{
  "capability": "write-file-range",
  "path": "/path/to/file",
  "ranges": "1-2,5-6",
  "contentBlocks": ["new content for lines 1-2", "new content for lines 5-6"]
}
```

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `path` | string | Yes | Path to the file to modify |
| `ranges` | string | Yes | Comma-separated line numbers or ranges (e.g., `"2,5,8"` or `"1-3,7-9"`) |
| `contentBlocks` | array[string] | Yes | Array of content blocks, one per range. Use empty strings to delete lines. |

**Range Formats:**
- Single line: `"5"` - Replaces line 5
- Line range: `"1-10"` - Replaces lines 1 through 10
- Multiple ranges: `"1-5,20-30"` - Replaces multiple separate ranges
- Head: `"head"` - Prepends content before line 1
- Tail: `"tail"` - Appends content after last line

**Processing:**
- Ranges are processed in **ascending order** (top-to-bottom)
- Each edit's line numbers are adjusted by the running offset from previous edits
- Position tracking ensures correct line targeting when adding/removing lines
- File is written atomically (temp file + rename)

**Examples:**

```json
// Replace single line
{
  "capability": "write-file-range",
  "path": "File.hs",
  "ranges": "5",
  "contentBlocks": ["new content for line 5"]
}

// Replace multiple individual lines
{
  "capability": "write-file-range",
  "path": "File.hs",
  "ranges": "2,5,8",
  "contentBlocks": [
    "replace line 2",
    "replace line 5",
    "replace line 8"
  ]
}

// Delete lines (empty content blocks)
{
  "capability": "write-file-range",
  "path": "File.hs",
  "ranges": "3,7",
  "contentBlocks": ["", ""]
}

// Replace ranges with multi-line content
{
  "capability": "write-file-range",
  "path": "File.hs",
  "ranges": "1-2,5-6",
  "contentBlocks": [
    "new line 1\nnew line 2",
    "new line 5\nnew line 6"
  ]
}
```

**Returns:**
```json
{
  "path": "/path/to/file",
  "rangesModified": 2,
  "linesWritten": 6
}
```

**Behavior:**
- **Preserves trailing newline:** If the original file ends with a newline, the output will too
- **Creates file for head/tail:** If file doesn't exist and using `head` or `tail`, creates the file
- **Error for missing file:** Returns error if file doesn't exist for non-head/tail operations
- **Sequential processing:** Multiple ranges are processed top-to-bottom with automatic position adjustment

**Error Responses:**
```json
{
  "error": "Number of content blocks (1) must match total lines in ranges (2)"
}
```

#### patch-file

Applies a unified diff patch to a file atomically with context validation.

**Parameters:**
```json
{
  "capability": "patch-file",
  "path": "/path/to/file",
  "patch": "--- a/src/File.hs\n+++ b/src/File.hs\n@@ -10,5 +10,6 @@ import Foo\n+import Data.Text (Text)\n@@ -100,5 +101,5 @@ func1 x =\n-  oldBody\n+  newBody"
}
```

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `path` | string | Yes | Path to the file to patch |
| `patch` | string | Yes | Unified diff patch content |

**Patch Format:**
Follows standard unified diff format:
- File headers (`---` and `+++` lines) are ignored
- Hunk headers start with `@@` (e.g., `@@ -10,5 +11,6 @@`)
- Context lines have no prefix
- Removed lines start with `-`
- Added lines start with `+`

**Features:**
- **Atomic application:** All hunks are validated before any changes are applied
- **Context validation:** Each hunk's context lines must match exactly
- **Overlap detection:** Hunks that would overlap are rejected
- **Bottom-to-top application:** Hunks are applied in descending line order to avoid line number shifts

**Returns:**
```json
{
  "path": "/path/to/file",
  "hunksApplied": 2,
  "hunksRejected": 0,
  "linesChanged": 3
}
```

**Error Responses:**
```json
{
  "error": "Context mismatch at line 100: Context before hunk doesn't match"
}
```

### Range Specification Types

```haskell
-- | Range specification for file operations.
data RangeSpec
    = Lines (Int, Int)  -- ^ 1-based, inclusive line range (start, end)
    | Head              -- ^ Before line 1 (prepend)
    | Tail              -- ^ After last line (append)
    deriving (Show, Eq)

-- | Result of a read file range operation.
data ReadFileRangeResult = ReadFileRangeResult
    { readFilePath :: FilePath
    , readFileContent :: Text
    , readFileLinesRead :: Int
    }

-- | Result of a write file range operation.
data WriteFileRangeResult = WriteFileRangeResult
    { writeFilePath :: FilePath
    , writeFileRangesModified :: Int
    , writeFileLinesWritten :: Int
    }

-- | Result of a patch file operation.
data PatchResult = PatchResult
    { patchFilePath :: FilePath
    , patchHunksApplied :: Int
    , patchHunksRejected :: Int
    , patchLinesChanged :: Int
    }
```

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

## Lua Tools

Lua tools provide embedded scripting capabilities through a sandboxed Lua interpreter. This enables agents to orchestrate complex workflows by combining multiple tools through Lua scripts.

### Recursive Language Models (LRM) with Lua

The Lua toolbox enables a powerful pattern called **Recursive Language Models (LRM)**. By configuring an agent to reference itself in `extraAgents` and granting the Lua toolbox permission to call the resulting `io_prompt_agent_{slug}` tool, you create an agent that can recursively invoke itself for complex reasoning tasks.

### Configuration

```json
{
  "tag": "OpenAIAgentDescription",
  "contents": {
    "slug": "local_lrmlua",
    "flavor": "KimiV1",
    "modelUrl": "https://api.moonshot.ai/v1",
    "apiKeyId": "kimi",
    "modelName": "kimi-k2.5",
    "announce": "The main agent, capable of reasoning.",
    "systemPrompt": [
      "You are a helpful software agent trying to solve user requests"
    ],
    "extraAgents": [
      {
        "slug": "local_lrmlua",
        "path": "kimi-10.lrmlua.json"
      }
    ],
    "bashToolboxes": [
      {
        "tag": "SingleTool",
        "contents": "./tools/askuser/ask-user.bash"
      }
    ],
    "builtinToolboxes": [
      {
        "tag": "LuaToolbox",
        "contents": {
          "name": "lua",
          "description": "Sandboxed Lua interpreter",
          "maxMemoryMB": 256,
          "maxExecutionTimeSeconds": 300,
          "allowedTools": [
            "bash_ask_user",
            "io_prompt_agent_local_lrmlua",
            "sqlite_shared_working_memory_query"
          ],
          "allowedPaths": [
            "./repro-cases",
            "./README.md"
          ],
          "allowedHosts": [
            "localhost",
            "127.0.0.1"
          ]
        }
      },
      {
        "tag": "SqliteToolbox",
        "contents": {
          "name": "shared_working_memory",
          "description": "A base to help you coordinate large works.",
          "path": "./dev-memory.db",
          "access": "read-write"
        }
      }
    ]
  }
}
```

### LuaToolbox Configuration Fields

| Field | Type | Description |
|-------|------|-------------|
| `name` | string | Unique name for this toolbox instance (used as tool prefix) |
| `description` | string | Human-readable description |
| `maxMemoryMB` | integer | Maximum Lua heap memory in megabytes |
| `maxExecutionTimeSeconds` | integer | Maximum script execution time in seconds |
| `allowedTools` | [string] | Whitelist of tool names Lua scripts can call via the portal |
| `allowedPaths` | [string] | Whitelist of filesystem paths accessible to Lua scripts |
| `allowedHosts` | [string] | Whitelist of network hosts accessible to Lua HTTP module |

### Security Features

The Lua toolbox provides a sandboxed execution environment:

- **Memory limits**: Lua state memory is constrained via allocator hooks
- **Timeout enforcement**: Scripts that exceed `maxExecutionTimeSeconds` are terminated
- **Path sandboxing**: Filesystem access restricted to `allowedPaths` (prevents symlink traversal)
- **Host whitelisting**: HTTP requests limited to `allowedHosts`
- **Tool whitelist**: Only tools in `allowedTools` can be called through the portal
- **Dangerous functions removed**: `os.execute`, `io.popen`, `loadfile`, `dofile`, etc. are removed
- **Empty whitelist = no access**: Secure defaults - empty lists mean no access

### Lua Standard Library Modules

Scripts have access to these built-in modules:

| Module | Functions | Description |
|--------|-----------|-------------|
| `json` | `encode`, `decode`, `encode_pretty` | JSON manipulation |
| `http` | `get`, `post`, `put`, `delete` | HTTP requests (host-restricted) |
| `time` | `now`, `sleep`, `format` | Time utilities |
| `fs` | `read_file`, `write_file`, `list_dir` | File system (sandboxed) |
| `text` | `split`, `join`, `trim`, `match` | String utilities |
| `tools` | `call` | Tool portal integration |

### Example Lua Script

```lua
local json = require("json")
local tools = require("tools")

-- Call the recursive agent for complex reasoning
local reasoning_result = tools.call("io_prompt_agent_local_lrmlua", {
    what = "Analyze this codebase structure and identify potential refactoring opportunities"
})

-- Process the result
if reasoning_result.status == "ok" then
    local analysis = json.decode(reasoning_result.result_txt)
    
    -- Query the shared working memory
    local db_result = tools.call("sqlite_shared_working_memory_query", {
        sql = "INSERT INTO analysis_results (content) VALUES ('" .. analysis.summary .. "')"
    })
    
    return {
        success = true,
        analysis = analysis,
        stored = db_result.status == "ok"
    }
else
    return {
        success = false,
        error = reasoning_result.error
    }
end
```

### Tool Naming

The Lua toolbox exposes a single tool named `lua_{name}_execute`:

```json
{
  "script": "local json = require('json'); return json.encode({status='ok'})",
  "timeout": 60
}
```

### Error Handling

Lua script errors are captured and returned to the LLM:

```haskell
data ScriptError
    = LuaRuntimeError [Aeson.Value]      -- Syntax or runtime error
    | TimeoutError Int                   -- Script exceeded time limit
    | MemoryError Int                    -- Script exceeded memory limit
    | SandboxError Text                  -- Attempted sandbox violation
    | ToolInvocationError Text           -- Error calling another tool
    | InitializationError Text           -- Failed to initialize Lua state
```

## Skills System

The Skills system provides procedural knowledge and executable capabilities via progressive disclosure, following the [agentskills.io](https://agentskills.io) specification.

### Overview

Skills are packages of related functionality that can be dynamically enabled/disabled during a session. They implement progressive disclosure:

1. **Initially**: Only metadata tools are visible (describe, enable, disable, list)
2. **After enable**: Script tools become available for execution

```
Session Start
      │
      ▼
┌─────────────┐
│ skill_list  │  ← Always available
│ skill_desc  │  ← Always available
│ skill_enable│  ← Always available
└──────┬──────┘
       │
       ▼ (user calls skill_enable_pdf-processing)
┌─────────────┐
│ skill_list  │
│ skill_desc  │
│ skill_enable│
│ skill_pdf_* │  ← NEW: Script tools now visible
└─────────────┘
```

### Skill Structure

A skill directory contains:

```
skill-directory/
├── SKILL.md          # Frontmatter + instructions
├── scripts/          # Executable scripts
│   ├── extract-text.sh
│   └── convert.sh
└── references/       # Documentation (optional)
    └── api-docs.md
```

### SKILL.md Format

```markdown
---
name: pdf-processing
description: Extract and manipulate PDF files
license: MIT
compatibility: Linux, macOS
metadata:
  author: team-pdf
  version: "1.0"
---

# PDF Processing

This skill provides tools for working with PDF documents.

## Usage

Enable the skill, then use the available script tools...
```

### Skill Types (`System.Agents.Tools.Skills.Types`)

```haskell
-- | Validated skill name (1-64 chars, lowercase, digits, hyphens)
newtype SkillName = SkillName { unSkillName :: Text }

-- | Complete skill with metadata, instructions, scripts, and references
data Skill = Skill
    { skillMetadata :: SkillMetadata
    , skillInstructions :: Text
    , skillPath :: FilePath
    , skillScripts :: [ScriptInfo]
    , skillReferences :: [ReferenceInfo]
    }

-- | Metadata from SKILL.md frontmatter
data SkillMetadata = SkillMetadata
    { smName :: SkillName
    , smDescription :: Text
    , smLicense :: Maybe Text
    , smCompatibility :: Maybe Text
    , smMetadata :: Map Text Text
    }

-- | Script following the describe/run protocol
data ScriptInfo = ScriptInfo
    { siName :: ScriptName
    , siPath :: FilePath
    , siDescription :: Maybe Text
    , siArgs :: [ScriptArgInfo]
    }
```

### Skill State

```haskell
-- | Session state tracking which skills and scripts are enabled
newtype SkillsSessionState = SkillsSessionState
    { sssActiveSkills :: Map SkillName SkillScriptsState
    }

-- | Script state within a skill
type SkillScriptsState = Map ScriptName ScriptState
data ScriptState = Enabled | Disabled

-- | Monoid instance for folding over session turns
instance Monoid SkillsSessionState where
    mempty = SkillsSessionState Map.empty
    -- Later state overrides earlier state
```

### Toolbox Integration (`System.Agents.Tools.Skills.Toolbox`)

```haskell
-- | Compute all available skill tools from session state
computeSkillTools :: SkillsStore -> Session -> [ToolRegistration]
computeSkillTools store session =
    let state = foldSession session
        -- Always available
        metaTools = concatMap makeMetaTools (allSkills store)
        -- Available only when enabled
        scriptTools = 
            concatMap (makeScriptToolsForSkill state store) 
                      (sssActiveSkills state)
     in listTool ++ metaTools ++ scriptTools

-- Tool naming convention
skill2LLMName :: Text -> SkillName -> ToolName
-- skill_describe_pdf-processing
-- skill_enable_pdf-processing
-- skill_pdf-processing_extract-text
```

### Generated Tools

For each skill, these tools are generated:

| Tool | Purpose | Always Available |
|------|---------|------------------|
| `skill_list` | List all skills | Yes |
| `skill_describe_{name}` | Get skill metadata | Yes |
| `skill_enable_{name}` | Enable skill scripts | Yes |
| `skill_disable_{name}` | Disable skill scripts | Yes |
| `skill_{name}_{script}` | Execute script | No (requires enable) |

### Skill Sources

Skills can be loaded from:

```haskell
data SkillSource
    = SkillDirectory FilePath           -- Local directory
    | SkillGitRepo GitUrl (Maybe Subdirectory)  -- Git repository
```

Configuration:

```json
{
  "skillSources": [
    { "tag": "SkillDirectory", "contents": "./skills" },
    { "tag": "SkillGitRepo", 
      "contents": { 
        "url": "https://github.com/org/skills-repo",
        "subdir": "pdf-tools"
      }
    }
  ],
  "autoEnableSkills": ["core-utils"]
}
```

### Progressive Disclosure Benefits

1. **Reduced context window**: Only relevant tools visible
2. **Discoverability**: Users learn about skills organically
3. **Modularity**: Skills are self-contained packages
4. **Safety**: Scripts only accessible after explicit enable
5. **Auditability**: State changes tracked in session

## Tool Validation (`System.Agents.Tools.Validation`)

Tool input validation helps LLMs self-correct when they make incorrect tool calls.

### Validation Types

```haskell
-- | Single validation error with context
data ValidationError = ValidationError
    { errorPath :: Text       -- JSON path (e.g., "user.name")
    , errorMessage :: Text    -- Human-readable description
    }

-- | Validation configuration
data ValidationConfig = ValidationConfig
    { allowExtraProperties :: Bool
    , strictMode :: Bool
    }
```

### Validation Function

```haskell
-- | Validate tool input against its schema
validateToolInput :: 
    [ParamProperty] ->  -- Tool schema
    Aeson.Value ->      -- Input value
    [ValidationError]   -- Empty if valid

-- Example usage:
let errors = validateToolInput toolSchema inputValue
case errors of
    [] -> proceedWithToolCall
    errs -> returnValidationErrors errs
```

### Supported Validations

| Check | Description |
|-------|-------------|
| Required fields | Ensures required properties are present |
| Type checking | Validates string, number, boolean, enum, object |
| Enum values | Checks string is in allowed values list |
| Nested objects | Recursively validates nested structures |
| Extra properties | Optionally rejects unknown properties |

### CLI: check-tool-call

```bash
# Validate a tool call payload
echo '{"filepath": "/path/to/file"}' | \
    agents-exe check-tool-call --tool ./tools/read-file.sh

# Example output (invalid):
# Tool call validation failed for 'read-file' with 2 errors:
# 1. filepath: Required property missing
# 2. content: Required property missing
#
# Please correct these issues and try again.
```

### Error Formatting

```haskell
formatValidationErrors :: Text -> [ValidationError] -> Text
-- Produces:
-- Tool call validation failed for 'tool-name' with 2 errors:
--
-- 1. filters.status: Invalid enum value: pending. Allowed: active, inactive
-- 2. user.age: Expected number but got string
--
-- Please correct these issues and try again.
```

## Tool Portal (`System.Agents.ToolPortal`)

The Tool Portal enables inter-toolbox communication, allowing tools to invoke other tools through a controlled callback mechanism.

### Use Cases

- **Lua scripts** calling other tools via `tools.call()`
- **Orchestration tools** that coordinate multiple operations
- **Composite tools** that build on existing tools

### Portal Types

```haskell
-- | Tool portal callback type
type ToolPortal = ToolCall -> IO ToolResult

-- | Portal execution errors
data PortalError
    = PortalToolNotFound Text
    | PortalToolNotAllowed Text [Text]
    | PortalInvalidArguments Text
    | PortalExecutionError Text
```

### Creating a Portal

```haskell
import System.Agents.ToolPortal

-- Create portal from registered tools
let portal = makeToolPortal tracer registrations

-- Create context with portal
let ctx = mkPortalContext
        sessId convId turnId mAgentId mSession
        callStack maxDepth (Just portal) allowedTools
```

### Lua Integration

Lua scripts can call tools through the portal:

```lua
local tools = require("tools")

-- Call another tool
local result = tools.call("read_file", {
    filepath = "/path/to/file"
})

-- Access result
print(result.data)
print(result.duration)
```

### Security

- **Tool whitelist**: Only allowed tools can be called
- **No nested portals**: Prevents infinite recursion
- **Execution tracking**: Each portal call is timed and logged
- **Minimal context**: Portal tools execute without their own portal

### Portal Result

```haskell
data ToolResult = ToolResult
    { resultData :: Aeson.Value
    , resultDuration :: NominalDiffTime
    , resultTraceId :: Text
    }
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
    , ctxToolPortal :: Maybe ToolPortal
    , ctxAllowedTools :: [Text]
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

### OS Integration (Subcall Visibility)

For TUI visibility of subcall conversations, the context includes OS integration fields:

```haskell
data ToolExecutionContext = ToolExecutionContext
    { -- ... existing fields ...
    , ctxWorld :: Maybe World
    -- ^ OS World for ECS operations. Enables subcall conversations
    -- to be tracked as first-class entities in the OS.
    , ctxEventQueue :: Maybe (TQueue OSEvent)
    -- ^ Event queue for OS event emission. Enables the TUI to receive
    -- notifications about subcall lifecycle (start, progress, completion).
    , ctxParentConversation :: Maybe ConversationId
    -- ^ Parent conversation ID for nested agent calls.
    }
```

**Helper Functions:**
```haskell
-- | Create a nested context for subcall execution.
mkSubcallContext ::
    ToolExecutionContext ->
    Maybe World ->
    Maybe (TQueue OSEvent) ->
    ConversationId ->
    ToolExecutionContext

-- | Get the subcall depth (0 for root conversations).
getSubcallDepth :: ToolExecutionContext -> Int

-- | Check if this context is for a subcall.
isSubcallContext :: ToolExecutionContext -> Bool
```

## Tool Result Types

```haskell
data CallResult call
    = BlobToolSuccess call ByteString (Maybe MediaType)
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
    | DeveloperToolAgentValidationResult call AgentValidationResult
    | DeveloperToolCreateResult call CreateResult
    | DeveloperToolReadFileRangeResult call ReadFileRangeResult
    | DeveloperToolWriteFileRangeResult call WriteFileRangeResult
    | DeveloperToolPatchResult call PatchResult
    | LuaToolResult call Aeson.Value
    | LuaToolError call Text
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
| Lua | `lua_{toolbox}_execute` | `lua_utils_execute` |
| IO | `io_{slug}` | `io_calculator` |
| IO (Agent) | `io_prompt_agent_{slug}` | `io_prompt_agent_helper` |
| Skill (meta) | `skill_{action}_{name}` | `skill_describe_pdf-processing` |
| Skill (script) | `skill_{name}_{script}` | `skill_pdf-processing_extract-text` |

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
    
    -- Lua tools from builtin toolboxes
    luaTools <- readLuaToolsRegistrations tracer luaToolboxes
    
    -- Skills tools from skill sources
    skillsStore <- loadSkillsFromSources skillSources
    let skillsTools = computeSkillTools skillsStore session
    
    -- Combine all
    let allTools = ioTools ++ bashTools ++ mcpTools ++ openApiTools ++ 
                   postgrestTools ++ sqliteTools ++ systemTools ++ 
                   devTools ++ luaTools ++ skillsTools
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
    | FileNotFoundError FilePath
    | FileTooLargeError FilePath Int
    | UnsupportedFileTypeError FilePath Text
    | SessionStoreNotConfiguredError
    | SessionNotFoundError Text
    | InvalidSessionIdError Text
    | SessionAccessDeniedError Text SessionIntrospectionScope
    | MissingParameterError Text
```

### Developer Toolbox Errors

```haskell
data DeveloperToolError
    = CapabilityNotEnabledError Text
    | ValidationError Text
    | ScaffoldError Text
    | FileExistsError FilePath
    | InvalidTemplateError Text
    | InvalidRangeError Text
    | RangeOutOfBoundsError Text
    | PermissionError Text
    | PatchValidationError PatchError
```

### Patch Errors

```haskell
data PatchError
    = PatchParseError Text
    | PatchContextMismatch Int Text
    | PatchHunkOverlap Int Int
    | PatchFileNotFound FilePath
    | PatchInvalidLineNumber Int
```

### Validation Errors

```haskell
data ValidationError = ValidationError
    { errorPath :: Text
    , errorMessage :: Text
    }
```

### Portal Errors

```haskell
data PortalError
    = PortalToolNotFound Text
    | PortalToolNotAllowed Text [Text]
    | PortalInvalidArguments Text
    | PortalExecutionError Text
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
9. **Progressive disclosure**: Use Skills for complex tool suites
10. **Portal safety**: Always whitelist tools for portal access
11. **Recursive agents**: When using LRM pattern, set appropriate maxDepth to prevent infinite recursion
12. **Lua security**: Always specify allowedTools, allowedPaths, and allowedHosts - empty means no access
13. **Media output**: Declare `output-media-type` for tools that produce binary content
14. **Mixed responses**: Use `MixedResponse` for rich multi-modal tool outputs
15. **File range operations**: Use `read-file-range` and `write-file-range` for precise file editing rather than reading/writing entire files
16. **Line numbers**: When using `read-file-range`, the output includes line numbers to help LLMs understand file structure
17. **Range formatting**: Always use 1-based line numbers for ranges (e.g., "1-10" for lines 1 through 10)
18. **Atomic file edits**: For complex multi-range edits, use `write-file-range` with contentBlocks array
19. **Patch for context validation**: Use `patch-file` when context validation is needed before applying changes
20. **Session introspection**: Enable session introspection capabilities in SystemToolbox for cross-session analysis

## Example: Complete Tool Configuration

```json
{
  "slug": "file-agent",
  "apiKeyId": "openai",
  "flavor": "openai",
  "modelUrl": "https://api.openai.com/v1",
  "modelName": "gpt-4o",
  "announce": "A file management assistant with vision",
  "systemPrompt": ["You help users manage and analyze files."],
  "toolDirectory": "tools",
  "bashToolboxes": [
    { "path": "./extra-tools", "name": "extras" }
  ],
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
        "description": "System context and session memory",
        "capabilities": ["date", "hostname", "working-directory", "attach-file", "list-sessions", "search-sessions"],
        "envVarFilter": null,
        "sessionIntrospectionScope": "subtree",
        "sessionIntrospectionMaxResults": 50,
        "sessionIntrospectionIncludeToolOutputs": false
      }
    },
    {
      "tag": "DeveloperToolbox",
      "contents": {
        "name": "dev",
        "description": "Development utilities",
        "capabilities": ["validate-tool", "scaffold-agent", "scaffold-tool", "read-file-range", "write-file-range", "patch-file"]
      }
    },
    {
      "tag": "LuaToolbox",
      "contents": {
        "name": "lua",
        "description": "Lua scripting tools",
        "maxMemoryMB": 256,
        "maxExecutionTimeSeconds": 300,
        "allowedTools": ["bash_read_file", "sqlite_analytics_query", "system_system_attach_file"],
        "allowedPaths": ["./scripts", "./data"],
        "allowedHosts": ["localhost"]
      }
    }
  ],
  "skillSources": [
    { "tag": "SkillDirectory", "contents": "./skills" }
  ],
  "autoEnableSkills": ["core-utils"],
  "extraAgents": [
    {"slug": "helper", "path": "./helper.json"}
  ]
}
```

## Related Modules

| Module | Purpose |
|--------|---------|
| `System.Agents.Media.Types` | Media types for multi-modal support |
| `System.Agents.Tools.Base` | Core tool types |
| `System.Agents.Tools.Context` | Tool execution context |
| `System.Agents.Tools.Bash` | Bash script execution |
| `System.Agents.Tools.BashToolbox` | Bash tool management |
| `System.Agents.Tools.McpToolbox` | MCP server integration |
| `System.Agents.Tools.OpenAPIToolbox` | OpenAPI conversion |
| `System.Agents.Tools.SqliteToolbox` | SQLite tools |
| `System.Agents.Tools.SystemToolbox` | System information and session introspection |
| `System.Agents.Tools.DeveloperToolbox` | Development utilities |
| `System.Agents.Tools.LuaToolbox` | Lua scripting |
| `System.Agents.Tools.Skills.Toolbox` | Skills system |
| `System.Agents.Tools.Skills.Types` | Skill types |
| `System.Agents.Tools.Validation` | Input validation |
| `System.Agents.ToolPortal` | Inter-tool communication |
| `System.Agents.ToolRegistration` | Tool registration |
| `System.Agents.ToolSchema` | Schema definitions |
| `System.Agents.OS.Events` | OS event types for subcall visibility |

