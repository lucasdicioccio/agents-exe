# LuaToolbox Guide

The LuaToolbox provides a sandboxed Lua interpreter for agent-scriptable tool orchestration.

## Overview

The LuaToolbox allows agents to write and execute Lua scripts that can:

- Process data using JSON and text manipulation
- Call other tools through the tool portal
- Perform sandboxed filesystem operations
- Make HTTP requests to whitelisted hosts
- Control execution flow with time functions

## Configuration

Add to your agent configuration:

```json
{
  "builtinToolboxes": [
    {
      "tag": "LuaToolbox",
      "contents": {
        "name": "lua",
        "description": "Sandboxed Lua interpreter",
        "maxMemoryMB": 256,
        "maxExecutionTimeSeconds": 300,
        "allowedTools": ["bash", "sqlite"],
        "allowedPaths": ["./scripts", "./data"],
        "allowedHosts": ["localhost"]
      }
    }
  ]
}
```

### Configuration Options

| Option | Type | Description |
|--------|------|-------------|
| `name` | string | Unique name for this toolbox instance |
| `description` | string | Human-readable description |
| `maxMemoryMB` | integer | Maximum Lua heap memory in megabytes |
| `maxExecutionTimeSeconds` | integer | Maximum script execution time in seconds |
| `allowedTools` | array of strings | Whitelist of tool names that can be called |
| `allowedPaths` | array of strings | Whitelist of filesystem paths accessible |
| `allowedHosts` | array of strings | Whitelist of network hosts accessible |

## Security

The Lua sandbox implements defense-in-depth:

- **Removed dangerous functions**: `os.execute`, `io.popen`, `dofile`, `loadfile`, `package.loadlib`
- **Path sandboxing**: Filesystem access is restricted to `allowedPaths`
- **Host whitelisting**: HTTP requests only allowed to `allowedHosts`
- **Tool whitelist**: Only whitelisted tools can be called through the portal
- **Memory limits**: Enforced via Lua allocator hooks
- **Timeout enforcement**: Scripts are terminated if they exceed the time limit
- **Secure defaults**: Empty whitelist means NO access

### Empty Whitelist = No Access

By design, empty arrays for `allowedPaths` and `allowedHosts` mean NO access:

```json
{
  "allowedPaths": [],    // No filesystem access
  "allowedHosts": []     // No network access
}
```

## Available Modules

### json

Encode/decode JSON data:

```lua
local json = require("json")

-- Encode Lua table to JSON string
local str = json.encode({name = "test", value = 42})
-- Returns: '{"name":"test","value":42}'

-- Decode JSON string to Lua table
local obj = json.decode('{"x": 1, "y": "hello"}')
-- Returns: {x = 1, y = "hello"}
```

**Functions:**
- `json.encode(value)` - Encode a Lua value to JSON string
- `json.decode(string)` - Decode JSON string to Lua value

### text

UTF-8 string operations:

```lua
local text = require("text")

-- Split string by delimiter
local parts = text.split("a,b,c", ",")
-- Returns: {"a", "b", "c"}

-- Find substring position
local start, finish = text.find("hello world", "world")
-- Returns: 7, 11

-- Trim whitespace
local trimmed = text.trim("  hello  ")
-- Returns: "hello"

-- Check prefix/suffix
local starts = text.startswith("hello", "he")  -- true
local ends = text.endswith("hello", "lo")      -- true

-- Case conversion
local lower = text.lower("HELLO")  -- "hello"
local upper = text.upper("hello")  -- "HELLO"

-- String length
local len = text.len("hello")  -- 5

-- Global substitution
local result, count = text.gsub("hello world", "world", "lua")
-- Returns: "hello lua", 1
```

**Functions:**
- `text.split(str, delim)` - Split string by delimiter
- `text.find(str, pattern)` - Find substring position
- `text.trim(str)` - Remove leading/trailing whitespace
- `text.startswith(str, prefix)` - Check if string starts with prefix
- `text.endswith(str, suffix)` - Check if string ends with suffix
- `text.lower(str)` - Convert to lowercase
- `text.upper(str)` - Convert to uppercase
- `text.len(str)` - Get string length
- `text.gsub(str, pattern, replacement)` - Global substitution

### time

Time functions:

```lua
local time = require("time")

-- Get current timestamp (Unix epoch)
local now = time.now()

-- Sleep for seconds
local ok = time.sleep(2)

-- Format timestamp
local formatted = time.format(now, "%Y-%m-%d %H:%M:%S")
-- Returns: "2024-01-15 14:30:00"

-- Calculate difference
local diff = time.diff(timestamp1, timestamp2)
```

**Functions:**
- `time.now()` - Get current timestamp as number (Unix epoch)
- `time.sleep(seconds)` - Sleep for specified seconds
- `time.format(timestamp, format)` - Format timestamp using strftime format
- `time.diff(t1, t2)` - Calculate difference between two timestamps

### fs

Sandboxed filesystem access:

```lua
local fs = require("fs")

-- Read file
local content, err = fs.read("./data/file.txt")
if content then
    print(content)
else
    print("Error: " .. err)
end

-- Write file
local ok, err = fs.write("./data/output.txt", "hello world")

-- Check existence
local exists = fs.exists("./data")

-- Check type
local isFile = fs.isfile("./data/file.txt")
local isDir = fs.isdir("./data")

-- Create directory
local ok = fs.mkdir("./data/newdir")

-- List directory
local entries, err = fs.list("./data")
for i, entry in ipairs(entries) do
    print(entry)
end

-- Patch file (search and replace)
local success, err, diff = fs.patch("./file.txt", "old", "new")
```

**Functions:**
- `fs.read(path)` - Read file contents
- `fs.write(path, content)` - Write file contents
- `fs.exists(path)` - Check if path exists
- `fs.isfile(path)` - Check if path is a file
- `fs.isdir(path)` - Check if path is a directory
- `fs.mkdir(path)` - Create directory
- `fs.list(path)` - List directory contents
- `fs.patch(path, search, replace)` - Search and replace in file

**Security Note:** All paths are validated against `allowedPaths`. Attempts to access paths outside the sandbox will fail.

### http

HTTP requests (host-restricted):

```lua
local http = require("http")

-- GET request
local result = http.get("http://localhost:3000/api")
if result then
    print("Status: " .. result.status)
    print("Body: " .. result.body)
end

-- POST request
local result = http.post("http://localhost:3000/api", '{"key": "value"}')

-- Generic request
local result = http.request({
    method = "PUT",
    url = "http://localhost:3000/api/item",
    body = '{"name": "test"}',
    headers = {
        ["Content-Type"] = "application/json",
        ["Authorization"] = "Bearer token123"
    }
})
```

**Functions:**
- `http.get(url, [options])` - Perform GET request
- `http.post(url, body, [options])` - Perform POST request
- `http.request(options)` - Perform generic HTTP request

**Return value (table):**
- `status` - HTTP status code (number)
- `headers` - Response headers (table)
- `body` - Response body (string)

**Security Note:** All hosts are validated against `allowedHosts`. Attempts to access non-whitelisted hosts will fail.

### tools

Call other tools through the portal:

```lua
local tools = require("tools")

-- Generic tool call
local result = tools.call("bash", {command = "ls -la"})
print(result.data.stdout)

-- Convenience wrappers
local r = tools.bash.run("echo hello")
print(r.data.exitCode, r.data.stdout)

-- SQLite query
local rows = tools.sqlite.query("/path/to/db.sqlite", "SELECT * FROM users")
for _, row in ipairs(rows.data.rows) do
    print(row[1], row[2])
end

-- SQLite execute
local result = tools.sqlite.execute("/path/to/db.sqlite", "INSERT INTO users VALUES (?, ?)", {1, "John"})
print(result.data.lastInsertRowId)

-- PostgREST operations
local users = tools.postgrest.get("/users")
tools.postgrest.post("/users", {name = "New User"})

-- System info
local info = tools.system.info("date")
```

**Functions:**
- `tools.call(toolName, args)` - Generic tool invocation
- `tools.bash.run(command, [opts])` - Execute bash command
- `tools.sqlite.query(db, sql, [params])` - Execute SELECT query
- `tools.sqlite.execute(db, sql, [params])` - Execute non-SELECT statement
- `tools.postgrest.get(endpoint, [opts])` - GET request
- `tools.postgrest.post(endpoint, body, [opts])` - POST request
- `tools.postgrest.patch(endpoint, body, [opts])` - PATCH request
- `tools.postgrest.delete(endpoint, [opts])` - DELETE request
- `tools.system.info(capability)` - Get system information

**Tool Whitelist:** Only tools listed in `allowedTools` can be called.

## Error Handling

Use Lua's `pcall` for error handling:

```lua
local ok, result = pcall(function()
    return tools.bash.run("rm -rf /")
end)

if not ok then
    print("Error: " .. result)
else
    print("Success: " .. result.data.stdout)
end
```

## Examples

### Database Query and Process

```lua
local tools = require("tools")
local json = require("json")

-- Query database
local result = tools.sqlite.query("analytics.db", "SELECT * FROM users LIMIT 10")

-- Process results
local processed = {}
for _, row in ipairs(result.data.rows) do
    table.insert(processed, {
        id = row[1],
        name = row[2],
        email = row[3]
    })
end

-- Save processed data
local fs = require("fs")
fs.write("./output/users.json", json.encode(processed))

return {count = #processed, saved = true}
```

### HTTP API Test

```lua
local http = require("http")
local json = require("json")

local result = http.get("http://localhost:3000/health")
if result.status == 200 then
    local data = json.decode(result.body)
    if data.status == "ok" then
        return {healthy = true, version = data.version}
    else
        return {healthy = false, error = "Service unhealthy"}
    end
else
    return {healthy = false, error = "HTTP " .. result.status}
end
```

### File Processing Workflow

```lua
local fs = require("fs")
local text = require("text")

-- Read input file
local content = fs.read("./data/input.txt")
if not content then
    return {error = "Failed to read input"}
end

-- Process lines
local lines = text.split(content, "\n")
local filtered = {}
for _, line in ipairs(lines) do
    if text.startswith(line, "ERROR:") then
        table.insert(filtered, line)
    end
end

-- Write output
local output = table.concat(filtered, "\n")
fs.write("./output/errors.txt", output)

return {processed = #lines, errors = #filtered}
```

### Tool Orchestration

```lua
local tools = require("tools")
local time = require("time")
local json = require("json")

-- Run system command
local start = time.now()
local result = tools.bash.run("find /var/log -name '*.log' | head -10")

if result.data.exitCode ~= 0 then
    return {error = "Command failed", stderr = result.data.stderr}
end

-- Parse output
local files = {}
for line in result.data.stdout:gmatch("[^\n]+") do
    table.insert(files, line)
end

-- Query database for each file
local fs = require("fs")
local dbResults = {}
for _, file in ipairs(files) do
    local query = string.format("SELECT * FROM logs WHERE path = '%s'", file)
    local dbResult = tools.sqlite.query("logs.db", query)
    table.insert(dbResults, {
        file = file,
        entries = dbResult.data.rowCount
    })
end

-- Generate report
local report = {
    duration = time.diff(time.now(), start),
    files = #files,
    details = dbResults
}

-- Save report
fs.write("./output/report.json", json.encode(report))

return report
```

## Best Practices

1. **Always use `pcall`** for tool calls that might fail
2. **Check return values** - many functions return `nil, error` on failure
3. **Use absolute paths** in `allowedPaths` for security
4. **Limit `maxExecutionTimeSeconds`** to prevent runaway scripts
5. **Start with minimal `allowedTools`** and add as needed
6. **Use the convenience wrappers** (`tools.bash.run`, etc.) for cleaner code
7. **Validate inputs** before passing to tools
8. **Use `json` module** for structured data exchange

## Troubleshooting

### "attempt to call a nil value"

This usually means you're trying to call a function that has been sandboxed. Check the sandbox restrictions.

### "PathNotAllowed" or "PathOutsideSandbox"

The path you're trying to access is not in `allowedPaths`. Add the path to your configuration.

### "Host not in allowed list"

The host is not in `allowedHosts`. Add the hostname to your configuration.

### "Tool not allowed"

The tool is not in `allowedTools`. Add the tool name to your configuration.

### Timeout errors

Increase `maxExecutionTimeSeconds` or optimize your script to run faster.

### Memory errors

Increase `maxMemoryMB` or reduce memory usage in your script.

