# Adding New Executable-Program Tools

Executable-program tools (also called "bash tools") are external programs that `agents-exe` can invoke as part of agent execution. These tools extend agent capabilities by performing side effects like reading files, installing packages, probing the network, or interacting with external APIs.

## Tool Protocol Overview

Bash tools must adhere to a simple protocol with two commands:

### 1. The `describe` Command

When called with a single `describe` argument (no other parameters), the tool must return a JSON description of its interface:

```bash
$ ./my-tool describe
{
  "slug": "my_tool",
  "description": "What this tool does",
  "args": [
    {
      "name": "arg_name",
      "description": "Argument description",
      "type": "string",
      "backing_type": "string",
      "arity": "single",
      "mode": "dashdashspace"
    }
  ],
  "empty-result": { "tag": "AddMessage", "contents": "No results found" }
}
```

### 2. The `run` Command

When called with `run` followed by arguments, the tool executes its function and **must** write output to stdout:

```bash
$ ./my-tool run --arg-name value
Tool output here
```

## JSON Description Format

### Top-Level Fields

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `slug` | String | Yes | Unique identifier for the tool (no spaces) |
| `description` | String | Yes | Human-readable description for the LLM |
| `args` | Array | Yes | List of argument definitions |
| `empty-result` | Object | No | Behavior when tool returns empty output |

### Argument Definition

Each argument in the `args` array has the following fields:

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `name` | String | Yes | Argument name (used in JSON input) |
| `description` | String | Yes | Description for the LLM |
| `type` | String | Yes | Semantic type (e.g., "string", "number") |
| `backing_type` | String | Yes | Implementation type (e.g., "string") |
| `arity` | String | Yes | "single" or "optional" |
| `mode` | String | Yes | How to pass the argument (see below) |

### Argument Modes

The `mode` field determines how arguments are passed to the script:

| Mode | Behavior | Example |
|------|----------|---------|
| `positional` | Added as consecutive arguments | `script run value` |
| `dashdashspace` | Added as `--name value` (two args) | `script run --name value` |
| `dashdashequal` | Added as `--name=value` (one arg) | `script run --name=value` |
| `stdin` | Concatenated to stdin with blank separators | `echo "value" \| script run` |

## Empty Result Behavior

The optional `empty-result` field controls what happens when the tool produces no output:

```json
// Do nothing (default behavior)
{ "tag": "DoNothing" }

// Add a message when output is empty
{ "tag": "AddMessage", "contents": "No results found" }
```

## Session Context via Environment Variables

When a tool is executed within a session, `agents-exe` passes session context via environment variables. This allows tools to access session metadata without requiring the LLM to provide it as explicit arguments.

### Available Environment Variables

| Variable | Description | Example Value |
|----------|-------------|---------------|
| `AGENT_SESSION_ID` | UUID of the current session | `550e8400-e29b-41d4-a716-446655440000` |
| `AGENT_CONVERSATION_ID` | UUID of the conversation | `6ba7b810-9dad-11d1-80b4-00c04fd430c8` |
| `AGENT_TURN_ID` | UUID of the current turn | `7c8b9d20-beef-22e2-91c5-11d15fe541d9` |
| `AGENT_AGENT_ID` | UUID of the executing agent (if available) | `a1b2c3d4-e5f6-7890-abcd-ef1234567890` |
| `AGENT_SESSION_JSON` | Full session serialized as JSON (when requested) | `{...}` |

### Use Cases for Context

Tools can use session context for:

- **Audit logging**: Include session IDs in external API calls for traceability
- **Context-aware processing**: Access conversation history to make smarter decisions
- **Correlation**: Tag external telemetry with session identifiers
- **Agent-specific behavior**: Adjust behavior based on which agent is executing the tool

### Example: Tool with Session Context

```bash
#!/bin/bash

if [ "$1" == "describe" ]; then
    cat <<'EOF'
{
  "slug": "session-aware-logger",
  "description": "Logs operations with session context for audit trails",
  "args": [
    {
      "name": "operation",
      "description": "The operation being logged",
      "type": "string",
      "backing_type": "string",
      "arity": "single",
      "mode": "dashdashspace"
    }
  ]
}
EOF
    exit 0
fi

# Access session context from environment (with defaults)
SESSION_ID="${AGENT_SESSION_ID:-unknown}"
CONVERSATION_ID="${AGENT_CONVERSATION_ID:-unknown}"
TURN_ID="${AGENT_TURN_ID:-unknown}"
AGENT_ID="${AGENT_AGENT_ID:-unknown}"

# Parse the operation argument (skipping 'run')
OPERATION=""
while [[ $# -gt 0 ]]; do
    case $1 in
        --operation)
            OPERATION="$2"
            shift 2
            ;;
        *)
            shift
            ;;
    esac
done

# Log with context
echo "[session=$SESSION_ID turn=$TURN_ID] Executing: $OPERATION"

# Optionally access full session JSON for complex operations
if [ -n "$AGENT_SESSION_JSON" ]; then
    # Parse session JSON for additional context
    TURN_COUNT=$(echo "$AGENT_SESSION_JSON" | jq '.turns | length')
    echo "[session=$SESSION_ID] Turn $TURN_COUNT"
fi
```

### Note on Session JSON

The `AGENT_SESSION_JSON` variable is only set when the full session context is available and requested. It contains the complete serialized session, which can be large. Tools should:

1. Check if the variable is set before using it
2. Handle potentially large JSON payloads efficiently
3. Consider using `jq` or similar for parsing

## Complete Example Tool

Here's a complete example of a file-reading tool:

```bash
#!/bin/bash
# File: tools/read-file.sh

if [ "$1" == "describe" ]; then
    cat <<'EOF'
{
  "slug": "read_file",
  "description": "Reads the contents of a file at the specified path",
  "args": [
    {
      "name": "path",
      "description": "Absolute or relative path to the file to read",
      "type": "string",
      "backing_type": "string",
      "arity": "single",
      "mode": "dashdashspace"
    }
  ],
  "empty-result": { "tag": "AddMessage", "contents": "File is empty or could not be read" }
}
EOF
    exit 0
fi

# Extract the path argument
FILE_PATH=""
while [[ $# -gt 0 ]]; do
    case $1 in
        --path)
            FILE_PATH="$2"
            shift 2
            ;;
        *)
            shift
            ;;
    esac
done

if [ -z "$FILE_PATH" ]; then
    echo "Error: No path specified" >&2
    exit 1
fi

if [ ! -f "$FILE_PATH" ]; then
    echo "Error: File not found: $FILE_PATH" >&2
    exit 1
fi

cat "$FILE_PATH"
```

## Tool Discovery

Tools are discovered from the `tools` directory relative to your `agent.json` file:

```
project/
├── agent.json          # Agent definition
└── tools/              # Tool directory
    ├── read-file.sh    # Executable tool script
    ├── git-helper      # Another tool
    └── ...
```

`agents-exe`:
1. Lists all executable files in the tools directory
2. Calls each with `describe` to load its interface
3. Makes successfully loaded tools available to the agent

## Using `agents-exe` as a Tool

`agents-exe` itself adheres to the same protocol, allowing you to nest agents:

```bash
# This allows agents-exe to be used as a tool by another agents-exe instance
agents-exe describe
agents-exe run --prompt "Hello"
```

This is useful for:
- Creating hierarchies of agents with different access rights
- Running agents across container boundaries
- Delegating to specialized sub-agents

## Best Practices

1. **Always validate arguments** - Check for missing or invalid arguments before processing
2. **Write to stdout** - The LLM receives stdout; use stderr for errors
3. **Return non-zero on errors** - Use appropriate exit codes
4. **Keep descriptions concise** - LLMs work better with clear, brief descriptions
5. **Use appropriate modes** - Choose argument modes that make sense for your tool's interface
6. **Handle empty results gracefully** - Consider using `empty-result` to provide helpful messages
7. **Document context usage** - If your tool uses environment variables, document it for users
8. **Provide sensible defaults** - Use bash parameter expansion for optional context variables: `${AGENT_SESSION_ID:-unknown}`

## Debugging Tools

Test your tool independently before using it with `agents-exe`:

```bash
# Test the describe command
./my-tool describe | jq .

# Test the run command
./my-tool run --arg-name value

# Verify exit codes
echo $?  # Should be 0 on success, non-zero on error
```

Use `agents-exe check` to validate all tools in your agent configuration.

