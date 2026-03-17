# CLI Reference

Complete reference for the Agents CLI commands and options.

## Global Options

These options apply to all commands:

```bash
agents-exe [GLOBAL_OPTS] COMMAND [COMMAND_OPTS]
```

| Option | Default | Description |
|--------|---------|-------------|
| `--api-keys FILE` | `~/.config/agents-exe/secret-keys` | Path to API keys JSON file |
| `--agent-file FILE` | (from config) | Agent configuration file(s) |
| `--log-file FILE` | `agents-logfile` | Raw log output file |
| `--log-http URL` | - | HTTP endpoint for JSON logs |
| `--log-json-file FILE` | - | Local JSON log file |
| `--session-json-file-prefix PREFIX` | - | Prefix for session JSON files |

## Commands

### check

Validate agent configuration and display loaded tools.

```bash
agents-exe check [--agent-file FILE]
```

**Output:**
```
my-agent: A helpful file assistant (12 tools)
```

**Exit codes:**
- `0` - All agents loaded successfully
- `1` - Configuration errors found

**Example:**
```bash
# Check default agents
agents-exe check

# Check specific agent
agents-exe check --agent-file ./custom-agent.json
```

### run

Execute a one-shot agent conversation.

```bash
agents-exe run [OPTIONS]
```

**Options:**

| Option | Description |
|--------|-------------|
| `--session-file FILE` | Resume from existing session |
| `--prompt TEXT` | Initial prompt text |
| `--file FILE` | Read prompt from file |
| `--shell COMMAND` | Use shell command output as prompt |
| `--sep4 TEXT` | Short separator (4 chars) |
| `--sep40 TEXT` | Long separator (40 chars) |

**Examples:**

```bash
# Simple prompt
agents-exe run --agent-file agent.json --prompt "Hello!"

# Read from file
agents-exe run --agent-file agent.json --file prompt.txt

# Shell command output as prompt
agents-exe run --agent-file agent.json --shell "git diff"

# Resume session
agents-exe run --agent-file agent.json --session-file session.json

# Multi-part prompt with separators
agents-exe run \
  --prompt "Review this code:" \
  --sep4 "----" \
  --file code.py \
  --sep40 "========================================" \
  --prompt "What improvements can be made?"
```

### tui

Start the interactive Terminal UI.

```bash
agents-exe tui [--agent-file FILE...]
```

**Features:**
- Real-time streaming responses
- Multiple agent support (Tab to switch)
- Tool call visualization
- Session persistence

**Keyboard Shortcuts:**
- `Tab` / `Shift+Tab` - Switch between agents
- `Enter` - Send message
- `Ctrl+C` - Quit
- `Up/Down` - Scroll history

**Examples:**

```bash
# Single agent TUI
agents-exe tui --agent-file agent.json

# Multi-agent TUI
agents-exe tui \
  --agent-file coder.json \
  --agent-file reviewer.json \
  --agent-file tester.json
```

### mcp-server

Start an MCP (Model Context Protocol) server.

```bash
agents-exe mcp-server [--agent-file FILE...]
```

**Description:**
Exposes loaded agents as MCP tools for integration with MCP clients like Claude Desktop.

**Examples:**

```bash
# Single agent MCP server
agents-exe mcp-server --agent-file agent.json

# Multi-agent MCP server
agents-exe mcp-server \
  --agent-file research-agent.json \
  --agent-file writing-agent.json
```

### echo-prompt

Process and echo the prompt without calling the LLM.

```bash
agents-exe echo-prompt [OPTIONS]
```

**Options:** Same as `run` command.

**Use case:** Verify prompt construction before sending to LLM.

**Example:**
```bash
agents-exe echo-prompt \
  --prompt "Context:" \
  --file context.txt \
  --prompt "Question:" \
  --file question.txt
```

### session-print

Display a session file in markdown format.

```bash
agents-exe session-print [OPTIONS] SESSIONFILE
```

**Options:**

| Option | Description |
|--------|-------------|
| `--show-tool-call-results` | Include tool call outputs |
| `--n-turns N` | Limit to last N turns |
| `--repeat-system-prompt` | Show system prompt each turn |
| `--repeat-tools` | Show available tools each turn |
| `--antichronological` | Newest first (default: oldest first) |

**Examples:**

```bash
# Print full session
agents-exe session-print session.json

# Print with tool results
agents-exe session-print --show-tool-call-results session.json

# Last 5 turns only
agents-exe session-print --n-turns 5 session.json

# Reverse order
agents-exe session-print --antichronological session.json
```

### list-tool-calls

Extract and list all tool calls from a session file.

```bash
agents-exe list-tool-calls [OPTIONS] SESSIONFILE
```

**Options:**

| Option | Description |
|--------|-------------|
| `-f, --format FORMAT` | Output format: human, json, brief (default: human) |

**Description:**
Parses a session file and extracts all tool calls made during the conversation.
Useful for debugging, auditing, and replaying specific tool calls.

**Output formats:**
- `human` - Detailed human-readable format with indices, turn numbers, and arguments
- `json` - JSON array for machine processing
- `brief` - Compact tabular format (index, name, arguments preview)

**Examples:**

```bash
# List tool calls in human-readable format
agents-exe list-tool-calls session.json

# Output as JSON
agents-exe list-tool-calls session.json --format json

# Brief format for quick overview
agents-exe list-tool-calls session.json --format brief
```

**Example output (human format):**
```
Found 3 tool call(s):

[0] bash_read-file
  Turn: 1
  Arguments: 
    {"filepath":"./src/Main.hs"}

[1] bash_grep-files
  Turn: 2
  Arguments: 
    {"pattern":"TODO","filepath":"./src"}

[2] bash_write-file
  Turn: 2
  Arguments: 
    {"filepath":"./src/Main.hs","content":"..."}

To replay a tool call:
  agents-exe replay-tool-call --session <file> --tool-call <index> --tool <tool-path>
```

### replay-tool-call

Replay a specific tool call from a session file, with validation.

```bash
agents-exe replay-tool-call --session FILE --tool-call INDEX --tool TOOLPATH [OPTIONS]
```

**Options:**

| Option | Description |
|--------|-------------|
| `-s, --session FILE` | Path to the session file containing the tool call |
| `-i, --tool-call INDEX` | Index of the tool call to replay (0-based) |
| `-t, --tool TOOLPATH` | Path to the tool script to execute |
| `--validate-only` | Only validate arguments, don't execute |
| `--raw` | Show raw output instead of formatted |

**Description:**
Extracts a tool call from a session file, validates its arguments against the tool's schema,
and optionally executes the tool with the same arguments. This is useful for:

- Debugging failed tool calls
- Reproducing tool execution for testing
- Validating historical tool calls against updated schemas

**Exit codes:**
- `0` - Validation passed (and tool executed if not --validate-only)
- `1` - Validation failed or tool execution failed

**Examples:**

```bash
# First, list tool calls to find the index
agents-exe list-tool-calls session.json

# Validate and execute a tool call
agents-exe replay-tool-call \
  --session session.json \
  --tool-call 0 \
  --tool ./tools/bash/read-file.sh

# Only validate without executing
agents-exe replay-tool-call \
  --session session.json \
  --tool-call 1 \
  --tool ./tools/bash/grep-files.sh \
  --validate-only

# Execute and show raw output
agents-exe replay-tool-call \
  --session session.json \
  --tool-call 2 \
  --tool ./tools/bash/write-file.sh \
  --raw
```

**Example output:**
```
✓ Tool call validation passed
Tool: read-file
Arguments: {"filepath":"./src/Main.hs"}

Executing tool...

=== Tool Output ===
module Main where

main :: IO ()
main = putStrLn "Hello, World!"
===================
```

### init

Initialize a new agent configuration.

```bash
agents-exe init [--agent-file FILE]
```

**Creates:**
- `agent.json` - Agent configuration
- `tools/` - Tool directory
- `secret-keys` - API keys file (if doesn't exist)

**Example:**
```bash
agents-exe init --agent-file ./my-agent.json
```

### new

Create scaffolding for new agents or tools.

```bash
agents-exe new (agent|tool) [OPTIONS]
```

#### new agent

Create a new agent configuration file from a template.

```bash
agents-exe new agent [OPTIONS] SLUG
```

**Options:**

| Option | Description |
|--------|-------------|
| `--file FILE` | Output file path (default: `./{slug}.json`) |
| `--preset PRESET` | Model preset: `openai`, `mistral`, `ollama` (default: `openai`) |
| `--model MODEL` | Override the model name from preset |
| `--force` | Overwrite existing file |

**Presets:**

| Preset | Model URL | Default Model | API Key ID |
|--------|-----------|---------------|------------|
| `openai` | https://api.openai.com/v1 | gpt-4-turbo-preview | main-key |
| `mistral` | https://api.mistral.ai/v1 | mistral-large-latest | mistral-key |
| `ollama` | http://localhost:11434/v1 | llama3.2 | ollama-key |

**Examples:**

```bash
# Create agent with OpenAI preset (default)
agents-exe new agent my-assistant

# Create agent with specific preset
agents-exe new agent my-assistant --preset mistral

# Create agent with custom model
agents-exe new agent coder --preset openai --model gpt-4o

# Create agent with custom output path
agents-exe new agent my-assistant --file ./agents/assistant.json

# Overwrite existing
agents-exe new agent my-assistant --force
```

**Generated agent file:**
```json
{
  "agent": {
    "slug": "my-assistant",
    "apiKeyId": "main-key",
    "flavor": "OpenAIv1",
    "modelUrl": "https://api.openai.com/v1",
    "modelName": "gpt-4-turbo-preview",
    "announce": "a helpful assistant powered by gpt-4-turbo-preview",
    "systemPrompt": [
      "You are my-assistant, a helpful AI assistant.",
      "You provide clear, accurate, and concise responses.",
      "When using tools, you explain your actions to the user."
    ],
    "toolDirectory": "tools",
    "mcpServers": [],
    "openApiToolboxes": null,
    "postgrestToolboxes": null,
    "builtinToolboxes": [],
    "extraAgents": null
  }
}
```

#### new tool

Create a new tool script from a template.

```bash
agents-exe new tool [OPTIONS] SLUG
```

**Options:**

| Option | Description |
|--------|-------------|
| `--file FILE` | Output file path (default: `./tools/{slug}`) |
| `--language LANG` | Language: `bash`, `python`, `haskell`, `node` (default: `bash`) |
| `--force` | Overwrite existing file |

**Examples:**

```bash
# Create bash tool (default)
agents-exe new tool my-tool

# Create Python tool
agents-exe new tool my-tool --language python

# Create Haskell tool
agents-exe new tool my-tool --language haskell

# Create Node.js tool
agents-exe new tool my-tool --language node

# Custom output path
agents-exe new tool my-tool --file ./scripts/my-tool.sh
```

**Generated bash tool:**
```bash
#!/bin/bash
# my-tool - A bash tool for agents-exe

set -euo pipefail

# Agents-exe tool protocol: describe|run
# Environment variables available during 'run':
#   AGENT_SESSION_ID      - UUID of the current session
#   AGENT_CONVERSATION_ID - UUID of the conversation
#   AGENT_TURN_ID         - UUID of the current turn
#   AGENT_AGENT_ID        - UUID of the executing agent (if available)

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
    # Access arguments via environment or command line
    echo "Tool my-tool executed"
    ;;
  *)
    echo "Usage: my-tool <describe|run>" >&2
    exit 1
    ;;
esac
```

**Next steps after creating a tool:**
1. Edit the `args` array in the describe function
2. Implement the `run` function logic
3. Test with: `agents-exe describe-tool ./tools/my-tool`

### spec

Display embedded specification documentation.

```bash
agents-exe spec TOPIC
```

**Topics:**

| Topic | Description |
|-------|-------------|
| `bash-tools` | Binary tool protocol specification |

**Examples:**

```bash
# Display bash-tools specification
agents-exe spec bash-tools
```

**Use case:** Learn the protocol for writing bash tools without leaving the CLI.

### describe-tool

Display information about a tool script.

```bash
agents-exe describe-tool TOOL_PATH
```

**Description:**
Loads and displays the tool's description (the output of `tool describe`).

**Example:**
```bash
agents-exe describe-tool ./tools/my-tool.sh
```

**Output:**
```json
{
  "slug": "my-tool",
  "description": "What this tool does",
  "args": [...],
  "empty-result": {...}
}
```

### check-tool-call

Validate a tool call payload against a tool schema (reads JSON from stdin).

```bash
cat payload.json | agents-exe check-tool-call --tool TOOLPATH
```

**Options:**

| Option | Description |
|--------|-------------|
| `-t, --tool TOOLPATH` | Path to the tool script to validate against |

**Description:**
Validates that a JSON payload from stdin matches the tool's declared schema.
Returns exit code 0 if valid, 1 if invalid with detailed error messages.

**Example:**
```bash
# Create a test payload
echo '{"filepath": "/path/to/file"}' | agents-exe check-tool-call --tool ./tools/read-file.sh

# Validate from file
cat payload.json | agents-exe check-tool-call --tool ./tools/my-tool.sh
```

**Example output (valid):**
```
✓ Tool call payload is valid
```

**Example output (invalid):**
```
Tool call validation failed for './tools/my-tool.sh (my-tool)' with 2 errors:

1. filepath: Required property missing

2. content: Required property missing

Please correct these issues and try again.
```

### export

Export agent/tool configurations to archive or git.

```bash
agents-exe export [OPTIONS]
```

**Source Options:**

| Option | Description |
|--------|-------------|
| `--all` | Export all loaded agents |
| `--agent-slug SLUG` | Export specific agent by slug |
| `--tools-only` | Export only tools, not agent config |
| `--tool TOOLNAME` | Export specific tool |

**Destination Options:**

| Option | Description |
|--------|-------------|
| `-o, --output FILE` | Output file path |
| `--git-url URL` | Git remote URL |
| `--git-branch BRANCH` | Git branch |
| `--git-message MESSAGE` | Commit message |
| `--git-push` | Push after commit |
| `--git-tag TAG` | Create git tag |

**Format Options:**

| Option | Description |
|--------|-------------|
| `--format FORMAT` | Archive format: tar, tar.gz, zip |
| `--namespace NAMESPACE` | Namespace for export (e.g., "team.project") |
| `--no-tools` | Exclude tools from export |
| `--no-mcp` | Exclude MCP servers from export |

**Examples:**

```bash
# Export to tar.gz
agents-exe export --output ./my-agent.tar.gz

# Export to git
agents-exe export \
  --git-url https://github.com/user/agents-repo \
  --git-branch main \
  --git-message "Update agent" \
  --git-push

# Export tools only
agents-exe export --tools-only --output ./tools.tar.gz

# Export specific agent
agents-exe export --agent-slug my-agent --output ./agent.tar.gz

# With namespace
agents-exe export --namespace team-a.project-1 --output ./export.tar.gz
```

### import

Import agent/tool configurations from archive or git.

```bash
agents-exe import [OPTIONS]
```

**Source Options:**

| Option | Description |
|--------|-------------|
| `-f, --from-file FILE` | Import from archive file |
| `--git-url URL` | Import from git URL |
| `--git-ref REF` | Git ref (branch, tag, commit) |
| `--namespace NAMESPACE` | Namespace to import from |

**Destination Options:**

| Option | Description |
|--------|-------------|
| `--to-current` | Import to current directory |
| `--to PATH` | Import to specific path |
| `--to-config-dir` | Import to config directory |
| `--install-to-agent FILE` | Install tools to agent's tool directory |
| `--install-to-tooldir DIR` | Install tools to specific directory |

**Mode Options:**

| Option | Description |
|--------|-------------|
| `--overwrite` | Overwrite existing files |
| `--merge` | Merge with existing files |
| (default) | Fail on conflict |

**List Options:**

| Option | Description |
|--------|-------------|
| `--list-namespaces` | List available namespaces (git only) |
| `--list-tools` | List available tools (git only) |

**Examples:**

```bash
# Import from archive
agents-exe import --from-file ./agent.tar.gz

# Import from git
agents-exe import \
  --git-url https://github.com/user/agents-repo \
  --git-ref main

# Import tools to agent
agents-exe import \
  --from-file ./tools.tar.gz \
  --install-to-agent ./my-agent.json

# Import with overwrite
agents-exe import --from-file ./agent.tar.gz --overwrite

# List namespaces
agents-exe import --git-url https://github.com/user/repo --list-namespaces

# Import specific namespace
agents-exe import \
  --git-url https://github.com/user/repo \
  --namespace team-a.tools \
  --to ./tools/
```

### session-edit

Edit a session file.

```bash
agents-exe session-edit [OPTIONS] SESSIONFILE
```

**Options:**

| Option | Description |
|--------|-------------|
| `--compress-turns N` | Compress turns older than N |
| `--remove-tool-calls` | Remove all tool calls |
| `--remove-empty-messages` | Remove empty messages |

**Examples:**

```bash
# Compress old turns
agents-exe session-edit --compress-turns 10 session.json

# Clean up session
agents-exe session-edit --remove-empty-messages session.json
```

### describe

Output self-describing schema for the agent.

```bash
agents-exe describe
```

**Output:** JSON schema for tool calling.

**Use case:** Integration with external systems that need to understand the agent's interface.

### cowsay

Display a fun message (utility command).

```bash
agents-exe cowsay [MESSAGE]
```

**Example:**
```bash
agents-exe cowsay "Hello, agents!"
```

### self-describe

Output information about the agents-exe binary itself.

```bash
agents-exe self-describe
```

**Output:** Version, build info, and available commands.

## Configuration File

Project-level configuration in `agents-exe.cfg.json`:

```json
{
  "agentsConfigDir": "/custom/config/path",
  "agentsDirectories": [
    "./agents",
    "./more-agents"
  ],
  "agentsFiles": [
    "./main-agent.json"
  ],
  "agentsLogs": {
    "logJsonHttpEndpoint": "http://localhost:8080/log",
    "logJsonPath": "./logs/agents.json",
    "logRawPath": "./logs/agents.log",
    "logSessionsJsonPrefix": "./sessions/session"
  }
}
```

**Search order:**
1. Current directory
2. Parent directories (upward search)

## Environment Variables

| Variable | Description |
|----------|-------------|
| `AGENTS_API_KEY` | Default API key (overrides file) |
| `AGENTS_CONFIG_DIR` | Config directory path |
| `AGENTS_LOG_LEVEL` | Logging verbosity |

## Exit Codes

| Code | Meaning |
|------|---------|
| `0` | Success |
| `1` | General error |
| `2` | Invalid arguments |
| `3` | Configuration error |
| `4` | Agent loading error |
| `5` | Tool execution error |
| `10` | Export error |
| `11` | Import error |

## Examples

### Complete Workflow

```bash
# 1. Initialize new agent
agents-exe new agent my-assistant

# 2. Create a custom tool
agents-exe new tool file-reader --language python

# 3. Edit tool and agent configurations
# (edit files as needed)

# 4. Check configuration
agents-exe check --agent-file ./my-assistant.json

# 5. Test with one-shot
agents-exe run --agent-file ./my-assistant.json --prompt "Test"

# 6. Start interactive session
agents-exe tui --agent-file ./my-assistant.json

# 7. Export for sharing
agents-exe export --agent-file ./my-assistant.json --output ./my-assistant.tar.gz

# 8. Import elsewhere
agents-exe import --from-file ./my-assistant.tar.gz --to ./new-location/
```

### Multi-Agent Setup

```bash
# Start multi-agent TUI
agents-exe tui \
  --agent-file ./router.json \
  --agent-file ./coder.json \
  --agent-file ./reviewer.json

# Start multi-agent MCP server
agents-exe mcp-server \
  --agent-file ./router.json \
  --agent-file ./coder.json
```

### Automation

```bash
# Process all files in directory
for file in *.txt; do
    agents-exe run \
        --agent-file ./processor.json \
        --prompt "Process this file:" \
        --file "$file" \
        --prompt "End of file."
done

# Git pre-commit hook
agents-exe run \
    --agent-file ./reviewer.json \
    --shell "git diff --cached" \
    --prompt "Review these changes."
```

### Developer Workflow

```bash
# Learn the tool protocol
agents-exe spec bash-tools

# Create and validate a tool
agents-exe new tool my-validator --language bash
# (edit the tool)
agents-exe describe-tool ./tools/my-validator
agents-exe check-tool-call ./tools/my-validator --arg input="test"

# Create agent with dev tools
agents-exe new agent dev-assistant
# (add DeveloperToolbox to builtinToolboxes)
# Now the agent can validate tools and scaffold new ones!
```

### Debugging Tool Calls

```bash
# Run an agent session
agents-exe run --agent-file ./my-agent.json --prompt "Read the README"

# Later, inspect the session
cat conv.*.json | tail -1 | jq .

# List all tool calls from the session
agents-exe list-tool-calls conv.*.json

# Replay a specific tool call for debugging
agents-exe replay-tool-call \
  --session conv.*.json \
  --tool-call 0 \
  --tool ./tools/read-file.sh \
  --validate-only

# Actually replay the tool call
agents-exe replay-tool-call \
  --session conv.*.json \
  --tool-call 0 \
  --tool ./tools/read-file.sh
```

