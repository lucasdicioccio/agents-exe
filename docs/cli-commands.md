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

### describe

Output self-describing schema for the agent.

```bash
agents-exe describe
```

**Output:** JSON schema for tool calling.

**Use case:** Integration with external systems that need to understand the agent's interface.

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
agents-exe init --agent-file ./my-agent.json

# 2. Edit configuration
# (edit my-agent.json)

# 3. Check configuration
agents-exe check --agent-file ./my-agent.json

# 4. Test with one-shot
agents-exe run --agent-file ./my-agent.json --prompt "Test"

# 5. Start interactive session
agents-exe tui --agent-file ./my-agent.json

# 6. Export for sharing
agents-exe export --agent-file ./my-agent.json --output ./my-agent.tar.gz

# 7. Import elsewhere
agents-exe import --from-file ./my-agent.tar.gz --to ./new-location/
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
  --agent-file ./coder.json \
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

