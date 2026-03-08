# CLI Reference

Complete reference for `agents-exe` command-line interface.

## Global Options

These options are available for all commands:

```
--api-keys FILE          Path to JSON file containing API keys
--log-file FILE          Raw log file (default: agents-logfile)
--log-json-file FILE     JSON log file
--log-http URL           HTTP log sink
--session-json-file-prefix PREFIX   Prefix for session JSON files
--agent-file FILE        Root agent description file (can be specified multiple times)
```

## Commands

### check

Validate agent configuration files.

```bash
agents-exe check --agent-file agent.json
```

### tui

Launch the interactive terminal UI.

```bash
agents-exe tui --agent-file agent.json
```

### run

Run a one-shot agent invocation.

```bash
agents-exe run --agent-file agent.json --prompt "Hello!"
agents-exe run --agent-file agent.json --file prompt.txt
agents-exe run --agent-file agent.json --session-file session.json --prompt "Continue"
```

Options:
- `--session-file FILE` - Resume from or store session
- `--prompt TEXT` - Prompt text paragraph
- `--file FILE` - Read prompt from file
- `--shell CMD` - Use shell command output as prompt
- `--sep4 TEXT` - Short separator
- `--sep40 TEXT` - Long separator

### echo-prompt

Print the rendered prompt without running the agent.

```bash
agents-exe echo-prompt --prompt "Hello!"
```

### describe

Self-describe the agent capabilities.

```bash
agents-exe describe --agent-file agent.json
```

### init

Initialize a new agent configuration.

```bash
agents-exe init --agent-file agent.json
```

### mcp-server

Run an MCP server with agent indirection (tools call agents).

```bash
agents-exe mcp-server --agent-file agent.json
```

### session-print

Print a session file in markdown format.

```bash
agents-exe session-print session.json
agents-exe session-print session.json --show-tool-call-results
agents-exe session-print session.json --n-turns 10
agents-exe session-print session.json --antichronological
```

Options:
- `--show-tool-call-results` - Show tool call results
- `--n-turns N` - Limit to first N turns
- `--repeat-system-prompt` - Repeat system prompt each turn
- `--repeat-tools` - Repeat tools list each turn
- `--antichronological` - Show newest first

### export

Export agent/tool configurations.

```bash
# Export current agent
agents-exe export --agent-file agent.json --output export.tar.gz

# Export all agents
agents-exe export --all --output export.tar.gz

# Export by slug
agents-exe export --agent-slug my-agent --output export.tar.gz

# Export tools only
agents-exe export --tools-only --output tools.tar.gz

# Export specific tool
agents-exe export --tool my-tool --output tool.tar.gz

# Export to git
agents-exe export --git-url https://github.com/user/repo.git --git-push

# Export with namespace
agents-exe export --agent-file agent.json --namespace team.project --output export.tar.gz
```

Options:
- `--format FORMAT` - Archive format: tar, tar.gz, zip
- `--no-tools` - Exclude tools from export
- `--no-mcp` - Exclude MCP servers from export
- `--git-branch BRANCH` - Git branch
- `--git-message MESSAGE` - Commit message
- `--git-tag TAG` - Create git tag

### import

Import agent/tool configurations.

```bash
# Import from file
agents-exe import --from-file archive.tar.gz

# Import from git
agents-exe import --git-url https://github.com/user/repo.git

# Import tools only
agents-exe import --from-file archive.tar.gz --tools-only

# Import with overwrite
agents-exe import --from-file archive.tar.gz --overwrite

# List namespaces in git repo
agents-exe import --git-url https://github.com/user/repo.git --list-namespaces

# List tools in git repo
agents-exe import --git-url https://github.com/user/repo.git --list-tools
```

Options:
- `--to PATH` - Import to specific path
- `--to-current` - Import to current directory
- `--to-config-dir` - Import to config directory
- `--install-to-agent AGENTFILE` - Install tools to agent
- `--install-to-tooldir TOOLDIR` - Install tools to directory
- `--merge` - Merge with existing files
- `--namespace NAMESPACE` - Import from namespace

## Toolbox Commands

The `toolbox` command group provides direct tool manipulation without requiring agent instantiation.

### toolbox list

List available tools from various sources.

```bash
# List tools from an agent
agents-exe toolbox list --agent-file agent.json

# List tools from a directory
agents-exe toolbox list --tool-dir ./tools

# List tools from MCP config
agents-exe toolbox list --mcp-config mcp.json
```

Options:
- `--format FORMAT` - Output format: table (default), json, names

**Output Formats:**

- `table` - Human-readable table with name, type, and description
- `json` - JSON array with tool details
- `names` - One tool name per line

### toolbox nest

Create a nested bash-compatible tool from multiple tools.

```bash
# Basic usage
agents-exe toolbox nest --agent-file agent.json --output combined-tool

# With custom name
agents-exe toolbox nest --agent-file agent.json --output combined-tool --name my-tool

# Include only specific tools
agents-exe toolbox nest --agent-file agent.json --output combined-tool \
  --include-tool tool1 --include-tool tool2

# Exclude specific tools
agents-exe toolbox nest --agent-file agent.json --output combined-tool \
  --exclude-tool tool3
```

Options:
- `--agent-file FILE` - Path to agent JSON file
- `--output FILE` - Output path for nested tool script
- `--name NAME` - Name for the nested tool (defaults to agent slug)
- `--include-tool TOOL` - Include specific tool (can be specified multiple times)
- `--exclude-tool TOOL` - Exclude specific tool (can be specified multiple times)

The generated script implements the standard bash-tool interface:

```bash
./combined-tool describe              # Show ScriptInfo JSON
./combined-tool run --tool=<name> --arg='<json>'  # Execute a sub-tool
```

**Use Cases:**

- Testing MCP tools using simple bash scripts
- Creating composite tools that delegate to multiple underlying tools
- Using tools in environments that only support bash-style tool invocation
- Packaging agent tools as standalone utilities

### toolbox mcp

Run an MCP server directly from tools without agent indirection.

```bash
# From agent file
agents-exe toolbox mcp --agent-file agent.json

# From tool directory
agents-exe toolbox mcp --tool-dir ./tools

# From MCP toolbox
agents-exe toolbox mcp --mcp-toolbox myserver:config.json

# Multiple sources
agents-exe toolbox mcp \
  --agent-file agent1.json \
  --tool-dir ./extra-tools \
  --mcp-toolbox extra:extra.json

# With OpenAPI spec
agents-exe toolbox mcp --openapi-spec spec.json

# With PostgREST config
agents-exe toolbox mcp --postgrest-config config.json
```

Options:
- `--agent-file FILE` - Load tools from agent configuration
- `--tool-dir DIR` - Load bash tools directly from directory
- `--mcp-toolbox NAME:CONFIG` - Load tools from MCP server
- `--openapi-spec FILE` - Load tools from OpenAPI specification
- `--postgrest-config FILE` - Load tools from PostgREST configuration
- `--server-name NAME` - Server name (default: agents-exe-toolbox-mcp)
- `--server-version VERSION` - Server version (default: 0.1.0)
- `--stdio` - Use stdio transport (default)
- `--http-port PORT` - Use HTTP transport (not yet implemented)
- `--log-level LEVEL` - Log level: debug, info, warning, error
- `--keep-prefixes` - Keep LLM prefixes in tool names
- `--strip-prefixes` - Strip LLM prefixes (default)
- `--use-original-names` - Use original tool names

**Name Mapping Strategies:**

- `keep-prefixes` - Keep prefixes like `bash_`, `mcp_`, `openapi_`, `postgrest_`
- `strip-prefixes` - Remove prefixes (e.g., `bash_hello` becomes `hello`)
- `use-original-names` - Use original names where available

**Use Cases:**

- Testing MCP tool clients without setting up a full agent
- Combining tools from multiple sources into a single MCP server
- Running lightweight tool servers without LLM configuration
- Debugging tool behavior without agent complexity
- Exposing tools directly to MCP-compatible clients

## Environment Variables

Tools executed by agents have access to these environment variables:

- `AGENT_SESSION_ID` - Current session UUID
- `AGENT_CONVERSATION_ID` - Conversation UUID
- `AGENT_TURN_ID` - Current turn UUID
- `AGENT_AGENT_ID` - Agent UUID (if available)
- `AGENT_SESSION_JSON` - Full session as JSON (if available)

## Exit Codes

- `0` - Success
- `1` - General error
- Other codes may be returned by specific commands

## See Also

- `docs/internal-tool-mapping.md` - Tool name mapping internals

