# agents-exe

Your favorite agent framework for orchestrating LLM-based agents with extensible tool support.

## Overview

`agents-exe` is a command-line tool for running and managing AI agents that can use various tools to accomplish tasks. It supports:

- Multiple LLM providers (OpenAI, OpenRouter, etc.)
- Bash-based tools (external scripts)
- MCP (Model Context Protocol) servers
- OpenAPI specifications as tool sources
- PostgREST database APIs as tool sources
- Agent composition and delegation

## Installation

```bash
cabal build
```

## Quick Start

Initialize a new agent:
```bash
agents-exe init --agent-file agent.json
```

Run an agent:
```bash
agents-exe run --agent-file agent.json --prompt "Hello!"
```

## Tool Manipulation Commands

The `toolbox` command group provides direct tool manipulation without requiring agent instantiation:

### List Available Tools

List tools from various sources:
```bash
# List tools from an agent
agents-exe toolbox list --agent-file agent.json

# List tools from a directory
agents-exe toolbox list --tool-dir ./tools

# List tools from an MCP config
agents-exe toolbox list --mcp-config mcp.json

# Output formats
agents-exe toolbox list --agent-file agent.json --format table  # Default
agents-exe toolbox list --agent-file agent.json --format json
agents-exe toolbox list --agent-file agent.json --format names
```

### Nest Tools

Create a single nested bash-compatible tool from multiple tools:
```bash
# Create a nested tool from an agent's tools
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

The generated script supports the standard bash-tool interface:
```bash
./combined-tool describe    # Show tool information
./combined-tool run --tool=<name> --arg='<json>'  # Run a sub-tool
```

### MCP Server from Tools

Run an MCP server directly from tools without agent indirection:
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

# With custom server name and logging
agents-exe toolbox mcp --agent-file agent.json \
  --server-name "my-tool-server" \
  --server-version "1.0.0" \
  --log-level debug

# Name mapping strategies
agents-exe toolbox mcp --agent-file agent.json --strip-prefixes
agents-exe toolbox mcp --agent-file agent.json --keep-prefixes
agents-exe toolbox mcp --agent-file agent.json --use-original-names
```

## Other Commands

- `check` - Validate agent configuration
- `tui` - Interactive terminal UI
- `run` - Run a one-shot agent invocation
- `echo-prompt` - Print the rendered prompt
- `describe` - Self-describe the agent
- `init` - Initialize a new agent
- `mcp-server` - Run MCP server with agent indirection
- `session-print` - Print session in markdown format
- `export` - Export agent/tool configurations
- `import` - Import agent/tool configurations

## Configuration

Agents are configured via JSON files. See the documentation in `docs/` for details.

## License

Apache-2.0

