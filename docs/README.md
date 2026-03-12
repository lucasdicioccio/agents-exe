# Agents - AI Agent Framework

A Haskell-based framework for building and orchestrating AI agents with support for multi-agent hierarchies, tool systems, and LLM integrations.

## Overview

The Agents framework provides a complete infrastructure for:

- **Agent Definition**: JSON-based agent configurations with system prompts, tool directories, and LLM settings
- **Multi-Agent Orchestration**: Hierarchical agent trees with parent-child relationships and cross-agent references
- **Tool System**: Extensible tool registration with support for bash scripts, MCP servers, and OpenAPI integrations
- **Session Management**: Persistent conversation sessions with turn-based interactions
- **Multiple Interfaces**: CLI, TUI (Terminal UI), and MCP server modes

## Quick Start

### Installation

```bash
# Build the project
cabal build

# Run tests
cabal test
```

### Creating an Agent

Create an `agent.json` file:

```json
{
  "slug": "my-agent",
  "apiKeyId": "openai",
  "flavor": "openai",
  "modelUrl": "https://api.openai.com/v1",
  "modelName": "gpt-4",
  "announce": "A helpful assistant",
  "systemPrompt": ["You are a helpful assistant."],
  "toolDirectory": "tools",
  "mcpServers": [],
  "extraAgents": []
}
```

### Running the Agent

```bash
# Check agent configuration
agents-exe check --agent-file agent.json

# Run in one-shot mode
agents-exe run --agent-file agent.json --prompt "Hello!"

# Start interactive TUI
agents-exe tui --agent-file agent.json

# Start MCP server
agents-exe mcp-server --agent-file agent.json
```

## Configuration

### API Keys

Store API keys in `~/.config/agents-exe/secret-keys`:

```json
{
  "openai": "sk-...",
  "openrouter": "sk-..."
}
```

### Project Configuration

Create `agents-exe.cfg.json` in your project root:

```json
{
  "agentsDirectories": ["./agents"],
  "agentsFiles": ["./main-agent.json"],
  "agentsLogs": {
    "logJsonHttpEndpoint": "http://localhost:8080/log",
    "logJsonPath": "./logs/agents.json",
    "logRawPath": "./logs/agents.log"
  }
}
```

## Project Structure

```
agents/
├── app/                      # Application entry point
│   └── Main.hs              # CLI argument parsing and command routing
├── src/
│   └── System/Agents/
│       ├── Base.hs          # Core types (Agent, AgentId, ConversationId)
│       ├── Runtime.hs       # Agent runtime and execution
│       ├── AgentTree.hs     # Multi-agent hierarchy management
│       ├── Session/         # Session management
│       ├── Tools/           # Tool system
│       ├── MCP/             # Model Context Protocol
│       ├── TUI/             # Terminal UI
│       ├── CLI/             # Command implementations
│       ├── ExportImport/    # Tool sharing
│       └── FileLoader/      # File loading utilities
├── docs/                    # Documentation
└── test/                    # Test suite
```

## Key Features

### Multi-Agent Hierarchies

Agents can reference other agents via:
- **Tool Directory**: Child agents in a subdirectory
- **Extra Agents**: Explicit references via `extraAgents` field

### Tool Types

1. **Bash Tools**: Executable scripts in the tool directory
2. **MCP Tools**: Model Context Protocol servers
3. **OpenAPI Tools**: REST API endpoints via OpenAPI specs
4. **IO Tools**: Haskell-based tool implementations

### Session Persistence

Sessions are automatically saved and can be resumed:

```bash
# Resume a session
agents-exe run --agent-file agent.json --session-file session.json

# Print session history
agents-exe session-print session.json
```

## Documentation

- [Architecture](architecture.md) - Runtime and core architecture
- [Tool System](tools.md) - Tool registration and execution
- [MCP Protocol](mcp.md) - Model Context Protocol integration
- [Session Management](sessions.md) - Session lifecycle and persistence
- [Terminal UI](tui.md) - TUI interface
- [CLI Reference](cli-commands.md) - Command reference
- [Export/Import](export-import.md) - Tool sharing and distribution
- [File Loader](file-loader.md) - File loading utilities

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│                        CLI / TUI / MCP                       │
└──────────────────────┬──────────────────────────────────────┘
                       │
┌──────────────────────▼──────────────────────────────────────┐
│                      AgentTree                               │
│         (multi-agent hierarchy management)                   │
└──────────────────────┬──────────────────────────────────────┘
                       │
┌──────────────────────▼──────────────────────────────────────┐
│                     Runtime                                  │
│    (agent execution, tool registration, LLM calls)           │
└──────────────────────┬──────────────────────────────────────┘
                       │
        ┌──────────────┼──────────────┐
        ▼              ▼              ▼
┌──────────────┐ ┌──────────┐ ┌──────────────┐
│   Session    │ │  Tools   │ │     LLM      │
│  Management  │ │  System  │ │  Integration │
└──────────────┘ └──────────┘ └──────────────┘
```

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Run tests: `cabal test`
5. Submit a pull request

## License

See the project LICENSE file for details.

