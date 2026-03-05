# an LLM-agent tool

A handy LLM-agent tool, with a variety of calling and configuration modes so
that the LLM adapts to your workflow rather than the opposite.

## Quick Start

Getting started:

```console
agents-exe init
```

will guide you into writing an `agent.json` and its tools directory.

## Command Reference

### Global Options

These options apply to all commands:

| Option | Default | Description |
|--------|---------|-------------|
| `--api-keys FILE` | `~/.config/agents-exe/secret-keys` | Path to JSON file containing API keys |
| `--log-file LOGFILE` | `agents-logfile` | Raw log file for debugging |
| `--log-http URL` | (none) | HTTP endpoint for JSON log sink |
| `--log-json-file FILE` | (none) | Local JSON file log sink |
| `--session-json-file-prefix PREFIX` | (none) | Prefix for session JSON files |
| `--agent-file AGENTFILE` | (auto-discovered) | Root agent description file(s). Can be specified multiple times. Defaults to `agent.json` |

### Commands

#### `check`

Validates agent configuration files.

```console
agents-exe check
agents-exe check --agent-file custom-agent.json
```

#### `init`

Initializes a new agent project with a sample `agent.json` and tools directory structure.

```console
agents-exe init
agents-exe init --agent-file my-agent.json
```

#### `tui` - Terminal User Interface

Launches an interactive terminal UI for chatting with agents.

```console
agents-exe tui
agents-exe tui --agent-file my-agent.json
```

#### `run` - Execute a One-Shot Command

Runs a single prompt through an agent and outputs the result.

**Prompt Options:**

| Option | Short | Description |
|--------|-------|-------------|
| `--prompt TEXT` | `-p` | Add a text paragraph to the prompt |
| `--file FILE` | `-f` | Include contents of a file in the prompt |
| `--shell CMD` | | Include stdout of a shell command in the prompt |
| `--sep4 SEP` | `-s` | Add a short separator (4 repetitions) |
| `--sep40 SEP` | `-S` | Add a long separator (40 repetitions) |
| `--session-file FILE` | | Resume from or save to a session file |

**Examples:**

```console
# Simple prompt
agents-exe run --prompt "Hello, how are you?"

# Include file contents
agents-exe run --prompt "Summarize this:" --file README.md

# Include command output
agents-exe run -p "Explain this diff:" -s "---" --shell "git diff"

# Multiple prompts (concatenated with newlines)
agents-exe run -p "First part" -p "Second part"

# Resume a session
agents-exe run --session-file previous.json -p "Continue where we left off"
```

#### `echo-prompt`

Echoes the constructed prompt without sending it to the agent. Useful for debugging prompt construction.

```console
agents-exe echo-prompt -p "Hello" -f README.md
```

#### `describe`

Outputs a JSON self-description of the tool interface. Useful when agents-exe is used as a sub-tool.

```console
agents-exe describe
```

#### `mcp-server`

Runs agents-exe as an MCP (Model Context Protocol) server, exposing agents as tools via stdin/stdout.

```console
agents-exe mcp-server
agents-exe mcp-server --agent-file agent1.json --agent-file agent2.json
```

The MCP server exposes each configured agent as a callable tool to MCP clients like Claude Desktop.

#### `session-print`

Prints a session file in human-readable markdown format.

| Option | Description |
|--------|-------------|
| `--show-tool-call-results` | Show the results of tool calls |
| `--n-turns N` | Limit output to first N turns |
| `--repeat-system-prompt` | Repeat system prompt at each turn |
| `--repeat-tools` | Repeat available tools at each turn |
| `--antichronological` | Display newest turns first (default: oldest first) |

**Examples:**

```console
# Print entire session
agents-exe session-print my-session.json

# Print with tool results
agents-exe session-print --show-tool-call-results my-session.json

# Print last 10 turns only, newest first
agents-exe session-print --n-turns 10 --antichronological my-session.json
```

## Using as a Tool

The primary intended usage is to run agents-exe as a standalone application.

### Agents

An agent comprises a repeatable, parameterized LLM.

An agent requires:
- An LLM endpoint
- An API key for the endpoint
- A model
- A system prompt
- A set of command line tools
- A set of helper agents

The model and system-prompt are the typical LLM parameters you encounter.
Large, general models are more capable but more expensive. The system-prompt
gives instructions to align and guide the LLM output.

Command-line tools extend the capabilities of your LLM by performing side
effects from `agents-exe` (for instance, installing packages, probing the
network, reading files, etc).

Helper agents allow you to offload a part of the reasoning to more specialized
agents. This mechanism allows the main LLM to be responsible for coming up with a
solution and prompting other agents. The set of helper agents is also
referenced in a paragraph added to the system prompt so that you can reference
agents by name in your user prompts.

Both command line tools and helper agents are exposed to the model as extra
functions (or tools). If the LLM wants to run a function, `agents-exe` will do
so. When an LLM requires more than one function, `agents-exe` runs all the
functions concurrently.

### Adding New Executable-Program Tools

Tools are executable programs (often bash scripts) that adhere to the following specifications:
- When called with `describe` (no parameter), the program MUST return a description of the parameters allowed
- When called with `run` (with arguments/stdin), the program SHOULD always write something to stdout

The description format is a JSON-encoded object with the following structure:

```json
{
  "slug": "tool_name",
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
  ]
}
```

**Argument Modes:**

| Mode | Behavior |
|------|----------|
| `positional` | Added as consecutive arguments |
| `dashdashspace` | Added as `--arg-name value` (two arguments) |
| `dashdashequal` | Added as `--arg-name=value` (single argument) |
| `stdin` | Concatenated to stdin with blank line separators |

See `System.Agents.Tools.Bash.ScriptInfo` for the proper definition.

Agents-exe adheres to the same specifications, allowing you to use an
`agents-exe` invocation directly as a tool. This flexibility is useful when
running a hierarchy of agents with different access rights or across container
boundaries.

### Adding New Tools Using an MCP Server

Agents-exe can act as an MCP client of MCP servers.
For now, only the local-executable transport is supported.

Adding executables is done via the agent configuration:

```json
{
  "tag": "OpenAIAgentDescription",
  "contents": {
    "slug": "my-agent",
    "mcpServers": [
      {
        "tag": "McpSimpleBinary",
        "contents": {
          "name": "my-mcp-server",
          "executable": "/path/to/executable",
          "args": ["arg0", "arg1"]
        }
      }
    ]
  }
}
```

For MCP servers which do not advertise tools-changed notifications, agents-exe
resorts to periodic polling to refresh the tools list.

### Adding New Agents

Agents defined in `agents-exe` can call other agents much like tools (and
indeed, sub-agents are exposed as "expert tools" to LLMs). Agents and
sub-agents can share the same API key but they do not have to.

Sub-agents can have further agents themselves. They are loaded from JSON files
in the tool-directory (relative to the agent's JSON file parent directory).

A typical config-tree for an agent:

```
/some-dir           # the agent root dir
  /agent.json       # the agent-definition file
  /tools            # the tool dir
    /aloha.json     # some tool
    /expert-1.json  # sub-agent definition
    /expert-1-tools # sub-agent's tool dir
      /...
    /ping.sh        # some bash tool
```

### API Keys

Agents need an API key to authenticate against the LLM endpoint.
By default `agents-exe` locates keys in `~/.config/agents-exe/secret-keys` but you can override with `--api-keys`.

The keys file format:

```json
{
  "keys": [
    {
      "id": "my-key-id",
      "value": "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    }
  ]
}
```

The `id` must match the `apiKeyId` field in the agent's JSON description.

### Environment Variables

- `EDITOR` or `GIT_EDITOR` - Used for editing files (notably in the `init` command)

## Using as an MCP Server

The `agents-exe mcp-server` command runs a (stdin/stdout) MCP server exposing
agents as individual tools. It works with Claude Desktop and other MCP clients.

Tool names are deterministic across runs when using the same `--agent-file` arguments in the same order.

## Using as a Library

The code is designed to be usable as a Haskell library.

Coding style guidelines:
- Prefer qualified imports over prefixed/suffixed functions
- Keep prefixed/suffixed function names for helper variations
- Prefer passing multiple arguments
- Bundle arguments into Runtime or Props objects for long-running values or context
- No monad-tower, run in IO (library users can add their own)
- Use contravariant tracer to surface logs, be generous with traces

# Build and Run

## Directly from Source

```console
git clone https://github.com/lucasdicioccio/agents-exe
cd agents-exe
cabal run -- agents-exe --help
cabal run -- agents-exe init
```

## Container Build

With [Podman](https://podman.io) (for [Docker](https://docker.com), replace with `docker`):

```console
git clone https://github.com/lucasdicioccio/agents-exe
cd agents-exe/bundling
podman build -f Containerfile.build -t agents-exe
podman run -it --entrypoint=bash agents-exe:latest
```

Then inside the container:

```console
agents-exe init
agents-exe tui
```

## JSON Logging

Agents-exe logs extensively. The default "Show Format" is for debugging.
For mechanical consumption, use JSON logging:

```console
agents-exe --log-json-file logs.json run -p "Hello"
agents-exe --log-http https://example.com/log-sink run -p "Hello"
```

## Advanced Configuration

Create an `agents-exe.cfg.json` in your project root (searched upward from current directory):

```json
{
  "agentsConfigDir": "/some/dir",
  "agentsDirectories": ["./agents"],
  "agentsFiles": [],
  "agentsLogs": {
    "logJsonHttpEndpoint": "https://example.org/log-sink",
    "logRawPath": "agents-exe.raw",
    "logJsonPath": "logs.json",
    "logSessionsJsonPrefix": "sessions"
  }
}
```

Absent this file, agents load from `~/.config/agents-exe/default`.

# Roadmap

## LLM Providers

- [x] Mixed-API keys between agents and sub-agents
- [x] OpenAI
- [x] Mistral
- [x] Ollama
- [x] Moonshot
- [ ] vLLM

## MCP Support

- [ ] Server: expose tool registration directly
- [ ] Server: forward tool-reloads/notifications
- [ ] Server: expose completion/prompt/resources
- [ ] Networked transport

## Framework Features

- [ ] HTTP server with metrics
- [ ] Better async-linking
- [ ] CLI tooling to directly inspect/call tools
- [ ] Improve TUI mode

