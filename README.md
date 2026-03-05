# an LLM-agent tool

A handy LLM-agent tool, with a variety of calling and configuration modes so
that the LLM adapts to your workflow rather than the opposite.

## Using as a tool

The primary intended usage is to run agents-exe a standalone application.

### command line

Agents-exe is primarily configured using command line arguments

```console
agents-exe --help
```

### Global Options

These options can be used with any command:

| Option | Description |
|--------|-------------|
| `--api-keys FILE` | Path to JSON file containing API keys (default: `~/.config/agents-exe/secret-keys`) |
| `--log-file FILE` | Raw log file (default: `agents-logfile`) |
| `--log-http URL` | HTTP log sink for JSON logs |
| `--log-json-file FILE` | Local JSON file log sink |
| `--session-json-file-prefix PREFIX` | Local JSON sessions file prefix |
| `--agent-file FILE` | Root agent(s) description files (can be specified multiple times, defaults to `agent.json`) |

### Commands

#### `init` - Initialize a new agent

```console
agents-exe init
```

Creates a new agent configuration file (`agent.json`) and its tools directory.
Will guide you through setting up:
- An LLM endpoint and API key
- A model and system prompt
- A tools directory

#### `check` - Verify agent configuration

```console
agents-exe check
```

Validates agent configuration files and prints agent information.
Useful for debugging configuration issues.

#### `tui` - Terminal User Interface

```console
agents-exe tui
```

Launches an interactive terminal UI for chatting with agents.
Supports multiple agent files for multi-agent workflows.

#### `run` - Execute a one-shot prompt

```console
agents-exe run [OPTIONS]
```

Execute a single prompt and exit. Supports prompt composition via multiple options:

| Option | Short | Description |
|--------|-------|-------------|
| `--prompt TEXT` | `-p` | Add a text paragraph to the prompt |
| `--file FILE` | `-f` | Include contents of a file in the prompt |
| `--shell COMMAND` | | Include stdout of a shell command in the prompt |
| `--sep4 TEXT` | `-s` | Add a short separator (4 repetitions of TEXT) |
| `--sep40 TEXT` | `-S` | Add a long separator (40 repetitions of TEXT) |
| `--session-file FILE` | | Extra session file to resume/store |

**Examples:**

Simple prompt:
```console
agents-exe run --prompt "can you report the latency to github.com"
```

Prompt with file content:
```console
agents-exe run \
  --prompt "resume the following content:" \
  --file "README.md"
```

Prompt with shell output:
```console
agents-exe run -p "explain the diff:" \
  -s "#" \
  --shell "git diff"
```

#### `echo-prompt` - Preview prompt composition

```console
agents-exe echo-prompt [OPTIONS]
```

Same options as `run`, but only prints the composed prompt without executing it.
Useful for testing prompt scripts.

#### `describe` - Self-describe as a tool

```console
agents-exe describe
```

Outputs a JSON description of agents-exe as a tool, following the tool specification format.
Useful when using agents-exe as a sub-agent in other agent configurations.

#### `mcp-server` - Run as MCP server

```console
agents-exe mcp-server
```

Runs agents-exe as a (stdin/stdout) MCP server, exposing agents as individual tools.
Compatible with MCP clients like Claude-desktop.

The name of tools exposed over MCP is deterministic across runs when using
the same `--agent-file` arguments in the same order.

#### `session-print` - Print session files

```console
agents-exe session-print [OPTIONS] SESSIONFILE
```

Print a session file in markdown format.

| Option | Description |
|--------|-------------|
| `--show-tool-call-results` | Show the results of tool calls in the output |
| `--n-turns N` | Limit output to the first N turns |
| `--repeat-system-prompt` | Repeat the system prompt at each turn |
| `--repeat-tools` | Repeat the available tools at each turn |
| `--antichronological` | Display turns in antichronological order (newest first) |

**Examples:**

Print an entire session:
```console
agents-exe session-print my-session.json
```

Print only the first 5 turns:
```console
agents-exe session-print --n-turns 5 my-session.json
```

Print with tool call results included:
```console
agents-exe session-print --show-tool-call-results my-session.json
```

### Agents

An agent comprises a repeatable, parameterized LLM.

An agent requires:
- An LLM endpoint
- An API key for the endpoint
- A model
- A system prompt
- A set of command line tools
- A set of helper agents

The model and system-prompt are the typical LLM parameter you encounter.
Large, general models are more capable but more expensive.  The system-prompt
gives instructions to align and guide the LLM output.

Command-line tools extend the capabilities of your LLM by performing side
effects from `agents-exe` (for instance, installing packages, probing the
network, reading files, etc).

Helper agents allow you to offload a part of the reasoning to more specialized
agents.  This mechanism, the main LLM will be responsible for coming up with a
solution and prompting other agents.  The set of helper agents is also
referenced in a paragraph added to the system prompt so that you can reference
agents by name in your user prompts. 

Both command line tools and helper agents are exposed to the model as extra
functions (or tools).  If the LLM wants to run a function, `agent-exe` will do
so.  When an LLM requires more than one function, `agents-exe` runs all the
functions concurrently.


### Adding new executable-program tools


Tools are executable programs (often, you might want to encapsulate bash scripts) that adhere to the following specifications:
- when called with `describe` (no parameter), the program MUST return a description of the parameters allowed
- when called with `run` (with arguments/stdin) executes the command, the program SHOULD always write something to stdout

The description format is a JSON-encoded object having the following grammar-ish.
This description format allows `agents-exe` to expose a tool to an LLM and convert back-and-forth.

```
description .=
  json-object
    [ "slug" .= snakeText
    , "description" .= text
    , "args" .= json-array<arg>
    ]

arg .=
  json-object
    [ "name" .= text
    , "description" .= text
    , "type" .= text
    , "backing_type" .= text
    , "arity" .= argArity
    , "mode" .= argMode
    ]

snakeText = many (alphanum | "_")

backing_type .= model-specific-values

argArity .= "single"

argMode .= "positional" | "dashdashspace" | "dashdashequal" | "stdin"
```

See `System.Agents.Tools.Bash.ScriptInfo` for the proper definition.
Not all fields are useful.

The `run` command then is called depending on the description. In short:
- arguments are added in the same order as they are defined
- `positional` arguments are just added consecutively with the value from the LLM
- `dashdashspace` arguments are called with `--arg-name` `value-from-llm` as two consecutive command arguments
- `dashdashequal` arguments are called with `--arg-name=value-from-llm` as a single command argument
- `stdin` arguments are concatenated to the standard input, and extra blank line is added between two arguments

For instance, if a tool describes:
- "name" "positional"
- "env" "dashdashspace"
- "topic" "positional"
- "tag" "dashdashequal"
- "comment" "stdin"
- "rebuttal" "stdin"

The command will translate the following LLM input:

```json
{
  "name": "John",
  "env": "prod",
  "topic": "Salmons",
  "tag": "fish",
  "comment": "good article",
  "rebuttal": "uninteresting"
}
```

into a processing that would be morally equivalent to

```
mytool John --env prod Salmons --tag=fish <( echo "good article"; echo ""; echo "uninteresting")
```

Agents-exe adheres to the same specifications, allowing you to use an
`agents-exe` invocation directly as a tool. This flexibility is useful when
running a hierarchy of agents with different access rights or across container
boundaries. 

### Adding new tools using an MCP-server

Agents-exe can act as an MCP-client of MCP-servers.
For now, only the local-executable transport is supported.

Adding executables is as follows:

```json
{
  "tag": "OpenAIAgentDescription",
  "contents": {
    "slug": "my-agent",
    "...": "other-structures",
    "mcpServers": [
      {
        "tag": "McpSimpleBinary",
        "contents": {
          "name": "my-mcp-server",
          "executable": "/path/to/executable",
          "args": [
            "arg0",
            "arg1"
          ]
        }
      }
    ]
  }
}
```

For MCP-servers which do not advertise tools-changed notifications, agents-exe
resorts to periodic polling to refresh tools list.


### Adding new agents

Agents defined in `agents-exe` can call to other agents much like tools (and
indeed, sub-agents are exposed as "expert tools" to LLMs).  Agents and
sub-agents can share a same API-key but they do not have to.

Sub-agents can have further agents themselves. They are loaded from json files
in the tool-directory (relative to the agent's json file parent directory).  At this point, no provision is made to prevent cycles.

A typical config-tree for an agent is as follows on the filesystem.

```console
/some-dir      # the agent root dir
  /agent.json  # the agent-definition file
  /tools             # the tool dir
    /aloha.json      # some aloha tool
    /expert-1.json   # subagent agent-definition file
    /expert-1-tools  # subagent's tool dir
      /...
    /ping.sh         # some ping tool
```

### API Keys

Agents need an API key to authenticate against the LLM endpoint.
By default `agents-exe` locates the keys file in `agents-exe.keys` but you can override this path as an argument.

The content of the keys file is as follows:

```json
{
  "keys": [
    {
      "id": "my-mistral-1",
      "value": "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    },
    {
      "id": "my-mistral-2",
      "value": "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    },
    {
      "id": "openai",
      "value": "sk-proj-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    }
  ]
}
```

The `id` key must match the `apiKeyId` field in the agent's JSON description.


### environment variables

Certain environment variables can influence command behaviors:
- `EDITOR` or `GIT_EDITOR` are used to edit files (notably in the `init` command)

## Using as an MCP server

The command `agents-exe mcp-server` runs a (stdin/stdout) MCP server exposing
agents as individual tools.  It has been shown to work with Claude-desktop.  I
would call the support as still experimental at this point, expect some changes
and extra features (e.g., exposing just bash-tools without agent files so that
the MCP-client becomes the agent).

The name of tools exposed over MCP is deterministic across runs provided you
run the `mcp-server` command with the same `--agent-file` in the same order.
This can be annoying when reading logs. I suspect I'll eventually make them
more deterministic.

## Using as a library

The code for should be pretty easy to use in a library settings provided you
know a bit of Haskell.

Coding style follows these guidelines:
- prefer qualified imports over prefixed/suffixed functions
- keep prefixed/suffixed function names for helper variations
- prefer passing multiple arguments
- then only bundle arguments into Runtime or Props objects depending on whether you store long-running values or you pass along a context
- no monad-tower, run in IO, library users can then pick their monad-tower or effect-system
- use contravariant tracer to surface logs, be generous with traces

# Build and run

Note that once installed, you need not prefix commands below with `cabal run`.

## Directly build from source using Haskell tools

```console
git clone https://github.com/lucasdicioccio/agents-exe
cd agents-exe
```

```console
cabal run -- agents-exe --help
cabal run -- agents-exe init
```

## Usage

Then choose to either run the Terminal UI.
```console
agents-exe tui
```

Or interact via the TUI.
```console
agents-exe tui
```

On a Linux machine with tools like `ping` and `notify-send` installed, you can execute example tools:
```console
agents-exe run --prompt "can you report the latency to github.com"
```

The `run` command can also include file contents with `--file`, separators, and even execute shell commands.
Multiple `--prompt` are allowed and concatenated into a single prompt with newline spacing, which allows for simple templating.

```
agents-exe run \
  --prompt "resume the following content:" \
  --file "README.md"
```

```
agents-exe run -p "explain the diff:" \
  -s "#" \
  --shell "git diff"
```

## Session Print Command

The `session-print` command allows you to view saved session files in a human-readable markdown format.
Sessions are JSON files that store the conversation history between users and agents, including tool calls and responses.

### SessionPrintOptions

The `SessionPrintOptions` type controls how session files are displayed:

| Option | Type | Description |
|--------|------|-------------|
| `sessionPrintFile` | `FilePath` | Path to the session JSON file to print (required positional argument) |
| `showToolCallResults` | `Bool` | When enabled with `--show-tool-call-results`, displays the actual results returned by tool calls |
| `nTurns` | `Maybe Int` | When specified with `--n-turns N`, limits output to the first N turns |
| `repeatSystemPrompt` | `Bool` | When enabled with `--repeat-system-prompt`, repeats the system prompt at each turn |
| `repeatTools` | `Bool` | When enabled with `--repeat-tools`, repeats the available tools at each turn |
| `order` | `SessionPrintOrder` | Use `--antichronological` for newest-first order (default: chronological) |

### Usage Examples

Print an entire session:
```console
agents-exe session-print my-session.json
```

Print only the first 5 turns:
```console
agents-exe session-print --n-turns 5 my-session.json
```

Print with tool call results included:
```console
agents-exe session-print --show-tool-call-results my-session.json
```

Combine options:
```console
agents-exe session-print --n-turns 10 --show-tool-call-results my-session.json
```

The output is formatted as markdown with:
- Session metadata (ID and fork information)
- Chronologically ordered turns (user turns and LLM turns)
- System prompts, user queries, and LLM responses
- Tool calls made by the LLM
- Tool responses (when `--show-tool-call-results` is enabled)

## JSON logging

Agents-exe logs _a lot_ of information.  The default format is defined the
"Show Format" and is hard to parse and consume in a mechanical way. The main
purpose of the Show Format is for developping and debugging agents-exe,
sometimes when agents-exe fails to load it's useful to look at these logs.

Agents-exe also provide a more synthetic (as in loses information) JSON format
for mechanical consumption. The exact format is still a bit liberal so do not
build your business around this JSON structure, however for typical
conversation summarization, tracing you'll get plenty of information.

To enable HTTP of file logging of JSON conversation digests, start agents-exe
with the command line argument:

```console
agents-exe --log-json-file logfile.json run -p "hi"
```
or 

```console
agents-exe --log-http http://example.com/log-sink run -p "hi"
```

(or both).

The log file will contain detailed JSON entries, in a better format than the "raw log file".
The HTTP endpoint will format perform HTTP POSTs with the same content.


## Advanced configuration with agents-exe.cfg.json

Agents-exe command line arguments allows you to configure and call to different
LLMs. At some point, it is tedious to keep repeating command line arguments to
pick an agent-file or another. Also, sometimes you need more sophisticated
setups, with varying agents based on varying depoloyments.

The modus-operandi of the command line is that arguments overrides a "base
configuration".  The base configuration itself is configurable, which helps
defining different configuration on different projects.

Agents-exe starts by locating an `agents-exe.cfg.json` file from the
current-dir and searching up in the file-hierarchy until the root directory. 
The format of `agents-exe.cfg.json` defines the following json format:

```json
{ "agentsConfigDir": "/some/dir"
, "agentsDirectories":["./agents"]
, "agentsFiles":[]
, "agentsLogs":
  { "logJsonHttpEndpoint":"https://example.org/log-sink"
  , "logRawPath":"agents-exe.raw"
  , "logJsonPath":"logs.json"
  }
}
```

The `agentsConfigDir` key is optional and defines a directory where other
configurations may be loaded (for now only the `secret-keys` file has a
meaningful location in this directory).

The two other 'agents' keys provide ways to list individual agents JSON definition files
(and their tool and agents subtrees). Whether to use Directories or Files
depends on your own workflow: you may need fewer configuration changes if you
pick directories rather than individual files, however any `.json` file that is
not an agent-definition will crash agents-exe.

Absent an `agents-exe.cfg.json`, agents are loaded from the directory
`${HOME}/.config/agents-exe/default` and the key-secrets from
`${HOME}/.config/agents-exe/secret-keys`.

In a similar manner, `logHttpEndpoint` and `logJsonPath` provide a default (as
in overrideable by the command line args) HTTP or File endpoints.

## Building from the Containerfile

With [Podman](https://podman.io), for [Docker](https://docker.com), replace with `docker`.

```console
git clone https://github.com/lucasdicioccio/agents-exe
cd agents-exe/bundling
podman build -f Containerfile.build -t agents-exe
podman run -it --entrypoint=bash agents-exe:latest
```

And then from inside the container.

```console
agents-exe init
agents-exe tui
```

# Unordered Roadmap

## LLM-providers Support

At this point I do not have the resources to enable and test many LLM-provider
although we'll get there eventually.  By order of precedence I plan to be
compatible with: OpenAI, OpenAI-claimed-compatible APIs, Ollama, others.

- [x] Mixed-API keys between agents and sub-agents
- [x] OpenAI
- [x] Mistral
- [x] Ollama
- [x] Moonshot
- [ ] vLLM

## MCP support

- server: expose a toolregistration directly
- server: forward tool-reloads/notifications
- server: expose completion/prompt/resources, somehow
- networked transport

## Framework features

- internal machinery
  - http-server with metrics and/or prodapi-endpoints
  - better async-linking when it makes sense
- better CLI tooling
  - to more-directly inspect/call a tool
  - improve the 'tui' mode

## HTTP logging

Agents-exe can log HTTP traffic details for debugging or auditing purposes.

To enable HTTP of file logging of JSON conversation digests, start agents-exe with the command line argument:

```console
agents-exe --log-json-file logfile.json
```
or 

```console
agents-exe --log-http http://example.com/logz
```

(or both).

The log file will contain detailed JSON entries, in a better format than the "raw log file".

