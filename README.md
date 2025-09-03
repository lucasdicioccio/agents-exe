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

Getting started:

```console
agents-exe init
```

will guide you into writing a `agent.json` and its tools directory

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

```
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

Then choose to either run the Terminal UI.
```console
cabal run -- agents-exe tui
```

Or interact via the CLI.
```console
cabal run -- agents-exe cli
```

On a Linux machine with tools like `ping` and `notify-send` installed, you can execute example tools:
```console
cabal run -- agents-exe run --prompt "can you report the latency to github.com"
```

The `run` command can also include file contents with `--file`, separators, and even execute shell commands.
Multiple `--prompt` are allowed and concatenated into a single prompt with newline spacing, which allows for simple templating.

```
cabal run -- agents-exe run \
  --prompt "resume the following content:" \
  --file "README.md"
```

```
cabal run -- agents-exe run -p "explain the diff:" \
  -s "#" \
  --shell "git diff"
```

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
}
```

The `agentsConfigDir` key is optional and defines a directory where other
configurations may be loaded (for now only the `secret-keys` file has a
meaningful location in this directory).

The two other keys provide ways to list individual agents JSON definition files
(and their tool and agents subtrees). Whether to use Directories or Files
depends on your own workflow: you may need fewer configuration changes if you
pick directories rather than individual files, however any `.json` file that is
not an agent-definition will crash agents-exe.

Absent an `agents-exe.cfg.json`, agents are loaded from the directory
`${HOME}/.config/agents-exe/default` and the key-secrets from
`${HOME}/.config/agents-exe/secret-keys`.

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
agents-exe cli
```

# Unordered Roadmap

## LLM-providers Support

At this point I do not have the resources to enable and test many LLM-provider
although we'll get there eventually.  By order of precedence I plan to be
compatible with: OpenAI, OpenAI-claimed-compatible APIs, Ollama, others.

- [x] Mixed-API keys between agents and sub-agents
- [x] OpenAI
- [x] Mistral
- [ ] vLLM
- [ ] Ollama

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
  - make the 'cli' mode tolerable or remove it
  - improve the 'tui' mode

## HTTP logging

Agents-exe can log HTTP traffic details for debugging or auditing purposes.

To enable HTTP of file logging of JSON conversation digests, start agents-exe with the command line argument:

```console
agents-exe --log-json-file logfile.json
```
or 

```console
agents-exe --log-http logfile.json
```

(or both).

The log file will contain detailed JSON entries, in a better format than the "raw log file".


