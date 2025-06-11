# an LLM-agent tool

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

An agent embodies a repeatable, parameterized LLM.

An agent thus requires: 
- an LLM endpoint
- an API key for the endpoint
- a model
- a system-prompt
- a set of command line tools
- a set of helper agents

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


### Adding new tools

Tools are executable program (most often, you'll want to wrap bash scripts) that conform to the following specification:
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

Note that agents-exe itself conforms to the above specification.  Which allows
you to use an agents-exe invocation direcly as a tool.  This mechanism gives
some extra flexibility when executing a tree of agents with different access
rights or when you want to run across container boundaries.

### Adding new agents

Agents defined in `agents-exe` can call to other agents much like tools (and
indeed, sub-agents are exposed as "expert tools" to LLMs).  Agents and
sub-agents can share a same API-key but they do not have to.

At this point, sub-agents cannot have further agents themselves, however
sub-agents can have their own tools. If you really want to experiment with a
tree of agents, you can simulate that by using `agents-exe` itself as a tool.

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

The key `id` field is the one that has to match the `apiKeyId` field in agents
JSON description.


### environment variables

A few environment variables may affect somme commands.

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

## Directly build from source using Haskell tools

```console
git clone https://github.com/lucasdicioccio/agents-exe
cd agents-exe
```

```console
cabal run -- agents-exe --help
cabal run -- agents-exe init
```

Then either run the Terminal UI.

```console
cabal run -- agents-exe tui
```


Or run the interactive CLI.
```console
cabal run -- agents-exe cli
```

If you are on a linux machine with some tools like `ping` an `notify-send`
installed, you should be able to run the example tools.

```console
cabal run -- agents-exe run --prompt "can you report the latency to github.com"
```

The `run` command can also take filename if the prompt is prefixed with an `@` (inspired from cURL).

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

- server: reloads/notifications
- server: expose completion/prompt/resources, somehow
- client mode as a consumer of tools
- networked transport

## Framework features

- internal machinery
  - metrics and prodapi-endpoints
  - we need to allow an orchestrator to introspect what happened
  - some storage

- consider allowing sub-agents to also call other agents
