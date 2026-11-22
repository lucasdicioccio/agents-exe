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
| `commands` | Array | No | Subcommands, each its own tool (see [Subcommands](#subcommands)) |
| `protocol` | Number | No | `1` (default) or `2` (see [Protocol v2](#protocol-v2-framed-output)) |
| `process` | Object | No | Protocol 2 only: how output is cut into events (see [Protocol v2](#protocol-v2-framed-output)) |
| `runtime` | Object | No | Protocol 2 only: how the tool is run: user, sandbox, envdir, rundir, timeouts (see [Runtime](#protocol-v2-runtime)) |

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

## Subcommands

A program that does several things through subcommands, each with its own
arguments, lists them in `commands`. It is then exposed as one tool per
subcommand, named `bash_<slug>_<name>`, whose arguments are the top-level
`args` (shared by every subcommand) followed by the subcommand's own:

```json
{
  "slug": "kv",
  "description": "a key-value store",
  "args": [ { "name": "store", "description": "store name", "type": "string",
              "backing_type": "string", "arity": "single", "mode": "dashdashspace" } ],
  "commands": [
    { "name": "get", "description": "reads a key",
      "args": [ { "name": "key", "description": "the key", "type": "string",
                  "backing_type": "string", "arity": "single", "mode": "dashdashspace" } ] },
    { "name": "set", "description": "writes a key",
      "args": [ { "name": "key", "description": "the key", "type": "string",
                  "backing_type": "string", "arity": "single", "mode": "dashdashspace" },
                { "name": "value", "description": "the value", "type": "string",
                  "backing_type": "string", "arity": "single", "mode": "positional" } ],
      "empty-result": { "tag": "AddMessage", "contents": "done" } }
  ]
}
```

`run` receives the subcommand name first, then the arguments:

```bash
$ ./kv run set --store s1 --key k v
```

Each command has `name` (ASCII letters, digits, `_`, `-`; unique),
`description`, and optionally `args`, `empty-result` and `output-media-type`
(overriding the top-level ones). A subcommand may not redeclare a shared
argument. A description that breaks these rules fails to load. Use
`agents-exe check-tool-call --tool ./kv --command set` to validate a payload
against one subcommand. The design notes are in
`todos/bash-tools-subcommands.md`.

## Protocol v2: framed output

A tool that adds `"protocol": 2` to its `describe` output is a v2 tool. The
full design is `todos/bash-tools-v2.md`; what is implemented today is its
first slice: `oneshot` tools (run to completion, as v1) whose standard
output is cut into **events** that the runtime validates and renders.

```json
{
  "protocol": 2,
  "slug": "scan",
  "description": "Scans files and reports findings",
  "args": [],
  "process": {
    "mode": "oneshot",
    "output": {
      "framing": "jsonl",
      "event": { "type": "object",
                 "properties": { "file": {"type": "string"}, "level": {"enum": ["info", "error"]} },
                 "required": ["file"] }
    }
  }
}
```

| `process` field | Values | Meaning |
|---|---|---|
| `mode` | `oneshot` (default), `stream` | `stream`: see [Protocol v2: stream](#protocol-v2-stream). `service` is not implemented yet: a tool declaring it fails to load |
| `output.framing` | `jsonl` (default), `lines`, `chunks` | One JSON value per line, one text line per event, or the raw output |
| `output.event` | a JSON Schema | The shape of one event under `jsonl` |
| `ready` | `{"kind": "exit"}`; for a `stream` also `{"kind": "line", "match": REGEX}` and `{"kind": "immediate"}` | When the tool counts as started; only `exit` applies to a `oneshot` |

What the model receives as the tool result:

* `jsonl`: a JSON array of the events, one per non-empty line. A line that
  is not JSON, or does not match `output.event`, is kept as
  `{"invalid": {"raw": "<the line>", "error": "<why>"}}`, never dropped.
* `lines`: the non-empty lines, joined by newlines.
* `chunks`: the output unchanged (the only framing allowed with
  `output-media-type`).

An empty output still gets the `empty-result` message. While the tool runs,
each event is also reported as progress, as it arrives:
`{"stream": "stdout", "event": <event>, "events": <count>}`.

The event schema supports a practical subset of JSON Schema: `type`
(including `integer` and lists of types), `enum`, `const`, `properties`,
`required`, `additionalProperties`, `items`, `minItems`/`maxItems`,
`minLength`/`maxLength`, `minimum`/`maximum`, `allOf`, `anyOf`, `oneOf`,
`not`, and boolean schemas. Other keywords are ignored.

A v1 tool ignores every v2 field, so `process` without `"protocol": 2`
changes nothing. Declarations a later slice will implement (`process.input`,
the `service` mode) make the tool fail to load rather than run
without them. `commands` works in v2 too: every subcommand shares the
`process` and `runtime` declarations.

### Protocol v2: stream

A `stream` tool runs for as long as it has something to report: a log
follower, a long build, a watcher that exits when its condition is met. Its
standard output is cut into events with the same framing and validation as
a `oneshot`, but the whole output is never kept:

* each event is reported as progress as it arrives
  (`{"stream": "stdout", "event": <event>, "events": <count>}`), which is
  what `get-tool-call-status` shows while the call runs;
* the call completes when the tool exits, and the result is its **latest
  events**: under `jsonl`, `{"events": [...], "elided": <n>}`; under `lines`
  the lines, and under `chunks` the chunks, preceded by
  `[<n> earlier lines elided]` when some were dropped. The agent's
  `streamResultEvents` (default 200) says how many are kept;
* under `chunks` each read of standard output is an event;
* a tool that exits with a failure fails the call, and the error carries the
  tail of its standard error and the result so far;
* no events and an `empty-result` message: the message is the result.

```json
{
  "protocol": 2,
  "slug": "follow_build",
  "description": "Follows a build until it ends, reporting each step",
  "args": [],
  "process": {
    "mode": "stream",
    "output": { "framing": "jsonl", "event": { "type": "object", "required": ["step"] } },
    "ready": { "kind": "line", "match": "\"step\": *\"started\"" }
  },
  "runtime": { "timeout": { "start": 30, "stop": 5 } }
}
```

`runtime.timeout.start` bounds reaching `ready`, not the whole run: under
`{"kind": "line"}`, the first line of standard output matching the POSIX
extended regular expression `match` (that line is still an event); under
`{"kind": "exit"}`, the whole run. A `stream` tool declaring
`timeout.start` without one of those fails to load, as does a `line` ready
under `chunks` framing and `output-media-type`. Once ready, the tool runs
until it exits or the call is cancelled (`cancel-tool-call`, or the agent's
`asyncCallTimeoutSeconds`), then gets its `cancel` signal and, after
`timeout.stop`, `SIGKILL`.

A stream is best run as an asynchronous call: give the tool a `RunAsync`
disposition in the agent's `toolCallPolicyConfig` (and an asynchronous
`executionMode`), so the model gets the `running` placeholder, can follow
the events with `get-tool-call-status`, and gets the result when the tool
exits. Under a synchronous disposition the call simply waits for the exit,
as any call does. Standard input gets the `stdin`-mode arguments and is
closed, as for a `oneshot`: talking to a running tool is the `service` mode.

### Protocol v2: runtime

A v2 tool may say how it wants to be run, in the spirit of a systemd unit or
a daemontools service directory. The tool names **what** it needs; the
operator names **how** it is enforced, with options of `agents-exe` (given
before the subcommand). A tool that declares something the operator did not
configure **fails to load**, naming the field: it never runs unconfined, or
as the wrong user, by silence.

```json
{
  "protocol": 2,
  "slug": "report",
  "description": "Builds the daily report",
  "args": [],
  "runtime": {
    "run_as": { "user": "reports", "group": "reports" },
    "sandbox": { "fs": { "read": ["/var/log"], "write": ["state"] }, "net": "none", "tmp": "private" },
    "envdir": "report",
    "environment": { "LC_ALL": "C" },
    "rundir": "state",
    "timeout": { "start": 60, "stop": 5 },
    "cancel": "SIGTERM"
  }
}
```

| `runtime` field | Meaning | Operator option |
|---|---|---|
| `envdir` | A directory name under the envdir root, read the daemontools way at every call: one file per variable, the file name is the name, the first line (trailing spaces and tabs removed) is the value; an empty file unsets the variable | `--envdir-root DIR` (there is no implicit location) |
| `environment` | Literal variables; they win over the envdir | |
| `rundir` | A directory relative to the tool's directory (no `..`), created if missing, made the working directory | |
| `timeout.start` | Seconds the call has to complete (for a `stream`: to reach `ready`); past it the tool is stopped and the call fails | |
| `timeout.stop` | Seconds between the cancel signal and `SIGKILL` (default 0.1) | |
| `cancel` | The signal sent first when the call is cancelled or times out: `"SIGTERM"` (default) or `"SIGINT"` | |
| `run_as` | `{"user", "group"}`: run the tool as this user (and group; by default the user's primary group) | `--run-as-mechanism setpriv\|sudo\|runuser` |
| `sandbox` | `{"fs": {"read": [...], "write": [...]}, "net": "host"\|"none", "tmp": "shared"\|"private"}`: what the tool may touch | `--sandbox bwrap\|landlock\|auto` |

The environment a tool sees, from lowest to highest precedence: the
`agents-exe` environment, the session context variables, the envdir,
`environment`, then the tool's own `env`-mode arguments.

**run_as.** `setpriv` and `runuser` need `agents-exe` to run as root (or
with `CAP_SETUID`/`CAP_SETGID`); `sudo` runs `sudo -n --preserve-env -u
USER [-g GROUP]`, so the sudoers rule must allow it without a password and
with `SETENV` (the envdir travels in the environment, never on the command
line). The rundir is created by the `agents-exe` user: give the tool's user
access to it.

**sandbox.** Under `bwrap` (bubblewrap) the tool runs in a mount namespace
where only the system's executables and libraries (`/usr`, `/bin`, `/lib*`,
`/etc`, read-only), `/proc`, `/dev`, `/tmp` (the host's under `shared`, a
fresh tmpfs under `private`), the tool's own directory (read-only) and the
declared paths exist. `fs.read` paths are mounted read-only and `fs.write`
paths read-write; relative paths are relative to the tool's directory, and a
declared path that does not exist fails the call. `net: none` is a network
namespace with no interface. The tool dies with `agents-exe`. bubblewrap
needs unprivileged user namespaces, which some distributions restrict.

Under `landlock` nothing is hidden: the tool sees the whole file system, but
the kernel denies every access outside its rules. It may read and execute
the same system paths, `/proc` and its own directory, read `fs.read`, and do
anything under `fs.write` and (`tmp: shared`) `/tmp`; `/dev/null`,
`/dev/zero` and `/dev/full` are writable and `/dev/random`, `/dev/urandom`
readable. On kernels with Landlock ABI 6 or later it can only signal
processes inside its own sandbox. Landlock needs no privilege and no
namespaces, only a kernel that has it enabled (`landlock` in
`/sys/kernel/security/lsm`); the rules are applied by `agents-exe` itself,
which the tool is started through (`agents-exe __agents-exe-landlock-exec
... -- tool run ...`) and which execs the tool once the rules are in place.
The rules hold for the tool's children too, and set `no_new_privs`, so
set-user-ID programs (such as `sudo`) do not gain privileges inside. It
cannot honour everything bubblewrap does, and a tool asking for it fails to
load under `--sandbox landlock`, naming the reason: `net: none` (Landlock
only denies TCP bind and connect, UDP and unix sockets stay open),
`tmp: private`, and a tool whose directory is under `/tmp` with
`tmp: shared` (a path cannot be made read-only beneath one that is
writable). A kernel without Landlock fails every sandboxed tool to load,
saying why.

`--sandbox auto` picks, per tool, the strongest mechanism that honours every
declared field: bubblewrap when it can build a sandbox on this host (probed
once per process), else Landlock when the kernel has it and the tool asks
for nothing Landlock cannot enforce; a tool that neither can honour fails to
load, giving both reasons. `run_as` and `sandbox` compose: the sandbox is
set up for the process that runs as the user (under `landlock`, that user
must be able to execute the `agents-exe` binary).

`timeout.idle` and `on_exit` only apply to a `service`, and
`cancel: {"send": ...}` needs an open stdin (`process.input`): until those
slices land, a tool declaring them fails to load. An unknown field in
`runtime` (a typo such as `run-as`) also fails to load.

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

## Inspecting Tool Schemas

The `check` command supports an optional `--tools` flag that allows you to inspect how tools are serialized for LLM calls. This is useful for debugging tool definition issues:

```bash
# Default behavior - just validate and show tool counts
agents-exe check

# List tools with their names and descriptions
agents-exe check --tools=list

# Show the internal agents-exe tool representation as JSON (wrapped in markdown details tags)
agents-exe check --tools=agents-exe

# Show the OpenAI LLM tool representation as JSON (wrapped in markdown details tags)
agents-exe check --tools=openai
```

The `--tools` option accepts the following modes:

| Mode | Description |
|------|-------------|
| `none` | Default. Only show agent names and tool counts. |
| `list` | Print a markdown list of tool names and descriptions. |
| `agents-exe` | Output the internal tool representation as JSON within `<details>` tags. |
| `openai` | Output the OpenAI LLM tool schema (as sent to the API) within `<details>` tags. |

This feature is particularly useful when:
- Debugging why a tool isn't being called by the LLM
- Verifying that tool schemas are correctly formed
- Documenting available tools for users
- Comparing internal vs. LLM-facing representations

