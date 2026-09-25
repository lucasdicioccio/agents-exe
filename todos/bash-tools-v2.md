# Spec: bash tools v2, long-running tool processes

Status: proposal, 2026-09-25, from issue #539 and a robot-chat conversation
the same day. Nothing implemented. The v1 protocol (`documentation/binary-tool.md`)
stays valid unchanged: a tool that does not say otherwise is a v1 tool.

## Goal

A bash tool today is one process per call: `describe` once, then `run` with
the arguments, stdin written once and closed, stdout collected until exit,
and that stdout is the tool result. Progress exists (`tool.progressed`
events carry each line as it arrives, see `runProcessReportingOutput`), but
the model only ever sees the final stdout, and a tool cannot be talked to
while it runs.

v2 lets a tool declare that it is a **process** rather than a function:

* it may keep running after answering, streaming information out in
  chunks the runtime understands (a declared event shape);
* its stdin may stay open, the runtime feeding it JSON events shaped as the
  tool declares, so the model (or another tool, or mail) can keep talking to
  it;
* from the model's perspective such a tool is stateful: starting it returns
  a handle (start it twice and there are two handles, each addressable on
  its own); the tool family gets one typed `send` tool taking a handle and
  an event, and one shared system tool (`processes`: list, status, tail,
  stop) serves every family, so process management never multiplies the
  session's tools;
* the tool declares how it wants to be run, in the spirit of a systemd
  unit or a daemontools service directory: the user to run as, an envdir,
  a run directory, timeouts, what to do when it exits.

The runtime change is large (D2 below says how it lands in slices); the
protocol change is small, and this document is mostly the protocol.

## 1. The `describe` document, v2

A v2 tool adds `"protocol": 2` to its `describe` output. Everything v1
defines keeps its meaning (`slug`, `description`, `args`, `empty-result`).
Three optional objects are new.

```json
{
  "protocol": 2,
  "slug": "log_watcher",
  "description": "Follows a log file and reports matching lines",
  "args": [ { "name": "path", "type": "string", "backing_type": "string",
              "arity": "single", "mode": "dashdashspace",
              "description": "the file to follow" } ],
  "process": {
    "mode": "stream",
    "output": { "framing": "jsonl", "event": { "type": "object",
                "properties": { "line": {"type": "string"}, "level": {"type": "string"} },
                "required": ["line"] } },
    "input":  { "framing": "jsonl", "event": { "type": "object",
                "properties": { "op": {"enum": ["pattern", "pause", "resume"]},
                                "value": {"type": "string"} },
                "required": ["op"] } },
    "ready": { "kind": "line", "match": "^ready$" }
  },
  "runtime": {
    "run_as": { "user": "logs", "group": "logs" },
    "envdir": "env",
    "environment": { "LC_ALL": "C" },
    "rundir": "state",
    "timeout": { "start": 10, "stop": 5, "idle": null },
    "on_exit": "report",
    "cancel": "SIGTERM"
  }
}
```

### 1.1 `process`: what kind of process this is

| Field | Values | Meaning |
|---|---|---|
| `mode` | `oneshot` (default), `stream`, `service` | `oneshot` is v1: run to completion, stdout is the result. `stream`: the result is the sequence of output events until exit; the call completes when the process exits. `service`: the process is expected to outlive the call; starting it returns a handle and the model works with it through the process tool (§3). |
| `output.framing` | `jsonl` (default for v2), `lines`, `chunks` | How stdout is cut into events: one JSON object per line, one text line per event, or raw chunks as they arrive (binary-safe, for tools that produce no lines). |
| `output.event` | a JSON Schema | The shape of one output event under `jsonl`. Validated by the runtime; an event that does not match is delivered as an `invalid` event carrying the raw line, never dropped silently. Optional under `lines`/`chunks`. |
| `input.framing` | `jsonl` (default), `lines` | How the runtime writes events to stdin. Absent `input`: stdin gets the v1 `mode: stdin` arguments and is closed, as today. |
| `input.event` | a JSON Schema | What the runtime accepts to send to this process; also the schema of the `event` argument of the family's typed `send` tool (§2.4). |
| `digest` | `{"per": "round"}`, `{"per": "events", "n": N}`, `{"per": "seconds", "n": N}` (default `round`) | For a `service`: how chatty output is folded into mail (§2.3). Declared by the tool, per tool, because its author knows what an event is worth. |
| `ready` | `{"kind": "line", "match": REGEX}`, `{"kind": "exit"}`, `{"kind": "immediate"}` | When the process counts as started: a stdout line matching (its first events are still delivered), its exit (a `oneshot`), or right away. Bounded by `runtime.timeout.start`. |

stderr is never part of the result. It is captured into the process's
bounded output ring (§2.3) and traced, as today.

### 1.2 `runtime`: how to run it

Every field is optional; an absent field keeps v1 behaviour (run as the
agents-exe process, its environment plus the session context variables,
its working directory, no timeouts beyond the call's, cancellation kills
the process group).

| Field | Meaning |
|---|---|
| `run_as` | `{"user", "group"}`: run the process as this user and group. The runtime uses one mechanism it was configured with (`setpriv`, `sudo -u`, `runuser`, or a refusal when none is configured); the tool never names the mechanism. Same shape and reasoning as salmon's `RunAs`/`Mechanism` (`multi-user-privilege-separation.md` there). |
| `envdir` | A subdirectory name under the operator's explicit `--envdir-root`, read the daemontools way: one file per variable, file name is the name, file content is the value. Read at start; absent files are absent variables. Lets an operator provision secrets without touching the tool or the checkout. There is no implicit default location: a tool that declares `envdir` while no `--envdir-root` was given fails to load with a `LoadingError` naming the field. |
| `environment` | Literal variables, added last (they win over `envdir`). |
| `rundir` | A directory, relative to the tool's directory, created if missing and made the process's working directory; where a `service` keeps its state. |
| `timeout` | `start`: seconds to reach `ready` or fail the start; `stop`: seconds between the cancel signal and `SIGKILL`; `idle`: seconds without any output or input event after which a `service` is stopped (`null`: never). |
| `on_exit` | For a `service`: `report` (default: the model is told, the handle becomes final), `restart` (bring it back, bounded by a small backoff; every restart is an event), `fail` (the call that started it fails if it has not completed yet). |
| `cancel` | The signal sent first on cancel/stop: `SIGTERM` (default), `SIGINT`, or a JSON event `{"send": {...}}` written to stdin before the signal, for tools that prefer a polite request. |

These are declarations, not privileges: an agent's configuration (and the
operator's `--set`/`--pin` parameters, `tool-partial-application.md`) may
refuse or override them. `run_as` in particular is only honoured when the
operator configured a mechanism; otherwise the tool loads with a
`LoadingError` naming the field, the same way a `with` binding that names
an undeclared parameter does today.

### 1.3 `commands` and `help`: several actions in one binary (from #539)

Out of scope of the process story but part of the same `describe` bump:
`"commands": [ {"name", "description", "args"} ]` lets one binary expose
several tools (`slug.name` each), and `help` (`./tool help [command]`)
gives the runtime a longer text to disclose on demand rather than in every
system prompt. A `setup` step (`./tool setup`) that provisions the tool's
own dependencies is left for a later spec: it is an operator concern, not a
runtime one.

## 2. The runtime side

### 2.1 A `oneshot` v2 tool

Identical to v1 except that `output.framing`/`output.event` apply: the
result delivered to the model is the events, rendered as a JSON array (or
the joined lines), and each event is also a `tool.progressed` payload as it
arrives. Nothing else changes; this is the cheap slice that makes the new
`describe` real.

### 2.2 A `stream` tool

The call runs as an asynchronous call (`RunAsync`, `async-tool-calls.md`):
the model gets the `running` placeholder, events arrive as progress, the
call completes when the process exits, and the result is the full event
sequence (bounded: the last N events, N from the agent's config, the count
of elided ones stated). `wait`, `get-tool-call-status` and
`cancel-tool-call` work unchanged; `get-tool-call-status` shows the latest
events, which is the "tail" the model needs mid-run.

### 2.3 A `service` tool: the process handle

Starting a `service` tool is a call that completes at `ready`: its result
is a **process handle** `{"process": "<id>", "tool": "log_watcher",
"status": "running", "since": ...}`, an entity in the OS world like a tool
call is today (`OS.Conversation.ToolCalls`), owned by the session. Each
start is a new process with its own handle: an agent that starts
`log_watcher` on two files holds two handles and talks to either by its
handle; nothing is deduplicated or shared between starts. A handle has:

* the declared `input.event` schema (what `send` accepts);
* a bounded ring of its output events and of its stderr lines (same shape
  as a node's output ring in salmon's `serve`: small, per process, what an
  operator wants when it fails);
* its lifecycle: `starting`, `running`, `stopping`, `exited` (code),
  `failed` (why), `restarting` (n);
* the `runtime` it was started with, resolved (which user, which envdir).

A process belongs to the session that started it. It survives across the
session's runs (it is exactly the "background call" case, `evictIdle` and
`hasRunningCalls` already keep such a session resident) but **not** a
restart of agents-exe: `recoverOnStartup` marks it `failed: lost on
restart`, as it does for orphaned async calls, and the model learns it
from the process tool. Durable services are a node of their own in
whatever supervises agents-exe, not this spec (D3).

Output events of a running service are delivered to the model as **mail**
(`session-mailbox.md`): each event, or a digest of them as the tool's
`digest` declares (default: per round) when they are chatty, is an envelope
from `process:<id>`, so the model's turn
is woken the same way any other asynchronous news wakes it, with the same
`wakeOn` controls. This is the `notify` progress level that spec deferred
(D9 there), given a producer.

### 2.4 Process tools: one typed `send` per family, one shared `processes`

Two layers, split by what varies:

**Per family, one typed tool.** For each `service` tool `T` in the toolbox
the runtime generates, at load, one companion tool `T.send` (`slug.name`,
the same naming as `commands`, §1.3). Its arguments are `process` (the
handle) and `event`, whose schema is the declared `input.event` verbatim,
so the model gets a typed function per family instead of an untyped
argument, and a family with two running processes is addressed by handle:
`log_watcher.send {process: p1, event: ...}` and `{process: p2, ...}`. The
answer is the events the process produced in reply within a short window,
or `accepted`. The companion exists from load, not from start, so the tool
list (and the tool cache) does not change while processes come and go.

**Shared across families, one tool.** Everything that does not depend on a
family's schema is one system capability with sub-commands (the shape of
`get-tool-call-status` and friends), so a session holding ten service
families still has one management tool:

| Sub-command | Arguments | Answer |
|---|---|---|
| `list` | | every process of the session (and, for a root agent, of its sub-sessions), with tool, status and age |
| `status` | `process` | the handle with its counters, last events, last stderr lines, and the accepted input schema |
| `tail` | `process`, `n` | the last `n` output events, oldest first |
| `stop` | `process`, `force?` | the declared cancel, then `SIGKILL` after `timeout.stop`; the exit becomes the answer |

The model discovers which processes exist through `list` and `status`; a
tool's `description` should say it is a service and name the events it
accepts, in a sentence.

## 3. What the model sees, end to end

1. The tool list holds `log_watcher` (start it), `log_watcher.send`
   (talk to one of its processes) and `processes` (list, status, tail,
   stop, for anything started). Starting is an ordinary tool call with the
   declared `args`, and may be done twice: each start returns its own handle.
2. The result is the handle. The model continues; when the watcher reports
   a line, mail arrives: `from process:p1 {"line": "...", "level": "error"}`.
3. The model calls `log_watcher.send {process: p1, event: {op: "pattern",
   value: "ERROR"}}`; the answer is `accepted` or the watcher's reply.
4. `processes stop p1` when done; or the session ends and every process of
   it is stopped with its declared cancel (a session delete stops them the
   way it cancels runs).

## Decisions (proposed)

D1. **The protocol number, not new commands.** `"protocol": 2` in
`describe` is the whole opt-in; `run` keeps its name and argument passing,
so a v2 tool can be tried as a v1 tool by ignoring the new fields.

D2. **Independent features, each shippable** (the feature `9914ff73` is
split as needed). (1) `describe` v2 parsing, `oneshot` with framing and
validation; (2) `runtime` for `oneshot` tools (`envdir` with
`--envdir-root`, `environment`, `rundir`, `timeout.stop`, `cancel`) and
`run_as` behind an operator-configured mechanism, which ships ahead of the
process modes because a low-privilege `oneshot` tool is valuable alone;
(3) `stream` on the existing async engine; (4) `service`: the handle
entity, mail delivery and `digest`, the typed `T.send` companions, the
shared `processes` capability, and the `runtime` fields only a service
uses (`timeout.idle`, `on_exit`). #539's `commands`/`help` are a fifth,
independent slice.

D3. **Not durable across an agents-exe restart.** A service is lost with the
process that started it and reported as such; supervising long-lived
services belongs to systemd/salmon, which can restart agents-exe itself.
Revisit only with a concrete need (`os-as-standalone-server.md` G11 says
the same of watches and run handles).

D4. **Events, not bytes, at the model boundary.** The model never sees a
raw stream; it sees validated events (or `invalid` ones with the raw
text). `chunks` framing exists for tools, not for the model: the runtime
delivers chunks to progress consumers and to the ring only.

D5. **Mail, not polling, for a service's output.** The existing mailbox is
how asynchronous news reaches a session; a second channel would duplicate
its wake rules. Chatty services get digests, bounded by the ring, and the
digest policy is the tool's own declaration.

D6. **A typed `send` per family, a shared `processes` for the rest.** One
handle per start, several handles per family; the family's input schema
lives in one generated `T.send` tool present from load, while `list`,
`status`, `tail` and `stop` are shared so the session's tool list does not
grow with the number of families' management surfaces.

D7. **Explicit envdir root.** Secrets are provisioned where the operator
says (`--envdir-root`), never found by convention next to the tool.

## Resolved in review (2026-09-25, PR #574 comments)

* `send`: one tool per process family with a handle and a typed event; a
  second start of the same tool gives a second handle, addressable on its
  own; process management (`list`, `status`, `tail`, `stop`) is shared so it
  does not pollute the session's tools (§2.4, D6).
* `run_as` for `oneshot`: yes, split into its own feature ahead of the
  process modes (D2).
* Digest policy: per tool, declared in `process.digest` (§1.1, §2.3).
* `envdir`: explicit, an operator `--envdir-root` with a per-tool
  subdirectory, no implicit location (§1.2, D7).

## Open questions

* The typed `T.send` companion is generated at load for every `service`
  tool, started or not. Should an agent configuration be able to omit it
  (a service the model may start but never talk to)?
