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
  a handle, and one system tool handles every ongoing tool process
  (list, status, tail, send, stop) instead of a bespoke tool per process;
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
| `input.event` | a JSON Schema | What the runtime accepts to send to this process; also what the model sees as the argument schema of `send` (§3). |
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
| `envdir` | A directory, relative to the tool's directory, read the daemontools way: one file per variable, file name is the name, file content is the value. Read at start; absent files are absent variables. Lets an operator provision secrets without touching the tool. |
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
call is today (`OS.Conversation.ToolCalls`), owned by the session, with:

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
(`session-mailbox.md`): each event, or a digest of them per round when
they are chatty, is an envelope from `process:<id>`, so the model's turn
is woken the same way any other asynchronous news wakes it, with the same
`wakeOn` controls. This is the `notify` progress level that spec deferred
(D9 there), given a producer.

### 2.4 One tool for every process: `processes`

Rather than a bespoke `send_to_log_watcher` per tool, the system toolbox
gains one capability with sub-commands (the shape of `get-tool-call-status`
and friends):

| Sub-command | Arguments | Answer |
|---|---|---|
| `list` | | every process of the session (and, for a root agent, of its sub-sessions), with status and age |
| `status` | `process` | the handle with its counters, last events, last stderr lines |
| `tail` | `process`, `n` | the last `n` output events, oldest first |
| `send` | `process`, `event` | writes one event to the process's stdin; `event` is validated against the declared `input.event`; the answer is the events the process produced in reply within a short window, or `accepted` |
| `stop` | `process`, `force?` | the declared cancel, then `SIGKILL` after `timeout.stop`; the exit becomes the answer |

The model discovers which processes exist and what they accept through
`list` and `status` (progressive disclosure: the input schema is shown
there, not in every system prompt). A tool's `description` should say it is
a service and name the events it accepts, in a sentence.

## 3. What the model sees, end to end

1. The tool list holds `log_watcher` (start it) and `processes` (talk to
   anything started). Starting is an ordinary tool call with the declared
   `args`.
2. The result is the handle. The model continues; when the watcher reports
   a line, mail arrives: `from process:p1 {"line": "...", "level": "error"}`.
3. The model calls `processes send {process: p1, event: {op: "pattern",
   value: "ERROR"}}`; the answer is `accepted` or the watcher's reply.
4. `processes stop p1` when done; or the session ends and every process of
   it is stopped with its declared cancel (a session delete stops them the
   way it cancels runs).

## Decisions (proposed)

D1. **The protocol number, not new commands.** `"protocol": 2` in
`describe` is the whole opt-in; `run` keeps its name and argument passing,
so a v2 tool can be tried as a v1 tool by ignoring the new fields.

D2. **Four slices, each shippable.** (1) `describe` v2 parsing, `oneshot`
with framing and validation; (2) `stream` on the existing async engine;
(3) `runtime` (`envdir`, `environment`, `rundir`, timeouts, cancel) for all
modes, `run_as` behind an operator-configured mechanism; (4) `service`:
the handle entity, mail delivery, the `processes` capability. Each slice is
a feature depending on the previous one; #539's `commands`/`help` are a
fifth, independent slice.

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
its wake rules. Chatty services get digests, bounded by the ring.

## Open questions

* Should `send` be its own top-level tool per service (a tool named after
  the process, with the declared `input.event` as its schema), generated
  when the process starts, so the model gets a typed function instead of an
  untyped `event` argument? It reads better in the tool list and worse in
  the tool cache and in `agentView`; the `processes` tool could stay as the
  discovery and control surface either way.
* `run_as` for a `oneshot` tool is a small, valuable change on its own
  (an agent runs a tool as a low-privilege user); should it ship before
  the rest, as part of slice 3 or ahead of it?
* Digest policy for chatty services: per round, per N events, or per
  declared `"digest": {...}` in `process`? A per-tool declaration is the
  least surprising for the tool author.
* `envdir` relative to the tool directory ties secrets to the checkout;
  an operator may prefer `--envdir-root` on the agents-exe command line
  with per-tool subdirectories. Both can hold.
