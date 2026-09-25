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
Three optional objects and one flag (`check`, §1.5) are new.

```json
{
  "protocol": 2,
  "slug": "log_watcher",
  "check": true,
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
    "sandbox": { "fs": { "read": ["/var/log"], "write": ["state"] }, "net": "none" },
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
| `sandbox` | `{"fs": {"read": [...], "write": [...]}, "net": "host"\|"none", "tmp": "shared"\|"private"}`: what the process may touch (§1.4). Enforced by one operator-configured mechanism, `bwrap` or `landlock`; never images or containers. |
| `envdir` | A subdirectory name under the operator's explicit `--envdir-root`, read the daemontools way: one file per variable, file name is the name, file content is the value. Read at start; absent files are absent variables. Lets an operator provision secrets without touching the tool or the checkout. There is no implicit default location: a tool that declares `envdir` while no `--envdir-root` was given fails to load with a `LoadingError` naming the field. |
| `environment` | Literal variables, added last (they win over `envdir`). |
| `rundir` | A directory, relative to the tool's directory, created if missing and made the process's working directory; where a `service` keeps its state. |
| `timeout` | `start`: seconds to reach `ready` or fail the start; `stop`: seconds between the cancel signal and `SIGKILL`; `idle`: seconds without any output or input event after which a `service` is stopped (`null`: never). |
| `on_exit` | For a `service`: `report` (default: the model is told, the handle becomes final), `restart` (bring it back, bounded by a small backoff; every restart is an event), `fail` (the call that started it fails if it has not completed yet). |
| `cancel` | The signal sent first on cancel/stop: `SIGTERM` (default), `SIGINT`, or a JSON event `{"send": {...}}` written to stdin before the signal, for tools that prefer a polite request. |

### 1.3 `commands` and `help`: several actions in one binary (from #539)

Out of scope of the process story but part of the same `describe` bump:
`"commands": [ {"name", "description", "args"} ]` lets one binary expose
several tools (`slug.name` each), and `help` (`./tool help [command]`)
gives the runtime a longer text to disclose on demand rather than in every
system prompt. A `setup` step (`./tool setup`) that provisions the tool's
own dependencies is left for a later spec: it is an operator concern, not a
runtime one.

### 1.4 `sandbox`: what the process may touch, without images

A small declaration, no more: `fs.read` and `fs.write` are lists of paths
(absolute, or relative to the tool's directory), `net` is `host` (default)
or `none`, `tmp` is `shared` (default) or `private`. Everything not listed
is not granted; the tool's own directory is always readable so it can run.
Like `run_as`, the tool names what it needs and the **operator** names how
it is enforced (`--sandbox bwrap`, `--sandbox landlock` or `--sandbox auto`);
a tool that declares `sandbox` with no mechanism configured fails to load
with a `LoadingError`, it never runs unconfined by silence. There is **no
silent default**: the two mechanisms give different guarantees (bubblewrap
hides paths and really cuts the network, Landlock denies access and covers
TCP only), so picking one for the operator could quietly weaken what a tool
asked for.

`--sandbox auto` is the explicit opt-in to "per tool, the strongest
mechanism that can honour every declared field": bubblewrap first, then
Landlock, and a tool declaring `net: none` or `tmp: private` skips Landlock
rather than being weakened, failing to load when no mechanism qualifies.
It is the ordered-backends idea of §1.5 applied to mechanisms, and
`agents-exe check --probe` prints what `auto` resolved to for each tool. `sandbox` composes with
`run_as` (the sandbox is set up for the process that runs as that user).

* **bubblewrap** builds a mount namespace: only the declared paths (plus the
  minimum to exec the tool) exist, `net: none` is a network namespace,
  `tmp: private` a fresh tmpfs, and the process dies with agents-exe. It
  needs unprivileged user namespaces, which many distributions restrict: a
  plain `bwrap` on the Ubuntu machine this was written on failed with
  `setting up uid map: Permission denied` (`kernel.apparmor_restrict_unprivileged_userns=1`;
  not conclusive, that shell may itself have been inside another sandbox).
* **Landlock** is a kernel security module needing no privilege and no
  namespaces: the runtime applies a ruleset in the child before `exec`.
  Paths outside the ruleset are denied (not hidden), and on kernels with
  network rules it can deny TCP bind and connect, which is not the same as
  no network (to verify in the slice, then state precisely in `check`).

`agents-exe check` probes the configured mechanism for real, the way it would
run a tool, and reports what it can and cannot enforce (for example
"`net: none` best effort under landlock"). Where a mechanism cannot honour a
declared field the tool fails to load naming the field; it is never silently
weakened.

**Non-goal: image or container isolation.** agents-exe has no notion of an
image, and this spec adds none. A tool that wants podman or docker calls it
from its own script, like any other command, and agents-exe sees an ordinary
process. (Reviewed against `juhp/encapsule`, an interactive podman
dev-shell: useful vocabulary for named capabilities, the wrong shape for a
non-interactive tool call.)

These are declarations, not privileges: an agent's configuration (and the
operator's `--set`/`--pin` parameters, `tool-partial-application.md`) may
refuse or override them. `run_as` in particular is only honoured when the
operator configured a mechanism; otherwise the tool loads with a
`LoadingError` naming the field, the same way a `with` binding that names
an undeclared parameter does today.

### 1.5 Health: a real probe, and ordered backends per capability

The pattern comes from Agent-Reach (a "capability layer" that selects and
health-checks the upstream tools an agent uses, MIT): the same capability
can be served by several tools, and whether one *works here, now* is found
by running a cheap command, never by looking the binary up on `PATH` (a
stale shim passes `which` and cannot execute).

**`check`, a third verb.** A v2 tool may say `"check": true` in `describe`
and then answers `./tool check`: a cheap, **read-only** self-test (no
writes, no login, no remote mutation, no long-lived process; a `service`
is not started) that prints one JSON document and exits 0 when the tool is
usable:

```json
{ "ok": false, "detail": "jina reader answered 429", "fix": "set JINA_API_KEY in the envdir" }
```

`detail` and `fix` are for the operator, `fix` is a prescription. The
runtime runs `check` exactly as it would run the tool (same `runtime`:
`run_as`, `sandbox`, `envdir`, timeouts), so a probe that passes proves the
sandbox and the user work too. A tool that cannot be checked without side
effects omits `check`; it is then `unverified` (it loaded and `describe`
answered, nothing more) and is never reported `ok`.

**A capability is an ordered list of backends.** The model sees one tool,
`web_read`; the agent configuration says which tool binaries can serve it,
preferred first:

```json
{ "capability": "web_read", "backends": ["tools/web/read-jina", "tools/web/read-curl"] }
```

All backends of a capability must declare the same `args` (checked at load,
a `LoadingError` otherwise) so the model's schema does not depend on which
one is active. Switching backends is reordering the list, or an operator
override `--backend web_read=read-curl` that moves the named backend first
(an unknown name is ignored, so a stale override never hides a working
backend). The **active backend** is the first one whose probe is `ok`,
else the first `unverified` one, else none (the capability fails to load
when required, is omitted with a warning otherwise). Every tool result
carries the backend that served it, so the model can say what it used.

**A failed call does not silently retry the next backend.** The model sees
the failure and which backend produced it; automatic fallback is left out of
the first slice (open question).

**`agents-exe check --probe [--json]`** runs every probe and reports, per
capability, the active backend and every candidate:

```json
{ "capability": "web_read", "active": "read-jina",
  "backends": [ { "name": "read-jina", "status": "ok", "ms": 310 },
                { "name": "read-curl", "status": "unverified" } ] }
```

`status` is `ok`, `failed` (with `detail` and `fix`), `unverified` or
`skipped` (a sandbox mechanism that cannot enforce a declared field, §1.4).
Plain `check` keeps its meaning (configuration loads: exit 0 / 1); with
`--probe` it exits 2 when the configuration loads but a required capability
has no active backend. Do not confuse it with `ready` (§1.1): `ready` says a
process that was started is up, `check` says a tool could work before
anything is started.

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
`run_as` and `sandbox` behind operator-configured mechanisms (`bwrap`,
`landlock`), which ship ahead of the
process modes because a low-privilege `oneshot` tool is valuable alone;
(3) `stream` on the existing async engine; (4) `service`: the handle
entity, mail delivery and `digest`, the typed `T.send` companions, the
shared `processes` capability, and the `runtime` fields only a service
uses (`timeout.idle`, `on_exit`). #539's `commands`/`help` are a fifth,
independent slice, and (§1.5) the `check` verb with `check --probe --json`
is a sixth, small one that lands with (1); capabilities with ordered
backends and the `--backend` override are a seventh, depending on it.

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
lives in one generated `T.send` tool present from load (an agent
configuration may omit it, saving its tokens), while `list`,
`status`, `tail` and `stop` are shared so the session's tool list does not
grow with the number of families' management surfaces.

D7. **Explicit envdir root.** Secrets are provisioned where the operator
says (`--envdir-root`), never found by convention next to the tool.

D8. **Process isolation, not image isolation.** `sandbox` is enforced by
bubblewrap or Landlock, chosen by the operator (explicitly, or with the
opt-in `--sandbox auto`, never by a silent default); agents-exe never builds,
pulls or names an image. It composes with `run_as`, refuses rather than
weakens, and is probed by `check`.

D9. **Probe, don't look up.** Whether a tool works is found by running it
(`check`, under its real `runtime`), read-only, reported as `ok`, `failed`,
`unverified` or `skipped`; a tool that cannot be probed safely is
`unverified`, never `ok`. Backends are an ordered list per capability, the
first `ok` one is active, and the tool result names the backend used.
Pattern from Agent-Reach (§1.5).

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
* The generated `T.send` companion is optional per agent: an agent
  configuration may omit it for a service the model may start but never talk
  to, which saves the tool's tokens (owner's answer, 2026-09-25; D6).
* Isolation backend: bubblewrap and Landlock, no images; whoever wants podman
  calls it from the tool's own script (§1.4, D8).
* Sandbox mechanism selection: no silent default; the operator picks, or opts
  in to `--sandbox auto` (strongest mechanism that honours every declared
  field), shown by `check --probe` (owner, 2026-09-25; §1.4, D8).
* Health: adopt Agent-Reach's doctor pattern (ordered backends, a real probe,
  `--json`), but not Agent-Reach itself, which is deferred as heavy
  (owner, 2026-09-25; §1.5, D9).

## Open questions

* Where is a capability's backend list declared: in the agent configuration
  (as sketched), or in a manifest next to the tool directories so several
  agents share it?
* When are probes run and how long are they cached: once at load, lazily on
  the first call, or with a TTL? A probe that costs a network call should
  not run on every session start.
* Automatic fallback to the next backend on a failed call: worth it for
  transient failures if the tool declares which exit codes mean "try
  another" (`fallback_on`), or is the model seeing the failure enough?
