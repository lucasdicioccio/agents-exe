# Asynchronous Tool Calls

By default an agent runs every tool call of a turn one after another and only
answers the LLM once all of them are done. In asynchronous mode, calls run
concurrently in the background, the LLM can be handed a partial answer while
they run, and it can ask about, wait for, or cancel them.

This is useful when a turn issues several slow calls (builds, test runs, HTTP
requests), and when you want the model (or the user) to keep working while a
long call finishes.

For deferred calls completed by an *external* worker (rather than in this
process), see [durable-workflows-howto.md](durable-workflows-howto.md).

## Enabling it

Async execution is configured per agent in the agent JSON:

```json
{
  "slug": "my-agent",
  "executionMode": "asynchronous",
  "asyncYieldStrategy": "yieldOnAnyProgress",
  "maxConcurrency": 4,
  "asyncCallTimeoutSeconds": 900,
  "toolCallPolicyConfig": {
    "default": {"tag": "runSync"},
    "rules": [
      {"tool": "build_project", "disposition": {"tag": "runAsync"}},
      {"tool": "run_tests", "disposition": {"tag": "runAsync"}}
    ]
  }
}
```

| Field | Meaning | Default |
|-------|---------|---------|
| `executionMode` | `synchronous` or `asynchronous` | `synchronous` |
| `toolCallPolicyConfig` | Which tools run in the background (`runAsync`), inline (`runSync`), isolated (`runIsolated`), or deferred to an external worker (`defer`) | everything `runSync` |
| `asyncYieldStrategy` | When a step hands control back to the LLM while calls run | `yieldWhenAllDone` |
| `maxConcurrency` | Calls running at once for this agent | 4 |
| `asyncCallTimeoutSeconds` | Give up on a call that runs this long | no limit |

Yield strategies:

| Value | Behaviour |
|-------|-----------|
| `yieldWhenAllDone` | Wait for every call of the turn (like synchronous mode, but concurrent) |
| `yieldOnAnyProgress` | Answer the LLM as soon as one call finishes |
| `{"tag": "yieldOnTimeout", "milliseconds": 500}` | Answer after the given delay, finished or not |

Both `executionMode` and `toolCallPolicyConfig` are needed: the mode enables
the machinery, the policy decides which calls actually go to the background.

The settings apply everywhere an agent runs: `run` (one-shot), the TUI, the
`session` commands, and agents called as tools by other agents.

## Wrappers

`rules` pick how a call runs. `wrappers` add decorators around it: a timeout,
retries, a cap on the result, a hook that may deny or defer the call. Every
wrapper whose `match` holds applies, in file order, the first one outermost.

```json
"toolCallPolicyConfig": {
  "default": {"tag": "runSync"},
  "rules": [],
  "wrappers": [
    {"match": {"tool": "http_*"},
     "decorators": [{"tag": "retries", "count": 2}, {"tag": "timeout", "seconds": 30}]},
    {"match": {"tool": "bash_*", "args": {"path": "command", "glob": "rm *"}},
     "decorators": [{"tag": "before", "hook": {"command": "hooks/approve"}}]},
    {"match": {"args": {"all": [{"path": "env", "equals": "production"},
                                {"not": {"path": "dry_run", "equals": true}}]}},
     "decorators": [{"tag": "before", "hook": {"command": "hooks/approve"}}]}
  ]
}
```

`match` has two optional parts, and both must hold:

* `tool`: a glob (`*` only) on the name of the tool as the model calls it.
* `args`: a predicate on the arguments of the call.

| Predicate | Holds when |
|-----------|-----------|
| `{"path": p, "equals": v}` | there is a value at `p` and it is the JSON value `v` |
| `{"path": p, "glob": g}` | there is a string at `p` and it matches the glob `g` |
| `{"path": p, "exists": true}` | there is a value at `p` (`null` counts); `false` for the opposite |
| `{"all": [...]}`, `{"any": [...]}`, `{"not": {...}}` | every, at least one, or not |

A path is a dot-separated list of object keys and array indices
(`options.targets.0`); the empty path is the whole arguments object.

Three things to know when a wrapper guards a tool:

* A missing value fails `equals` and `glob`. Write "unless `dry_run` is true"
  with `not`, as above: it then also covers calls that leave `dry_run` out.
* The predicate sees the arguments as the model sent them. A value added by a
  binding is not seen, the same rule as for hooks. Arguments that are not
  valid JSON are `null` to the predicate.
* A predicate that cannot be read (a misspelt operator, two operators in one
  object) is a loading error for the agent, not a rule that never matches.

## What the LLM sees

A call that has not finished when the LLM is asked for its next completion
gets a placeholder tool message instead of a result:

```json
{"status": "running",
 "message": "This tool call has not finished yet. Its result will be delivered in a later message. Use get-tool-call-status with this tool_call_id to inspect it, or cancel-tool-call to stop it.",
 "tool_call_id": "call_abc"}
```

Every tool call always gets exactly one tool message, as OpenAI-compatible
APIs require.

When the call finishes, its result is delivered in the next user message,
once, as a notice:

```
Background tool calls finished since their placeholder responses:

tool_call_id call_abc (run_tests) completed:
42 tests passed
```

If the model already read the result with `get-tool-call-status`, the notice
does not repeat it:

```
tool_call_id call_abc (run_tests) completed: result already read with get-tool-call-status
```

A session with a mailbox (the TUI, one-shot runs, the server) also gets a
`ToolCallFinished` mail for each finished call, which is what wakes a waiting
session. That mail is not shown to the model: the result reaches it once, as
the call's tool message or in the notice above. The one exception is mail
about a call the current process does not know (a durable mailbox read after a
restart), which is shown like any other mail, since the call itself is then
reported as orphaned.

If the LLM ends its turn while calls are still running, the session does not
stop: it waits for the results and sends them.

## Tool-call capabilities

Enable these in the System Toolbox so the model can manage its own background
calls:

```json
{"builtinToolboxes": [
  {"tag": "SystemToolbox",
   "contents": {
     "Name": "system",
     "Description": "System context and background tool calls",
     "Capabilities": ["get-tool-call-status", "list-running-tool-calls", "cancel-tool-call", "send-to-tool-call"]
   }}
]}
```

| Capability | Use |
|------------|-----|
| `list-running-tool-calls` | The calls still running in this session |
| `get-tool-call-status` | Status, progress and (once final) result of a call; can block until it finishes with `wait_for_completion` |
| `cancel-tool-call` | Interrupt a running call |
| `send-to-tool-call` | Write to the sub-agent a running `prompt_agent_<slug>` call started |

Calls are addressed by the `tool_call_id` the model itself used (e.g.
`call_abc`); the internal UUID also works.

`send-to-tool-call` takes `tool_call_id`, `text`, and optionally
`expects_reply` and `interrupt`. It is `send-message` to the session of the
helper, found from the call instead of from a session id, with the same mail
scopes. The helper reads the message before its next completion, so a
message that arrives while the helper writes its final answer is not read.
The call must still be running and must be a sub-agent call: there is nothing
to write to in a bash or MCP call. It works under the runner (the TUI and
`agents-server`, where the helper is a session of its own) and in `run`.

`get-tool-call-status` reports `orphaned` for a call that was running when its
process went away (e.g. the session was saved and reloaded elsewhere). Such a
call can never finish, so the stepper resolves it as failed rather than
waiting for it.

## Across a restart

A background call lives in the process that started it. When that process
goes away while the call runs, the call is lost, and what happens next depends
on the tool.

By default the call is reported as orphaned, as described above: the model is
told the result is lost and decides what to do. Nothing is run twice.

A tool that is safe to run twice can be marked so, with the `rerunOnRestart`
decorator in a `wrappers` rule:

```json
{"toolCallPolicyConfig": {
   "default": {"tag": "runSync"},
   "rules": [{"tool": "bash_run_tests", "disposition": {"tag": "runAsync"}}],
   "wrappers": [
     {"match": {"tool": "bash_run_tests"},
      "decorators": [{"tag": "rerunOnRestart", "times": 1}]}
   ]}}
```

A call of such a tool that was running when its process went away is started
again by the next process, from the beginning, with the same arguments and
under the same `tool_call_id`. Its result is delivered like any late result,
with a note that the call ran again:

```
tool_call_id call_abc (bash_run_tests) completed (run again from the start after a restart):
42 tests passed
```

What this does and does not give:

* It is a re-run, not a resume. Nothing of the first run is kept (output,
  progress, a half-written file), and the tool is executed at least twice in
  total. Only mark tools for which that is harmless: reads, builds, test runs,
  idempotent requests.
* `times` (default 1) is how many times one call may be started again. Once
  it is used up, a further restart reports the call as orphaned. This bounds a
  tool that takes its process down with it.
* The session is the record: a call is known to be in flight because the
  stored session says `running` for it. No other storage is involved, so it
  works with every session store, in the TUI and on the server alike.
* The call is started again when the session next runs. The server and the
  TUI do that by themselves at startup for the sessions a previous process
  left running (`recoverOnStartup`); elsewhere (`agents-exe session resume`),
  it happens when you resume.
* A call that ended just before the restart, and whose result is in the
  session's mailbox, is not run again.
* A call that was cancelled (`cancel-tool-call`, cancelling the run) is not
  started again.
* Not covered: the calls of a step that was interrupted before it was stored.
  The session then still ends with the LLM's request for them, and the next
  run executes that whole step again, whatever the tools are marked with.
  This is how every interrupted step has always been handled.

## Progress

Tools that run in the background can report progress, which the model reads
through `get-tool-call-status` and the TUI shows next to the running call.

* **Bash tools** report their latest output line, at most twice a second:
  `{"stream": "stdout", "line": "compiling module 12/40", "lines": 12, "bytes": 480}`.
* **Agents called as tools** report each step of the sub-agent:
  `{"message": "sub-agent calling read_file", "turns": 7}`. This applies to a
  `prompt_agent_<slug>` call that still runs in-tool (narrowed with
  `bindings`/`with`/`as`, or outside the runner). Under the runner, a
  sub-agent call with no narrowing runs as its own session instead
  (`todos/os-as-standalone-server.md`, Phase 5): its progress is that
  session's own `session.updated`/`text.delta`/`tool.*` events, not this
  per-step callback.
* **Your own tools** can report anything JSON through `ctxProgressCallback` in
  the `ToolExecutionContext`.

### Asking for the model's attention

Progress is read by the model when it asks. A tool that should not wait for
that reports a payload at the **notify** level: any JSON object whose `level`
is `"notify"`, with the text in `message`.

* **Bash tools** write a line starting with `::notify::` on standard error:
  `echo "::notify:: 3 tests are red, still running" >&2`.
* **Your own tools** call `ctxProgressCallback` with `notifyProgress "…"`.

Such a payload is kept as progress like any other, and is also sent to the
session as mail:

```
[mail 6f1c… from tool call 0d5e…]
tool call call_abc (run_tests) notifies:
3 tests are red, still running
```

The model gets it with its next completion. If the session was idle, waiting
for its background calls, or in `wait`, the mail wakes it, and the call goes
on running. A call the step is still attached to is not interrupted: the mail
is delivered when the step hands control back. A call sends at most 16 such
mails, and they count against the mailbox bound; what is over is progress
only.

This needs `executionMode: asynchronous` (always on under the runner: the TUI
and `agents-server`), because only a call run by the async engine has
somewhere to report to.

MCP and OpenAPI tools do not report progress yet. An OpenAPI call waits for
one complete HTTP response, so there is nothing to stream. MCP *does* define
progress notifications (`ProgressNotification` in `System.Agents.MCP.Base`),
but the client currently ignores them; forwarding them to
`ctxProgressCallback` would make MCP tools report progress like bash tools.

The 50 most recent progress entries per call are kept.

## Cancelling

`cancel-tool-call` interrupts the call's thread. A bash tool runs its script
in its own process group, so cancelling kills the script *and* whatever it
started. The same happens when a call exceeds `asyncCallTimeoutSeconds`.

A call is cancelled through the engine that started it, wherever that engine
is in the process: cancelling works the same after the session was paused and
resumed with a newly built agent. Only a call whose process is gone cannot be
interrupted; it is reported as orphaned.

Background calls are cancelled when the agent that owns them finishes or
fails, so nothing keeps running (or keeps a subprocess alive) after a run.

## In the TUI

Running calls appear under the turn that issued them, with their latest
progress:

```
[Partial] > run the tests and the linter
  ✓ lint (call_1): completed
  ⏳ run_tests (call_2): running: 128 tests passed
```

A line above the conversation lists what is still running. While background
calls run and the LLM has nothing to do, the TUI accepts input, so you can
keep talking; whichever comes first — your message or the results — is sent
to the model.

## In one-shot runs

`agents-exe run` waits for background calls: they cannot outlive the process.

It stops early only when a turn waits on *deferred* calls, which an external
worker has to complete. It then prints a JSON report and stores the session so
it can be continued:

```json
{"status": "paused",
 "reason": "waiting for deferred tool calls",
 "session_id": "…",
 "deferred_calls": [{"tool": "approve_deploy", "tool_call_id": "call_9", "continuation_token": "…"}]}
```

Continue it with `agents-exe session complete` and `agents-exe session resume`
(see [durable-workflows-howto.md](durable-workflows-howto.md)).

## Logs

Each step of a background call is traced, with the ids needed to follow one
call through a log:

| `kind` | When | Extra fields |
|---|---|---|
| `tool_call.queued` | the call is accepted and waits for a free slot (`maxConcurrency`) | |
| `tool_call.started` | it got a slot and runs | `queued_ms` |
| `tool_call.progressed` | the tool reported progress | `payload_bytes` (the payload itself is not logged) |
| `tool_call.completed` | it returned a result | `elapsed_ms` |
| `tool_call.failed` | it threw | `elapsed_ms`, `error` |
| `tool_call.timed_out` | it outlived `asyncCallTimeoutSeconds` | `timeout_seconds` |
| `tool_call.cancelled` | it was cancelled (`cancel-tool-call`, a pause that cancels, the end of the run) | |

Every line has `session_id`, `conversation_id`, `tool_call_id`, `tool`, and
`provider_call_id` when the provider gave one; a call made by a sub-agent
also has `sub_agent: true`. A call ends with exactly one of the last four.

```json
{"ts":"…","kind":"tool_call.started","session_id":"…","conversation_id":"…","tool_call_id":"…","tool":"bash_run_tests","provider_call_id":"call_abc","queued_ms":0}
{"ts":"…","kind":"tool_call.completed","session_id":"…","conversation_id":"…","tool_call_id":"…","tool":"bash_run_tests","provider_call_id":"call_abc","elapsed_ms":8412}
```

Where they go:

- `agents-server` (and `agents-exe serve`): its JSON log on stderr.
- `agents-exe tui`, `run` and `mcp-server`: the file given with
  `--log-json-file` (same fields, no `ts`), and, in Haskell `show` form, the
  `--log-file`.

In Haskell, these are `AsyncTrace` values
(`System.Agents.Session.Async.Engine`). An agent hands its `ctxAsyncTracer`
to the engine it creates; `AgentFactory.buildAgent` sets it from its own
tracer, as `AsyncToolCallTrace`.

## Session files

Partial turns are stored in the session, with each call's state. `session-print`
shows them:

```
## ⏸️ Step 4: Partial Turn (in progress)
_(Some tool calls had not finished when this turn was sent)_

### ⏳ Tool Call Status

- `lint` (`call_1`): completed
- `run_tests` (`call_2`): running (placeholder sent)
```

## Limits

* Background calls live in the process that started them. A session reloaded
  elsewhere reports them as orphaned, or runs them again when their tool is
  marked `rerunOnRestart` (see "Across a restart"). A call is never resumed
  from where it stopped.
* `runAsync` returns only the session. To pause and resume in the same
  process, use `runAsyncKeepingAgent` and resume with the agent it returns:
  that agent holds the engine that owns the running calls. A session resumed
  with a freshly built agent still gets their results and can still cancel
  them, but the new engine's `maxConcurrency` does not count them and they are
  not cancelled when the resumed run ends.
* `run` and `runWithProgress` return only a final result, so they cannot hand
  back a session that waits on deferred calls: they throw
  `BlockedOnDeferredCalls`, which carries that session. Use `runUntilBlocked`
  or `runAsync` where deferred calls are expected.
* `maxConcurrency` is per agent. To cap several agents together, share one
  limit built with `newAsyncConcurrencyLimit` between their engines.
* The only limit on background calls is that number of calls running at once.
  There is no rate limit (calls started per second or minute).
