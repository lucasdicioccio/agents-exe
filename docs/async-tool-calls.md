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
     "Capabilities": ["get-tool-call-status", "list-running-tool-calls", "cancel-tool-call"]
   }}
]}
```

| Capability | Use |
|------------|-----|
| `list-running-tool-calls` | The calls still running in this session |
| `get-tool-call-status` | Status, progress and (once final) result of a call; can block until it finishes with `wait_for_completion` |
| `cancel-tool-call` | Interrupt a running call |

Calls are addressed by the `tool_call_id` the model itself used (e.g.
`call_abc`); the internal UUID also works.

`get-tool-call-status` reports `orphaned` for a call that was running when its
process went away (e.g. the session was saved and reloaded elsewhere). Such a
call can never finish, so the stepper resolves it as failed rather than
waiting for it.

## Progress

Tools that run in the background can report progress, which the model reads
through `get-tool-call-status` and the TUI shows next to the running call.

* **Bash tools** report their latest output line, at most twice a second:
  `{"stream": "stdout", "line": "compiling module 12/40", "lines": 12, "bytes": 480}`.
* **Agents called as tools** report each step of the sub-agent:
  `{"message": "sub-agent calling read_file", "turns": 7}`.
* **Your own tools** can report anything JSON through `ctxProgressCallback` in
  the `ToolExecutionContext`.

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
  elsewhere reports them as orphaned.
* A pause/resume in the same process keeps the running calls only if the agent
  was built with `withAsyncEngine`; `runAsync` does not hand the engine back.
* If the model reads a result with `get-tool-call-status`, the delivery notice
  repeats it once.
* `maxConcurrency` is per agent. To cap several agents together, share one
  limit built with `newAsyncConcurrencyLimit` between their engines.
