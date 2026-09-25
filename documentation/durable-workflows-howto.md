# Durable Workflows How-To

This guide shows how to exercise and extend the durable-workflow features in
`agents-exe`.  Durable workflows let an agent turn pause after some tool calls,
persist its state, and resume later when external results arrive.

The canonical example flow is:

1. The user asks a question.
2. The LLM replies with three tool calls.
3. The runtime executes one call immediately and defers the other two.
4. The process yields (and can even be restarted).
5. External workers complete the deferred calls.
6. The runtime wakes the session with the results.
7. The LLM receives all three results and continues.

This guide covers:

* [A ready-to-run demonstrator](#running-the-demonstrator) that exercises the
  flow without calling a real LLM.
* [Core concepts](#core-concepts) behind durable execution.
* [CLI commands](#cli-commands-for-operators) for operating durable sessions.
* [Building your own durable agent](#building-your-own-durable-agent) in
  Haskell.

To run durable agents behind an HTTP API, with sessions in SQLite, see
[agents-server.md](agents-server.md).

---

## Running the demonstrator

The repository contains a self-contained executable called
`durable-workflow-demo`.  It uses a mock LLM so you can run it without API
keys.

Build and run:

```bash
cabal run durable-workflow-demo
```

You should see output similar to:

```
Durable workflow demonstrator
=============================

Step 1: run the async scheduler on the LLM turn.
  [executing in-process] fetch_local
Yielded partial turn:
  completed: 1
    - fetch_local: result from fetch_local
  deferred: 2
    - fetch_remote_a token: 550e8400-e29b-41d4-a716-446655440000
    - fetch_remote_b token: 6ba7b810-9dad-11d1-80b4-00c04fd430c8

Step 2: complete the deferred calls from the outside world.
  providing result for token 550e8400-e29b-41d4-a716-446655440000
  providing result for token 6ba7b810-9dad-11d1-80b4-00c04fd430c8
Turn is now complete.

Step 3: resume the session until the LLM produces a final answer.

=== Final LLM response ===
All three calls are complete. Results:
- fetch_local: result from fetch_local
- fetch_remote_a: external result for fetch_remote_a
- fetch_remote_b: external result for fetch_remote_b
```

The source is in `examples/durable-workflow-demo/Main.hs` and is heavily
commented.  It is the best starting point for adapting the workflow to your
own agents.

---

## Core concepts

Durable workflows are built from four composable layers.

### 1. Execution mode

`Agent.ctxExecutionMode` is either `Synchronous` (default) or `Asynchronous`.
Only asynchronous agents yield partial turns.

```haskell
asyncAgent = agent{ctxExecutionMode = Asynchronous}
-- or
asyncAgent = withExecutionMode Asynchronous agent
```

### 2. Tool-call policy

A `ToolCallPolicy` is a pure function that decides, for every tool call,
whether to run it synchronously, defer it, run it asynchronously, or isolate
it:

```haskell
policy :: ToolExecutionContext -> LlmToolCall -> ToolCallDisposition
policy _ctx call
    | callName call == "fetch_local"  = RunSync
    | callName call == "bash_command" = RunIsolated (Docker "agents-exe/bash-runner:latest")
    | otherwise                       = Defer (Reason "waiting for external service")
```

Dispositions can be decorated with timeouts, retries, cache keys, or labels:

```haskell
Decorate [WithTimeout 30, WithRetries 2] RunSync
```

Agents loaded from JSON can declare the same policy without Haskell code; see
[Configuring an agent for durable mode](#configuring-an-agent-for-durable-mode).

### 3. Pluggable executor

`ToolExecutor` decouples *how* a call runs from the session loop.  The
runtime provides `inProcessExecutor`, `yieldingExecutor`, `cachingExecutor`,
`isolatedExecutor`, and `mkDurableExecutor` to compose them.

For most use cases you only need to set `ctxToolCallPolicy`; the session loop
falls back to the agent's native `toolCall` for synchronous calls.

### 4. Persistence and wake/resume

When a call is deferred, the runtime optionally stores a serialisable
`ToolContinuationSnapshot` in a `ContinuationStore` and emits a
`PartialUserTurn`.  Later:

* `wakeSession` injects external results.
* `resumeSession` continues execution until completion or the next yield.
* `SessionBackend` (file, SQLite, or composite) stores session state across
  process restarts.

---

## CLI commands for operators

The `agents-exe session` command group creates and operates on stored
sessions.  These commands load sessions through the configured file-based
`SessionStore` and use the first supplied `--agent-file` when execution needs
to advance.  Every command that advances execution forces the agent into
asynchronous mode.

### Configuring an agent for durable mode

To defer calls from the CLI, declare a tool-call policy in the agent JSON.  The
policy and execution mode are applied everywhere the agent runs: `run`, the
TUI, the `session` commands, and sub-agents.

```json
{
  "slug": "my-agent",
  "executionMode": "asynchronous",
  "toolCallPolicyConfig": {
    "default": {"tag": "runSync"},
    "rules": [
      {"tool": "fetch_remote_a", "disposition": {"tag": "defer", "reason": "external worker"}},
      {"tool": "fetch_remote_b", "disposition": {"tag": "defer", "reason": "external worker"}},
      {"tool": "build_project", "disposition": {"tag": "runAsync"}}
    ]
  }
}
```

Rules match on the exact tool name; calls that match no rule get `default`.
Available disposition tags are `runSync`, `runAsync`, `runIsolated` (with a
`spec`), `defer` (with a `reason`), and `decorate` (with `decorators` and
`inner`).  Without `toolCallPolicyConfig`, every call runs synchronously and
nothing is ever deferred.

See [async-tool-calls.md](async-tool-calls.md) for the related
`asyncYieldStrategy`, `maxConcurrency`, and `asyncCallTimeoutSeconds` fields.

### Walkthrough

```bash
# 1. Create a session holding the initial prompt.  The LLM is not called yet.
agents-exe --agent-file ./my-agent.json session start \
    --prompt "Fetch the local and remote data, then summarise it"
# session-id: <session-id>

# 2. Advance until the turn yields on deferred calls.
agents-exe --agent-file ./my-agent.json session resume <session-id>

# 3. See what is waiting on the outside world.
agents-exe session pending <session-id>

# 4. Provide each deferred result.
agents-exe session complete <token> result.txt

# 5. Let the LLM continue with all results.
agents-exe --agent-file ./my-agent.json session resume <session-id>
```

`agents-exe run` can start the same flow: when a turn waits on deferred calls
it stops, stores the session, and prints a JSON report with the session id
and continuation tokens.  Continue it with `session complete` and
`session resume`.

The TUI (`agents-exe tui`) can also answer deferred calls directly, without
the `session` CLI: a run that stops on deferred calls shows a Pending panel
listing each call, and `Ctrl+Y` (`answer-pending`) puts the message editor
into "answer mode" for the selected one (`Ctrl+O` moves the selection; `Ctrl+W`
fails it instead) — type the result and send it, which
calls the same `completeCall` mechanism (with `autoResume` set) the server's
own `GET /v1/sessions/:id/pending` workers use, so the run resumes on its
own. See `documentation/tui.md`'s "Pending calls" section.

### Start a new session

```bash
agents-exe --agent-file ./my-agent.json session start --prompt "..."
agents-exe --agent-file ./my-agent.json session start --step --prompt "..."
```

Creates a new asynchronous session whose first turn contains the prompt, stores
it, and prints its `session-id`.  By default no scheduling happens, which
leaves every step to an external process or operator.  With `--step`, it also
runs one scheduling step right away.  It accepts the same prompt options as
`run` (`--prompt`, `--file`, media attachments, and so on).

### Run exactly one step

```bash
agents-exe --agent-file ./my-agent.json session step <session-id>
```

Runs a single scheduling step and prints the resulting session state: a turn
waiting for the LLM, the LLM's tool calls, a yielded partial turn, or the
final answer.

### Pause after one async step

```bash
agents-exe --agent-file ./my-agent.json session pause <session-id>
```

Runs one asynchronous step and persists the resulting session, like `step`.
If the turn yields, it prints the continuation tokens for deferred calls.

### Resume until completion or next yield

```bash
agents-exe --agent-file ./my-agent.json session resume <session-id>
```

Calls `resumeSession` and persists after each yield.

### List pending deferred calls

```bash
agents-exe session pending <session-id>
```

Shows each deferred call's tool name, call id, continuation token, and
disposition.

### Inject an external result

Create a result file.  JSON is parsed as a `UserToolResponse`; anything else
is treated as plain text.

```bash
# JSON result
echo '{"type":"text","content":"42"}' > result.json
agents-exe session complete <token> result.json

# Plain-text result
echo "42" > result.txt
agents-exe session complete <token> result.txt
```

`complete` scans all sessions in the store to find the one containing the
token.

### Run deferred isolated calls

```bash
agents-exe --agent-file ./my-agent.json session run-isolated <session-id>
```

If the agent has `ctxDeploymentRunner` configured, this executes deferred
`RunIsolated` calls through the runner and injects their results back into the
session.

---

## Building your own durable agent

The demo is a minimal Haskell program.  The key pieces are reproduced below.

### 1. Make the agent asynchronous and set a policy

```haskell
import System.Agents.Session.Base

myPolicy :: ToolCallPolicy
myPolicy _ctx call
    | callName call == "fetch_local" = RunSync
    | otherwise                      = Defer (Reason "external")

agent' = agent
    { ctxExecutionMode = Asynchronous
    , ctxToolCallPolicy = myPolicy
    }
```

### 2. Run the async scheduler

```haskell
import System.Agents.Session.Step (runStepM)

(_agent, result) <- runStepM convId agent' session0
case result of
    Left final      -> putStrLn "Session completed immediately."
    Right session1  -> putStrLn "Session yielded."
```

### 3. Wake with external results

```haskell
import System.Agents.Session.Wake (wakeSession)

let responses =
        [ (token, TextResponse "external answer")
        | (token, _call) <- deferredCalls
        ]
session2 <- wakeSession session1 responses
```

### 4. Resume

```haskell
import System.Agents.Session.Wake (resumeSession)

final <- resumeSession convId agent' session2
case final of
    Left (llmTurn, _session) -> print (llmTurn.llmResponse.responseText)
    Right session3           -> putStrLn "Yielded again."
```

### 5. Add durable storage

```haskell
import System.Agents.Session.Async (mkSqliteContinuationStore)
import System.Agents.SessionStore (mkSqliteSessionStore)
import Database.SQLite.Simple (open)

conn <- open ".agents-durable.db"
backend <- mkSqliteSessionStore conn
store   <- mkSqliteContinuationStore conn

let durableAgent = withDurableWorkflows backend store agent'
```

`withDurableWorkflows` is a convenience combinator that installs both a
session backend and a continuation store.

### 6. Add a tool cache

```haskell
import System.Agents.Tools.Cache (mkSqliteToolCache)

cache <- mkSqliteToolCache ".agents-cache.db"
let cachedAgent = withToolCache agent' cache
```

Cached synchronous calls are skipped on resume if their result is already in
the cache.

---

## Isolated execution

To run a tool call outside the current process, provide a `DeploymentRunner`:

```haskell
import System.Agents.Session.Base (dockerRunner, localProcessRunner)

runner = localProcessRunner "./worker.sh"
-- or
runner = dockerRunner "agents-exe/bash-runner:latest"

isolatedAgent = withDeploymentRunner runner durableAgent
```

The worker receives a stable JSON envelope on stdin and must print a result
envelope on stdout.  See `System.Agents.Session.Isolation` for the envelope
schema, or look at the worker script in `test/DurableWorkflowTests.hs`.

---

## Testing

The durable-workflow implementation is covered by:

* `test/DurableWorkflowTests.hs` — policy, wake/resume, cache, backends,
  isolation envelopes, and integration.
* `test/DurableWorkflowDeterminismTests.hs` — resume-twice and edge-case
  tests.
* `test/SessionDurableTests.hs` — CLI helper tests.

Run them with:

```bash
cabal test agents-tests
```

---

## Further reading

* `todos/durable-workflows.md` — design plan and architectural decisions.
* `todos/durable-workflows.progress.md` — implementation progress.
* `todos/durable-workflows-cli-start-plan.md` — design of `session start` and
  `session step`.
* `examples/durable-workflow-demo/Main.hs` — runnable mock-LLM demo.
* `documentation/cli-commands.md` — full CLI reference, including `agents session`.
* `documentation/sessions.md` — session storage and multi-location stores.
* `documentation/async-tool-calls.md` — background tool calls in the same process,
  with progress, cancellation and partial answers.
* `documentation/agents-server.md` — the same flow over HTTP, with sessions in SQLite
  and live events.

