# Plan: Durable Workflows for agents-exe
See [`todos/durable-workflows.progress.md`](durable-workflows.progress.md) for implementation progress.

## Goal
## Goal

Turn agents-exe into a system suitable for:

- **Durable executions**: sessions can be stored in and resumed from databases.
- **Isolated deployments**: tool calls can run inside Docker or as local processes, transparently.
- **Transparent synchronous/asynchronous tool calls**: the runtime can pause a turn after any subset of tool calls, yield to the outside world, and wake up later when results arrive.

Requested example flow:

1. user-turn 1: some question
2. agent-turn 1: reply with 3 tool calls
3. user-turn: perform 1 tool call, mark 2 as deferred
4. system: yields (possibly even reboots)
5. system: wakes up, executes deferred tool calls and places results, then wakes up again
6. user-turn: provides 3 tool results to agent
7. agent: continues

This document lists the primitives and architectural changes needed to support that flow.

---

## Current state

The codebase already has scaffolding:

- `ExecutionMode` (`Synchronous` / `Asynchronous`)
- `PartialUserTurn`
- `ContinuationToken`, `AsyncToolResponse` (`ToolComplete` / `ToolYield`)
- `ContinuationStore` / `ToolContinuation`
- `ToolCache`
- OS persistence layer with SQLite/Postgres backends
- File-based `SessionStore`

What is missing:

- The async scheduler does not actually use `ctxAsyncToolCall` or `AsyncToolResponse`; it just runs tools synchronously and pauses after each one.
- There is no per-call decision primitive for sync vs. deferred vs. isolated execution.
- There is no "wake up and inject external results" primitive.
- `ContinuationStore` has stubbed load/list implementations.
- `ToolExecutionContext` contains non-serializable fields (portal, world, event queue), making durable continuations fragile.
- Session storage is file-only.
- There is no abstraction for running a tool call outside the current process (Docker / subprocess).

---

## Phase 1 — Core primitives (types & interfaces)

### 1.1 Rich tool-call state inside a turn

Introduce an explicit state machine for every LLM-issued tool call within a user turn:

```text
Ready      -> Running | Deferred | Completed
Running    -> Completed | Failed
Deferred   -> Ready | Completed   (via external wake)
Completed  -> terminal
```

Add a `TrackedToolCall` type wrapping `LlmToolCall` with:

- `tcId :: ToolCallId` — stable ID for matching external results
- `tcState :: ToolCallState`
- `tcResult :: Maybe UserToolResponse`
- `tcContinuation :: Maybe ContinuationToken`
- `tcPolicy :: AppliedPolicy` — record of why it ran this way

Refactor `PartialUserTurnContent` to hold `[TrackedToolCall]` instead of separate `pCompletedResponses`, `pPendingCalls`, and `pPendingContinuations` lists.

### 1.2 Tool-call policy / decorator

Add a pure decision primitive:

```haskell
data ToolCallDisposition
  = RunSync
  | RunAsync                  -- yield a continuation token
  | RunIsolated IsolationSpec -- run outside the current process
  | Defer Reason              -- intentionally pause (e.g., approval)
  | Decorate [Decorator] ToolCallDisposition

type ToolCallPolicy =
  ToolExecutionContext -> LlmToolCall -> ToolCallDisposition
```

This is the requested decoration primitive. It lets the runtime decide, per call, whether to execute immediately, defer, or run in isolation.

Provide `defaultToolCallPolicy = const $ const RunSync` for backward compatibility.

### 1.3 Pluggable executor interface

Decouple *how* a tool call runs from the session loop:

```haskell
data ToolExecutor = ToolExecutor
  { execSync  :: ToolExecutionContext -> LlmToolCall -> IO UserToolResponse
  , execAsync :: ToolExecutionContext -> LlmToolCall -> IO AsyncToolResponse
  }
```

Concrete executors to provide:

- `inProcessExecutor` — current behavior
- `yieldingExecutor` — always returns `ToolYield`
- `cachingExecutor :: ToolCache -> ToolExecutor -> ToolExecutor`
- `isolatedExecutor :: DeploymentRunner -> ToolExecutor`

Replace the single `Agent.ctxAsyncToolCall` with an optional `ToolExecutor` selected by the policy.

---

## Phase 2 — Async scheduler in the session loop

### 2.1 Replace the one-at-a-time async loop

Current `runStepMAsync` executes exactly one pending call per step and then pauses.

New scheduler behavior, given a set of `TrackedToolCall`s:

1. Classify each call via `ToolCallPolicy`.
2. Run all `RunSync` calls and collect results.
3. For `RunAsync` / `Defer` calls, generate continuation tokens and move them to `Deferred`.
4. Yield a `PartialUserTurn` containing:
   - completed calls,
   - deferred calls with tokens,
   - any remaining ready calls.

This matches the requested flow:

> perform 1 tool call, mark 2 as deferred → system yields → wakes up, executes deferred calls, places results → user-turn provides 3 tool results.

### 2.2 Continuation snapshot

When a call is deferred, generate a `ContinuationToken` and store a serializable `ToolContinuationSnapshot`:

- `token`
- `sessionId`
- `toolCallId`
- `llmToolCall`
- `cacheKey`
- `policy` / `isolationSpec`
- serializable context fields: `ctxSessionId`, `ctxConversationId`, `ctxTurnId`, `ctxCallStack`, `ctxAllowedTools`, `ctxParentConversation`

Do **not** store the non-serializable fields (`ctxToolPortal`, `ctxWorld`, `ctxEventQueue`). Re-hydrate those on wake.

---

## Phase 3 — Durable session storage

### 3.1 Generalize `SessionStore`

The current `SessionStore` is file-only. Introduce a backend interface:

```haskell
data SessionBackend = SessionBackend
  { sbStore  :: SessionId -> Session -> IO ()
  , sbLoad   :: SessionId -> IO (Maybe Session)
  , sbList   :: IO [(SessionId, UTCTime)]
  , sbDelete :: SessionId -> IO ()
  }
```

Backends:

- `FileSessionStore FilePath`
- `SqliteSessionStore Connection`
- `CompositeSessionStore [SessionBackend]` for read fallback with a single write target

### 3.2 Use OS persistence (optional)

The OS layer already has `SqliteBackend`, component tables, and migrations. Consider adding a `SessionComponent` persisted via `persistComponent`/`loadComponent`. This reuses ECS migrations and unifies backends.

If session storage stays separate, at least align IDs: `SessionId`/`TurnId`/`ConversationId` are UUID-based, and OS `EntityId` is also UUID-based, so conversion is trivial.

---

## Phase 4 — Wake / resume API

### 4.1 Inject external results

```haskell
wakeSession ::
     Session
  -> [(ContinuationToken, UserToolResponse)]
  -> IO Session
```

Behavior:

- Find the `PartialUserTurn` and matching deferred `TrackedToolCall`s.
- Move them to `Completed` with the provided result.
- Update the cache if configured.
- If all calls are complete, convert the `PartialUserTurn` to a full `UserTurn`.
- Otherwise leave remaining deferred/ready calls in place.

### 4.2 Resume execution

```haskell
resumeSession ::
     ConversationId
  -> Agent r
  -> Session
  -> IO (Either r Session)
```

- If the latest turn is `PartialUserTurn`, run the scheduler on remaining ready/deferred calls.
- If the latest turn is `UserTurn`, continue to the LLM step.
- Returns `Left r` on completion, `Right Session` when it needs to yield again.

### 4.3 Complete continuations from external workers

```haskell
completeContinuation ::
     ContinuationStore
  -> ToolCache
  -> ContinuationToken
  -> UserToolResponse
  -> IO Bool
```

This already exists as `resumeAsyncToolCall` but the `ContinuationStore` implementation is stubbed. Finish the load/list implementations and ensure proper JSON round-tripping.

---

## Phase 5 — Isolated deployment primitives

### 5.1 `DeploymentRunner` abstraction

```haskell
data DeploymentRunner = DeploymentRunner
  { drName    :: Text
  , drExecute :: IsolationSpec -> LlmToolCall -> IO (Either IsolationError UserToolResponse)
  }
```

Implementations:

- `localProcessRunner` — fork a worker process
- `dockerRunner` — run inside a container
- `functionRunner` — for serverless/FaaS (future)

### 5.2 Serialization contract for isolated calls

Define a stable JSON envelope consumed by any external runner:

```json
{
  "token": "...",
  "toolCall": { ... },
  "contextSnapshot": { ... },
  "policy": { "tag": "RunIsolated", "spec": { "image": "...", "sandbox": "..." } }
}
```

Result envelope:

```json
{ "token": "...", "status": "success", "result": { ... } }
```

This makes Docker vs. local execution transparent to the agent.

### 5.3 Tool-call decorator for isolation

Example policy:

```haskell
isolateBash :: ToolCallPolicy
isolateBash ctx call
  | toolName call == "bash_command" = RunIsolated (Docker "agents-exe/bash-runner:latest")
  | otherwise                       = RunSync
```

---

## Phase 6 — Integration & agent combinators

### 6.1 Extend `Agent` record

Add to `Agent r`:

- `ctxToolCallPolicy :: ToolCallPolicy`
- `ctxSessionBackend :: Maybe SessionBackend`
- `ctxContinuationStore :: Maybe ContinuationStore`
- `ctxDeploymentRunner :: Maybe DeploymentRunner`

Keep defaults so existing agents compile unchanged.

### 6.2 New combinators

```haskell
withToolCallPolicy    :: ToolCallPolicy -> Agent r -> Agent r
withSessionBackend    :: SessionBackend -> Agent r -> Agent r
withContinuationStore :: ContinuationStore -> Agent r -> Agent r
withDeploymentRunner  :: DeploymentRunner -> Agent r -> Agent r
```

### 6.3 Update `agentStoreSession`

`System.Agents.Combinators.StoreSessionProgress` should store via the configured `SessionBackend` rather than only files, while still supporting file fallback.

---

## Phase 7 — CLI / operator API

New commands for real operation:

- `agents session pause <session-id>` — yield after current step
- `agents session resume <session-id>` — continue execution
- `agents session pending <session-id>` — list deferred calls / continuation tokens
- `agents session complete <token> <result-file>` — inject an external result
- `agents session run-isolated <session-id>` — poll and execute `RunIsolated` calls

---

## Phase 8 — Testing strategy

Add tests for:

1. Policy classification: a policy can mark some calls sync and some deferred.
2. Partial turn serialization: a session with deferred calls round-trips through SQLite/file backends.
3. Wake/resume: `wakeSession` injects results and eventually produces a full `UserTurn`.
4. Cache integration: deferred calls whose result is later cached are resolved on resume.
5. Isolation contract: a `DeploymentRunner` can execute a tool call via a subprocess and return a result envelope.
6. Determinism: the same session state resumed twice produces the same outcome.

---

## Open questions

1. **Should `PartialUserTurnContent` keep its current shape or be rewritten around `TrackedToolCall`?**  
   Rewriting is cleaner but touches JSON serialization and existing tests.

2. **Should session storage move into the OS ECS persistence layer or stay separate?**  
   Moving into ECS unifies migrations/backends; keeping it separate is less invasive.

3. **How do we handle non-serializable `ToolExecutionContext` fields on resume?**  
   Recommended: store a serializable snapshot and re-hydrate portal/world/event queue in the runtime.

4. **Do we want true concurrency for sync calls or sequential execution?**  
   Sequential is simpler and matches current cache semantics; concurrency can be added later behind a policy flag.

5. **Should `ToolCallId` be a UUID or an index into the turn?**  
   A UUID is safer for external workers and database keys; an index is smaller and deterministic. Hybrid: `ToolCallId = TurnId + Int index`.

---

## Suggested first milestone

The smallest vertical slice that proves the design:

1. Add `TrackedToolCall` and refactor `PartialUserTurnContent`.
2. Implement `ToolCallPolicy` with `RunSync` / `Defer` decisions.
3. Update the async scheduler to batch sync calls and yield deferred ones.
4. Implement `wakeSession`.
5. Add a SQLite-backed `SessionBackend`.
6. Write a test exercising the full requested flow:
   - 3 tool calls issued,
   - policy executes 1 and defers 2,
   - session persists,
   - wake injects 2 results,
   - session resumes and produces a `UserTurn` with all 3 results.

This milestone delivers the requested primitives without yet building Docker runners or Postgres backends.

