# Durable Workflows — Implementation Progress

Progress tracker for `todos/durable-workflows.md`.

## Phase 1 — Core primitives (types & interfaces) ✅ COMPLETE

### 1.1 Rich tool-call state inside a turn

- Added durable workflow primitives to `System.Agents.Session.Types`:
  - `ToolCallId` — stable UUID-based identifier for matching external results
  - `ToolCallState` — `Ready | Running | Deferred | Completed | Failed`
  - `TrackedToolCall` — wraps `LlmToolCall` with id, state, result, continuation token, and applied policy
  - `AppliedPolicy` — records the policy decision applied to a call
- Refactored `PartialUserTurnContent` to hold `[TrackedToolCall]` instead of separate completed/pending/continuation lists.
- Added backward-compatible JSON parsing for legacy sessions and helper views:
  - `partialCompletedResponses`
  - `partialPendingCalls`
  - `partialPendingContinuations`
  - `cacheKeyForTrackedCall`
- Updated dependent modules (`Session.Step`, `Session.Loop`, `Session.OpenAI`, `Session.Search.Index`, `Session.Edit`) to use the new model.

### 1.2 Tool-call policy / decorator

- Added to `System.Agents.Session.Types`:
  - `ToolCallDisposition` — `RunSync | RunAsync | RunIsolated IsolationSpec | Defer Reason | Decorate [Decorator] ToolCallDisposition`
  - `IsolationSpec` — `Docker Text | LocalProcess FilePath | FunctionRunner Text`
  - `Reason` — human-readable reason for defer/isolation
  - `Decorator` — `WithTimeout | WithRetries | WithCache | WithLabel`
- Added `ToolCallPolicy` type alias in new `System.Agents.Session.Durable` module.
- Provided `defaultToolCallPolicy = const $ const RunSync` for backward compatibility.
- Added `flattenDisposition` helper to extract decorators from a disposition tree.

### 1.3 Pluggable executor interface

- Created `System.Agents.Session.Durable` with:
  - `ToolExecutor` record (`execSync` / `execAsync`)
  - `inProcessExecutor` — current behavior
  - `yieldingExecutor` — always returns `ToolYield`
  - `cachingExecutor` — cache wrapper
  - `isolatedExecutor` — delegates `RunIsolated` calls to a `DeploymentRunner`
  - `DeploymentRunner` and `IsolationError` abstractions
- Updated `System.Agents.Session.Base.Agent`:
  - Added `ctxToolCallPolicy`, `ctxToolExecutor`, `ctxContinuationStore`, `ctxDeploymentRunner`
  - Removed `ctxAsyncToolCall`
  - Added combinators: `withToolCallPolicy`, `withToolExecutor`, `withContinuationStore`, `withDeploymentRunner`
- Updated all agent construction sites (`OneShot`, `MCP.Server`, `AgentTree.OneShotTool`) to initialize the new fields.

### Verification

- Library builds with `-Wall -Werror`.
- Test suite `agents-tests` passes.

## Phase 2 — Async scheduler in the session loop ✅ COMPLETE

### 2.1 Policy-driven async scheduler

- Updated `runStepMAsync` / `executeTrackedCalls` in `System.Agents.Session.Step` to:
  - classify every `Ready` `TrackedToolCall` via `Agent.ctxToolCallPolicy`;
  - execute all `RunSync` / `RunIsolated` calls in one batch (with cache lookup);
  - move `RunAsync` and `Defer` calls to the `Deferred` state, generate continuation tokens, and store snapshots.
- If all calls complete, the scheduler emits a full `UserTurn`; otherwise it yields a `PartialUserTurn`.
- Updated `naiveStep` and `naiveTilNoToolCallStep` so partial turns with remaining ready/deferred calls continue via `AskUserPrompt` rather than prematurely asking the LLM.

### 2.2 Serializable continuation snapshots

- Added `ToolExecutionContextSnapshot` to `System.Agents.Tools.Context` containing only serialisable fields (`ctxSessionId`, `ctxConversationId`, `ctxTurnId`, `ctxCallStack`, `ctxAllowedTools`, `ctxParentConversation`).
- Added `ToolContinuationSnapshot` to `System.Agents.Session.Async` and a helper `mkToolContinuationSnapshot`.
- Deduplicated `ContinuationToken` by re-exporting the one from `System.Agents.Session.Types` in `System.Agents.Session.Async`.

### 2.3 Wake / resume primitives

- Created `System.Agents.Session.Wake` with:
  - `wakeSession :: Session -> [(ContinuationToken, UserToolResponse)] -> IO Session`
  - `wakeSessionWithCache :: Maybe ToolCache -> Session -> [(ContinuationToken, UserToolResponse)] -> IO Session`
  - `resumeSession :: ConversationId -> Agent r -> Session -> IO (Either r Session)`
- `wakeSession` finds the latest `PartialUserTurn`, matches deferred calls by token, moves them to `Completed`, updates the cache when provided, and converts the turn to a full `UserTurn` when every call is complete.
- `resumeSession` runs the agent step-by-step until it either completes (`Left r`) or yields again because deferred calls remain (`Right Session`).

### 2.4 SQLite `ContinuationStore` load/list

- Finished `sqliteLoadContinuation` and `sqliteListPending` with proper JSON round-tripping of `ToolContinuationSnapshot`.
- Changed `ContinuationStore` to store/load `ToolContinuationSnapshot` instead of the in-memory `ToolContinuation`.
- Updated `resumeAsyncToolCall` to use the snapshot's cache key.

### Verification

- Library builds with `-Wall -Werror`.
- Test suite `agents-tests` passes, including new `DurableWorkflowTests` covering policy classification, continuation-store round-tripping, wake/resume, cache integration, and snapshot serialisation.

## Phase 3 — Durable session storage ✅ COMPLETE

### 3.1 Generalized `SessionBackend` interface

- Added `SessionBackend` record to `System.Agents.SessionStore`:
  - `sbStore :: SessionId -> Session -> IO ()`
  - `sbLoad :: SessionId -> IO (Maybe Session)`
  - `sbList :: IO [(SessionId, UTCTime)]`
  - `sbDelete :: SessionId -> IO ()`
- Added `sessionIdToConversationId` / `conversationIdToSessionId` conversion helpers.

### 3.2 Concrete backends

- `FileSessionStore FilePath`:
  - Newtype wrapper and `mkFileSessionStore` constructor.
  - Implements `SessionBackend` by reusing the existing file-based `SessionStore` logic (`mkSimpleSessionStore`, `storeSession`, `readSession`, `findSessionFiles`).
- `SqliteSessionStore Connection`:
  - Newtype wrapper and `mkSqliteSessionStore` constructor.
  - Stores sessions as JSON in a `sessions` table with `session_id`, `created_at`, `updated_at`, and `json` columns.
  - `initializeSessionSchema` creates the table and an index on `updated_at`.
- `CompositeSessionStore [SessionBackend]`:
  - Newtype wrapper and `mkCompositeSessionStore` constructor.
  - Reads fall back across all backends in order.
  - Writes (store/delete) go to the first backend only (primary target).

### 3.3 `Agent` integration

- Added `ctxSessionBackend :: Maybe SessionBackend` to `Agent` in `System.Agents.Session.Base`.
- Added `withSessionBackend :: SessionBackend -> Agent r -> Agent r` combinator.
- Updated `System.Agents.Combinators.StoreSessionProgress`:
  - Added `backendStoreCallback` for storing progress via a `SessionBackend`.
  - `agentStoreSession` now uses `ctxSessionBackend` when present, falling back to the provided file `SessionStore` when absent.
  - The optional explicit `FilePath` still receives an additional copy in both cases.
- Updated all `Agent` construction sites (`OneShot`, `MCP.Server`, `AgentTree.OneShotTool`) and the test helper to initialize `ctxSessionBackend = Nothing`.

### 3.4 Tests

- Extended `DurableWorkflowTests` with a Phase 3 group covering:
  - File backend store/load/delete/list round-trip.
  - SQLite backend store/load/delete/list round-trip.
  - Composite backend read fallback, primary-only write, primary-only delete, and aggregated listing.
  - `withSessionBackend` + `agentStoreSession` integration ensuring the backend is used and the file store is bypassed.

### Verification

- Library builds with `-Wall -Werror`.
- Test suite `agents-tests` passes, including all Phase 2 and Phase 3 durable-workflow tests.

## Next: Phase 4 — Wake / resume API

Remaining work from the plan:

- `wakeSession` and `resumeSession` exist but could be exposed via CLI commands (Phase 7).
- Finish `ContinuationStore` load/list implementations are already complete in Phase 2.
- Add CLI/operator API for pause/resume/complete/pending operations (Phase 7).

