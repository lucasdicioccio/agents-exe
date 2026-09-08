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

## Phase 4 — Wake / resume API ✅ COMPLETE

### 4.1 Inject external results

- `wakeSession` and `wakeSessionWithCache` are implemented in `System.Agents.Session.Wake` (introduced in Phase 2).
- They find the latest `PartialUserTurn`, match deferred calls by token, move them to `Completed`, update the cache, and convert the turn to a full `UserTurn` when all calls are complete.

### 4.2 Resume execution

- `resumeSession` is implemented in `System.Agents.Session.Wake` (introduced in Phase 2).
- It resumes a session from a partial or completed user turn, running the scheduler until completion or until deferred calls remain.

### 4.3 Complete continuations from external workers

- `resumeAsyncToolCall` uses the SQLite `ContinuationStore` with proper JSON round-tripping of `ToolContinuationSnapshot`.
- `csLoad` / `csListPending` implementations are complete (Phase 2).

### Note

Phase 4's core functions were already implemented during Phase 2. CLI exposure (Phase 7) is out of scope for this phase.

### Verification

- Library builds with `-Wall -Werror`.
- Test suite `agents-tests` passes, including Phase 2 wake/resume tests.

## Phase 5 — Isolated deployment primitives ✅ COMPLETE

### 5.1 `DeploymentRunner` abstraction

- Moved `DeploymentRunner` and `IsolationError` to a new module `System.Agents.Session.Isolation`.
- Changed `DeploymentRunner` to accept a stable `IsolationEnvelope` instead of raw `IsolationSpec` + `LlmToolCall`, so external workers receive a language-agnostic document.
- `System.Agents.Session.Durable` re-exports the runner/error types and the new envelope helpers for backward compatibility.

### 5.2 Serialization contract for isolated calls

- Added stable envelope types in `System.Agents.Session.Isolation`:
  - `IsolationEnvelope` — input envelope with `token`, `toolCall`, `contextSnapshot`, `policy`, and optional `reason`.
  - `IsolationResultEnvelope` — result envelope with `token`, `status` (`success`/`error`), `result`, and `error`.
  - `IsolationResultStatus` — `IsolationSuccess` / `IsolationFailure`.
- Added helpers:
  - `mkIsolationEnvelope`
  - `mkIsolationSuccessEnvelope`
  - `mkIsolationErrorEnvelope`
  - `parseIsolationResultEnvelope`
  - `parseIsolationResultEnvelopeLBS`
- Documented the JSON shapes in Haddock comments.

### 5.3 Concrete runners

- `localProcessRunner :: FilePath -> DeploymentRunner`
  - Serialises the envelope to compact JSON.
  - Forks the worker process and writes the envelope to its stdin.
  - Reads stdout and parses the result envelope.
  - Validates that the result token matches the input token.
  - Returns `IsolationError` for non-zero exit codes, unparseable output, or token mismatches.
- `dockerRunner :: Text -> DeploymentRunner`
  - Runs `docker run --rm -i <image>` with the envelope on stdin.
  - Reads stdout and parses the result envelope.
  - Returns `IsolationError` if Docker is unavailable or the container fails.
- `functionRunner :: DeploymentRunner`
  - Documented placeholder for future serverless/FaaS execution.
  - Always returns `IsolationError "functionRunner is a future placeholder ..."`.

### 5.4 Integration with the policy-driven scheduler

- Updated `isolatedExecutor` in `System.Agents.Session.Durable` to build an `IsolationEnvelope` for each isolated call (fresh continuation token + serialisable context snapshot).
- Updated `executeCall` in `System.Agents.Session.Step` so that when an agent has `ctxDeploymentRunner` configured but no explicit `ctxToolExecutor`, isolated calls are dispatched through the runner and non-isolated calls fall back to the agent's `toolCall`.
- This makes `RunIsolated` policy decisions transparent to the session loop.

### 5.5 Example isolation-by-tool-name policy

- Added `isolatedToolNamePolicyTest` showing:
  - `bash_command` isolated via `localProcessRunner`.
  - Other tools executed synchronously in-process.
  - The scheduler produces a full `UserTurn` containing both responses.

### 5.6 Tests

- Extended `DurableWorkflowTests` with a Phase 5 group covering:
  - `IsolationEnvelope` JSON round-trip.
  - `IsolationResultEnvelope` JSON round-trip and parser helpers.
  - `localProcessRunner` with a simple bash worker script.
  - `dockerRunner` envelope construction (execution skipped when Docker is unavailable).
  - Integration test for isolation-by-tool-name policy.

### Verification

- Library builds with `-Wall -Werror`.
- Test suite `agents-tests` passes, including all Phase 2, Phase 3, Phase 4, and Phase 5 durable-workflow tests.

## Phase 6 — Integration & agent combinators ✅ COMPLETE

### 6.1 `Agent` record defaults

- Verified that `System.Agents.Session.Base.Agent` exposes all durable-workflow fields:
  - `ctxToolCallPolicy :: ToolCallPolicy` (defaults to `defaultToolCallPolicy`)
  - `ctxToolExecutor :: Maybe ToolExecutor` (defaults to `Nothing`)
  - `ctxContinuationStore :: Maybe ContinuationStore` (defaults to `Nothing`)
  - `ctxDeploymentRunner :: Maybe DeploymentRunner` (defaults to `Nothing`)
  - `ctxSessionBackend :: Maybe SessionBackend` (defaults to `Nothing`)
- Confirmed all existing agent construction sites (`OneShot`, `MCP.Server`, `AgentTree.OneShotTool`) still compile unchanged and initialise every field.

### 6.2 Agent combinators

- Added/verified all requested combinators in `System.Agents.Session.Base`:
  - `withToolCallPolicy`
  - `withToolExecutor`
  - `withContinuationStore`
  - `withDeploymentRunner`
  - `withSessionBackend`
  - Convenience `withDurableWorkflows :: SessionBackend -> ContinuationStore -> Agent r -> Agent r`
  - Convenience `withAsyncConfig :: ExecutionMode -> Maybe ToolCache -> ToolCallPolicy -> Agent r -> Agent r`
  - Convenience `withDurableExecutor :: Maybe ToolCache -> Maybe DeploymentRunner -> Agent r -> Agent r`

### 6.3 Progress / storage helpers

- Added `backendWithCallbackStoreCallback :: SessionBackend -> OnSessionProgress -> OnSessionProgress` to `System.Agents.Combinators.StoreSessionProgress`.
  - Stores each progress event via the configured backend and then forwards the event to an additional user-supplied callback.
- Verified `agentStoreSession` still correctly wires `ctxSessionBackend`, file fallback, and optional extra file copy.

### 6.4 Durable executor helpers

- Verified `System.Agents.Session.Durable` provides:
  - `mkDurableExecutor` — cache + runner + native tool call
  - `cachedInProcessExecutor` — in-process execution with caching
  - `composeExecutors` — conditional dispatch by disposition

### 6.5 Integration tests

- Fixed and extended `test/DurableWorkflowTests.hs` Phase 6 group:
  - `durableWorkflowIntegrationTest` — end-to-end session with a policy that defers calls, a continuation store, a SQLite session backend, and a cache; verifies yield, persistence, wake, cache population, and resume.
  - `withDurableWorkflowsTest`
  - `withAsyncConfigTest`
  - `mkDurableExecutorTest`
  - `cachedInProcessExecutorTest`
  - `composeExecutorsTest`
  - `agentStoreSessionWithCallbackTest`

### Verification

- Library builds with `-Wall -Werror`.
- Test suite `agents-tests` passes, including all Phase 2, Phase 3, Phase 4, Phase 5, and Phase 6 durable-workflow tests.

## Phase 7 — CLI / operator API ✅ COMPLETE

### 7.1 New `session` subcommand group

- Added `System.Agents.CLI.SessionDurable` module with:
  - `SessionDurableOptions` and `SessionDurableCommand` types.
  - `handleSessionDurable` dispatcher.
- Integrated the new command group into `app/Main.hs` under `agents session <subcommand>`.
- Added `uuid` to the `agents-exe` executable build-depends for parsing session/token UUID arguments.

### 7.2 `agents session pause <session-id>`

- Loads the session from the configured file-based `SessionStore`.
- Loads the first supplied agent file (same mechanism as one-shot) and sets execution mode to `Asynchronous`.
- Runs one step via `runStepM`.
- Persists the resulting session (completed or yielded) back to the `SessionStore`.
- Prints the yielded state and any deferred continuation tokens.

### 7.3 `agents session resume <session-id>`

- Loads the session and agent as above.
- Calls `resumeSession` from `System.Agents.Session.Wake`.
- Runs until completion or until the session yields again, persisting after each yield.
- Prints the final LLM response or the yielded partial-turn summary.

### 7.4 `agents session pending <session-id>`

- Loads the session from the `SessionStore`.
- Finds the latest `PartialUserTurn`.
- Lists all deferred `TrackedToolCall`s with their `ToolCallId`, `ContinuationToken`, tool name, and policy/disposition.

### 7.5 `agents session complete <token> <result-file>`

- Reads the result file. If it parses as a `UserToolResponse` JSON value, that value is used; otherwise the raw file contents are treated as a plain-text response.
- Scans all sessions in the `SessionStore` to locate the session containing the matching continuation token.
- Calls `wakeSession` with the token/result pair.
- Persists the updated session.
- Prints whether the turn is now complete or still partial.

### 7.6 `agents session run-isolated <session-id>` (optional)

- Loads the session and agent.
- Finds deferred calls whose disposition is `RunIsolated`.
- If the agent has a configured `ctxDeploymentRunner`, executes those calls through the runner and injects the results via `wakeSession`.
- Persists the updated session and prints per-call results/errors.
- If no runner is configured, reports the number of deferred isolated calls found.

### 7.7 Pure helpers and tests

- Exported pure helpers from `System.Agents.CLI.SessionDurable` for testing:
  - `formatContinuationToken` / `parseContinuationToken`
  - `parseResultFile`
  - `extractDeferredCalls`
  - `extractIsolatedCalls`
- Added `test/SessionDurableTests.hs` with unit tests for:
  - Continuation token formatting/parsing round-trip.
  - Result-file parsing for JSON and plain-text responses.
  - Deferred-call extraction from partial turns.
  - Isolated-call extraction from manually constructed partial turns.
  - Pending/complete integration test via a temporary file session store.
- Registered the new test module in `test/Main.hs` and `agents.cabal`.

### Verification

- Library builds with `-Wall -Werror`.
- Executable `agents-exe` builds with `-Wall -Werror`.
- Test suite `agents-tests` passes, including all Phase 2–6 durable-workflow tests and the new Phase 7 `SessionDurableTests`.

## Next: Phase 8 — Testing strategy

Remaining work from the plan:

- Policy classification tests (already covered in Phase 2 `DurableWorkflowTests`).
- Partial turn serialization through SQLite/file backends (already covered in Phase 3).
- Wake/resume tests (already covered in Phase 2/4).
- Cache integration tests (already covered in Phase 2).
- Isolation contract tests (already covered in Phase 5).
- Determinism/resume-twice tests (can be added as part of Phase 8).

