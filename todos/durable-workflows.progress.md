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

## Next: Phase 2 — Async scheduler in the session loop

Remaining work from the plan:

- Update `runStepMAsync` to classify calls via `ToolCallPolicy`, batch `RunSync` calls, and yield `RunAsync`/`Defer` calls with continuation tokens.
- Generate serializable `ToolContinuationSnapshot`s for deferred calls (without non-serializable context fields).
- Implement `wakeSession` and `resumeSession`.
- Finish `ContinuationStore` load/list implementations.

