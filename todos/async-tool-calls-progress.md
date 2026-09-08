# Async Tool Calls Implementation Progress

## Plan
See `todos/async-tool-calls.md`.

## Phase Status

- [x] Phase 1: ECS Tool-Call Entities
- [x] Phase 2: Async Engine
- [x] Phase 3: System Capability (building blocks only — see review below)
- [x] Phase 3.5: End-to-End Correctness (added after review; blocks Phase 4)
- [x] Phase 4: TUI / OneShot UX (TUI rendering not yet checked by hand)
- [x] Phase 5: Cleanup & Hardening (tracing not done — see notes)

## Success Criteria Status

Revised after the 2026-09-16 review, then after Phase 3.5 and Phase 4. The
criteria are met in the library and tests, and async execution is now enabled
from the agent JSON in the TUI, OneShot, sub-agents, and `session` commands.
The TUI rendering has not been checked by hand.

| Criterion | Status | Notes |
|-----------|--------|-------|
| Two long-running tool calls issued in one LLM turn execute concurrently | Met | `asyncEngineTests`; async calls now start before inline calls so they overlap. |
| LLM receives a partial user turn as soon as the first call finishes (`YieldOnAnyProgress`) | Met (stepper) | Running calls get a placeholder tool message; late results are delivered in the next user turn. `AsyncToolCallsTests`. |
| LLM can call `get_tool_call_status` and see structured progress or final result | Met | Accepts the provider id. Bash tools report output lines; sub-agent tools report their steps. |
| LLM can call `cancel_tool_call` to stop a running call | Met | Interrupts the engine thread via `ctxCancelToolCall`; a running subprocess is terminated (`processKilledOnCancel`). |
| Tool calls appear as ECS entities in the OS `World` | Met | Covered by `sessionStepEntityTests` and `toolCallEntityTests`. |
| Existing synchronous behavior unchanged when `ExecutionMode = Synchronous` | Met | Same turns as before. Sync steps only differ when a session already holds background calls from an async run. |
| `get_tool_call_status` returns `orphaned` for calls referenced in history that no longer exist | Met | Capability returns `orphaned` with `is_final: true`; the stepper fails such calls. Verified across a save/reload round-trip in a fresh world (`orphanedAcrossRestart`). |

## Phase 1 Completion Notes

Phase 1 is complete. The following was done:

### ECS Tool-Call Components
- `System.Agents.OS.Conversation.Types` already had `ToolCallConfig`/`ToolCallState` extended with:
  - `tcSessionId`, `tcConversationId` on `ToolCallConfig`.
  - `tcProgress :: [ToolCallProgress]`, `TcCancelled` on `ToolCallState`.
  - `ToolCallProgress` / `ProgressKind` for structured progress.

### Tracked Tool Calls
- `System.Agents.Session.Types` already had `tcEntityId :: Maybe EntityId` added to `TrackedToolCall`.

### Tool-Call Entity Helpers
- Extended `System.Agents.OS.Conversation.ToolCalls`:
  - Added `ensureToolCallComponents` / `ensureToolCallComponentsIO` for idempotent component-store registration (safe to call on every step).
  - `createToolCallEntity`, `startToolCall`, `completeToolCall`, `failToolCall`, `cancelToolCall`, `addToolCallProgress`.
  - Query helpers `findToolCallEntityBySessionId` and `listToolCallsBySessionAndConversation`.

### Session Step Integration
- Updated `System.Agents.Session.Step`:
  - `prepareAgentWorld` ensures tool-call component stores are registered idempotently and threads the registered world through the agent/context.
  - `ensureTrackedCallEntities` promotes every `TrackedToolCall` to an OS entity when a `World` is available.
  - `executeTrackedCallWithEntity` updates the OS entity state to `TcExecuting` before running a call and `TcCompleted` (with the JSON result) after.
  - Both `runStepMSync` and `runStepMAsync` now create/update OS entities for every tool call.
  - Existing behavior is unchanged when no `World` is configured.

### Tests
- Added `sessionStepEntityTests` to `test/OS/ConversationTests.hs` covering:
  - Sync step creates completed tool-call entities.
  - Async step creates entities for both sync-completed and deferred calls.
  - Async step with all-sync policy completes all calls and creates entities.

### Baseline
- Full test suite passes.

## Phase 2 Completion Notes

Phase 2 is complete. The async engine now runs `RunAsync` tool calls concurrently, keeps OS entity state in sync, and supports structured progress callbacks.

### Async Engine (`System.Agents.Session.Async.Engine`)
- `AsyncEngine` manages a shared semaphore for max concurrency and a registry of active batches keyed by `ToolCallId`.
- `startAsyncBatch` spawns each call in a background `Async` thread, injects a per-call progress callback into `ToolExecutionContext`, and records the call in the registry.
- `waitForProgress` / `waitForProgressTimeout` block until at least one call reaches a final state.
- `finalizeCompleted` collects finished calls and removes them from the running set and engine registry.
- `cancelToolCall` / `cancelAsyncBatch` send async exceptions to background threads and mark OS entities as `TcCancelled`.

### Yield Strategy
- `AsyncYieldStrategy` (`YieldOnAnyProgress`, `YieldWhenAllDone`, `YieldOnTimeout`) is defined in `System.Agents.Session.Types`.
- `waitAccordingToStrategy` in `System.Agents.Session.Step` implements the three strategies.
- The default strategy is `YieldWhenAllDone` (set by `withAsyncConfig`).

### Progress & Lifecycle
- When a background call starts, the engine emits a `ToolCallProgress` entry with `ProgressStarted` (payload `{"started":true}`) alongside setting the entity status to `TcExecuting`.
- The progress callback writes `ProgressPartial` entries with arbitrary JSON payloads to the entity.
- `completeToolCall` and `failToolCall` in `System.Agents.OS.Conversation.ToolCalls` now leave the state unchanged if it is already `TcCancelled`, preventing late engine completions from overwriting a cancellation.

### Helpers
- Added `isToolCallCompletedIO` and `findToolCallEntityByToolCallId` to `System.Agents.OS.Conversation.ToolCalls`.

### Tests
- Added `asyncEngineTests` to `test/OS/ConversationTests.hs` covering:
  - Two `RunAsync` calls execute concurrently (total time < 300 ms for two 200 ms sleeps).
  - Progress callback emits `ToolCallProgress` entries, including `ProgressStarted`.
  - `cancelToolCall` marks an entity as `TcCancelled` and a later engine completion does not overwrite it; a second cancellation attempt returns `False`.

### Baseline
- Full test suite passes.

## Phase 3 Completion Notes

Phase 3 is complete. Agents can now inspect and cancel async tool calls through the system toolbox.

### Capability Constructors
- Added to `SystemToolCapability` in `System.Agents.Base`:
  - `SystemToolGetToolCallStatus` serialized as `get-tool-call-status`.
  - `SystemToolListRunningToolCalls` serialized as `list-running-tool-calls`.
  - `SystemToolCancelToolCall` serialized as `cancel-tool-call`.

### Types (`System.Agents.Tools.SystemToolbox.Types`)
- `GetToolCallStatusParams` — `tool_call_id`, `include_progress`, `wait_for_completion`, `timeout_seconds`.
- `ToolCallStatusResult` — status, tool name, timing, result, progress, `is_final`.
- `ListRunningToolCallsResult` / `RunningToolCallInfo`.
- `CancelToolCallParams` / `CancelToolCallResult`.

### Implementation (`System.Agents.Tools.SystemToolbox.ToolCallStatus`)
- `getToolCallStatus` looks up the OS entity by `ToolCallId` and translates `ToolCallStatus` to LLM-facing strings (`pending`, `running`, `completed`, `failed`, `cancelled`).
  - Supports `wait_for_completion` with a configurable timeout (polls every 50 ms).
  - If the entity is missing, searches the session's `PartialUserTurn` history and returns `orphaned` when the call is referenced but no longer exists.
- `listRunningToolCalls` returns all non-final tool-call entities for the current session/conversation.
- `cancelToolCallById` marks a non-final entity as `TcCancelled` and reports the previous status; returns `cancelled: false` if already final.

### Registration (`System.Agents.ToolRegistration`)
- Updated `capabilityToText` and `buildSystemToolParams` to expose the new capabilities and their parameters (`tool_call_id`, `include_progress`, `wait_for_completion`, `timeout_seconds`, `reason`).
- Routed `get-tool-call-status`, `list-running-tool-calls`, and `cancel-tool-call` in the `system_info` tool handler.

### Exports
- `System.Agents.Tools.SystemToolbox` re-exports the new types and functions.
- `agents.cabal` exposes `System.Agents.Tools.SystemToolbox.ToolCallStatus`.

### Tests
- Added `toolCallStatusTests` to `test/OS/ConversationTests.hs` covering:
  - `get-tool-call-status` returns `pending` then `running`.
  - `get-tool-call-status` returns `completed` after completion.
  - `get-tool-call-status` returns `orphaned` for a tracked call missing an OS entity.
  - `list-running-tool-calls` returns only non-final calls.
  - `cancel-tool-call` marks a running entity as cancelled and a second attempt reports already final.

### Baseline
- Full test suite passes (870 tests).

## Review (2026-09-16)

State at review time: branch `asynctools`, build green, 870 tests pass.
Phases 1–3 delivered the building blocks (entities, concurrent engine,
status/list/cancel capabilities), but the feature does not work end-to-end
in a real session. Findings:

### R1. The LLM cannot address its own tool calls
- `get-tool-call-status` / `cancel-tool-call` take the internal `ToolCallId`
  UUID generated in `mkReadyTrackedCall` (`Session/Step.hs`, `newToolCallId`).
- The model only ever sees the provider's id (e.g. `call_abc123`). There is no
  mapping between the two and the UUID is never surfaced to the model.

### R2. Partial answers never reach the LLM correctly
- `turnToMessages (PartialUserTurn …)` in `Session/OpenAI.hs` emits tool
  messages for completed calls only. A running call has no tool message, which
  OpenAI-compatible APIs reject.
- `naiveTilNoToolCallStep` (`Session/Step.hs`) only treats `Ready` / `Deferred`
  as outstanding. With only `Running` calls left, it asks for an LLM completion
  with a partial set of responses, then pushes an `LlmTurn` on top. The running
  calls' results are buried and never delivered.
- `continuePartialTurn` → `executeTrackedCalls` prepends a new
  `PartialUserTurn` instead of replacing the head (unlike `Session/Wake.hs`,
  which does `newTurn : drop 1 turns`). Every resume duplicates the turn in
  history. Pre-existing bug, but running calls now trigger it on every resume.

### R3. `cancel-tool-call` does not stop anything
- `cancelToolCallById` (`Tools/SystemToolbox/ToolCallStatus.hs`) only calls
  `TCT.cancelToolCall`, i.e. flips the entity to `TcCancelled`.
- The background thread keeps running. `Engine.cancelToolCall` does kill the
  thread, but the capability cannot reach the engine from
  `ToolExecutionContext`.
- The capability test only asserts the status flag.

### R4. The engine does not survive between steps
- `runAsyncWithProgress` (`Session/Loop.hs`) returns only the `Session`; the
  agent carrying `ctxAsyncEngine` is dropped. The caller resumes with an agent
  that has no engine.
- `ensureAsyncEngine` then creates a fresh engine on each step, so
  `defaultMaxConcurrency = 4` is per step rather than global, and the
  cancellation registry is lost.

### R5. The stepper does not handle orphaned calls
- `pollRunningCall` leaves a call `Running` forever when its entity is missing
  (e.g. after a restart), so the partial turn never resolves.
- Only the capability reports `orphaned`, and it returns `is_final: false` for
  a call that can never finish.

### R6. Smaller issues
- **Crash on malformed calls:** `requireEntityId` in
  `Session/Async/Engine.hs` calls `error` when a call has no entity id. That
  happens whenever `parseToolCallFromLlmToolCall` fails, so a malformed call
  under an async policy crashes the step instead of degrading to inline.
- **No real overlap:** in `executeTrackedCalls`, inline calls run sequentially
  before the async batch is started.
- **No progress emitters:** nothing calls `ctxProgressCallback`. Also,
  `mkSubcallContext` propagates the callback into sub-agent contexts, which
  would write their progress into the parent call's entity.
- **Hard to enable:** the agent JSON has `executionMode` and
  `toolCallPolicyConfig`, but only the durable `session` CLI applies them
  (`applyAgentDurableConfig` in `CLI/SessionDurable.hs`). The TUI and OneShot
  ignore them, and there is no config for yield strategy or concurrency.

## Phase 3.5 Completion Notes

Done 2026-09-16. Build green, 875 tests pass (5 new end-to-end tests).

### Semantics
- **Placeholders.** `partialToolMessages` (`Session/Types.hs`) emits one tool
  message per call. Final calls carry their result. Non-final calls, and calls
  whose result was delivered late, get a JSON placeholder
  (`status`, `tool_call_id`, a hint to use `get-tool-call-status` /
  `cancel-tool-call`). Used by `OpenAI.hs`, both naive step functions, and
  `Wake.hs`. `Failed` calls now also get their tool message (previously
  dropped).
- **Late delivery.** `collectLateResults` (`Session/Step.hs`) finds `Running`
  calls in partial turns below the head, polls them, marks finished ones
  `tcDeliveredLate = True` (new field on `TrackedToolCall`, optional in JSON),
  and `lateResultsQuery` renders them into the next user turn's `userQuery`
  (media is attached). The partial turn keeps showing the placeholder, so
  history matches what the model saw. Runs in both sync and async steps.
- **Stopping.** `naiveTilNoToolCallStep` no longer stops on an LLM turn without
  tool calls while `hasBackgroundCalls`. It asks for a user turn, and the
  stepper blocks until the results arrive (all under `YieldWhenAllDone`,
  otherwise at least one).
- **Refresh.** `runStepMAsync` refreshes the head partial turn before each
  step (`refreshHeadPartialTurn`). Background completions are picked up, and
  the head becomes a full `UserTurn` once every call is final.
- **Resume.** `executeTrackedCalls` takes `replaceHead`; `continuePartialTurn`
  replaces the head instead of stacking. It also waits, per yield strategy, on
  calls carried over from earlier steps together with newly started ones.
  Finalization accepts `Completed` or `Failed`.
- **Message order.** `OpenAI.hs` now emits tool messages before the user
  message in a turn (required once a user turn carries both).

### Engine / ids / cancellation
- `ToolCallConfig.tcProviderCallId` stores the provider's id
  (`createToolCallEntityWithProviderId`); `findToolCallEntityByProviderCallId`
  resolves it within a session and conversation, preferring in-flight then
  most recently started calls (Kimi-style ids repeat).
- Capabilities accept the provider id or the internal UUID;
  `list-running-tool-calls` returns the provider id. Session fallback reports
  final calls from history, `Running` as `orphaned` (`is_final: true`), and
  deferred as `pending`. `waitForFinal` blocks on STM instead of polling.
- `runStepMAsync` installs one engine on the agent (`prepareAsyncEngine`) and
  keeps it on `Evolve`. `buildContext` exposes `Engine.cancelToolCall` as
  `ctxCancelToolCall`. `cancel-tool-call` uses it, falling back to marking the
  entity.
- Engine: asynchronous exceptions are no longer swallowed (cancel really
  interrupts the tool), each call unregisters itself when it finishes,
  `cancelToolCall` reports whether the entity actually ended up cancelled,
  and `TCT.cancelToolCall` never overwrites a final state. `startAsyncBatch`
  ignores calls without an entity; the stepper runs those inline.
- Orphans: `pollRunningCall` fails a `Running` call whose entity (or world) is
  missing.
- `withAsyncEngine` registers tool-call stores first and stores the resulting
  world on the agent, so engine and agent share the same world value.
- Waits use `System.Timeout` rather than `registerDelay`, which needs the
  threaded RTS (the test suite is not threaded).

### Tests (`test/AsyncToolCallsTests.hs`)
- Fast and gated slow call with `YieldOnAnyProgress`: partial turn, placeholder
  seen by the LLM, stepper blocks and delivers the late result exactly once, no
  duplicated partial turn, every completion pairs each tool call with exactly
  one tool message, session stops afterwards.
- `cancel-tool-call` by provider id interrupts a sleeping tool (its
  `onException` handler runs, it never finishes), entity is `cancelled`, next
  step finalizes the turn.
- `Running` call with a missing entity resolves as orphaned.
- Partial turn with a deferred and a running call resumed twice stays a single
  head turn.
- Malformed `RunAsync` call runs inline instead of crashing.
- `OS.ConversationTests` orphan capability test now uses a `Running` call and
  expects `is_final: true`.

### Known gaps carried forward
- `runAsync` / `runAsyncWithProgress` still return only the session. Resuming
  in-process with a fresh agent loses the engine registry unless
  `withAsyncEngine` was installed up front (documented on both).
- Placeholders do not include progress (history is pure; the model can call
  `get-tool-call-status`).
- If the LLM fetches a result via `get-tool-call-status`, the late-delivery
  notice still repeats it once.
- A head partial turn with only `Deferred` calls still spins under `run`
  (pre-existing; `runAsync` pauses correctly).
- Cancel against a call whose engine is gone only marks the entity; the thread
  (if any) keeps running.

## Phase 4 Completion Notes

### Enablement
- `Base.Agent` gains `asyncYieldStrategy` and `maxConcurrency`; the runtime
  `Agent` gains `ctxMaxConcurrency` (used when the engine is created).
- `applyAgentDurableConfig` / `buildToolCallPolicy` moved to
  `System.Agents.Session.AgentConfig` (re-exported by `CLI.SessionDurable`) and
  are applied in `OneShot.nodeToAgentWithThinking` (OneShot, TUI, `session`
  commands) and `AgentTree.OneShotTool.nodeToAgent` (sub-agents).
- `prepareAgentWorld` gives an asynchronous agent without a `World` a private
  one; otherwise `RunAsync` calls silently ran inline.
- `llmToolCallName` now lives in `Session.Types`.

Example agent JSON:

```json
"executionMode": "asynchronous",
"asyncYieldStrategy": "yieldOnAnyProgress",
"maxConcurrency": 4,
"toolCallPolicyConfig": {"default": {"tag": "runAsync"}, "rules": []}
```

### Progress events
- `OSEvent_ToolCallActivity ToolCallActivity` (`OS.Events`) with phases
  started / progressed / completed / failed / cancelled, keyed by session id,
  conversation id, tool-call id and provider id.
- The engine publishes them on the context's `ctxEventQueue`. The TUI already
  bridges its OS event queue to the Brick channel (`TUI.Core.startOSEventBridge`),
  so `convertOSEvent` maps them to `AppEvent_ToolCallActivity`; no
  `RuntimeBridge` change was needed.
- Tool-call progress entries on the entity are capped at 50 (newest first).

### TUI
- `TUI.ToolCallActivity` keeps a `ToolCallViews` map (per session, per call)
  in `UIState._toolCallViews`: latest phase, start time, last progress. Views
  are pruned when a session update shows the call is no longer running.
- Rendering: a "Background tool calls running: …" line above the turns, and
  one line per tracked call in partial turns (pending / deferred / running with
  latest progress / completed / delivered later / failed). No spinner.
- While background calls run and the LLM has nothing to do, the TUI asks for
  user input instead of blocking: the stepper's new `askUserQuery` races
  `usrQuery` against the background calls (per the yield strategy) and
  cancels `usrQuery` if the calls finish first. `usrQuery` must tolerate
  cancellation (the TUI reads a `BChan`, which is STM).

### OneShot / loops
- `--wait-for-async` was not added. `run` already waits for in-process
  background calls (they cannot outlive the process). Stopping early would
  only orphan them.
- New `Loop.runUntilBlocked` / `isBlockedOnDeferredCalls`: returns the session
  when the head partial turn only waits on deferred calls instead of spinning.
  OneShot uses it, stores the session under its session id too, and prints a
  JSON report (`status: paused`, `session_id`, deferred calls with continuation
  tokens) for `session complete` / `session resume`.
- Markdown export (`SessionPrint`) lists each call's status in partial turns
  and shows finished results. The search index includes failed results and
  names of all calls in partial turns.

### Progress emitters
- Bash tools: when `ctxProgressCallback` is set (async calls), the script runs
  through `Bash.runProcessReportingOutput`, which reports
  `{stream, line, lines, bytes}` at most every 0.5s per stream. The process is
  terminated if the call is cancelled. Synchronous calls keep the old path.
- Sub-agent tools (`OneShotTool`) report `{message, turns}` after each
  sub-agent step, and now rethrow async exceptions so cancelling them works
  (they were caught and turned into failures).
- `mkSubcallContext` no longer copies the parent's progress callback or cancel
  hook.

### Tests (`test/AsyncToolCallsTests.hs`, now 17)
- Async agent without a World; `ctxMaxConcurrency` bound; engine activity
  events; background results vs user query race (both ways); TUI view
  apply/prune/summaries; markdown partial turn; `runUntilBlocked`; subprocess
  output reports; subprocess terminated on cancel.
- `SessionDurableTests`: yield strategy / max concurrency application and JSON
  parsing. Full suite: 890 tests.

### Known gaps carried forward
- TUI rendering and the input race were not exercised by hand.
- Bash progress is tested on `runProcessReportingOutput` directly, not through
  a registered bash toolbox end to end.
- `terminateProcess` sends SIGTERM to the script only; its children may
  survive (Phase 5: process groups).
- `run` itself still spins on deferred-only partial turns; only
  `runUntilBlocked` users (OneShot) avoid it. The TUI still uses `run`.
- Earlier gaps (engine not returned by `runAsync`, repeated result after
  `get-tool-call-status`) remain.

## Phase 5 Completion Notes

### Shutdown and cancellation
- `Engine.shutdownAsyncEngine` cancels every batch the engine still owns.
  `Loop.run` / `runWithProgress` / `runUntilBlocked` shut the engine down when
  the run ends or throws (tracking the evolving agent in an `IORef`);
  `runAsync*` only on failure, since pausing with calls running is the point.
- Bash scripts run in their own process group and are killed with
  `SIGTERM`, then `SIGKILL` after 100ms, so cancelling kills what the script
  started. Verified: the test fails without the group kill.
- The TUI kills conversation threads when quitting (bounded to 2s), which runs
  the same cleanup.
- The TUI uses `runUntilBlocked`, so a conversation waiting only on deferred
  calls stops with a status message instead of spinning.

### Stale calls
- `asyncCallTimeoutSeconds` (agent JSON) → `ctxAsyncCallTimeout` →
  `aeCallTimeout`. A call that outlives it is interrupted (killing its
  subprocess) and reported as failed: "async tool call timed out after Ns".

### Orphans across a restart
- `get-tool-call-status` used to need `includeFullSession`, which is off by
  default (it also pushes the whole session into every bash tool's
  environment). Contexts now carry `ctxSessionToolCalls`: the tracked calls of
  the session's partial turns only. Without it, a reloaded session answered
  "tool call not found" instead of "orphaned".
- `orphanedAcrossRestart` writes a session with a running call to disk, reloads
  it in a fresh `World`, and checks both the capability and the stepper.

### Concurrency
- `maxConcurrency` stays per agent. `newAsyncConcurrencyLimit` +
  `mkAsyncEngineSharing` let a host share one limit across agents (each agent
  needs its own engine, since the executor is tied to its tools). No rate
  limiting (calls per minute) was added.

### Docs
- New `docs/async-tool-calls.md` covers configuration, placeholders and late
  delivery, the capabilities, progress, cancellation, the TUI, one-shot runs,
  session files and the limits. Linked from `docs/README.md`,
  `docs/durable-workflows-howto.md`.
- `docs/tools.md` lists the three capabilities; `docs/tui.md` documents
  background call display and input while calls run; `docs/cli-commands.md`
  documents the paused JSON report of `run`.
- The agent JSON in the new doc was checked by parsing it with `Base.Agent`.

### MCP / OpenAPI streaming (question from the plan)
- OpenAPI tools wait for one complete HTTP response: nothing to stream.
- MCP defines progress notifications and `ProgressNotification` exists in
  `System.Agents.MCP.Base`, but `MCP.Client` ignores them. Forwarding them to
  `ctxProgressCallback` would make MCP tools report progress like bash tools.
  Not done.

### Not done
- **Tracing.** `Agent` carries no tracer (tracers live in the builders'
  closures), so `Prod.Tracer` support would mean threading one through the
  agent and the engine. The activity events (`OSEvent_ToolCallActivity`) and
  the progress entries on the entity are the observability path for now.
- Rate limiting beyond a concurrency cap.
- A pre-existing docs bug found on the way, now fixed: JSON examples used key
  spellings the parsers reject. Every builtin toolbox (`Name`, `Description`,
  `Capabilities`, `FileSandbox`, …), file sandboxes (`fsbPredicate`,
  `fsbMaxFileSize`, `fsbName`), bash toolboxes (`Path`, `BasenameFilter`),
  OpenAPI/PostgREST servers (`SpecUrl`, `BaseUrl`, `Token`), the removed Lua
  `allowedPaths` field, the old SQLite `path`/`access` fields (now
  `Versioning`), and the kebab-case agent keys in
  `docs/advanced-configuration.md` (`api-key-id` → `apiKeyId`, …). Fixed in
  `docs/tools.md`, `docs/file-loader.md`, `docs/advanced-configuration.md` and
  the `Agent`/`bashToolboxes` Haddock in `System.Agents.Base`. Every JSON block
  in `docs/` now decodes into the real types; the throwaway checker used for
  this is in the session scratchpad.

### Tests (`test/AsyncToolCallsTests.hs`, now 21)
- Failing run cancels background calls; call timeout; orphan across a
  save/reload; subprocess killed on cancel; process group killed on cancel.
- Full suite: 894 tests, async group stable over repeated runs.

## Completion Plan

### Phase 3.5: End-to-End Correctness (done — see completion notes above)

1. **Call ids (R1).** Accept the provider's tool-call id in
   `get-tool-call-status` / `cancel-tool-call` / `list-running-tool-calls`,
   either by storing it on `ToolCallConfig` or by resolving it through the
   tracked calls. Keep the internal UUID as a fallback. `list-running-tool-calls`
   should return the id the model knows.
2. **Placeholder responses (R2).** For calls still `Running` (or `Deferred`)
   when the LLM is asked for a completion, emit a tool message such as
   `{"status":"running","tool_call_id":…,"progress":[…]}` so the request is
   valid and the model knows the call is pending.
3. **Delivering late results (R2).** A finished result cannot be attached to a
   tool message the model already answered. Inject it into the next user turn
   as a notice (e.g. "tool call X completed: …"). Update
   `naiveTilNoToolCallStep` so `Running` calls count as outstanding and so
   completed-late calls are surfaced.
4. **Partial turns (R2).** Replace the head `PartialUserTurn` on resume instead
   of prepending a new one (match `Session/Wake.hs`).
5. **One engine (R3, R4).** Either return the agent from
   `runAsync` / `runAsyncWithProgress`, or keep the engine alongside the OS
   `World` so every step shares it. Add a cancel hook to
   `ToolExecutionContext` so `cancel-tool-call` actually kills the thread via
   `Engine.cancelToolCall`.
6. **Orphaned calls (R5).** In `pollRunningCall`, mark a `Running` call whose
   entity is missing as `Failed` with an "orphaned" response. Return
   `is_final: true` for orphaned calls in the capability.
7. **Malformed calls (R6).** Replace the `error` in `requireEntityId` with a
   fallback that executes the call inline.
8. **End-to-end tests.**
   - Two slow calls with `YieldOnAnyProgress` → `PartialUserTurn` → resume →
     full `UserTurn`, with a valid OpenAI message list at every step (every
     tool call has exactly one tool message, no duplicated turns).
   - `cancel-tool-call` stops a real running tool (e.g. a sleeping bash
     command) and the thread is gone.
   - A `Running` call whose entity is missing is resolved as orphaned.

### Phase 4: TUI / OneShot UX (done — see completion notes above)

1. **Enablement.** `executionMode` and `toolCallPolicyConfig` already exist in
   the agent JSON; apply them in the TUI and OneShot agent builders (reuse
   `applyAgentDurableConfig`), and add `asyncYieldStrategy` and
   `maxConcurrency`. Without this nothing in the UI can be exercised.
2. **Progress events.** Add `AppEvent_ToolCallProgress` to
   `System.Agents.TUI.Types`. The engine emits through `ctxEventQueue` when
   present. Decide how the event reaches the TUI `BChan` given the TUI still
   goes through the legacy `RuntimeBridge`.
3. **Rendering.** Show running calls with a spinner and latest progress in
   `TUI/Render/Conversation.hs`; handle partial turns in `SessionPrint` /
   `formatSessionAsMarkdown`.
4. **OneShot.** Add `--wait-for-async`. Without it, exit with JSON listing the
   pending calls and the session to resume.
5. **A real progress emitter.** Make one tool call `ctxProgressCallback`
   (bash output lines are the obvious candidate). Stop propagating the
   callback into sub-agent contexts, or give sub-calls their own.

### Phase 5: Cleanup & Hardening (done except tracing — see completion notes above)

1. Cancel running batches on session abort, TUI quit, or exception; shut the
   engine down with the agent. Kill whole process groups on cancel.
   Stop `run` (and the TUI) from spinning on deferred-only partial turns.
2. Expire stale calls (timeout plus a check that the thread is still alive).
3. Global max concurrency / rate limiting, configurable (builds on Phase 3.5
   step 5).
4. Tracing for async calls and progress in `Prod.Tracer`.
5. Verify orphan handling across a real process restart + session reload.
6. Update `docs/tools.md`, `docs/tui.md`, `docs/cli-commands.md`, and document
   the agent JSON schema for async.
7. Clarify whether MCP / OpenAPI tools can stream into the progress callback.
