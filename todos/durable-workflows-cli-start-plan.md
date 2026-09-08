# Plan: Durable execution from the CLI

## Goal

Make the durable-workflow execution mode accessible from the CLI in one-shot
form, so a user can:

1. Start a brand-new session with a prompt using `agents session start ...`.
2. Have the agent run until it yields (partial turn, deferred tool calls) or
   completes.
3. Inspect pending calls with `agents session pending`.
4. Complete deferred calls with `agents session complete`.
5. Resume with `agents session resume`.
6. Iterate step by step, either letting the LLM continue or injecting tool
   results on behalf of the user.

This closes the gap between the existing `run` command (fully synchronous,
runs to completion) and the `session pause/resume/...` commands (which only
operate on already-existing sessions).

---

## Decisions on open questions

| Question | Decision |
|---|---|
| How many steps should `session start` run? | **Zero by default**, with an optional `--step` flag to run exactly one scheduling step. The goal is to forfeit as much scheduling as possible to external processes/users. |
| How is the initial turn created? | **Option B**: construct the initial `UserTurn` directly in `session start` with the prompt and media, then persist the session. |
| Isolated execution? | **Out of scope** for this milestone. Only `RunSync`, `RunAsync`, and `Defer` are supported. |
| Policy language? | **Keep it simple**: exact tool-name rules plus a default disposition. |
| Continuation store? | **Keep it simple**: rely on the file-based `SessionStore` and the in-memory partial turn. A SQLite continuation store can be added later. |

---

## Current state and gaps

### What already works

* The library has full durable-workflow primitives:
  * `ToolCallPolicy`, `ToolCallDisposition` (`RunSync`, `RunAsync`, `Defer`,
    `RunIsolated`).
  * `runAsync` in `System.Agents.Session.Loop` runs an async agent until it
    yields a `PartialUserTurn`.
  * `resumeSession` and `wakeSession` in `System.Agents.Session.Wake`.
  * `SessionBackend`, `ContinuationStore`, and CLI commands
    `session pause/resume/pending/complete/run-isolated`.
* The existing `run` command already accepts prompts, media, session files,
  and thinking-output options.

### What is missing

1. **No CLI command to create a new session in async mode.**
   * `run` always uses `mainOneShotTextWithThinking`, which creates a
     synchronous agent and calls `Loop.run` to completion.
   * `session pause/resume/...` require an existing `SessionId`; they cannot
     start from a user prompt.

2. **No way to configure a durable policy from the agent JSON file.**
   * `System.Agents.Base.Agent` (the JSON config type) has no fields for
     `executionMode`, `toolCallPolicy`, `toolExecutor`, `continuationStore`,
     `deploymentRunner`, or `sessionBackend`.
   * The CLI's `withAgentForSession` builds the runtime agent with
     `defaultToolCallPolicy`, so even in async mode every call runs
     synchronously and no yielding occurs.

3. **No shared parser for `run`-like options in the `session` command group.**
   * `parseOneShotOptions` is only used by `run`.
   * `session start` should accept the same prompt/media/thinking options.

4. **No single-step scheduler command wired to the CLI.**
   * `runAsync` exists in the library but is not used by any CLI handler.
   * `handlePause` calls `runStepM` once on an existing session, but it does
     not create the initial user turn from a prompt.

---

## Proposed CLI additions

### New command: `agents session start`

```bash
# Create a session with an initial UserTurn, do not call the LLM yet.
agents session start \
    --agent-file my-agent.json \
    --prompt "Research the weather in Paris and New York"

# Same, but also run one scheduling step (e.g. ask the LLM and possibly
# execute the first batch of tool calls).
agents session start --step \
    --agent-file my-agent.json \
    --prompt "Research the weather in Paris and New York"
```

Semantics:

* Creates a fresh `Session` with `sessionVersion = Just 2` and
  `sessionExecutionMode = Just Asynchronous`.
* Constructs an initial `UserTurn` containing the prompt (and any media
  attachments).
* Loads the agent, applies the durable configuration from the agent file
  (see below), and sets `ctxExecutionMode = Asynchronous`.
* By default, persists the session immediately without calling the LLM.
* With `--step`, calls `runStepM` exactly once before persisting.
* Prints:
  * the new `session-id`;
  * the current state (`UserTurn`, `LlmTurn`, `PartialUserTurn`, or
    completed);
  * continuation tokens for any deferred calls.

If the session completes after the optional step, the final LLM response is
printed as well.

### New command: `agents session step`

```bash
agents session step <session-id>
```

Runs exactly one scheduling step on an existing session and persists the
result. This is the workhorse for manual iteration:

1. `session start -p "..."` → session with `UserTurn`.
2. `session step <id>` → LLM produces tool calls (`LlmTurn`).
3. `session step <id>` → sync tools run, async/deferred calls yield
   (`PartialUserTurn`).
4. `session complete <token> result.json` → inject external result.
5. `session step <id>` → woken results processed, possibly another LLM turn.
6. Repeat until completion.

`session step` is functionally similar to the existing `session pause`, but
named for iteration rather than interruption. The existing `session pause`
can be kept for backward compatibility.

### Reuse `run` options

Both `session start` and `session step` should accept the same prompt-script
options as `run`:

```bash
agents session start \
    --agent-file my-agent.json \
    --prompt "Summarize" \
    --file report.md \
    --media screenshot.png \
    --thinking stderr
```

The cleanest way is to extract a shared parser type, e.g.
`OneShotPromptOptions`, from `System.Agents.CLI.OneShot` and reuse it for
`run`, `session start`, and (for prompt injection) future commands.

---

## Agent configuration for durable mode

To make durable execution usable without writing Haskell, the agent JSON
config needs to expose at least the following fields.

### Minimal viable config

```json
{
  "slug": "my-durable-agent",
  "executionMode": "asynchronous",
  "toolCallPolicy": {
    "default": "runSync",
    "rules": [
      {"tool": "bash_command", "disposition": {"tag": "defer", "reason": "approval required"}},
      {"tool": "fetch_remote", "disposition": {"tag": "runAsync"}}
    ]
  }
}
```

### Proposed JSON schema additions to `System.Agents.Base.Agent`

```haskell
data Agent = Agent
    { ...
    , executionMode :: Maybe ExecutionMode        -- default Synchronous
    , toolCallPolicyConfig :: Maybe ToolCallPolicyConfig
    }
```

Only `executionMode` and `toolCallPolicyConfig` are in scope for this
milestone.

### Tool-call policy config design

A declarative policy config that the runtime turns into a `ToolCallPolicy`:

```haskell
data ToolCallPolicyConfig = ToolCallPolicyConfig
    { tpcDefaultDisposition :: ToolCallDisposition
    , tpcRules :: [ToolCallPolicyRule]
    }

data ToolCallPolicyRule = ToolCallPolicyRule
    { tprToolName :: Text           -- exact match
    , tprDisposition :: ToolCallDisposition
    }
```

The runtime combinator:

```haskell
buildToolCallPolicy :: ToolCallPolicyConfig -> ToolCallPolicy
buildToolCallPolicy cfg _ctx call =
    fromMaybe cfg.tpcDefaultDisposition $
        find ((== callName call) . tprToolName) cfg.tpcRules
```

### Where the policy is applied

In `System.Agents.CLI.SessionDurable.withAgentForSession`, after building the
runtime agent from the OS tree, inspect the JSON `Agent` config:

```haskell
let agent' = maybe agent (withExecutionMode Asynchronous) (executionMode jsonAgent)
let agent'' = maybe agent' (withToolCallPolicy . buildToolCallPolicy) (toolCallPolicyConfig jsonAgent)
```

This makes `session start`, `session step`, `session pause`, and
`session resume` all use the same durable configuration.

---

## Implementation steps

### Step 1 — Shared prompt options

Refactor `System.Agents.CLI.OneShot` so that the prompt/media/thinking
options are reusable:

```haskell
data PromptScriptOptions = PromptScriptOptions
    { psoPromptScript :: PromptScript
    , psoMediaFiles :: [MediaReference]
    , psoThinkingOutput :: OneShot.ThinkingOutput
    }

parsePromptScriptOptions :: Parser PromptScriptOptions
```

Update `OneShotOptions` to contain `PromptScriptOptions` plus
`sessionFile`. Update `parseOneShotOptions` accordingly.

### Step 2 — Add durable config fields to JSON Agent

In `System.Agents.Base.Agent`:

* Add `executionMode :: Maybe ExecutionMode`.
* Add `toolCallPolicyConfig :: Maybe ToolCallPolicyConfig` with JSON
  instances.
* Keep defaults backward-compatible (`Nothing` = synchronous, default
  policy).

### Step 3 — Build policy from config

In `System.Agents.CLI.SessionDurable`:

* Add `buildToolCallPolicy` / `applyAgentDurableConfig`.
* Update `withAgentForSession` to read the JSON agent config and apply
  durable settings to the runtime agent.

### Step 4 — Implement `session start`

In `System.Agents.CLI.SessionDurable`:

* Add `SessionStart PromptScriptOptions Bool` command (the `Bool` is
  whether to run one step).
* Implement `handleStart`:
  1. Load agent tree and build runtime agent (with durable config).
  2. Interpret the prompt script (aliases, files, shell, media).
  3. Create a fresh `Session` (version 2, async mode).
  4. Construct the initial `UserTurn` with the prompt/media.
  5. If `--step`, call `runStepM` exactly once.
  6. Persist the resulting session.
  7. Print session id, current turn state, and any deferred tokens.

### Step 5 — Implement `session step`

In `System.Agents.CLI.SessionDurable`:

* Add `SessionStep SessionId` command.
* Implement `handleStep`:
  1. Load the existing session.
  2. Load the agent and apply durable config.
  3. Call `runStepM` exactly once.
  4. Persist the resulting session.
  5. Print the new turn state and any deferred tokens.

This is very close to the existing `handlePause`; consider sharing the
implementation.

### Step 6 — Wire the parsers

In `app/Main.hs`:

* Add `SessionStart` and `SessionStep` constructors to
  `SessionDurableCommand`.
* Add `start` and `step` subcommand parsers reusing
  `parsePromptScriptOptions` / `parseSessionIdArgument`.
* Update help text: `Operate durable sessions: start, step, pause, resume, ...`.

### Step 7 — Tests

* Unit tests for `ToolCallPolicyConfig` JSON parsing.
* Unit tests for `buildToolCallPolicy`.
* Integration test exercising the full iterative flow:
  * `session start -p "..."` creates a session with a `UserTurn`;
  * `session step <id>` produces an `LlmTurn` with tool calls;
  * `session step <id>` runs sync tools and defers one call
    (`PartialUserTurn`);
  * `session pending <id>` lists the deferred token;
  * `session complete <token> result.json` injects the result;
  * `session step <id>` processes the result and produces a final response.

---

## Suggested first milestone

The smallest vertical slice that proves the design:

1. Extract shared `PromptScriptOptions` from `run`.
2. Add `executionMode` and `toolCallPolicyConfig` to the JSON `Agent`.
3. Implement `buildToolCallPolicy` and apply it in `withAgentForSession`.
4. Add `agents session start` that creates a session with an initial
   `UserTurn` and optionally runs one step.
5. Add `agents session step` for single-step iteration.
6. Write one integration test exercising the full flow above.

---

## Related docs

* `todos/durable-workflows.md` — original design plan.
* `todos/durable-workflows.progress.md` — implementation progress.
* `docs/durable-workflows-howto.md` — user-facing how-to.
* `src/System/Agents/CLI/SessionDurable.hs` — existing session command
  handlers.
* `src/System/Agents/Session/Loop.hs` — `runAsync` and `runStepM`.

