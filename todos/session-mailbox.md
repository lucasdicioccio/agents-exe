# Spec: the session mailbox (agent-to-agent mail, suspended tool calls, interrupts, tool-call wrappers)

Status: proposal, 2026-09-22. Nothing implemented. Supersedes the designs in
issues #563 and #564, and most of #507 (see "What happens to the issues").

## Goal

One mechanism through which everything that happens *to* a running session
reaches it: a user message, a message from another agent's session, the result
of a tool call the LLM was told is "still running", the result of a deferred
call, a timer, a pause request.

Around it:

* (A2A) an agent can write to another session, running or idle, and get an
  answer back, across sessions and (on `agents-server`) across restarts;
* (suspension) a slow tool call is suspended *inside the runtime*: the LLM gets
  `running, id = call_abc` and takes its turn; it may then wait up to N seconds,
  poll, cancel, or ignore the call;
* (interrupts) while the runtime is blocked (on attached tool calls, on a
  `wait`, optionally on an LLM completion), higher-priority mail pre-empts the
  wait;
* (wrappers) tool calls are wrapped declaratively (timeout, retries, hooks that
  may deny / defer / rewrite), which is the existing, currently inert,
  `Decorator` made real.

## The OS picture

| OS | here |
|----|------|
| CPU running user code | the LLM completing a turn |
| syscall | a tool call |
| blocking syscall | an *attached* call: the step waits for it |
| `SIGTSTP` / `bg` | *detach*: the call keeps running, the LLM gets a placeholder |
| `wait(2)`, `kill(2)`, `/proc` | `wait`, `cancel-tool-call`, `get-tool-call-status` |
| interrupt controller + message queue | the mailbox |
| interrupt-enabled points | the receive points R1-R4 below |
| syscall filter / LSM hooks | wrappers |
| scheduler run queue | `Host.Runner`: a session with unread mail and no run is runnable |

## Current state

More of this exists than the issues assume. They predate `Session/Async*`,
`Session/Wake.hs` and `Host/Runner.hs`.

### What we can reuse as-is

* Background calls: `RunAsync`, the async engine, OS tool-call entities,
  placeholder tool messages (`partialToolMessages`), progress, cancellation of
  the whole process group (`docs/async-tool-calls.md`).
* `get-tool-call-status` (with `wait_for_completion` + `timeout_seconds`),
  `list-running-tool-calls`, `cancel-tool-call`.
* Deferred calls, continuation tokens, `wakeSessionWith`.
* `Host.Runner`: per-session lock, one run at a time, versioned writes
  (`sbCompareAndStore`), `SessionEvent` broadcast + `subscribe`, the HTTP event
  stream, lineage (`smParent`) and owners (`smOwner`).
* `AsyncToolResponse = ToolComplete | ToolYield`, which is the right result
  type for a wrapper that wants to suspend a call.

### Gaps

G1. A session has **half a dozen unrelated input channels**, each with its own
delivery rule:

| input | channel | "delivered once" is ensured by |
|-------|---------|-------------------------------|
| user text | `usrQuery :: IO (Maybe UserQuery)`, pulled | the caller |
| background result | STM over the OS entity (`awaitEntities`) | `tcDeliveredLate` flag in the turn |
| deferred result | `lsInbox :: TVar [(token, result)]` (`Runner.hs:204`) | `csComplete` + `TokenAlreadyCompleted` |
| user text, TUI | a `BChan` per conversation + "buffered messages" in the core, drained by `usrQuery` (`tui/…/Event/Conversation.hs`) | clearing the buffer on read |
| stop | `cancel` of the run thread (`cancelRun`) | - |
| pause, TUI | `step` polls `isConversationPaused` every 200 ms | - |

G2. **A busy session cannot be talked to.** `postMessage` answers
`RunInProgress` / `NotAcceptingMessages` (`Runner.hs:664`). Nothing can be
addressed to a session by another session at all. This is the A2A blocker.

G3. The one interrupt we have, `race agent.usrQuery (waitForRunningCalls …)`
(`Step.hs:630`), races two IO actions, so `usrQuery` "must tolerate
cancellation", and it only exists at one point (idle with background calls).
Attached calls, `wait_for_completion` and LLM completions are not
interruptible.

G4. Suspension is **static**: a tool is `runAsync` or it is not. There is no
"run it, and if it is still going after 10 s hand control back" per call;
`yieldOnTimeout` is per agent and per step. And a `runSync` call runs in the
step's own thread, so it can never be detached.

G5. **Decorators are parsed, stored, and never applied.** `WithTimeout`,
`WithRetries`, `WithCache`, `WithLabel` (`Session/Types.hs:468`) are flattened
into `ccDecorators` (`Step.hs:347`), which nobody reads; `Durable.hs` binds
them to `_decorators` four times. Policy rules match one exact tool name,
first match wins (`AgentConfig.hs:26`), so a decorator cannot be applied
across tools.

G6. "If the model reads a result with `get-tool-call-status`, the delivery
notice repeats it once" (documented limit): a symptom of G1, there is no
notion of a message having been consumed.

## Design

### 1. The mailbox

An envelope:

```haskell
data Envelope = Envelope
    { envId       :: MessageId      -- idempotency key; sender may supply it
    , envSeq      :: Int            -- assigned on accept; total order per session
    , envFrom     :: Sender
    , envPriority :: Priority       -- Normal | Interrupt
    , envHops     :: Int            -- A2A loop guard, see §5
    , envSentAt   :: UTCTime
    , envBody     :: MailBody
    }

data Sender
    = FromUser (Maybe Text)                 -- owner, when known
    | FromSession SessionId (Maybe Text)    -- and its agent slug
    | FromToolCall ToolCallId
    | FromSystem Text                       -- timers, watchers, the runner

data MailBody
    = UserMessage UserQuery
    | AgentMessage Text (Maybe MessageId) Bool      -- text, in-reply-to, expects-reply
    | ToolCallFinished ToolCallId ToolCallState UserToolResponse
    | ContinuationResult ContinuationToken UserToolResponse
    | WatchedEvent SessionId Text Aeson.Value       -- §7
    | Control ControlMsg                            -- Pause | Resume | CancelCalls [ToolCallId] | StopRun
```

Progress is *not* mail. It is state (the OS entity keeps the last 50 entries)
and is read, not delivered.

The interface, a record of functions like `ContinuationStore`, carried as
`ctxMailbox :: Maybe Mailbox` on `Agent`:

```haskell
data Mailbox = Mailbox
    { mbSend   :: Outgoing -> IO (Either SendError Receipt)  -- any thread, any session
    , mbUnread :: Cursor -> STM [Envelope]                   -- never removes anything
    , mbTrim   :: Cursor -> IO ()                            -- GC at or below the cursor
    }

data SendError = UnknownRecipient | MailboxFull | NotPermitted | TooManyHops
data Receipt   = Receipt { rcptId :: MessageId, rcptSeq :: Int, rcptDuplicate :: Bool }
```

**Transactional** means three things, and only these:

1. *Accept is atomic and idempotent.* One STM transaction (in memory) or one
   `INSERT … ON CONFLICT DO NOTHING` (durable) assigns `envSeq`. Resending the
   same `envId` returns the first receipt with `rcptDuplicate`.
2. *Reading does not consume.* There is no `pop`. A reader is a cursor.
3. *Consumption commits with its effect.* `Session` gains `mailCursor :: Int`
   (JSON default 0). A step that folds envelopes `≤ k` into a turn returns a
   session with `mailCursor = k`; the versioned store of that session *is* the
   ack. A crash between reading and storing redelivers into a session that
   does not contain the mail yet: exactly-once effect, no two-phase anything.

Point 3 is why #563's `TQueue` component is not enough (a `readTQueue`
followed by a crash loses the message), and it removes `tcDeliveredLate`-style
flags: "delivered" is `envSeq ≤ mailCursor`.

Because `mbUnread` is STM, a blocking receive is `retry`, a filtered receive
is a `filter`, and a receive with alternatives is `orElse`:

```haskell
awaitMail :: Mailbox -> Cursor -> (Envelope -> Bool) -> STM [Envelope]
awaitMail mb cur p = do
    es <- filter p <$> mbUnread mb cur
    when (null es) retry
    pure es
```

Two implementations:

* in memory (`TVar (Seq Envelope)`), for `run`, the TUI, sub-agents, tests;
* durable, for `Host`: a `session_mail` table next to the continuations
  (`session_id, seq, id UNIQUE, from_json, priority, hops, body_json,
  accepted_at`), SQLite and Postgres, fronted by the same `TVar` so
  `mbUnread` stays STM. The runner is single-process per session already
  (second writers are detected), so the `TVar` front is sound; a multi-process
  Postgres deployment would fill it from `LISTEN/NOTIFY`, later.

Bounded: `mailboxMaxUnread` (default 256). A full mailbox refuses with
`MailboxFull`; the sender's tool call fails, which is the backpressure.
`Control` and `ToolCallFinished` are exempt.

### 2. Receive points

The stepper reads the mailbox at four points. Everything else in this spec is
a consequence of what each point does.

| | when | blocking | reads |
|---|------|----------|-------|
| R1 | building a user turn, before `AskLlmCompletion` | no | all unread |
| R2 | idle: the LLM issued no calls, or only detached/deferred calls remain | yes | all unread |
| R3 | blocked on attached calls, or inside the `wait` tool | yes, as one arm of an `orElse` | `Interrupt` (R3a), any (R3b, `wait`) |
| R4 | during an LLM completion (opt-in) | `race` | `Interrupt` |

R1 generalises `collectLateResults`; R2 replaces the `race` in `askUserQuery`
and the `block` flag; both become:

```haskell
-- R2, no IO race, nothing to cancel
atomically $ awaitMail mb cur (const True)

-- R3a: attached calls vs. interrupts, one transaction
atomically $ (Left  <$> awaitMail mb cur isInterrupt)
    `orElse` (Right <$> awaitEntities world any attached)
```

A genuine `race` (thread cancellation) remains only where an IO action must
be abandoned: R4, and an inline call under `StopRun`.

**What the LLM sees.** Providers want exactly one tool message per call, then
optionally a user message. So mail is folded into the `userQuery` of the turn
being built, as `lateResultsQuery` does today, one block per envelope with a
header the model can quote back:

```
[mail m-7f3a from session 9c1e (agent "reviewer"), expects reply]
The migration in your PR drops an index that report_daily depends on.

[mail m-7f3b: tool call call_abc (run_tests) completed]
42 tests passed
```

The raw envelopes are also kept on the turn (`userMail :: [Envelope]`, new
optional field of `UserTurnContent` / `PartialUserTurnContent`), so the TUI,
the chat page and `session-print` render senders properly instead of parsing
a blob, and the audit trail #563 wanted (`imSourceAgent`) is in the session
file.

`Control` envelopes are consumed (cursor moves) but render nothing.

**Compatibility.** `ctxMailbox = Nothing` keeps today's code paths, `usrQuery`
included. With a mailbox, front-ends post `UserMessage` instead of answering
`usrQuery`: one-shot runs pre-load one envelope; the TUI's message editor posts
to the selected conversation's mailbox, which replaces both the per-conversation
`BChan` and the buffered messages (typing at a busy conversation *is* unread
mail), and the TUI's pause becomes `Control Pause` instead of a polling `step`;
`postMessage` posts. `usrQuery` stays on the record for embedders.

### 3. Suspended tool calls: attach and detach

In asynchronous mode **every call that has an OS entity runs in the engine**.
A disposition no longer says *where* a call runs but *how long the step stays
attached to it*:

| config | meaning |
|--------|---------|
| `{"tag":"runSync"}` | attached until done (today's behaviour, now detachable by an interrupt) |
| `{"tag":"runAsync"}` | attached per the agent's `asyncYieldStrategy` (unchanged) |
| `{"tag":"runAsync","attachSeconds":10}` | attached for 10 s, then detached |
| `{"tag":"runAsync","attachSeconds":0}` | detached at once |

`attachSeconds` is G4's answer: fast calls look synchronous, slow ones suspend
themselves. The wait is R3a with one more `orElse` arm per distinct deadline
(a `registerDelay` TVar).

Detaching is what `executeTrackedCalls` already does when it emits a
`PartialUserTurn` with `Running` calls; the placeholder gains the reason:

```json
{"status": "running", "tool_call_id": "call_abc",
 "detached": "still running after 10s",
 "message": "Its result will arrive as mail. Use wait, get-tool-call-status or cancel-tool-call."}
```

On completion the engine posts `ToolCallFinished` to the owning session's
mailbox **in the same STM transaction that marks the entity `TcCompleted`**,
so a result can neither be lost nor observed by `get-tool-call-status` before
it is mail. Fixing G6: when `get-tool-call-status` returns a final result, the
stepper marks that envelope as read-by-tool on the turn, and R1 renders it as
a one-line `already read` notice instead of repeating the payload.

The LLM's side, in the System Toolbox:

| capability | |
|------------|--|
| `wait` (new) | `{"for": ["call_abc"] \| "any-call" \| "mail", "timeout_seconds": 30}`. Returns on the first of: a named call is final, any mail, timeout. Says which. Peeks, never consumes: the mail it woke for is delivered by R1 in the same turn. |
| `get-tool-call-status` | unchanged; `wait_for_completion` becomes sugar for `wait` and gains interruptibility |
| `cancel-tool-call`, `list-running-tool-calls` | unchanged |
| `send-to-tool-call` (later) | for a sub-agent call: mail to the child session (§5). For bash: stdin, not planned. |

`wait` needs the mailbox, so `ToolExecutionContext` gains `ctxAwaitMail`, a
hook like `ctxCancelToolCall`. `wait` always takes a timeout, capped by
`maxWaitSeconds` (default 300): two agents waiting on each other time out
instead of deadlocking.

Background calls still die with their process (orphaned on reload). Unchanged,
out of scope.

### 4. Interrupts

Priority is set by the sender and checked on accept: the user and `Control`
may always interrupt; a session may interrupt its descendants; peers may not
(configurable, §5).

| `Interrupt` mail arrives while the step is… | effect |
|---|---|
| attached to engine calls (R3a) | all attached calls are detached; the LLM is asked at once, with the mail and placeholders. This is `^Z` then talk. |
| in the `wait` tool (R3b) | `wait` returns `{"woken_by": "mail"}`; any priority does this |
| in an inline call (no entity, no engine) | nothing until it returns; `StopRun` cancels the thread |
| in an LLM completion (R4), `interruptCompletions: true` | the completion is cancelled; the head `UserTurn` is **amended** with the mail (the LLM never answered it, so no turn is lost) and asked again |
| in an LLM completion, default | delivered at the next R1/R2 |
| idle (R2) | same as `Normal` |

`Normal` mail never pre-empts anything but `wait` and R2.

`Control`:

* `Pause`: the run stops at the next receive point, cursor committed, status
  `paused`; detached calls keep running unless `pauseCancelsCalls`. `Resume`
  (or, by config, any mail) makes it runnable again.
* `CancelCalls`: the engine's cancel; results come back as
  `ToolCallFinished … Failed`.
* `StopRun`: today's `cancelRun`, now reachable by mail.

### 5. Agent-to-agent

**Address**: a `SessionId`. Discovery is by the existing `list-sessions` /
`search-sessions`, plus `smAgent`, `smParent`.

**Routing.** `mbSend` needs to find the recipient's mailbox, in every front-end
and not only on the server (helper agents are used extensively in the TUI). A
`MailRouter` is the process's table of live mailboxes:

```haskell
data MailRouter = MailRouter
    { mrRegister :: SessionId -> MailboxInfo -> Mailbox -> IO (IO ())  -- returns unregister
    , mrLookup   :: SessionId -> IO (Maybe (MailboxInfo, Mailbox))
    , mrList     :: IO [(SessionId, MailboxInfo)]   -- agent slug, parent, status
    }
```

| front-end | router | a session is live… |
|-----------|--------|--------------------|
| `agents-server` | `Host.Runner` (durable mail; unknown-but-stored sessions are loaded on demand) | always |
| TUI | one per process, held next to `coreRef`; every conversation registers, sub-calls included | while its conversation exists |
| `run`, MCP server | one per process; the root and every sub-agent session register | until its loop returns |

It travels as `ctxMailRouter` on `Agent` and is handed down to sub-agents the
way `ctxWorld` and `ctxEventQueue` are, so a whole call tree shares one.
`mrList` is what makes in-memory sessions discoverable (the "ongoing sessions"
#563 wanted to list): `list-sessions` merges it with the persisted ones.

**Scheduling rule**: *a session with unread mail, no run, and a status that
accepts mail is runnable.* On the server the runner starts a run; in the TUI
and in `run` a session's loop is simply parked at R2 and wakes by itself.

Which mail wakes an **idle** session is a per-agent option, `wakeOn`, a list
among `user`, `tool`, `parent`, `child`, `peer`. Mail that does not wake is
still queued and delivered with the next thing that does. Default
`["user", "tool", "parent", "child"]`, deliberately a config knob and not a
rule of the design: it is too early to know what peers should be allowed to do
to each other (D10). The rule also replaces the special case in
`completeCall`: `autoResume` becomes "post `ContinuationResult`", and `lsInbox`
goes away.

`postMessage` during a run stops failing: it posts `UserMessage` and returns
the receipt (HTTP 202 with `message_id`, `seq`), optionally
`"interrupt": true`.

**Tools** (System Toolbox capabilities):

| capability | |
|------------|--|
| `send-message` | `{"to": "<session>", "text": …, "in_reply_to": "m-…", "expects_reply": true, "interrupt": false}` → receipt + the recipient's status (`running`, `idle`, `paused`) |
| `spawn-session` | `{"agent": "<slug>", "message": …}` → the new session's id. A session that is a child in lineage but *not* call/return: it outlives the tool call that created it, and answers by mail. `<slug>` is one of the caller's helpers, as for `prompt_agent_<slug>`, with the same `with` / `bindings` narrowing. |
| `wait` | §3, with `"for": "mail"` |

`spawn-session` exists in every front-end:

* server: `createSessionAs`, durable, survives restarts;
* TUI: a new conversation, created the way sub-calls are today
  (`OSEvent_SubcallStarted`, `conversationParentId`), so it shows up in the
  conversation list, the user can open it and type into it, and its mailbox is
  what the message editor posts to. It lives until the user closes it or its
  parent conversation goes away;
* `run`: a thread with an in-memory mailbox, cancelled with the root like
  background calls are (`withEngineShutdown`). A root that stops while
  spawned sessions still work loses them; an agent that wants their answers
  calls `wait` (D11).

Call/return sub-agents (`prompt_agent_<slug>`) are unchanged, but when they run
detached their placeholder and status report the **child session id**, so the
parent can `send-message` to a helper that is still working, and the helper's
final answer arrives as `ToolCallFinished`. "Interacting with a running tool
call" and "agent-to-agent chat" are the same mechanism.

**Permissions**: #563's scope, checked in `mbSend`, per sending agent:
`mailScope: own | children | subtree | all` (default `subtree`), and
`interruptScope` (default `children`). Through the HTTP API the existing owner
check applies.

**Loops**: a reply carries `envHops = hops of the mail being answered + 1`
(the stepper knows which mail the current turn consumed); `maxMailHops`
(default 16) refuses with `TooManyHops`. User mail resets to 0. Together with
`MailboxFull` and the `wait` cap, two agents cannot ping-pong or deadlock
forever.

### 6. Wrappers

Decorators become middleware over the executor, applied in one place
(`executeCall`, which both the inline path and the engine already go through):

```haskell
type Exec = ToolExecutionContext -> LlmToolCall -> IO AsyncToolResponse
type ToolMiddleware = Exec -> Exec

interpretDecorator :: WrapperEnv -> Decorator -> ToolMiddleware
-- Decorate [d1, d2] base  ==>  d1 (d2 exec): first listed is outermost
```

`AsyncToolResponse` rather than `UserToolResponse` so that a wrapper can
answer `ToolYield`: that is how a `before` hook turns a call into a deferred
one (an approval gate) from inside the engine, without blocking the step.

Config: `rules` keep choosing the base disposition (first match). A new
`wrappers` list contributes decorators; **every** match applies, in file
order, outermost first. The result is still one `Decorate […] base`, recorded
in `AppliedPolicy`, so the session file shows what wrapped each call.

```json
"toolCallPolicyConfig": {
  "default": {"tag": "runSync"},
  "rules": [{"tool": "run_tests", "disposition": {"tag": "runAsync", "attachSeconds": 10}}],
  "wrappers": [
    {"match": {"tool": "*"},        "decorators": [{"tag": "truncate", "maxBytes": 65536}]},
    {"match": {"tool": "http_*"},   "decorators": [{"tag": "retries", "count": 2, "backoffMs": 500},
                                                   {"tag": "timeout", "seconds": 30}]},
    {"match": {"tool": "deploy_*"}, "decorators": [{"tag": "before", "hook": {"command": "hooks/approve-deploy"}},
                                                   {"tag": "after",  "hook": {"tool": "audit_log"}}]}
  ]
}
```

`match`: `tool` (glob on the LLM-visible name), `toolbox`, later `args` (a
JSON predicate). Decorators:

| tag | |
|-----|--|
| `timeout`, `retries`, `cache`, `label` | the existing four, now applied. Per-call `timeout` wins over `asyncCallTimeoutSeconds`; each retry reports progress. |
| `truncate` | cap the result, say so to the model |
| `before` | hook sees `{tool, arguments, session_id, tool_call_id, agent}`; answers `continue` (optionally `arguments` rewritten), `deny {message}` (the message is the tool result, the tool does not run), `defer {reason}` (→ `ToolYield`, a continuation token, the usual external completion), `answer {result}` (short-circuit) |
| `after` | hook sees the above plus `result`, `status`, `duration_ms`; answers `continue` (optionally `result` rewritten), `annotate {text}` |

A hook target is `{"command": path}` (JSON on stdin, JSON on stdout, the
isolation-envelope conventions, non-zero exit = `deny` for `before`, ignored
with a trace for `after`), or `{"tool": name}`: any tool registered on the
agent, visible to the LLM or not, **including a sub-agent**, which gives
LLM-as-guard for free and lets hooks use bindings and secrets like any tool.

Two rules:

* Hooks see what the LLM sent. Bindings are merged inside `toolRun`, below
  `executeCall`, so bound values, secrets included, never reach a hook, and a
  `before` rewrite cannot override a binding.
* A hook failure (crash, timeout, bad JSON) is `deny` for `before` ("fail
  closed") and `continue` for `after`, both traced.

### 7. Watching other sessions

`Host.Runner` already broadcasts `SessionEvent` and serves it over HTTP. No
subscription registry, no filter algebra: a **watch** forwards matching events
of one session into the watcher's mailbox as `WatchedEvent`, and `wait` is the
only blocking primitive.

`watch-session {"session": …, "events": ["run.stopped", "tool.completed"], "tool": "deploy_*", "ttl_seconds": 600}`,
`unwatch-session`. Watches are in-memory, scoped like mail, capped per session,
and die with their TTL or either session. `SessionEvent` gains the two
tool-call events (`tool.started`, `tool.completed`); the rest of #563's event
list is already covered by `SessionUpdated` carrying the head turn.

## What happens to the issues

| issue | kept | changed | dropped |
|-------|------|---------|---------|
| #563 ph.1 inbox | inject a message, pause, cancel calls, scopes, audit of the sender | an `Agent`-level interface with a cursor in the session, not an ECS `TQueue` component: the server path does not keep sessions in the World, and a queue pop is not transactional with the session store | `InjectSystemInstruction`; `ForkConversation` (belongs with §5.2 of the partial-application spec) |
| #563 ph.2 event history | tool-call events | extend `Host.Runner.SessionEvent`; history = the stored session versions | a second `SessionEvent` type, the `TBQueue` ring |
| #564 ph.3 subscriptions | watching a session | events → mail (§7) | registry, `FilterCombinator`, quotas per subscriber |
| #564 ph.4 wait-for | blocking with a timeout | `wait` on one's own mailbox (§3) | `EventCondition` DSL. Note the sketch's `readTQueue` then `retry` on a non-match never progresses: `retry` undoes the read, so a non-matching head blocks the queue forever. |
| #507 suspend | suspended = no run, mail accumulates, cursor committed; `Pause`/`Resume` | - | `ManagedResource` tracking: engine shutdown, process groups and `evictIdle` already release what a run holds. Reopen if a toolbox leaks. |

## Phases

Each phase ships alone and leaves `ctxMailbox = Nothing` agents untouched.
Phases 0, 3 and 5 depend on nothing but Phase 1 (Phase 0 not even on that);
Phase 4 needs Phases 1-2 and, for its server part only, Phase 3.

### Phase 0: decorators are real
`interpretDecorator` for the existing four + `truncate`; applied in
`executeCall` from `tcPolicy`; `wrappers` with glob matching. Independent of
everything else. Closes G5.

### Phase 1: the mailbox, in memory
`Mailbox`, `Envelope`, `mailCursor`, `userMail`; R1 and R2; the engine posts
`ToolCallFinished`; late results and the `usrQuery` race ported; TUI and
one-shot post `UserMessage`. Visible change: typing during a busy run is
accepted and delivered at the next R1. Closes G1, G6.

### Phase 2: attach / detach
All entity-backed calls through the engine in async mode; `attachSeconds`;
R3a with interrupt-detaches; `wait` and `ctxAwaitMail`; an interrupt key in
the TUI. Closes G3 (except R4), G4.

### Phase 3: durable mail on the server
`session_mail` on both backends; `lsInbox` and the `completeCall` special case
replaced; the runnable rule and `wakeOn`; `postMessage` during a run → 202;
`Control` by mail; mail in the event stream and on the chat page (plain text,
as that page is on purpose). Closes G2.

### Phase 4: agent-to-agent
`MailRouter` in the TUI, `run` and the server; `send-message`, `spawn-session`
in all three, scopes, hops, `wakeOn`; detached sub-agent calls expose the child
session id; `list-sessions` shows live in-memory sessions. The TUI comes first:
it is where helpers are used most and where a spawned session can be watched
and typed into, which is the cheapest way to find out what peers should be
allowed to do (D10).

### Phase 5: hooks
`before` / `after`, command and tool targets, `deny` / `defer` / `answer` /
rewrite.

### Phase 6: watches, R4, pause
`watch-session`, `tool.*` events, `interruptCompletions`, `Pause`/`Resume`.

## Decisions (proposed)

D1. **The cursor lives in the session**, so the versioned session store is the
ack. No per-message delivered flags, no separate consumer table.

D2. **Mail is rendered into the user query, and kept raw beside it.** The only
shape every provider accepts after tool messages; the raw copy is for UIs and
audit.

D3. **Reads never consume; only the stepper advances the cursor.** Tools
(`wait`, status) peek. One consumer per mailbox, so no claim/lease protocol.

D4. **In async mode, `runSync` means "attached forever", not "in the step's
thread".** Otherwise an interrupt cannot detach it. Calls without an entity
stay inline and uninterruptible, as today.

D5. **Interrupting a completion amends the unanswered head turn** rather than
stacking a second user turn or keeping a truncated LLM turn. Opt-in, because a
cancelled completion is paid for.

D6. **Wrappers are data in the disposition**, not a second mechanism: one
`Decorate`, visible in `AppliedPolicy`. Wrappers compose (all matches); base
dispositions do not (first match).

D7. **Hooks never see bound values**, and `before` fails closed.

D8. **Events become mail; `wait` is the only blocking primitive.** One thing
to make interruptible, time-bounded and deadlock-free, instead of three.

D9. **Progress is state, not mail.** It would drown the mailbox and the
context window; a tool that wants the model's attention can finish, or a
later `notify` progress level can post mail.

D10. **What wakes an idle session is configuration (`wakeOn`), not design.**
Decided 2026-09-22: too early to settle what peers may do to each other, so
the mechanism stays neutral and the default conservative. Same reasoning for
`mailScope` / `interruptScope`: knobs, with defaults we expect to revisit once
the TUI has been used with spawned sessions for a while.

D11. **A run never lingers for a reply.** It stops when the LLM is done, no
background call runs and nothing is unread, whatever `expects_reply` messages
are outstanding. An agent that wants the answer calls `wait`. `expects_reply`
is a hint to the recipient, never a runtime obligation.

D12. **Peer sessions exist in every front-end**, not only on the server
(decided 2026-09-22: helpers are used extensively in the TUI). Hence the
`MailRouter`, and `spawn-session` as a TUI conversation. Durability is the
only thing the server adds.

## Related docs

`docs/async-tool-calls.md`, `docs/durable-workflows-howto.md`,
`docs/agents-server.md`, `todos/async-tool-calls.md`,
`todos/tool-partial-application.md` (bindings, §5.2 fork).
