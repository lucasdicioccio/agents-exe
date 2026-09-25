# Spec: the OS as a standalone server (TUI and web UI as clients)

Status: proposal, 2026-09-23, revised the same day after checking service
readiness. Phases 0 and 1 done on branch `feature/os-standalone-server`
(commits 8de2a64..dc01dc8); All phases landed (8de2a64..771c988), with the open items listed
under "Remaining after Phase 5". Builds on
`todos/web-server-embedding.md` (done) and `todos/session-mailbox.md` (done).

## Goal

One long-running `agents-exe` process hosts the agents and their sessions. It
loads agents files the way the TUI does today (`agents-exe.cfg.json`,
`agentsFiles`, `agentsDirectories`, `--agent-file`, `--agent`). Clients attach
to it and drive it:

* the TUI, in the same process or over a socket, with no loss of features;
* the web chat page, as today;
* any program, through the HTTP API: create and message sessions, wake them
  (continuations, mail), interrupt them, cancel them, follow their events.

The end state: `agents-exe serve` runs standalone; `agents-exe tui` either
embeds the runner (today's single-process experience) or attaches to a
running server with `--attach URL`; both TUI modes are the same code.

## The OS picture

| OS | here |
|----|------|
| kernel | `Host` + `Host.Runner` (`SessionRunner`) |
| process table | the `sessions` table, `srLive` for the resident ones |
| syscall interface | the runner's operations, serialized as `Command` |
| kernel log / netlink | the `SessionEvent` broadcast, serialized as `Event` |
| terminal | the TUI: a client that only renders and issues commands |
| `init` | `agents-exe serve`: config loading, then the runner |

The ECS `System.Agents.OS.*` layer (World, components, `OS.Persistence`) is
**not** the kernel in this picture. In production it only tracks async
tool-call entities and subcall visibility for the TUI. The kernel's state
model is sessions, mail and continuations, and its source of truth is the
persisted session.

## Current state

Two runtimes exist and share only the agent tree loader and
`AgentFactory.buildAgent`.

| | TUI (`tui/`) | agents-server (`examples/agents-server`, `src/System/Agents/Host*`) |
|---|---|---|
| loop | `Session.Loop.runUntilBlocked`, one forked thread per conversation (`TUI/Event/Conversation.hs`, `spawnConversationIO`) | `Host.Runner.runLoop`, one run at a time per session, under a lock |
| agent build | `OneShot.nodeToAgent`, then the TUI rewrites `step` and `usrQuery` | `Runner.newAgent` (`buildAgent … RootAgent`, durable mailbox, router, spawn and watch hooks) |
| mail | `newInMemoryMailbox` + one process-local `MailRouter` in `Core` | `newDurableMailbox`, `session_mail` table, `serverMailRouter` |
| user input | a `BChan` per conversation plus a `TVar` of buffered messages | `postMessage`, which becomes `UserMessage` mail when the session is busy |
| pause | `Control Pause` mail since b62e2a6 | `pauseSession` → `Control Pause`, `POST /v1/sessions/:id/pause` |
| sessions | file store, `conv.<uuid>.json`, `SessionStore.listSessions` once at startup | `SessionBackend` (SQLite or Postgres), versioned `sbCompareAndStore`, `sbQuery` |
| config | `agents-exe.cfg.json`, `agentsDirectories`, `--agent`, `--params-file` (all in `app/Main.hs`) | flags only: `--agent-file` (repeatable), `--db`, `--set`/`--pin` |
| events out | `TQueue OSEvent` bridged to Brick; a whole-`Session` progress callback; heartbeat polling of TVars | `SessionEvent` broadcast `TChan`, per-session `subscribe`, SSE |
| identity | `ConversationId` generated independently of `SessionId` | `ConversationId` derived from `SessionId` |

### What we can reuse as-is

* `Host.withHost` / `withHostStores`: multi-file loading keyed by slug, plus
  agents stored in the database.
* `Host.Runner`: `createSession[As[WithParent]]`, `spawnSession`,
  `postMessage` (with `nmInterrupt`), `resume`, `completeCall`, `cancelRun`,
  `cancelAttachedCalls`, `pauseSession`, `getSession`, `awaitRun`,
  `deleteSession`, `recoverOnStartup`, `subscribe`/`subscribeSTM`.
* The HTTP API and SSE stream (`AgentsServer.Api`), MCP over HTTP, auth,
  `/openapi.json`, the chat page.
* Mail: `Envelope`, `MailBody`, `Sender`, `Priority`, `ControlMsg` all have
  JSON. Every client-facing thing is addressed by a UUID (`SessionId`,
  `ContinuationToken`, `ToolCallId`, `MessageId`).
* `Session` and `SessionMeta` have JSON; the TUI's usage, signals and
  trajectory views are pure functions over a `Session`, so they run on a
  client unchanged.

### Gaps

G1. **The TUI is the host, not a client.** `spawnConversationIO` builds the
agent, overrides `step` (pause polling, progress callback, `Stop` →
`AskUserPrompt` rewrite) and `usrQuery` (blocks on the `BChan`), forks
`Loop.runUntilBlocked`, and owns the World, the OSEvent queue and the
`MailRouter`. Quit is `killThread` on every conversation. None of this goes
through the runner.

G2. **The runner needs a first message.** `createSession` takes a
`NewMessage`; the TUI creates an empty conversation and waits for input.

G3. **Event shape.** The TUI renders from full `Session` snapshots (the
`OnSessionProgress` callback) and from `OSEvent_Subcall*` and
`OSEvent_ToolCallActivity`. The runner emits `SessionEvent` (meta plus head
turn, deferred calls, run start/stop, text deltas, tool start/stop) and
`AgentFactory` sets `ctxEventQueue = Nothing`, so subcall progress and
tool-call activity never reach the server or its clients. Nine of the fifteen
`OSEvent` constructors are never emitted.

G4. **No JSON for commands and events.** `SessionEvent`, `NewMessage`,
`RunMode`, `RunnerError`, `OSEvent`, `ToolCallActivity`, `ToolCallPhase`,
`SessionProgress`, `Outgoing`, `Receipt`, `SendError` have no codecs. The only
encoder is `eventFrame` in the server's `Api.hs`; the only decoders are its
request parsers.

G5. **No replay, no global feed.** Events carry no sequence number and are
not persisted; a reconnecting client gets a fresh `snapshot`. Subscriptions
are per `SessionId`; there is no owner-wide or server-wide feed, so a live
session list cannot be kept up to date.

G6. **Runner and HTTP operations the TUI relies on are missing:**

| TUI feature | today | runner / HTTP |
|---|---|---|
| fork at a turn (`handleForkAtTurn`) | local `Session` copy with `forkedFromSessionId` | none |
| continue a stored session with the currently selected agent | `handleRestoredConversation` | `smAgent` is fixed per session |
| edit / delete queued messages (`TUI/Event/Queue.hs`) | shared `TVar` of discrete messages read by the runtime | not needed: becomes a TUI-local draft (§5) |
| list sessions | `SessionStore.listSessions` | `sbQuery` directly on the backend, not a runner call |
| generic mail (send `AgentMessage`, `StopRun`, `CancelCalls [ids]`, `Resume`) | n/a | `sendControlMail` is internal; no route |
| complete a deferred call | not supported in the TUI | `completeCall`, `POST /v1/continuations/:token` |
| agent details (model, prompt, activation flags) for the Agents tab | live `OSAgentNode` TVars | `GET /v1/agents` has slug, description, tool names only |

G7. **Config parity.** `locateAgentsExeConfig`, `initArgParserArgs`,
`resolveAgentFiles`, `--params-file` handling and the default-directory
fallback live in `app/Main.hs` and are not in a library. The server cannot
load agents "like the TUI does".

G8. **Identity.** The TUI's `ConversationId` is independent of its
`SessionId` (see the comment in `spawnConversationIO`); the server derives
one from the other. `AgentId` is random per load; the slug is the stable
name. There are two `ConversationId` types (`Base` and `OS.Core.Types`).

G9. **Spawn semantics differ.** The TUI's `spawn-session` hook resolves a
slug against the roster of root agents; the server resolves helpers of the
calling node.

G10. **Sub-agent runs are not sessions.** A `prompt_agent_*` call runs its
child inside the tool call, so the child produces no `SessionEvent` and
cannot be `cancelRun`'d or subscribed to on its own. Only the TUI sees it,
through `OSEvent_Subcall*`.

G11. **Volatile per-session state.** Secret params (`lsParams`), watches and
the run `Async` are lost on restart or eviction. A client protocol needs a
"resupply secrets" step, or durable encrypted storage.

G12. **No CORS.** `checkOrigin` in `AgentsServer.Api` refuses non-loopback
`Origin`s when auth is off and lets everything through when it is on, but
the server never emits `Access-Control-Allow-*` headers and does not answer
preflight `OPTIONS`. A browser page served by another local web app (a
different port is a different origin) cannot call the API or open the SSE
stream directly; it has to proxy through its own backend.

G13. **Service packaging.** The server itself is service-ready: SIGTERM and
SIGINT trigger a graceful stop with `--shutdown-grace`, logs are JSON lines
on stderr, every path is a flag, `recoverOnStartup` runs at boot. But there
is no unit file, no install target, no `bundling/` entry and no
documentation for running it under systemd.

### Integration modes, as of 2026-09-23

| Mode | Status |
|---|---|
| server-side web app calls the API and renders its own chat | works |
| browser chat UI on another origin talks to the service directly | blocked by G12 |
| web app proxies the API under its own origin | works |
| MCP client (`ask_<slug>` tools, blocking, no session continuity) | works, coarse |
| another agents-exe agent uses it as an OpenAPI toolbox | works |
| deferred tool calls executed by the web app's own workers | works |
| TUI attached from elsewhere | not started |

## Design

### 1. The protocol module

A library module, `System.Agents.Protocol`, owning two sum types with JSON
instances. The HTTP layer, the SSE encoder, the in-process client and the
socket client all speak these; nothing else defines a wire format.

```haskell
data Command
    = CreateSession   { agent :: Text, message :: Maybe NewMessage, run :: Maybe RunMode, params :: Map ParamName Value, parent :: Maybe SessionId }
    | PostMessage     { session :: SessionId, message :: NewMessage, run :: Maybe RunMode, params :: Map ParamName Value }
    | Resume          { session :: SessionId, mode :: RunMode, params :: Map ParamName Value }
    | CompleteCall    { token :: ContinuationToken, result :: UserToolResponse, resume :: Bool, params :: Map ParamName Value }
    | CancelRun       SessionId
    | CancelAttached  SessionId
    | Pause           SessionId
    | SendMail        { session :: SessionId, body :: MailBody, priority :: Priority }   -- StopRun, CancelCalls, Resume, AgentMessage, …
    | ForkSession     { session :: SessionId, atTurn :: Maybe Int, agent :: Maybe Text }   -- newest-first index; turns have no id
    | ListSessions    SessionQuery
    | GetSession      SessionId
    | ListAgents
    | GetAgent        Text
    | DeleteSession   { session :: SessionId, mode :: DeleteMode }
    | Subscribe       { scope :: SubscribeScope, after :: Maybe EventSeq }

data Event = Event
    { evSeq     :: EventSeq          -- total order per server
    , evSession :: Maybe SessionId   -- Nothing for server-wide events
    , evBody    :: EventBody
    }

data EventBody
    = RunStarted RunMode | RunStopped SessionStatus | SessionFailed Text
    | SessionUpdated SessionMeta (Maybe Turn)
    | CallsDeferred [DeferredCallView]
    | TextDelta Text
    | ToolCallStarted … | ToolCallProgressed ToolCallActivity | ToolCallCompleted …
    | SubcallStarted { parent :: SessionId, child :: SessionId, slug :: Text, depth :: Int }
    | SubcallCompleted … | SubcallFailed …
    | SessionCreated SessionMeta | SessionDeleted SessionId
    | MailAccepted Envelope | MailRejected SendError
    | AgentsChanged
```

Notes:

* `NewMessage` gains `Maybe` semantics through `CreateSession.message`:
  `Nothing` creates an idle session with no turn (G2). The runner stores it
  with status `ready` and no run.
* `ForkSession` is new runner work (G6): copy the session up to `atTurn`,
  set `forkedFromSessionId`, optionally rebind `smAgent`. This also covers
  "continue with another agent": a fork at the head with a new agent.
* `SendMail` generalizes `sendControlMail`. It is how the TUI posts
  `StopRun` on quit instead of `killThread`, and how a client sends
  `AgentMessage` mail on behalf of another session.
* `OSEvent` is retired as a public type. `ToolCallActivity` and the
  `Subcall*` payloads move into `EventBody`; the unused constructors are
  dropped. `Session.Async.Engine` and `OneShotTool` emit through a runner
  hook (`ctxEmit :: Maybe (EventBody -> IO ())`) instead of `ctxEventQueue`,
  and the runner sets it (G3). `SubcallProgress` no longer carries a whole
  `Session`: a child that is a real session (see §4) emits its own
  `SessionUpdated`.

### 2. Sequence numbers and replay (G5)

* The runner stamps every event with a monotonically increasing `EventSeq`
  and keeps a bounded in-memory ring (say the last 4096 events).
* `Subscribe { after = Just n }` replays from the ring when `n` is inside
  it. When it is not, the server answers with a `snapshot` per session in
  scope (as today) followed by live events, and the client re-fetches
  `GetSession` for anything it cares about. This is the SSE `Last-Event-ID`
  contract.
* `SubscribeScope = OneSession SessionId | Owner (Maybe Text) | All`. `All`
  needs an admin owner when auth is on.
* Persisting the event log is deferred (see Decisions). The stored session
  plus `session_mail` already lets a client rebuild state; the ring only
  needs to cover reconnection hiccups.

### 3. `RunnerClient`: one interface, two implementations

```haskell
data RunnerClient = RunnerClient
    { rcCommand   :: Command -> IO (Either RunnerError Reply)
    , rcSubscribe :: SubscribeScope -> Maybe EventSeq -> IO (IO Event, IO ())   -- next, close
    }

inProcessClient :: SessionRunner -> RunnerClient
httpClient      :: BaseUrl -> Maybe BearerToken -> IO RunnerClient   -- HTTP + SSE
socketClient    :: FilePath -> IO RunnerClient                       -- Unix socket, same framing
```

The TUI depends only on `RunnerClient`. `agents-exe tui` builds
`inProcessClient` over a runner it starts itself; `agents-exe tui --attach`
builds `httpClient` or `socketClient`. No TUI code knows which.

### 4. The TUI as a client (G1, G6, G8, G9, G10)

* `Conversation` in the TUI becomes: `SessionId`, the latest `Session` and
  `SessionMeta` (from `GetSession` and `SessionUpdated`), derived views
  (tool-call views, signals, usage), unread markers. No `ThreadId`, no
  `BChan`, no callbacks, no `TuiAgent` with TVars.
* `Core` loses `coreWorld`, `coreOSEventQueue`, `coreMailRouter`,
  `coreBufferedMessages`, `corePausedConversations`. The agent list is
  `ListAgents` plus `AgentsChanged`.
* Send = `PostMessage` when the session is idle. While a run is active, send
  appends to a TUI-local draft (§5) rather than posting a discrete queued
  message; the draft is posted as one message when the run stops. "Send
  now" posts the draft at once (it becomes mail, folded at the next R1);
  an interrupt (`PostMessage` with `nmInterrupt`) bypasses the draft as
  today. Unread mail from other senders is shown read-only next to it (see
  Decisions, D3).
* Pause = `Pause`; unpause = `SendMail Control Resume`. `Paused` becomes a
  `SessionStatus` detail the server reports, not a TUI-local flag.
* Interrupt and hard cancel are already commands (`PostMessage` with
  `nmInterrupt`, `CancelAttached`). Quit = `SendMail StopRun` for every
  session the TUI started in embedded mode, and nothing when attached.
* Fork = `ForkSession`. Continue with another agent = fork at the head.
* Subcall visibility: a `prompt_agent_*` call spawns a real child session via
  `spawnSession` (the server already links `smParent`), so the child shows
  up through `SessionCreated` + its own events. The TUI's spawn-roster
  question (G9) goes away: the server resolves helpers, and `CreateSession`
  with a `parent` covers the "spawn any root as a child" case the TUI has
  today.
* Deferred calls: the TUI gains a pending-calls view and `CompleteCall`,
  which the server already supports.
* Sessions history tab = `ListSessions`, refreshed by `SessionCreated`,
  `SessionUpdated`, `SessionDeleted`.
* `ConversationId` in the TUI = `sessionIdToConversationId sid`, as on the
  server (G8).

The `OS.*` ECS layer is left in place for tool-call entities inside the
engine; the TUI stops constructing a `World`.

### 5. The draft buffer (G6, queued messages): a UI concern

Three messages typed in a row while the LLM is busy are almost always one
message being elaborated. So the TUI keeps, per conversation, **one editable
draft with append semantics** instead of a queue of discrete messages, and
posts it whole when it is the user's turn. The kernel never sees a draft: it
only ever receives real messages (`PostMessage`) and interrupts.

* **Where it lives:** in the TUI's `Conversation` state only. It is not
  persisted and does not survive a detach; a second client attached to the
  same session does not see it. That is fine: it is unsent text, like the
  contents of the composer.
* **Editing:** the queue tab becomes a draft editor. Collapsed, it shows the
  draft's first line and size; expanded, it is a text editor over the whole
  draft. Sending while busy appends a paragraph; the user can rewrite or
  clear it at will.
* **When it ships:** on `RunStopped` with a status that accepts input, the
  TUI posts the draft as one `PostMessage` and clears it. "Send now" from
  the editor posts it immediately; the server turns it into `UserMessage`
  mail and R1 folds it into the next user turn, which fires after each round
  of tool results too, so an elaboration typed during a tool call reaches
  the model at the next completion. A paused session keeps the draft until
  it is resumed.
* **What the LLM sees:** unchanged from the mailbox spec. R1 emits one tool
  message per call, then a user message with the folded mail. For a
  provider that rejects a user message directly after tool results, the
  agent opts into `mailInToolResult: true`: R1 then appends the folded
  mail as a trailing text block of the last tool result of that round, with
  the same `[mail …]` header. This is a property of mail folding, not of
  drafts, and applies to attached (sync) calls; detached and deferred
  results already arrive as mail of their own.

### 6. `agents-exe serve` and config parity (G7)

* Move from `app/Main.hs` into a library module (`System.Agents.CLI.Config`
  or similar): `locateAgentsExeConfig`, config parsing, `agentsFiles` plus
  `agentsDirectories` expansion, the default-directory fallback,
  `resolveAgentFiles` for `--agent`, `--params-file`, and the sessions-store
  choice.
* `HostConfig` gains a constructor from that config, so `withHost` can be
  fed either explicit `--agent-file`s or the resolved config.
* `agents-exe serve` = the current `agents-server` main, with config
  loading in front. `agents-server` keeps working as the same code with
  flags only. The SQLite session backend becomes the default for the TUI
  too; the file store stays readable through a composite backend for old
  `conv.*.json` history.
* `--socket PATH` on `serve` for a local, token-free attach; `--bind`/`--port`
  as today for HTTP.

### 7. HTTP surface additions

| Route | Command |
|---|---|
| `POST /v1/sessions` with no `prompt` | `CreateSession { message = Nothing }` |
| `POST /v1/sessions/:id/fork` | `ForkSession` |
| `POST /v1/sessions/:id/pause` | `Pause` |
| `POST /v1/sessions/:id/mail` | `SendMail` |
| `GET /v1/sessions/:id/mail?unread=` | inbox listing |
| `GET /v1/events?scope=&after=` | `Subscribe` across sessions, with `Last-Event-ID` |
| `GET /v1/agents/:slug` | extended with model, system prompt, tool activation |

`GET /v1/sessions/:id/events` stays, gains `Last-Event-ID`.

## Phases

Phases 0 and 1 need nothing and make the service usable from a local web
app on their own. Phase 2 needs nothing either. Phase 3 needs Phase 2.
Phase 4 needs 1, 2 and 3. Phase 5 needs only Phase 2.

### Phase 0: service readiness (G12, G13) — done

`--cors-origin ORIGIN` on `agents-server`, repeatable, `*` allowed only
without auth: emits `Access-Control-Allow-Origin` (echoing the matched
origin), `-Allow-Methods`, `-Allow-Headers` (`Authorization`,
`Content-Type`), `-Expose-Headers` (`Location`), `-Max-Age`, and answers
preflight `OPTIONS` with 204 before auth and before `checkOrigin`. An
origin listed here passes `checkOrigin` even with auth off. The SSE route
keeps working cross-origin through `?access_token=` since `EventSource`
cannot set headers. Docs: a "Running as a service" section in
`documentation/agents-server.md` with a systemd unit, and the CORS flag in the flag
table and the authentication section. Also add the undocumented
`cancel-attached`, `pause`, `interrupt`, `tool.started` and
`tool.completed` to the same doc. Tests: preflight, allowed and refused
origins, SSE with `access_token` from an allowed origin.

### Phase 1: `agents-exe serve` and config parity (G7) — done

Landed as `System.Agents.CLI.ConfigLoader` (not `CLI.Config`: the TUI
library already owns that module name), `hostConfigFromResolved`,
`agents-exe serve` reusing the global `--agent-file`/`--agent`/`--params-file`
flags, and `--socket`. The Unix listener stops accepting on SIGTERM and
unlinks its file but does not drain in-flight requests through
`--shutdown-grace`; only the TCP listener does.

Original scope:

Config loading into the library (§6); `serve` subcommand on `agents-exe`;
`--socket`. The `agents-server` executable stays as the flags-only entry
point over the same code. Docs: `documentation/agents-server.md` gains the config
section; `documentation/cli-commands.md` gains `serve`.

### Phase 2: the protocol, in the library

Split in three sequential steps: 2a the `Protocol` types with JSON, event
sequence numbers, the ring and replay, the cross-session feed, and the
server re-implemented on them; 2b the missing runner operations
(`listSessions`, `sendMail`, create with no message, `forkSession`) and
their routes; 2c `ctxEmit` replacing `ctxEventQueue` so subcall and
tool-activity events reach the runner stream, and `mailInToolResult`.

2a landed as `System.Agents.Protocol` (`Event { seq, session_id, owner,
body }`, `EventBody` with the old `SessionEvent` kinds plus
`session.created` / `session.deleted`, JSON for `NewMessage`, `RunMode`,
`RunnerError`, `DeleteMode`, `DeletionPlan`, `SubscribeScope`), a 4096-event
ring in the runner, `subscribe`/`subscribeSTM` taking a scope and an
`after`, `Last-Event-ID` and `?after=` on the session stream, and
`GET /v1/events?scope=owner|all`. Deviations from the sketch: `evSession`
and `evOwner` are always set, also on the server-wide events, because the
JSON merges them with the embedded `SessionMeta` fields; `RunnerError`'s
decoder is lossy (only the code survives, by design of the existing
`{error, message}` shape); the snapshot fallback on an unavailable replay
keeps the pre-existing tiny gap between snapshot and re-subscribe.

2b landed: `createSessionAs*` take `Maybe NewMessage` (promptless
`POST /v1/sessions` creates an idle `ready` session), `listSessions` on
the runner, `sendMail`/`listMail` with `POST`/`GET /v1/sessions/:id/mail`,
and `forkSession` with `POST /v1/sessions/:id/fork`. Deviation: `Turn` has
no id of its own, so forks are addressed by a 0-based newest-first turn
index (`at_turn`), exactly as the TUI's `handleForkAtTurn` does, and the
error is `UnknownTurn SessionId Int` (`unknown_turn`). The `Command`
sketch's `atTurn :: TurnId` is wrong until turns get ids.

2c landed: `ctxEmit :: Maybe (OSEmission -> IO ())` next to `ctxEventQueue`
(an intermediate `OSEmission` type avoids a `Session.Base` → `Protocol`
import cycle; the runner converts it to `EventBody`), the four kinds
`subcall.started` / `subcall.completed` / `subcall.failed` /
`tool.progressed`, the nine dead `OSEvent` constructors deleted, and
`mailInToolResult`. Deviations: subcall events are stamped with the
*parent* as `session_id` (so a subscriber of the parent sees them) and
carry the child as `child_session_id`. **Open gap:** `mailInToolResult` is
implemented in the synchronous step path only; the server always runs
agents asynchronously, so it has no effect on server sessions until the
async path (`runStepMAsync`) folds mail the same way. Tracked for Phase 5.

`System.Agents.Protocol` with `Command`, `Event`, `EventBody`, JSON
instances, and codecs for `NewMessage`, `RunMode`, `RunnerError`,
`UserToolResponse` views. `AgentsServer.Api` re-implemented on top of it (no
behaviour change; `eventFrame` goes away). Runner gains `EventSeq` stamping
and the ring, `listSessions`, `sendMail`, `createSession` with no message,
`forkSession`. `ctxEmit` replaces `ctxEventQueue`; `Engine` and
`OneShotTool` emit through it. `mailInToolResult` folding at R1 (§5). Tests: JSON round trips, replay from
the ring, fork, empty create, `mailInToolResult` folding.

### Phase 3: the TUI on the in-process runner

Split in three sequential steps: 3a `RunnerClient` and `inProcessClient`
in the library plus a serializable agent descriptor (`ListAgents` /
`GetAgent` with model, prompt and tool activation, which also fixes the
`GET /v1/agents` gap); 3b the TUI's startup, Agents tab, History tab and
conversation layer rewritten on `RunnerClient` (the large step); 3c
subcall visibility and tool-call activity from runner events, the
pending-calls view with `CompleteCall`, and retiring `OSEvent`,
`ctxEventQueue` and the TUI's event bridge.

3a landed (a78d280..bb4e003): `AgentDescriptor` (model, prompt, tools with
`Activation`, parameters, helpers) served by `GET /v1/agents`; `Command`
and `Reply`; `System.Agents.Host.Client` with `RunnerClient`,
`Subscription`, `inProcessClient owner runner` and one typed helper per
operation. The owner is the client's identity, not a `Command` field, so a
command cannot spoof another owner.

3b is itself split in four, because Brick's single `TuiState`/`AppEvent`
means old and new conversation models cannot coexist: 3b-i the new
`Conversation`/`Core`/`AppEvent` types and the startup on `Host` +
`SessionRunner` + `inProcessClient`, with handlers stubbed so the shape
compiles; 3b-ii the event bridge (`subscribeAll` → `AppEvent`) and the
command dispatch; 3b-iii the draft buffer and the History tab; 3b-iv
render and keymap cleanup, unit tests, smoke test. Known regression to
accept until Phase 5: `AppEvent_SubcallProgress` carried the child's whole
`Session`; the runner stream has no equivalent, so a subcall shows start,
completion and failure only.

3b landed (f73c018..d2887c6). The TUI is a `RunnerClient` over an
in-process runner: promptless `createSession` then `postMessage`; one
`subscribeAll` bridge thread feeding Brick; pause/resume, interrupt,
hard cancel, fork, restore (fork when the selected agent differs), F5 and
quit (`StopRun` mail, no `killThread`) all go through the client; the
draft buffer replaces the queue (Ctrl+A edit, Ctrl+G send now, Ctrl+D
clear; "edit" pulls the draft back into the composer); History refreshes
from events with a per-session cache and Loading/error states;
`HostConfig.hcLegacySessionDirs` composites old `conv.*.json` directories
as a read fallback for both the TUI and `serve`. Runner fix along the
way: `postMessage` accepts the first message of an empty `ready` session.
An end-to-end pty script against a fake OpenAI endpoint lives in
`checks/phase3b-iv-e2e/`.

3c landed (c5f017f, 5eeb4b0): `OSEvent` and `ctxEventQueue` are gone;
`OSEmission` + `ctxEmit` is the one emission mechanism, with
`queueEmitter` for local consumers; new event kind `hook.failed`; the
per-step subcall progress snapshot is gone with no successor until Phase
5. The TUI has a Pending panel and `answer-pending` (Ctrl+Y) that
completes the oldest deferred call through `completeCall` with
auto-resume. `UserToolResponse` has no error form, so there is no
`fail-pending`. Open: no selection among several pending calls; no UI
for `hook.failed`.

`inProcessClient`; the TUI conversation layer rewritten on `RunnerClient`
(§4). `agents-exe tui` starts a `Host` + `SessionRunner` over the SQLite
backend chosen from config (Phase 1), with the file store composited in for history.
Feature parity checklist: new, continue, fork, send, draft editor (§5), pause,
interrupt, hard cancel, quit, subcall visibility, tool-call activity, F5
tools refresh, export, attachments, plus the new pending-calls view. Remove
`coreWorld`, `coreOSEventQueue`, `coreMailRouter`, `coreBufferedMessages`,
`corePausedConversations`, `Loop` usage, `OneShot.nodeToAgent` usage in the
TUI.

### Phase 4: attach — done

Landed (20522f2..4e03ec9): `System.Agents.Host.Client.Http.httpClient`
over `http(s)://` and `unix://` URLs with bearer token, SSE parsing,
transparent reconnect with `Last-Event-ID` (backoff 100 ms to 5 s, 45 s
stall timeout, duplicates dropped), `AllSessions` falling back to the
owner scope on 403; `agents-exe tui --attach URL|PATH` with `--token` /
`--token-file`; `POST /v1/sessions` takes `parent`; `GET /v1/sessions/:id`
takes `wait`/`timeout`; streams send an `Agents-Replay` header and flush
headers at once (a warp header-buffering bug had hung the attached TUI);
the chat page follows `GET /v1/events`. Open: with `--attach`, `app/Main.hs`
still resolves local agent files, so `--agent SLUG` can fail when the slug
is unknown locally.

Original scope:

`httpClient` (HTTP + SSE with `Last-Event-ID`) and `socketClient`;
`agents-exe tui --attach URL|PATH`. The chat page moves to `GET /v1/events`
for its session list. Tests: a TUI-less client test that runs the server,
attaches, creates, interrupts, forks and replays after a dropped stream.

### Phase 5: sub-agents as sessions — done, with two documented gaps

Landed (d7850d0..771c988): `ctxRunSubagent` hook, installed by
`Runner.newAgent`, runs `prompt_agent_*` as a real child session
(`createSessionForNode`, `parent` = the calling session, helper resolved
by slug anywhere under the calling root) and awaits it; cancelling the
parent's call cancels the child; `subcall.started` carries the real
child id, so live progress is the child's own `session.updated`.
`mailInToolResult` now folds on the async path too. `tui --attach` no
longer resolves local agent files or creates the default config.
Gaps: (1) a `prompt_agent_*` call with any narrowing (`bindings`,
`with`, `as`, own or inherited) or whose helper is not declared under
the root still runs in-tool, because `ToolExecutionContext` cannot name
a narrowed `OSAgentNode` without a module cycle; (2) a `PartialUserTurn`
shown mid-round does not yet carry that round's mail in its placeholder
when `mailInToolResult` is on. The fake endpoint in `checks/` is a fixed
responder, so no end-to-end test exercises a sub-agent through the TUI.

Original scope:

`prompt_agent_*` spawns through `spawnSession` and waits on the child's
`RunStopped` (or its mail), so children are cancellable and observable on
their own and `SubcallProgress` with an embedded `Session` disappears. This
touches `OneShotTool` and the async engine; it is the riskiest phase and is
why it is last.

## Remaining after Phase 5

* G10 residue: done. Narrowed (`bindings`, `with`, `as`, own or inherited) and
  undeclared-helper `prompt_agent_*` calls now run as runner sessions: the hook
  gets a `SubagentNarrowing` and the tool's own node (as a `Dynamic`, which
  avoids the module cycle). A narrowed child is pinned live (never evicted);
  the narrowing is not persisted, so after a restart a resumed narrowed child
  runs un-narrowed (bindings may be secret).
* `mailInToolResult`: partial-turn placeholders lack the round's mail.
* G11: secret params, watches and the run handle are volatile (D7 stands);
  since 2026-09-25 `recoverOnStartup` fails the running calls of a session
  whose required params were lost, with a `params_required` detail.
* TUI: no selection among several pending calls; no view for `hook.failed`.
* Service packaging: docs only, no unit file shipped, no `bundling/` entry.
* `checks/` fake endpoint cannot script multi-turn answers, so sub-agent
  and tool-call flows have no pty end-to-end test.
* D8's out-of-scope items: multi-server Postgres, mid-`RunAsync` durability,
  per-owner API keys.

## Decisions (proposed)

D1. **The runner is the OS.** The ECS `World` is an engine-internal detail
for tool-call entities. `OS.Persistence`, `OS.Interfaces` remnants and the
unused `OSEvent` constructors are deleted rather than wired.

D2. **One wire format, defined once, in the library.** No JSON in
`examples/agents-server` beyond what the servant routes need to reference
the `Protocol` types.

D3. **Drafts are a UI concern; the kernel only knows messages and
interrupts.** Mail stays append-only and immutable. The TUI keeps one
editable draft per conversation with append semantics (§5), posted as one
`PostMessage` when it is the user's turn. Nothing about drafts crosses the
wire or is stored. Discrete queued messages, and deleting or reordering
them, are dropped: successive messages while the model is busy are treated
as one message being elaborated. Mail from other sessions and tool results
are shown read-only.

D4. **`SessionId` is the conversation.** `ConversationId` is always derived
from it, in every front-end.

D5. **The event ring is in memory; the event log is not persisted.**
Replay covers reconnects; a client that is behind by more than the ring
refetches sessions. Revisit only if a use case needs an audit trail (then
persist `Event` rows in the store, keyed by `EventSeq`).

D6. **Embedded mode is the same code as attached mode.** The TUI never gets
a privileged path to runtime internals again; anything it needs is a
`Command` or an `Event`, which keeps the web page and API clients at parity.

D7. **Secrets are resupplied by the client.** No durable secret storage in
this spec; `lsParams` stays volatile and the TUI resupplies from its
`--params-file` on `CreateSession`/`PostMessage`, as the web page does today.

D8. **Out of scope:** several servers on one Postgres, mid-tool-call
durability for `RunAsync` calls, per-owner API keys. They are documented as
unsupported in `documentation/agents-server.md` and are not needed for attach.

## Related docs

* `todos/web-server-embedding.md`, `.progress.md`: the Host/Runner and the
  HTTP server this builds on.
* `todos/session-mailbox.md`: mail, interrupts, control messages, watches.
* `documentation/agents-server.md`: the current API; sections to update are marked in
  Phases 0 and 1.
* `documentation/tui.md`: TUI features that form the parity checklist of Phase 2.
* `documentation/OS-API.md`: the ECS layer; its `OS.Interfaces` section is stale and
  goes with D1.
