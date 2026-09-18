# Spec: embedding agents-exe in a web server

Status: proposed (2026-09-18)

## Goal

Run agents from a long-lived HTTP server, with no TUI and no CLI, where:

* every session lives in a database, not in `conv.<uuid>.json` files;
* a client can start a session, send follow-up messages, watch progress live,
  list and complete deferred tool calls, resume, and cancel;
* the server can restart at any time without losing or corrupting sessions.

The deliverable is:

1. a small library API in `agents-lib` for hosting agents, with no HTTP
   dependencies;
2. a reference HTTP server, `agents-server`, built on that API.

## Non-goals (for this spec)

* Multi-tenancy: per-user isolation, per-user API keys, and sandboxing of bash
  or MCP tools. The data model reserves an `owner` column so this can come
  later without a migration (see [Later work](#later-work)).
* A Postgres backend. The interfaces below are designed so it can be added as
  a separate backend.
* Streaming LLM tokens. Clients get updates at step granularity.
* Agent definitions stored in the database. Agents still come from JSON files
  loaded at startup.
* Splitting the library to drop the `brick`/`vty` dependencies.

---

## Current state

### What we can reuse as-is

| Piece | Where | Notes |
|---|---|---|
| Loading the agent tree once, sharing it across concurrent runs | `AgentTree.withAgentTree`; used by `MCP/Server.hs:127` and `:222` | The MCP server already runs one `async` per request over a shared tree. |
| Stepping a session | `Session.Step.runStepM`, `Session.Loop.runUntilBlocked` (`Loop.hs:105`), `Session.Wake.resumeSession` | `runUntilBlocked` returns the session when it waits only on deferred calls. |
| Injecting external results | `Session.Wake.wakeSession` | Pure load, modify, return. |
| Session storage interface | `SessionStore.SessionBackend` (`SessionStore.hs:97`) | store / load / list / delete; file, SQLite, and composite versions. |
| Continuation index | `Session.Async.ContinuationStore` (`Async.hs:248`), SQLite table `tool_continuations` | Written by the scheduler (`Step.hs:490`, `:728`). |
| Declarative policy per agent | `toolCallPolicyConfig`, `executionMode`, etc.; applied by `Session.AgentConfig.applyAgentDurableConfig` | |
| Progress hook | `Combinators.StoreSessionProgress.agentWithSessionProgress` | Fires `SessionUpdated` before every step. |
| Restart handling for background calls | `Step.pollRunningCall` (`Step.hs:505`) | A `Running` call whose process is gone becomes `Failed` ("orphaned"). |
| Background execution | `Session.Base.withAsyncEngine`; on-demand World at `Step.hs:917` | Needs an OS `World` plus an `AsyncEngine`. |

### Gaps

**G1. A backend set on an agent after construction is ignored for progress
storage.** `OneShot.nodeToAgent` wraps the agent with `agentStoreSession`
while `ctxSessionBackend = Nothing`. `agentStoreSession`
(`StoreSessionProgress.hs:130`) checks `ctxSessionBackend` on the agent it
receives when it wraps, not at store time. So
`withSessionBackend backend =<< nodeToAgent …` still writes every step to the
file store.

**G2. Agent construction is copied three times** with small differences:
`OneShot.nodeToAgentWithThinking` (`OneShot.hs:293`),
`AgentTree.OneShotTool.nodeToAgent` (`OneShotTool.hs:511`, used for
sub-agents), and the MCP server (`MCP/Server.hs:~300`). The one-shot copy also
prints thinking to stdout/stderr. Progressive-disclosure tool filtering
(`agentEvaluateActiveTools`) is applied only on the one-shot path.

**G3. Sub-agent sessions always go to the file store.** `OneShotTool.nodeToAgent`
takes the file `SessionStore`, generates a fresh `ConversationId`, and records
no link to the parent session.

**G4. Session-reading tools and search read only the file store** (tools
fixed in Phase 3; the search index is CLI-only and stays on files). The
`SystemToolbox` session tools (`Tools/SystemToolbox/Session.hs`, via
`SessionIntrospectionConfig.introspectionStore`) and the search index
(`Session/Search/Index.hs`, via `indexSessionStore`) call
`SessionStore.listSessions` / `readSession` directly. `Props.sessionStore`
passes this down from `AgentTree`.

**G5. Sessions have two keys.** The file store is keyed by `ConversationId`,
durable commands by `SessionId`, and the two are generated separately
(`SessionDurable.hs` `handleStart`). `runOneShotWithConfig` stores a paused
session under both.

**G6. No concurrency control.** Every mutation is load, modify, store. Two
concurrent requests on one session (two `complete` calls, or `complete` during
a run) silently lose an update.

**G7. Continuation table and sessions drift apart.** `wakeSession` never calls
`csComplete`, so `tool_continuations` rows stay pending forever. Finding a
session from a token scans every session (`findSessionForToken` in
`SessionDurable.hs`) even though `tool_continuations.session_id` already maps
it.

**G8. Background calls only live as long as one loop call.** The loops
create an engine on demand and shut it down on exit (`withEngineShutdown`,
`Loop.hs:88`). A server that pauses and resumes over several HTTP requests
needs a World and engine per live session that outlive a single request.

**G9. No metadata beyond the JSON blob.** The `sessions` table has only
`session_id`, `created_at`, `updated_at`, `json`. Listing sessions by agent or
status, or finding ones interrupted by a restart, means decoding every blob.

**G10. The prompt-to-first-turn code lives in the CLI.** `handleStart` builds
the initial `UserTurn` inside `CLI/SessionDurable.hs`, so the library has no
reusable version.

---

## Design

### Overview

```
                 +-------------------------------------------+
  HTTP / SSE --> | agents-server (wai + warp)                |   examples/agents-server/
                 +---------------------+---------------------+
                                       |
                 +---------------------v---------------------+
                 | System.Agents.Host.Runner                 |   per-session lock, live runs,
                 |   SessionRunner                           |   events, cancellation, recovery
                 +---------+-------------------+-------------+
                           |                   |
        +------------------v-----+     +-------v----------------------+
        | System.Agents.Host     |     | SessionBackend (extended)    |
        |   Host, on top of      |     | ContinuationStore            |
        |   AgentFactory         |     | SQLite, one database file    |
        +------------------------+     +------------------------------+
```

The library adds three modules:

* `System.Agents.AgentFactory`: the single agent factory (done).
* `System.Agents.Host`: loaded agents, database, and stores.
* `System.Agents.Host.Runner`: session lifecycle on top of a `Host`.

None of them imports anything HTTP-related. The HTTP layer is a new executable.

### 1. Keys: one id per session

**Rule:** a session's `SessionId` is its only key. Wherever a
`ConversationId` is needed for the same session, it is
`sessionIdToConversationId sid`. The Host API never generates a separate
`ConversationId` for a session.

Sub-agent sessions get their own `SessionId` and record their parent (§3).

### 2. The agent factory and the Host

#### 2.1 `System.Agents.AgentFactory` (implemented, Phases 1–2)

```haskell
data AgentDeps = AgentDeps
    { adApiKeys           :: LoadedApiKeys
    , adSessionSink       :: SessionSink
    , adContinuationStore :: Maybe ContinuationStore
    , adToolCache         :: Maybe ToolCache
    , adCompletion        :: Maybe (OSAgentNode -> Completion)
    -- ^ Replaces the LLM call (tests pass a mock).
    }
defaultAgentDeps :: LoadedApiKeys -> AgentDeps               -- SinkNone
fileAgentDeps    :: SessionStore -> LoadedApiKeys -> AgentDeps  -- SinkFiles

data AgentRole
    = RootAgent
    | SubAgent { subParentConversation :: ConversationId, subCallStack :: [CallStackEntry] }

-- | The only way to turn an OSAgentNode into a runnable Agent.
buildAgent :: Tracer IO Trace -> AgentDeps -> AgentRole -> ConversationId -> OSAgentNode
           -> IO (Agent (LlmTurnContent, Session))
```

`buildAgent` takes a `ConversationId`, not a `SessionId`: the CLI and TUI key
their file store by conversation ID, and changing that is not part of this
work. Callers that follow the §1 rule pass `sessionIdToConversationId sid`.

What `buildAgent` does, in this order:

1. Builds the base `Agent` record: prompt, tools, `toolCall`, `toolPortal`,
   and `complete` (from `adCompletion`, or else the agent's OpenAI config).
2. Applies `applyAgentDurableConfig` for the node's JSON config, and installs
   `adContinuationStore`, `adToolCache`, and (for `SinkBackend`) the backend.
3. Applies `agentEvaluateActiveTools` (progressive disclosure) to every agent.
4. Wraps the agent with `agentPersistSession adSessionSink convId` **last**
   (fixes G1). `SessionSink` lives in `Combinators.StoreSessionProgress`:

   ```haskell
   data SessionSink = SinkBackend SessionBackend | SinkFiles SessionStore | SinkNone
   agentPersistSession :: SessionSink -> ConversationId -> Agent r -> Agent r
   ```

   `agentStoreSession` keeps its old behaviour for existing callers.
5. Never writes to stdout or stderr. `OneShot.nodeToAgentWithThinking` adds
   the thinking printer, the media injection, and the extra `--session-file`
   copy as decorators on the result.

All front-ends use it: one-shot `run` and the TUI (through
`OneShot.nodeToAgent`), sub-agent tools, the MCP server, and the durable
`session` commands.

**Sub-agents:** `turnAgentRuntimeIntoIOTool` takes an `AgentDeps` instead of a
`SessionStore` and API keys. It builds the sub-agent with
`SubAgent parentConvId stack` and the same conversation ID it uses for the
call stack and the OS World entity. That ID now also names the stored
sub-session; it used to be an unrelated random ID. The parent link is kept at
runtime (`ctxParentConversation`), and backends store it as
`parent_session_id` (Phase 3).

#### 2.2 `System.Agents.Host` (Phase 5)

```haskell
data Host = Host
    { hostAgents  :: Map AgentSlug OSAgentNode
    -- ^ Root agents loaded at startup, addressable by slug.
    , hostDeps    :: AgentDeps
    -- ^ SinkNone: the runner stores sessions itself (§4.2).
    , hostBackend :: SessionBackend
    , hostTracer  :: Tracer IO HostTrace
    }

data HostConfig = HostConfig
    { hcAgentFiles   :: [FilePath]
    , hcApiKeysFile  :: FilePath
    , hcDatabasePath :: FilePath
    , hcCompletion   :: Maybe (OSAgentNode -> Completion)
    }

-- | Load every agent tree, open the database, run migrations, and build
-- stores. Agent trees (and their MCP server processes) live as long as the
-- continuation.
withHost :: HostConfig -> Tracer IO HostTrace -> (Host -> IO a) -> IO a
```

### 3. Storage

#### 3.1 Extended `SessionBackend` (implemented, Phase 3)

`SessionStatus`, `sessionStatusOf`, `isBlockedOnDeferredCalls`, and
`hasBackgroundCalls` live in `Session.Types` (pure, re-exported by
`Session.Base`); the metadata types live in `SessionStore`.

```haskell
data SessionStatus = StatusIdle | StatusReady | StatusRunning | StatusWaitingExternal | StatusFailed

data SessionLabels = SessionLabels   -- Nothing keeps the stored value
    { slAgent :: Maybe Text, slParent :: Maybe SessionId, slOwner :: Maybe Text }

data SessionMeta = SessionMeta
    { smSessionId    :: SessionId
    , smAgent        :: Maybe Text
    , smParent       :: Maybe SessionId
    , smOwner        :: Maybe Text          -- reserved, unused for now
    , smStatus       :: SessionStatus
    , smStatusDetail :: Maybe Text          -- why it failed
    , smVersion      :: Int                 -- incremented on every write; 0 = never stored
    , smCreatedAt, smUpdatedAt :: UTCTime
    }

data SessionQuery = SessionQuery
    { sqAgent :: Maybe Text, sqStatuses :: Maybe [SessionStatus], sqParent :: Maybe SessionId
    , sqUpdatedBefore :: Maybe UTCTime, sqLimit :: Maybe Int }

data VersionConflict = VersionConflict { vcSessionId :: SessionId, vcExpected, vcActual :: Int }

data SessionBackend = SessionBackend
    { sbStore  :: SessionId -> Session -> IO ()     -- unconditional
    , sbLoad   :: SessionId -> IO (Maybe Session)
    , sbList   :: IO [(SessionId, UTCTime)]
    , sbDelete :: SessionId -> IO ()
    , sbStoreLabelled   :: SessionLabels -> SessionId -> Session -> IO ()
    , sbLoadMeta        :: SessionId -> IO (Maybe (Session, SessionMeta))
    , sbCompareAndStore :: SessionMeta -> Session -> IO (Either VersionConflict SessionMeta)
    , sbQuery           :: SessionQuery -> IO [SessionMeta]
    }
```

Unconditional stores (`sbStore`, `sbStoreLabelled`) increment the version,
keep stored labels where the new ones are `Nothing`, and set the status from
`sessionStatusOf`, unless the stored status is `StatusRunning`.
`sbCompareAndStore` writes the given metadata exactly, only if the stored
version equals `smVersion` (a missing session counts as 0).

Backend implementations:

* **SQLite**: compare-and-store is one conditional statement
  (`UPDATE … WHERE version = ? RETURNING …`, or an upsert with
  `DO UPDATE … WHERE sessions.version = 0` for version 0), so it is atomic
  without a lock. Rows from before the migration have version 0.
* **File**: metadata in a sidecar `meta.<uuid>.json` (not `conv.*`, so session
  listings ignore it). A session file without a sidecar reads as version 0.
  Compare-and-store is read-compare-write, not atomic across processes.
* **Composite**: writes and queries go to the primary backend; loads fall
  back in order.

`AgentFactory` stores through `sbStoreLabelled` with the agent's slug and,
for sub-agents, the parent session. A sub-agent's session ID is its
conversation ID, so its own sub-agents name it correctly as their parent.

#### 3.2 SQLite schema and migrations (implemented, Phase 3)

`SessionStore.runMigrations conn component migrations` applies each
not-yet-applied `Migration` in a transaction and records it in
`schema_migrations(component, version, applied_at)`, so the continuation
store and tool cache can have their own migration lists later.
`initializeSessionSchema` (called by `mkSqliteSessionStore`) runs the
`sessions` migrations:

```sql
-- migration 1: the original table and index (CREATE IF NOT EXISTS)
-- migration 2
ALTER TABLE sessions ADD COLUMN agent_slug TEXT;
ALTER TABLE sessions ADD COLUMN parent_session_id TEXT;
ALTER TABLE sessions ADD COLUMN owner TEXT;
ALTER TABLE sessions ADD COLUMN status TEXT NOT NULL DEFAULT 'ready';
ALTER TABLE sessions ADD COLUMN status_detail TEXT;
ALTER TABLE sessions ADD COLUMN version INTEGER NOT NULL DEFAULT 0;
CREATE INDEX IF NOT EXISTS idx_sessions_status ON sessions(status, updated_at);
CREATE INDEX IF NOT EXISTS idx_sessions_parent ON sessions(parent_session_id);
-- then each existing row's status is derived from its JSON
```

The `tool_continuations(session_id, completed_at)` index belongs to the
continuation store's migrations (Phase 4). Connection settings
(`journal_mode=WAL`, `busy_timeout`, `foreign_keys`) are set by `withHost`
when it opens the database (Phase 5), not by the library backends, which
work on connections their callers own.

#### 3.3 Reading sessions from tools (G4, implemented, Phase 3)

A read-only interface both stores implement:

```haskell
data CatalogEntry = CatalogEntry
    { ceConversationId :: ConversationId, ceUpdatedAt :: Maybe UTCTime
    , ceSession :: Maybe Session   -- Nothing when unreadable (locked file)
    , ceBusy :: Bool }             -- locked file, or status running
data SessionCatalog = SessionCatalog
    { catList :: IO [CatalogEntry], catRead :: ConversationId -> IO (Maybe Session) }
fileCatalog    :: SessionStore -> SessionCatalog
backendCatalog :: SessionBackend -> SessionCatalog
```

Entries are keyed by conversation ID, the vocabulary of the session tools
(and the same UUID as the session ID in a backend).
`SessionIntrospectionConfig.introspectionCatalog` and
`AgentTree.Props.sessionCatalog` take a catalog; the CLI and TUI pass
`fileCatalog store`, and the Host will pass `backendCatalog`.

The search index (`Session/Search`) stays on the file `SessionStore`: only
the `session-index` and `session-search` CLI commands use it, and it is built
around file paths and modification times.

#### 3.4 Continuations stay consistent (G7)

The session JSON is the source of truth. `tool_continuations` is an index
from token to session.

* New `Session.Wake.wakeSessionWith :: Maybe ContinuationStore -> Maybe ToolCache -> Session -> [(ContinuationToken, UserToolResponse)] -> IO Session`.
  After a successful wake it calls `csComplete` for every token it applied.
  `wakeSession` and `wakeSessionWithCache` become wrappers.
* New `findSessionForToken :: ContinuationStore -> SessionBackend -> ContinuationToken -> IO (Maybe SessionId)`
  uses `csLoad` and the snapshot's session id. It falls back to scanning only
  when the continuation store has no row, which covers sessions created before
  the store was installed. The CLI `complete` command switches to this.

### 4. `System.Agents.Host.Runner`: session lifecycle

```haskell
data SessionRunner  -- opaque

newSessionRunner :: Host -> IO SessionRunner
shutdownSessionRunner :: SessionRunner -> IO ()   -- cancels live runs, stores their sessions

data RunMode = StepOnce | UntilBlocked

createSession :: SessionRunner -> AgentSlug -> NewMessage -> Maybe RunMode -> IO (Either RunnerError SessionMeta)
postMessage   :: SessionRunner -> SessionId -> NewMessage -> Maybe RunMode -> IO (Either RunnerError SessionMeta)
resume        :: SessionRunner -> SessionId -> RunMode -> IO (Either RunnerError SessionMeta)
completeCall  :: SessionRunner -> ContinuationToken -> UserToolResponse -> Bool {- auto-resume -} -> IO (Either RunnerError SessionMeta)
cancelRun     :: SessionRunner -> SessionId -> IO (Either RunnerError SessionMeta)
getSession    :: SessionRunner -> SessionId -> IO (Maybe (Session, SessionMeta))
awaitRun      :: SessionRunner -> SessionId -> NominalDiffTime -> IO (Either RunnerError (SessionMeta, Bool {- run still active -}))
deleteSession :: SessionRunner -> SessionId -> DeleteMode -> IO (Either RunnerError DeletionPlan)
subscribe     :: SessionRunner -> SessionId -> IO (TChan SessionEvent)   -- dupTChan of a broadcast channel
recoverOnStartup :: SessionRunner -> IO [SessionId]

data NewMessage = NewMessage { nmText :: Text, nmMedia :: [MediaAttachment] }

data DeleteMode = DryRun | DeleteForReal

data DeletionPlan = DeletionPlan
    { dpSessions      :: [SessionId]   -- ^ the session and all descendants, leaves first
    , dpContinuations :: Int           -- ^ continuation rows removed (or that would be)
    , dpDryRun        :: Bool
    }

data RunnerError
    = UnknownAgent AgentSlug | UnknownSession SessionId | UnknownToken ContinuationToken
    | TokenAlreadyCompleted ContinuationToken
    | RunInProgress SessionId          -- ^ a live run owns the session (or, on delete, a descendant)
    | NotAcceptingMessages SessionId SessionStatus
    | Conflict VersionConflict
```

#### 4.1 Per-session state

```haskell
data LiveSession = LiveSession
    { lsLock   :: MVar ()                          -- serialises every mutation
    , lsRun    :: TVar (Maybe (Async ()))          -- the active run, if any
    , lsAgent  :: TVar (Maybe (Agent (LlmTurnContent, Session)))
    -- ^ The agent with its World + AsyncEngine, kept between runs so background
    -- calls started by an earlier run can still be polled and cancelled (G8).
    , lsEvents :: TChan SessionEvent               -- broadcast
    , lsLastTouched :: TVar UTCTime
    }
-- held in: TVar (Map SessionId LiveSession)
```

* The runner creates a `LiveSession` on first access. A reaper thread evicts
  entries idle for `hcLiveSessionTtl` (15 min, configurable) that have no active run
  and no `Running` calls, and calls `shutdownAsyncEngine` on eviction.
* The first run of a session builds its agent with `buildAgent` and then
  `withAsyncEngine` (after installing a World the way `Step.hs:917` does),
  and keeps it in `lsAgent`. Later runs reuse it, so the engine is not shut
  down between requests. That means the runner does **not** use
  `runUntilBlocked`/`withEngineShutdown` directly. It runs its own loop over
  `runStepM`, with the same stop conditions as `runUntilBlocked`, and shuts
  the engine down only on cancel, eviction, or runner shutdown.
* `Running` calls whose engine was lost (restart or eviction) are resolved as
  orphaned by `pollRunningCall` on the next step. No new code is needed.

#### 4.2 Mutation protocol

Every operation that changes a session:

1. Takes `lsLock`.
2. If a run is active and the operation is not `cancelRun`, returns
   `RunInProgress`. `completeCall` is the exception: see 4.3.
3. `sbLoadMeta`, then applies the change (`wakeSessionWith`, or pushes a
   `UserTurn`), then `sbCompareAndStore`.
4. Emits a `SessionEvent`, then releases the lock.

A run takes the lock for each step's store, not for the whole run. Inside the
run loop each step: `runStepM`, then `sbCompareAndStore` under `lsLock`, then
emit `session.updated`. A `VersionConflict` inside a run stops it with
`StatusFailed` with detail "concurrent modification". That cannot happen with a single
server process; it guards against a second process or the CLI writing to the
same database.

The runner sets `status = running` when a run starts and `sessionStatusOf s`
when it ends. On an exception it sets `StatusFailed` with `displayException e` as detail.
Progress storage installed by `buildAgent` (§2.5) is **disabled** for
runner-built agents (`SinkNone`),
because the runner does the versioned stores itself.

#### 4.3 Completing a deferred call during a run

`runUntilBlocked` only stops once nothing can progress in-process, so an
active run and a pending deferred call can coexist. For example, a deferred
call waits while a background call is still running.

`completeCall` on a session with an active run:

* takes the lock;
* applies `wakeSessionWith` to the **latest stored** session and stores it
  with CAS;
* sets a `TVar Bool` "external input arrived" flag on the `LiveSession`.

The run loop reloads the session from the backend before each step when that
flag is set, and resets it. Without an active run, `completeCall` stores the
change and, if `auto-resume` is true and the turn is now complete, starts an
`UntilBlocked` run.

#### 4.4 Follow-up messages

`postMessage` is valid only when the status is `StatusIdle` (head is a final
`LlmTurn`). It pushes

```haskell
UserTurn (UserTurnContent sysPrompt sysTools (Just (UserQuery text media)) []) Nothing
```

onto `turns` (newest first, as `handleStart` does), taking `sysPrompt` and
`sysTools` from the live agent. `naiveTilNoToolCallStep` then sends it to the
LLM on the next step (`Step.hs:1081`). Agents built by the runner keep
`usrQuery = pure Nothing`.

The code that builds the first turn from a prompt moves out of
`CLI/SessionDurable.handleStart` into
`Session.Types.newSessionFromPrompt :: SystemPrompt -> [SystemTool] -> NewMessage -> IO Session`
(fixes G10). `createSession` and `handleStart` both use it.

#### 4.5 Events

```haskell
data SessionEvent
    = RunStarted   SessionId RunMode
    | SessionUpdated SessionId SessionMeta Turn   -- ^ the new head turn
    | CallsDeferred SessionId [DeferredCallView]
    | RunStopped   SessionId SessionStatus
    | SessionFailed SessionId Text
```

`DeferredCallView` is the same data `agents-exe session pending` prints: tool
name, call id, token, disposition, and arguments. Move
`extractDeferredCalls` (`CLI/SessionDurable.hs:120`) into the library as
`Session.Types.pendingDeferredCalls :: Session -> [DeferredCallView]` and have
the CLI use it.

#### 4.6 Startup recovery

`recoverOnStartup` queries `status = running`. For each session found:

1. Load it.
2. Set its status to `sessionStatusOf s`.
3. Emit nothing, since no subscribers exist yet.

It does **not** resume automatically. Rows left `running` mean the process
died mid-step. The partially started step is lost, and any `Running` calls
become orphaned on the next resume. The server logs how many sessions it
recovered. Automatic resume can be added later as `--resume-interrupted`.

#### 4.7 Cancellation

`cancelRun` cancels the run's `Async`, then calls `shutdownAsyncEngine`, which
cancels background calls; they end as `Failed "async tool call was
cancelled"`. It then stores the session and sets its status from
`sessionStatusOf`. Sub-agent runs execute inside the parent's tool call, so
cancelling the parent cancels them.

#### 4.8 Waiting for a run

`awaitRun` blocks until the session's active run stops or the timeout
expires, whichever comes first. It returns the current meta and whether a run
is still active. With no active run it returns at once. It watches
`lsRun` (STM `waitCatch` on the `Async`, raced against a timer), so waiting
never holds `lsLock`. A waiter going away (for example an HTTP client
disconnecting) never cancels the run.

#### 4.9 Deleting sessions

Deletion always cascades: a session goes together with every descendant
(sub-agent sessions, found recursively through `sbQuery` on `parent`) and
their continuation rows. `deleteSession`:

1. Collects the tree and builds a `DeletionPlan`, deepest sessions first.
2. Returns `RunInProgress` if any session in the tree has an active run. This
   applies in both modes, so a dry run shows whether the real delete would
   succeed.
3. With `DryRun`, returns the plan and changes nothing.
4. With `DeleteForReal`, walks the plan in order. For each session it takes
   that session's `lsLock`, deletes its continuation rows, calls `sbDelete`,
   drops its `LiveSession` (shutting down its engine), and releases the lock.

Deleting leaves first means a crash part-way through never leaves a
sub-session whose parent is gone; running the delete again finishes the job.
This needs one new `ContinuationStore` field,
`csDeleteSession :: SessionId -> IO Int`, which deletes every row of a session
(pending or completed) and returns the count. The runner does the cascade,
not SQL foreign keys, so the file backend behaves the same.

### 5. HTTP API (`agents-server`)

The server is a new executable in `examples/agents-server/` using `wai` and
`warp`; these are the only new dependencies, and only for that executable. It
writes SSE by hand with a `responseStream`, which needs no `wai-extra`. All
bodies are JSON. Session objects embed the raw `Session` JSON that is already
stored, so clients can reuse `session-print` logic.

```
agents-server --agent-file a.json [--agent-file b.json …] --api-keys keys.json \
              --db ./agents-server.db --port 8080 [--bind 127.0.0.1]
```

The server binds to `127.0.0.1` by default. There is no authentication in
milestone 1 (see non-goals); the startup log says so.

| Method & path | Body | Success | Errors |
|---|---|---|---|
| `GET /v1/agents` | | `200 [{slug, description, tools:[name]}]` | |
| `POST /v1/sessions?wait=&timeout=` | `{agent, prompt, media?:[{mime, base64}], run?: "none"\|"step"\|"until_blocked"}` (default `until_blocked`) | `201 SessionView` | 404 unknown agent, 400 bad body |
| `GET /v1/sessions?agent=&status=&parent=&limit=&before=` | | `200 {sessions:[SessionMetaView], next_before}` | |
| `GET /v1/sessions/:id` | | `200 SessionView` | 404 |
| `POST /v1/sessions/:id/messages?wait=&timeout=` | `{prompt, media?, run?}` | `202 SessionView`, or `200` when waited | 404, 409 `run_in_progress`, 409 `not_accepting_messages` |
| `POST /v1/sessions/:id/resume?wait=&timeout=` | `{mode?: "step"\|"until_blocked"}` | `202 SessionView`, or `200` when waited | 404, 409 `run_in_progress` |
| `POST /v1/sessions/:id/cancel` | | `200 SessionMetaView` | 404, 409 `no_active_run` |
| `GET /v1/sessions/:id/pending` | | `200 {calls:[DeferredCallView]}` | 404 |
| `POST /v1/continuations/:token?wait=&timeout=` | `{result: UserToolResponse \| string, resume?: bool}` (default `true`) | `202 SessionView`, or `200` when waited or not resumed | 404 `unknown_token`, 409 `token_already_completed`, 409 `conflict` |
| `GET /v1/sessions/:id/events` | | `200 text/event-stream` | 404 |
| `DELETE /v1/sessions/:id?dry_run=` | | `200 {sessions:[id], continuations, dry_run}` | 404, 409 `run_in_progress` (also for a dry run, when the real delete would fail) |
| `GET /healthz` | | `200 {ok:true, live_sessions, active_runs}` | |

`SessionMetaView` = `{session_id, agent, parent_session_id, status,
status_detail, version, created_at, updated_at}`.
`SessionView` = `SessionMetaView` + `{session: <Session JSON>, pending:
[DeferredCallView]}`.
Errors are `{error: "<code>", message: "<text>"}`.

**Waiting.** Every endpoint that can start a run (create, messages, resume,
continuations) takes the same two query parameters:

* `wait=true|false`, default `false`. With `false`, the server answers as soon
  as the run has started. With `true`, it answers when the run stops (idle,
  blocked on deferred calls, or failed), using `awaitRun`.
* `timeout=<seconds>`, only used with `wait=true`. Default 120, maximum 600;
  larger values are clamped. If it expires, the server answers with the
  current state (`status: "running"`) and the run carries on. Clients then
  follow it through the events stream or by polling.

The response body is always the `SessionView` at the time of answering, so a
waiting client gets the final turn and any pending deferred calls in one
round trip. A client disconnecting while waiting does not cancel the run.
Creation always answers `201`; the other endpoints answer `200` when the run
has stopped (or none was started) and `202` while it is still going.

**Deleting.** `dry_run=true` returns the same body as a real delete,
listing every session and the number of continuation rows that would be
removed, and changes nothing.

SSE stream: each `SessionEvent` becomes `event: <kind>` plus
`data: <json>`, where kind is `run.started`, `session.updated`,
`calls.deferred`, `run.stopped`, or `session.failed`. On connect the server
first sends `event: snapshot` with the `SessionMetaView`, so clients never
need a separate GET to sync. The server sends a `: keepalive` comment every
15 s.

### 6. Tracing and logs

`HostTrace` wraps the existing traces (`OneShot.Trace`, tool registration,
OpenAI) plus runner events. The server prints them as JSON lines on stderr,
one object per line with `ts`, `session_id` when known, and `kind`. API keys
must never be logged. Phase 6 includes checking the HTTP and OpenAI traces for
headers or keys before they are printed.

---

## Phases

Each phase leaves `cabal build all` and `cabal test agents-tests` passing with
`-Wall -Werror`, and gets its own entry in a `todos/web-server-embedding.progress.md`
tracker.

### Phase 1: storage wiring fix (G1, G5) ✅

* `SessionSink` and `agentPersistSession`; keep `agentStoreSession` as a
  wrapper.
* `session start` derives its conversation ID from the session ID.
* Tests: an agent built with a SQLite backend and a mock LLM runs to
  completion and writes **no** files under a temp session directory; the row
  is keyed by the session's `SessionId`.

### Phase 2: single agent factory (G2, G3, G10) ✅

* `System.Agents.AgentFactory` with `AgentDeps`, `buildAgent`, and
  `AgentRole`; the three copies moved onto it; sub-agents get their parent
  and call stack from `SubAgent`.
* `newSessionFromPrompt` in the library.
* Tests: the existing suite unchanged; new `AgentFactoryTests`.

### Phase 3: metadata, versions, migrations, catalog (G4, G9) ✅

* Extended `SessionBackend`, `SessionLabels`, `SessionMeta`,
  `sessionStatusOf`, SQLite migrations, and the file sidecar.
* `SessionCatalog`; the session tools and `Props` switched to it.
* Sub-agents store their agent slug and `parent_session_id`.
* Tests (`SessionMetadataTests`): status derivation; version increments;
  labels kept; CAS success and conflicts (stale, and version 0 twice);
  running status kept by unconditional stores; `sbQuery` filters and limit;
  composite fallback; migrating a database from before metadata; migrations
  run once; file sidecar and legacy files; file and backend catalogs;
  list-sessions over a backend catalog; a parent agent calling a sub-agent
  tool (mock LLM) leaves a sub-session row naming the parent.

### Phase 4: continuation consistency (G7)

* `wakeSessionWith`, `findSessionForToken` via `csLoad`; switch the CLI
  `complete` command over.
* Tests: after a wake the continuation row has `completed_at` set; completing
  the same token twice is reported as already completed; a token lookup does
  not load unrelated sessions (count `sbLoad` calls with an instrumented
  backend).

### Phase 5: `SessionRunner` (G6, G8)

* Everything in §4, with `withHost`.
* Tests, all with a mock LLM through `hostCompletion`:
  * create, then run until blocked on deferred calls, `completeCall`
    with auto-resume, final answer;
  * two concurrent `completeCall`s for two tokens of the same turn: both
    results present afterwards (the lost-update regression);
  * `postMessage` while running gives `RunInProgress`; after idle it produces
    a second LLM turn;
  * `RunAsync` call started in run 1 finishes and is picked up in run 2
    (engine kept between runs);
  * cancel during a slow tool: status becomes not-running, background call
    `Failed "…cancelled"`;
  * recovery: a row with `status = running` and a `Running` call becomes
    `ready`; the next resume marks the call orphaned;
  * `awaitRun` returns when the run stops, and with "still active" after a
    short timeout on a slow tool;
  * delete cascade: a parent with a sub-session and continuation rows; the
    dry run lists all of them and leaves the database unchanged; the real
    delete removes all of them; both fail with `RunInProgress` while the
    sub-session's parent run is active.

### Phase 6: `agents-server` executable

* wai/warp app, routes, SSE, CLI flags, graceful shutdown on SIGTERM (stop
  accepting, `shutdownSessionRunner`, close the database).
* `docs/agents-server.md` user guide, plus links from
  `docs/durable-workflows-howto.md`.
* Tests: a `tasty` integration test that starts the app on a random
  port with a mock LLM and replays the canonical demo flow over HTTP,
  asserting SSE event order `snapshot`, `run.started`, `session.updated`…,
  `calls.deferred`, `run.stopped`. The same flow with `wait=true` needs no
  events stream: the create call returns the blocked session with its pending
  calls, and the continuation call returns the final answer.

---

## Later work

* **Multi-tenancy**: authentication middleware; `owner` filled from the
  caller and enforced in every `sbQuery`/`sbLoadMeta`; per-owner API keys in
  `buildAgent`; default policy `runIsolated` for bash and MCP tools, backed by
  `dockerRunner`.
* **Postgres backend**: `SessionBackend` and `ContinuationStore` against
  `postgresql-simple`, as a separate sub-library so the core does not depend
  on libpq. CAS is the same `UPDATE … WHERE version = ?`. With several server
  processes, `lsLock` only serialises within one process and CAS covers the
  rest; live-run ownership then needs a lease column (`run_owner`,
  `run_lease_until`).
* **Agents from the database** instead of files: needs `AgentTree` to accept
  in-memory configs (`Props.rootAgentFile` is a path today).
* **Token streaming**: streaming in `LLMs/OpenAI` and a `TokenDelta` event.
* **`agents-core` sub-library** without `brick`/`vty`.
* **MCP server over HTTP**: serve the existing MCP server
  (`MCP/Server.hs`) from `agents-server` using the Streamable HTTP transport,
  reusing the `Host` so MCP clients and the REST API share agents and
  storage.

## Decisions

Recorded 2026-09-18.

1. **Waiting for a run is chosen per request** with the `wait` and `timeout`
   query parameters (§5), on every endpoint that can start a run. The default
   is not to wait. The body's `run` field still controls how far the run goes.
2. **Deleting cascades** to sub-sessions and continuation rows, and supports
   a dry run (`?dry_run=true`) that reports what would be deleted (§4.9).
3. **Idle live sessions keep their engine for 15 minutes**
   (`hcLiveSessionTtl`, configurable). A background call still running when
   its session is evicted ends up orphaned.
4. **The MCP server over HTTP is wanted, but later** (see
   [Later work](#later-work)).

## Related docs

* `todos/durable-workflows.md`, `todos/durable-workflows.progress.md`
* `todos/async-tool-calls.md`
* `docs/durable-workflows-howto.md`, `docs/async-tool-calls.md`, `docs/sessions.md`
