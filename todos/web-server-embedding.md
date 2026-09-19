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
  later without a migration (see [Milestone 2](#milestone-2-the-later-work)).
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

#### 2.2 `System.Agents.Host` (implemented, Phase 5)

```haskell
data Host = Host
    { hostAgents         :: Map Text OSAgentNode   -- root agents, by slug
    , hostDeps           :: AgentDeps              -- root agents: SinkNone, the runner stores
    , hostSubAgentDeps   :: AgentDeps              -- sub-agents: SinkBackend hostBackend
    , hostBackend        :: SessionBackend
    , hostContinuations  :: ContinuationStore
    , hostTracer         :: Tracer IO HostTrace
    , hostLiveSessionTtl :: NominalDiffTime
    }

data HostConfig = HostConfig
    { hcAgentFiles :: [FilePath], hcApiKeysFile :: FilePath, hcDatabasePath :: FilePath
    , hcCompletion :: Maybe (OSAgentNode -> Completion), hcLiveSessionTtl :: NominalDiffTime }
defaultHostConfig :: [FilePath] -> FilePath -> FilePath -> HostConfig   -- TTL 15 minutes

withHost :: HostConfig -> Tracer IO HostTrace -> (Host -> IO a) -> IO a
```

`withHost` opens the database with `journal_mode = WAL` and
`busy_timeout = 5000`, runs the session and continuation migrations, loads
each agent file (sub-agent tools get `hostSubAgentDeps`, and session tools a
`backendCatalog`), and refuses duplicate root slugs (`HostError`). Both
dependency sets share the continuation store and the completion override.
The bundled SQLite is built with `THREADSAFE=1` (serialized), so one
connection is shared by all threads; no statement sequence relies on
`changes()`.

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
(`journal_mode=WAL`, `busy_timeout`) are set by `withHost`
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

#### 3.4 Continuations stay consistent (G7, implemented, Phase 4)

The session JSON is the source of truth. `tool_continuations` is an index
from token to session.

* `ContinuationStore.csFindSession :: ContinuationToken -> IO (Maybe SessionId)`
  answers for pending and completed tokens (`csLoad` only returns pending
  ones, so it cannot tell a completed token from an unknown one).
* `Session.Wake.wakeSessionWith :: Maybe ContinuationStore -> Maybe ToolCache -> Session -> [(ContinuationToken, UserToolResponse)] -> IO WakeOutcome`
  reports, per token, whether it was applied, already completed, or unknown,
  and calls `csComplete` for the applied ones. A token is already completed
  when a call of the session still carries it but is no longer deferred, or
  when the store knows it for this session (once a turn is complete, the
  session no longer holds its tokens). `wakeSession` and
  `wakeSessionWithCache` are wrappers returning the session.
* `Session.Wake.findSessionForToken :: Maybe ContinuationStore -> SessionBackend -> ContinuationToken -> IO (Maybe SessionId)`
  asks `csFindSession` first and scans the backend's sessions only for tokens
  the store does not know.
* `csComplete` uses `UPDATE … RETURNING` instead of `SELECT changes()`, which
  another user of the same connection could change in between.
* The continuation table's schema goes through `runMigrations` (component
  `continuations`); migration 2 adds the `(session_id, completed_at)` index.

The CLI `complete` command keeps its scan over the file store: CLI agents
have no continuation store. The runner (§4.3) uses `findSessionForToken` and
`wakeSessionWith`.

Agents store their session before each step, so the session a run stops in
is not stored by the agent: callers store it (the session commands and
one-shot `run` do; the runner stores after every step, §4.2).

### 4. `System.Agents.Host.Runner`: session lifecycle (implemented, Phase 5)

```haskell
data SessionRunner  -- opaque

newSessionRunner      :: Host -> IO SessionRunner
shutdownSessionRunner :: SessionRunner -> IO ()   -- cancels active runs, stores their sessions
withSessionRunner     :: Host -> (SessionRunner -> IO a) -> IO a
runnerStats           :: SessionRunner -> IO RunnerStats   -- live sessions, active runs

data RunMode = StepOnce | UntilBlocked

createSession :: SessionRunner -> Text -> NewMessage -> Maybe RunMode -> IO (Either RunnerError SessionMeta)
postMessage   :: SessionRunner -> SessionId -> NewMessage -> Maybe RunMode -> IO (Either RunnerError SessionMeta)
resume        :: SessionRunner -> SessionId -> RunMode -> IO (Either RunnerError SessionMeta)
completeCall  :: SessionRunner -> ContinuationToken -> UserToolResponse -> Bool {- auto-resume -} -> IO (Either RunnerError SessionMeta)
cancelRun     :: SessionRunner -> SessionId -> IO (Either RunnerError SessionMeta)
getSession    :: SessionRunner -> SessionId -> IO (Maybe (Session, SessionMeta))
awaitRun      :: SessionRunner -> SessionId -> NominalDiffTime -> IO (Either RunnerError (SessionMeta, Bool {- run still active -}))
deleteSession :: SessionRunner -> SessionId -> DeleteMode -> IO (Either RunnerError DeletionPlan)
subscribe     :: SessionRunner -> SessionId -> IO (IO SessionEvent)   -- blocking "next event"
recoverOnStartup :: SessionRunner -> IO [SessionId]

data NewMessage = NewMessage { nmText :: Text, nmMedia :: [MediaAttachment] }

data DeleteMode = DryRun | DeleteForReal

data DeletionPlan = DeletionPlan
    { dpSessions      :: [SessionId]   -- ^ the session and all descendants, deepest first
    , dpContinuations :: Int           -- ^ continuation rows removed (or that would be)
    , dpDryRun        :: Bool
    }

data RunnerError
    = UnknownAgent Text | UnknownSession SessionId | UnknownToken ContinuationToken
    | TokenAlreadyCompleted ContinuationToken
    | RunInProgress SessionId          -- ^ a run owns the session (on delete: one of the tree, or an ancestor)
    | NoActiveRun SessionId            -- ^ cancel without a run (HTTP 409 no_active_run)
    | NotAcceptingMessages SessionId SessionStatus
    | Conflict VersionConflict
```

Agents built by the runner always run asynchronously
(`withExecutionMode Asynchronous`), like the CLI `session` commands, so that
runs can stop and resume.

#### 4.1 Per-session state

```haskell
data LiveSession = LiveSession
    { lsLock        :: MVar ()                          -- serialises every change
    , lsRun         :: TVar (Maybe (Async ()))          -- the active run, if any
    , lsAgent       :: TVar (Maybe (Agent …))           -- kept between runs (G8)
    , lsLatest      :: TVar (Maybe (Session, SessionMeta))  -- last version stored or loaded
    , lsInbox       :: TVar [(ContinuationToken, UserToolResponse)]  -- see 4.3
    , lsLastTouched :: TVar UTCTime
    , lsEvicted     :: TVar Bool
    }
-- held in: TVar (Map SessionId LiveSession)
```

* The runner creates a `LiveSession` on first access. A reaper thread (every
  half TTL, between 50 ms and 60 s) evicts sessions idle for longer than
  `hostLiveSessionTtl` that have no active run and no `Running` call, and
  shuts their engine down. An operation that took a `LiveSession` just before
  it was evicted notices `lsEvicted` once it holds the lock, and retries with
  a fresh one.
* The first run of a session builds its agent with `buildAgent` and keeps it
  in `lsAgent`. `runStepM` installs a World and an engine on demand and
  returns the agent holding them; the runner keeps that agent, so the engine
  survives between runs. The runner has its own loop over `runStepM`, with
  the stop conditions of `runUntilBlocked`, and shuts the engine down only on
  cancel, eviction, deletion, or runner shutdown.
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

Writing the woken session while the run is in a step would make the run's
next versioned store conflict. So `completeCall` on a session with an active
run, under the lock:

* checks the token against the run's latest version (and the queue), and
  answers `UnknownToken` or `TokenAlreadyCompleted` right away;
* otherwise appends the result to `lsInbox` and returns.

Before each step and before deciding to stop, the run, under the lock,
applies the queued results with `wakeSessionWith` (which also marks the
continuations completed) and stores the result. A run that was about to
stop on deferred calls therefore carries on when their results are queued.
`cancelRun` applies the queue too.

Without an active run, `completeCall` applies the result and stores it at
once, and with auto-resume starts an `UntilBlocked` run when the session is
`ready`. Two concurrent completions of one turn are serialised by the lock:
the first stores, the second sees the first's version (tested).

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
name, call id, token, disposition, and the call with its arguments. It lives
in `Session.Types` with `pendingDeferredCalls :: Session -> [DeferredCallView]`;
the CLI's `extractDeferredCalls` wraps it.

Every stored version emits `SessionUpdated` (with the head turn), including
the stores at the start and end of a run. Events go through one runner-wide
broadcast channel: `subscribe` returns an action yielding the next event of
one session, so a subscriber keeps its stream when the session is evicted
and loaded again. `subscribeSTM` gives the same stream as a transaction that
consumes one event of any session and yields `Nothing` for another session's,
to combine with timers or flags (the HTTP events stream uses it). Filtering
must consume other sessions' events one transaction at a time: a `retry`
after `readTChan` rolls the read back, which in the first version of
`subscribe` blocked a subscriber for good on another session's event. Each
event is also traced as `HostRunnerTrace kind sid`.

#### 4.6 Startup recovery

`recoverOnStartup` queries `status = running`. For each session found that
this runner is not running itself:

1. Load it.
2. Set its status to `sessionStatusOf s`.
3. Emit nothing, since no subscribers exist yet.

It does **not** resume automatically. Rows left `running` mean the process
died mid-step. The partially started step is lost, and any `Running` calls
become orphaned on the next resume. The server logs how many sessions it
recovered. Automatic resume can be added later as `--resume-interrupted`.

#### 4.7 Cancellation

`cancelRun` cancels the run's `Async` without holding the lock (the run takes
it to store its steps), then, under the lock and only if that run still
owns the session:

1. shuts the engine down, which marks background calls cancelled in the
   World, and keeps the agent (and World) with no engine, so the next run
   gets a fresh engine on the same World;
2. applies queued external results;
3. refreshes the head partial turn from the World, so its cancelled calls
   become `Failed "async tool call was cancelled"`;
4. stores the session with the status its turns imply, and emits
   `RunStopped`.

Background calls below the head (the LLM already got a placeholder for them)
stay `Running` in the stored session. The next run's late-result collection
finds them cancelled in the kept World and tells the LLM, in a user message,
as for any late result (tested). If the session is evicted first, they are
reported as orphaned instead. Sub-agent runs execute inside the parent's
tool call, so cancelling the parent cancels them.

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
This needs two new `ContinuationStore` fields:
`csCountSession :: SessionId -> IO Int` for dry runs, and
`csDeleteSession :: SessionId -> IO Int`, which deletes every row of a session
(pending or completed) and returns the count. Deletion is also refused while
an *ancestor* of the session has an active run: its sub-agent calls write
into the tree, and would recreate what was deleted. The runner does the cascade,
not SQL foreign keys, so the file backend behaves the same.

### 5. HTTP API (`agents-server`) (implemented, Phase 6)

The server lives in `examples/agents-server/`, on `wai` and `warp`. The
application code is a private sub-library, `agents-server-internal`
(`AgentsServer.Api`, `AgentsServer.Log`, `AgentsServer.Server`), shared by the
`agents-server` executable and its `agents-server-tests` suite; `agents-lib`
does not depend on wai or warp. SSE is written by hand with `responseStream`
(no `wai-extra`). All bodies are JSON. Session objects embed the raw
`Session` JSON that is already stored, so clients can reuse `session-print`
logic. The user guide is `docs/agents-server.md`.

```
agents-server --agent-file a.json [--agent-file b.json …] --api-keys keys.json \
              [--db ./agents-server.db] [--port 8080] [--bind 127.0.0.1] \
              [--live-session-ttl 900] [--shutdown-grace 10]
```

The server binds to `127.0.0.1` by default. There is no authentication in
milestone 1 (see non-goals); the `server.started` log line says so.

| Method & path | Body | Success | Errors |
|---|---|---|---|
| `GET /v1/agents` | | `200 [{slug, description, tools:[name]}]` | |
| `POST /v1/sessions?wait=&timeout=` | `{agent, prompt, media?:[{mime, base64, filename?}], run?: "none"\|"step"\|"until_blocked"}` (default `until_blocked`) | `201 SessionView`, `Location` header | 404 `unknown_agent`, 400 `bad_request` |
| `GET /v1/sessions?agent=&status=&parent=&limit=&before=` | | `200 {sessions:[SessionMetaView], next_before}` | 400 |
| `GET /v1/sessions/:id` | | `200 SessionView` | 404 `unknown_session` |
| `POST /v1/sessions/:id/messages?wait=&timeout=` | `{prompt, media?, run?}` | `202`/`200 SessionView` | 404, 409 `run_in_progress`, 409 `not_accepting_messages` |
| `POST /v1/sessions/:id/resume?wait=&timeout=` | `{mode?: "step"\|"until_blocked"}` or no body | `202`/`200 SessionView` | 404, 409 `run_in_progress` |
| `POST /v1/sessions/:id/cancel` | | `200 SessionMetaView` | 404, 409 `no_active_run` |
| `GET /v1/sessions/:id/pending` | | `200 {calls:[DeferredCallView]}` | 404 |
| `POST /v1/continuations/:token?wait=&timeout=` | `{result: UserToolResponse \| string, resume?: bool}` (default `true`) | `202`/`200 SessionView` | 404 `unknown_token`, 409 `token_already_completed`, 409 `conflict` |
| `GET /v1/sessions/:id/events` | | `200 text/event-stream` | 404 |
| `DELETE /v1/sessions/:id?dry_run=` | | `200 {sessions:[id], continuations, dry_run}` | 404, 409 `run_in_progress` (also for a dry run, when the real delete would fail) |
| `GET /healthz` | | `200 {ok:true, live_sessions, active_runs}` | |

Other errors: 404 `not_found` (unknown path), 405 `method_not_allowed`, 413
`payload_too_large` (bodies over 32 MiB), 500 `internal_error`. A malformed
session id answers 404 `unknown_session`, a malformed token 404
`unknown_token`.

`SessionMetaView` is the `SessionMeta` JSON: `{session_id, agent,
parent_session_id, owner, status, status_detail, version, created_at,
updated_at}` (`owner` is always null in milestone 1).
`SessionView` = `SessionMetaView` + `{session: <Session JSON>, pending:
[DeferredCallView]}`.
Errors are `{error: "<code>", message: "<text>"}`.

**Listing.** Newest first by `updated_at`. `status` takes a comma-separated
list; `limit` is 1–500, default 50. When a page is full, `next_before` is the
`updated_at` of its last session, used as `before=` (strictly older) for the
next page. A session with exactly the boundary's `updated_at` would be
skipped; timestamps have nanosecond precision, so that takes two writes in
the same instant.

**Waiting.** Every endpoint that can start a run (create, messages, resume,
continuations) takes the same two query parameters:

* `wait=true|false`, default `false` (`?wait` alone means true). With
  `false`, the server answers as soon as the run has started. With `true`, it
  answers when the run stops (idle, blocked on deferred calls, or failed),
  using `awaitRun`.
* `timeout=<seconds>`, only used with `wait=true`. Default 120, maximum 600;
  larger values are clamped. If it expires, the server answers with the
  current state (`status: "running"`) and the run carries on. Clients then
  follow it through the events stream or by polling.

The response body is always the `SessionView` at the time of answering, so a
waiting client gets the final turn and any pending deferred calls in one
round trip. A client disconnecting while waiting does not cancel the run.
Creation always answers `201`. The other endpoints answer `202` when the
stored status is `running` at the time of answering (a run is still going,
including a continuation queued into an active run) and `200` otherwise.
On shutdown, waiting requests answer at once with the current state.

**Deleting.** `dry_run=true` returns the same body as a real delete,
listing every session and the number of continuation rows that would be
removed, and changes nothing.

**SSE stream.** Each `SessionEvent` becomes `event: <kind>` plus one
`data: <json>` line, where kind is `run.started` (`{session_id, mode}`),
`session.updated` (`SessionMetaView` + `head_turn`), `calls.deferred`
(`{session_id, calls}`), `run.stopped` (`{session_id, status}`), or
`session.failed` (`{session_id, message}`). On connect the server first sends
`event: snapshot` with the `SessionMetaView`, so clients never need a
separate GET to sync; it subscribes before loading the snapshot, so no event
falls in between. A run's `session.updated` for the running version comes
just before its `run.started`. The server sends a `: keepalive` comment
after 15 s without events (warp pauses its idle timeout while the handler
runs, so quiet streams and long waits are not cut). Streams end on shutdown.
Events are not replayed on reconnect. The stream reads events with
`subscribeSTM`, combined with the keepalive timer and the shutdown flag in
one transaction, so a timer firing never drops an event.

**Shutdown.** On SIGTERM or SIGINT: stop accepting connections, end event
streams and release waiting requests, give open requests
`--shutdown-grace` seconds, then `shutdownSessionRunner` (cancels active runs,
storing their sessions) and close the database.

### 6. Tracing and logs (implemented, Phase 6)

`HostTrace` wraps the existing traces (`OneShot.Trace`, tool registration,
OpenAI) plus runner events. The server prints them as JSON lines on stderr,
one object per line with `ts`, `kind`, and `session_id` when known.
`AgentsServer.Log` summarises each trace field by field instead of `show`ing
it: LLM traces give byte and token counts, the HTTP client trace gives
method, host, path, and status only (its request carries the API key in a
header), and tool and agent-tree traces give their constructor name. Prompts,
payloads, headers, and API keys are never printed; a smoke test with a fake
key checked the log. Each HTTP request logs method, path, status, and time
to first byte.

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

### Phase 4: continuation consistency (G7) ✅

* `csFindSession`, `wakeSessionWith` / `WakeOutcome`, `findSessionForToken`,
  continuation migrations, `RETURNING` in `csComplete`.
* Tests (`ContinuationConsistencyTests`, on a real paused run: async agent,
  defer policy, mock LLM, one SQLite database): a wake marks the continuation
  completed; a second wake reports the token as already completed and leaves
  the session unchanged; an unknown token is reported; the woken session
  resumes to a final answer; a token lookup through the index loads no
  session; without an index the sessions are searched; migrations run once.

### Phase 5: `SessionRunner` (G6, G8) ✅

* `System.Agents.Host` (`withHost`) and `System.Agents.Host.Runner` (§4).
* `DeferredCallView` / `pendingDeferredCalls`; `csCountSession` and
  `csDeleteSession`.
* Tests (`RunnerTests`, mock LLM through the host's completion override):
  deferred call completed with auto-resume, with the event sequence; two
  concurrent completions of one turn both land; messages refused during a
  run and accepted after, giving a second LLM turn; a background call
  started in one run picked up by the next; cancel during a background call,
  then the cancellation reported on the next run; recovery of a session left
  running, its call orphaned on resume; `awaitRun` with timeout and
  completion; delete cascade (refused during the parent's run, also for the
  sub-session; dry run changes nothing; real delete removes sessions and
  continuation rows); idle eviction and reload; `withHost` over agent and
  database files.

### Phase 6: `agents-server` executable ✅

* wai/warp app, routes, SSE, CLI flags, graceful shutdown on SIGTERM (stop
  accepting, `shutdownSessionRunner`, close the database).
* `docs/agents-server.md` user guide, plus links from
  `docs/durable-workflows-howto.md`.
* Tests (`agents-server-tests`, threaded, the application on a random port
  with a mock LLM): the demo flow over SSE (snapshot, then per run
  `run.started` … `calls.deferred`, `run.stopped`, then the continuation's
  run ending `idle`); the same flow with `wait=true` and no events stream
  (the create call returns the blocked session with its pending call, the
  continuation call the final answer); keepalives; listing pages and filters;
  delete with dry run; agents and health; error codes; shutdown releasing
  waiting requests and ending streams.

---

## Milestone 2: the later work

Recorded 2026-09-19, when the user asked to continue with the later work.
Each item becomes a phase, in this order: each phase is useful on its own,
and the riskier ones come after the ones they build on. Choices marked
*default* were made without asking; they are listed again under Decisions.

### Phase 7: authentication and owners ✅

* `agents-server --auth-tokens tokens.json`: `{"tokens": [{"owner": "alice",
  "sha256": "<hex>"}, {"owner": "bob", "token": "<plain>"}]}`. With it, every
  endpoint but `/healthz` needs `Authorization: Bearer <token>` (401
  `unauthorized` otherwise, with `WWW-Authenticate: Bearer`). Without it,
  nothing changes. Tokens are compared by SHA-256 digest (*default*: a
  static file, like the API keys file; no token issuing or expiry).
* Sessions created by a caller record its owner (`createSessionAs`).
  Sub-sessions record none: a session belongs to the owner of its **root**
  session (`sessionOwner` walks up `parent_session_id`). This needs no
  change to how sub-agents store themselves.
* Another owner's session answers 404, as if it did not exist, for every
  endpoint including continuations (404 `unknown_token`). Listing filters by
  owner (`SessionQuery.sqOwner`, index `(owner, updated_at)`, migration 3);
  `?parent=` lists the sub-sessions of an owned session.
* Sessions stored before authentication was turned on have no owner and are
  invisible to every caller (*default*).
* Not in this phase: per-owner API keys and a default `runIsolated` policy
  for bash and MCP tools. Both need agents built per owner, including
  sub-agent tools, which the host builds once at load time today. They stay
  in [Remaining later work](#remaining-later-work).

### Phase 8: MCP over HTTP ✅

* `POST /mcp` on `agents-server` (`AgentsServer.Mcp`) speaks MCP's Streamable
  HTTP transport. It handles JSON-RPC with aeson directly and reuses the
  `MCP/Base.hs` types for tools, tool lists, and the initialize result;
  tool results are written by hand, because `TextContentImpl` always
  writes `"annotations": null`. Each request, or batch, gets a plain JSON
  response. Messages that need no answer (notifications, client responses)
  get 202. `GET /mcp` answers 405: there are no server-initiated messages.
  Protocol versions 2025-06-18, 2025-03-26, and 2024-11-05: the client's is
  echoed when supported, else the latest.
* Methods: `initialize`, `ping`, `tools/list`, `tools/call`, and empty
  `resources/list` and `prompts/list`. Anything else is -32601; a bad
  or unknown tool is -32602; malformed JSON answers 400 with -32700.
* Tools: one `ask_<slug>` per root agent, input `{prompt}`. A call creates a
  session through the runner, owned by the caller, and waits up to 120 s.
  The result carries `_meta.session_id`. An idle session gives the final
  answer; `waiting_external` gives a sentence plus the pending calls as JSON
  (not an error); a run still going gives the session id; `failed` gives
  `isError: true`.
* The same bearer tokens and owners apply. `Mcp-Session-Id` is not used.
* The Streamable HTTP transport requires validating `Origin` against DNS
  rebinding. Without authentication, every endpoint (not only `/mcp`) refuses
  an `Origin` other than localhost, 127.0.0.1, or [::1] with 403
  `forbidden_origin`. With authentication, origins are not checked.

### Phase 9: TUI out of the core library ✅

* New public sub-library `agents-tui` (source directory `tui/`) with the 25
  modules that need brick or vty, or import them: `System.Agents.TUI.*`
  (except `TUI.ToolCallActivity`, which is pure and used by the tests),
  `System.Agents.CLI.TUI`, `System.Agents.CLI.Config`, and `System.Agents.CLI`.
  Module names do not change. Only `agents-exe` imports them.
* `agents-lib` drops `brick`, `vty`, `text-zipper`, and the unused
  `data-clist`; nothing in its dependency closure pulls brick or vty any
  more. `agents-exe` depends on both libraries.
* The moved files live in their own source directory: with a shared `src/`,
  GHC would compile core modules again inside `agents-tui` instead of using
  `agents-lib`.
* *Default*: `agents-lib` itself is the core instead of a new `agents-core`
  name, so nothing that depends on `agents-lib` breaks.

### Phase 10: Postgres backend

* A public sub-library `agents-postgres` (on `postgresql-simple`) with
  `mkPostgresSessionStore` and `mkPostgresContinuationStore`, the same
  schema and migrations as SQLite (component-scoped `schema_migrations`),
  CAS as `UPDATE … WHERE version = ? RETURNING`. Connections come from a
  small pool (`resource-pool`, if already in the plan; otherwise one
  connection behind an `MVar`).
* `System.Agents.Host.withHostStores` takes the two stores from the caller, so
  `agents-lib` does not depend on libpq. `agents-server --db` accepts
  `postgres://…` / `postgresql://…` as well as a SQLite path.
* With several server processes on one database, runs are not coordinated
  (a lease column is future work); CAS still detects conflicting writes.
* Tests: the backend contract (CAS, queries, continuations) and a runner flow
  against a throwaway cluster started with `initdb`/`pg_ctl` in a temp
  directory; the suite skips when the binaries are missing.

### Phase 11: token streaming

* `OpenAI` completions can stream (`"stream": true` with
  `stream_options.include_usage`): the SSE chunks are folded back into the
  usual response JSON, so parsing and tool calls are unchanged, and each text
  delta goes to a callback.
* `AgentDeps.adOnTextDelta` wires the callback; the runner gives each session's
  agent one that emits `TextDelta sid text` (`event: text.delta` on the SSE
  stream). Streaming is opt-in: `HostConfig.hcStreamTokens` /
  `agents-server --stream-tokens` (*default* off, as providers differ).
  Sub-agents do not stream.

### Phase 12: agents from the database

* Table `agents(slug, json, updated_at)` in the host's database. Agents in it
  are loaded next to `--agent-file` ones; a slug in both is an error.
* An agent tree can be loaded from an in-memory `AgentDescription`
  (`AgentTree.loadAgentTreeFrom`): the file-discovery step is replaced by a
  graph of one node. *Default*: database agents cannot use tools that need
  files (bash tool directories, OpenAPI/PostgREST files, skills); builtin
  toolboxes and MCP servers work, and `extraAgents` may name other database
  agents.
* Endpoints `PUT /v1/agents/:slug` and `DELETE /v1/agents/:slug`, allowed to
  the owners listed in `--admin-owners` (and to anyone without
  authentication). A change reloads that agent: new sessions use it, live
  sessions keep their built agent until evicted.

## Remaining later work

* **Per-owner API keys and isolation**: build agents per owner, including
  sub-agent tools; default policy `runIsolated` for bash and MCP tools, backed
  by `dockerRunner`.
* **Several server processes on one Postgres database**: live-run ownership
  through a lease column (`run_owner`, `run_lease_until`).
* **Database agents with files**: tool directories stored with the agent.

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
   [Phase 8](#phase-8-mcp-over-http)).

Recorded 2026-09-19 (defaults taken when the user asked to continue with the
later work; see [Milestone 2](#milestone-2-the-later-work)):

5. **Authentication is a static bearer-tokens file** mapping tokens (or their
   SHA-256) to owners. A session belongs to the owner of its root session;
   other owners get 404. Sessions without an owner are invisible once
   authentication is on.
6. **MCP over HTTP answers each request with plain JSON** and does not use
   MCP sessions.
7. **`agents-lib` becomes the core** by moving the TUI into a new
   `agents-tui` library, rather than creating an `agents-core` library.
8. **Postgres lives in its own `agents-postgres` library**; the host takes
   its stores from the caller.
9. **Token streaming is opt-in** (`--stream-tokens`).
10. **Database agents cannot use file-based tools** in their first version.

## Related docs

* `todos/durable-workflows.md`, `todos/durable-workflows.progress.md`
* `todos/async-tool-calls.md`
* `docs/durable-workflows-howto.md`, `docs/async-tool-calls.md`, `docs/sessions.md`
