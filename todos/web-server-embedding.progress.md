# Web server embedding — Implementation Progress

Progress tracker for `todos/web-server-embedding.md`.

## Phase 1 — Storage wiring fix ✅ COMPLETE

- `Combinators.StoreSessionProgress`:
  - `SessionSink` (`SinkBackend` / `SinkFiles` / `SinkNone`), chosen when the
    agent is built.
  - `agentPersistSession` and `sinkStoreCallback`.
  - `agentStoreSession` and `agentStoreSessionWithCallback` keep their
    behaviour (backend read from the wrapped agent, else files) on top of
    `sinkStoreCallback`.
- `CLI/SessionDurable.handleStart` derives the conversation ID from the
  session ID, so `session start --step` no longer writes a second
  `conv.<random>.json` next to `conv.<session-id>.json`.

## Phase 2 — Single agent factory ✅ COMPLETE

- New `System.Agents.AgentFactory`:
  - `buildAgent :: Tracer IO Trace -> AgentDeps -> AgentRole -> ConversationId -> OSAgentNode -> IO Agent`.
  - `AgentDeps` (API keys, session sink, continuation store, tool cache,
    completion override), `defaultAgentDeps`, `fileAgentDeps`.
  - `AgentRole` = `RootAgent | SubAgent parentConv callStack`.
  - `Trace` (moved from `OneShot`, which re-exports it) and
    `mapProgressiveDisclosureTrace`.
- Callers moved onto it, and their copies of the agent record, the OpenAI
  completion setup, API key lookup, and `toolRegistrationToSystemTool` removed:
  - `OneShot.nodeToAgent` / `nodeToAgentWithThinking` (also used by the TUI):
    thinking output, media injection, and the `--session-file` copy are now
    decorators on the factory's agent.
  - `AgentTree.OneShotTool.turnAgentRuntimeIntoIOTool` takes `AgentDeps`
    instead of a `SessionStore` and API keys. `OneShotTool.Trace` is now a
    single `OneShotTrace` constructor.
  - `MCP.Server.runAgentWithQuery`.
  - `CLI.SessionDurable.buildAgentForFile` (drops its own JSON re-parse).
- `toolRegistrationToSystemTool` has one definition, exported from
  `Combinators.ProgressiveDisclosure`.
- `Session.Types.newSessionFromPrompt`, used by `session start`.

### Behaviour changes

- Progressive disclosure now applies to the MCP server and the durable
  `session` commands too; before, only `run`, the TUI, and sub-agents had it.
- The MCP server applies the agent's `executionMode` / `toolCallPolicyConfig`
  and runs with `runUntilBlocked`. A turn waiting on deferred calls returns an
  error rather than looping forever.
- A sub-agent's session file is named after the conversation ID used for its
  call-stack entry and OS World entity, not an unrelated random ID.

### Verification

- `cabal build all --enable-tests` succeeded. (It did not check warnings: the
  library was not built with `-Wall` then, see the Phase 4 section.)
- `agents-tests`: 902 tests pass, including 8 new `AgentFactoryTests`.
- Smoke test of `session start`, `session start --step`, and `session step`
  with an isolated `HOME`: exactly one `conv.<session-id>.json` per session.

## Phase 3 — Metadata, versions, migrations, catalog ✅ COMPLETE

- `Session.Types`: `SessionStatus` (idle / ready / running /
  waiting_external / failed) with text and JSON forms, and `sessionStatusOf`.
  `isBlockedOnDeferredCalls` (from `Session.Loop`) and `hasBackgroundCalls` /
  `backgroundCalls` (from `Session.Step`) moved here; `Session.Loop` still
  re-exports `isBlockedOnDeferredCalls`.
- `SessionStore`:
  - `SessionLabels`, `SessionMeta`, `SessionQuery`, `VersionConflict`.
  - `SessionBackend` gains `sbStoreLabelled`, `sbLoadMeta`,
    `sbCompareAndStore`, and `sbQuery`. Every write increments the version;
    unconditional stores derive the status, except that a running status is
    kept.
  - SQLite: `runMigrations` with a component-scoped `schema_migrations`
    table; migration 2 adds the metadata columns and indexes and derives the
    status of existing rows. Compare-and-store is a single conditional
    statement with `RETURNING`.
  - File backend: `meta.<uuid>.json` sidecars.
  - Composite: writes and queries on the primary, loads with fallback.
  - `SessionCatalog` / `CatalogEntry`, `fileCatalog`, `backendCatalog`,
    `isFileBusy`.
- `StoreSessionProgress`: `sinkStoreCallback` and `agentPersistSession` take
  `SessionLabels`; `backendStoreCallbackWith`.
- `AgentFactory` labels sessions with the agent slug and, for sub-agents, the
  parent session. `OneShotTool` gives a sub-agent's session the ID of its
  conversation.
- Session tools (`SystemToolbox/Session`) read through
  `introspectionCatalog`; `getSessionModTime` removed. `AgentTree.Props`
  takes `sessionCatalog`; the CLI and TUI pass `fileCatalog`.
- `agents.cabal`: the library's `import: warnings` moved above `visibility`.
  Cabal ignored it in its old position, so the library was built without
  `-Wall -Werror` since the library was made public.

### Not done here

- The search index (`Session/Search`) stays on the file store (CLI-only).
- `PRAGMA` settings are left to `withHost` (Phase 5).

### Verification

- `agents-tests`: 923 tests pass, including 21 new `SessionMetadataTests`.
- The "forced full rebuild shows no warnings" claim made here (and in the
  Phase 3 commit message) was wrong: that rebuild did not recompile with
  `-Wall -Werror`. See the Phase 4 section for the real check and the fixes.

## Warning fixes after re-enabling `-Wall -Werror`

A rebuild of the library from an empty build directory under `-Wall -Werror`
reported warnings, now fixed:

- From Phases 2 and 3 (this branch): unused imports in
  `Tools/SystemToolbox/Session.hs`, `TUI/Event/Conversation.hs`,
  `CLI/SessionDurable.hs`, and `MCP/Server.hs`.
- Already present before this branch: a `name` binding shadowing a record
  field in `Session/Step.hs` (`lateResultsQuery`), and a missing signature on
  `resetQuitConfirmation` in `TUI/Event.hs`.

## Phase 4 — Continuation consistency ✅ COMPLETE

- `Session.Async`:
  - `ContinuationStore.csFindSession`: the session of a pending or completed
    token.
  - The continuation schema goes through `runMigrations` (component
    `continuations`); migration 2 adds a `(session_id, completed_at)` index.
  - `csComplete` detects the update with `RETURNING` instead of
    `SELECT changes()`.
- `Session.Wake`:
  - `wakeSessionWith` returns a `WakeOutcome` (applied / already completed /
    unknown tokens) and marks applied tokens completed in the store.
    `wakeSession` and `wakeSessionWithCache` wrap it.
  - `findSessionForToken` (index first, then a scan of the backend) and
    `sessionHasToken`.

### Not done here

- The CLI `complete` command keeps scanning the file store: CLI agents have
  no continuation store.

### Verification

- Library rebuilt from an empty build directory under `-Wall -Werror`:
  clean.
- `cabal build all --enable-tests` succeeded.
- `agents-tests`: 930 tests pass, including 7 new
  `ContinuationConsistencyTests`.

## Phase 5 — Session runner ✅ COMPLETE

- `System.Agents.Host`: `Host`, `HostConfig` / `defaultHostConfig`,
  `HostTrace`, `HostError`, `withHost` (WAL, busy timeout, migrations, agent
  files, sub-agent tools storing into the database).
- `System.Agents.Host.Runner`: `SessionRunner` with `createSession`,
  `postMessage`, `resume`, `completeCall`, `cancelRun`, `getSession`,
  `awaitRun`, `deleteSession` (dry run and cascade), `subscribe`,
  `recoverOnStartup`, `runnerStats`, and an idle-session reaper.
- `Session.Types`: `DeferredCallView` and `pendingDeferredCalls`; the CLI's
  `extractDeferredCalls` wraps them.
- `Session.Async`: `csCountSession` and `csDeleteSession`.

### Differences from the first version of the spec (spec updated)

- Results completed during a run are queued and applied by the run, instead
  of being written by `completeCall`, which would make the run's next
  versioned store conflict.
- Events use one runner-wide broadcast channel; `subscribe` returns a
  blocking "next event" action for one session, which survives eviction.
- `NoActiveRun` error; `csCountSession` for dry runs; deletion also refused
  while an ancestor runs; runner agents always run asynchronously.
- After a cancel, background calls below the head turn are reported as
  cancelled by the next run (late results), not by `cancelRun` itself.

### Verification

- Library under `-Wall -Werror`: clean. `cabal build all --enable-tests`
  succeeded.
- `agents-tests`: 940 tests pass, including 10 new `RunnerTests`. The runner
  tests passed 15 repeated runs. The test suite is not built with
  `-threaded`; the bundled SQLite is `THREADSAFE=1` (serialized), which the
  threaded server in Phase 6 relies on.

## Phase 6 — `agents-server` executable ✅ COMPLETE

- `examples/agents-server/`:
  - `src/AgentsServer/Api.hs`: the wai application (routing, JSON views,
    `wait`/`timeout`, SSE with keepalives and a shutdown flag).
  - `src/AgentsServer/Log.hs`: JSON-line logs on stderr, traces summarised
    field by field.
  - `src/AgentsServer/Server.hs`: CLI options, startup (host, runner,
    startup recovery), warp settings, SIGTERM/SIGINT handling.
  - `app/Main.hs`, `test/Main.hs`.
- `agents.cabal`: private library `agents-server-internal`, executable
  `agents-server`, test suite `agents-server-tests` (all threaded where it
  matters). wai 3.2.4 and warp 3.4.12 were already in the build plan, so
  nothing new is downloaded.
- `Host.Runner`:
  - Fixed `subscribe`: it filtered with `retry` after `readTChan`, which rolls
    the read back, so a subscriber blocked forever on the first event of
    another session. The Phase 5 tests only ever had one session emitting.
    New regression test `a subscriber skips other sessions' events` (fails on
    the old code: no events after 5 s).
  - New `subscribeSTM`, used by the events stream.
- `docs/agents-server.md`; links from `docs/durable-workflows-howto.md` and
  `README.md`; added to `extra-doc-files`.

### Differences from the first version of the spec (spec updated)

- The app code is a private sub-library so the test suite can run it.
- `SessionMetaView` is the `SessionMeta` JSON, so it includes `owner`
  (always null for now).
- Status codes: `202` exactly when the stored status is `running` at the
  time of answering, `200` otherwise; creation always `201`, with a
  `Location` header.
- In the events stream, a run's `session.updated` (running version) comes
  just before its `run.started`: the runner stores, then announces.
- Additions: `--live-session-ttl`, `--shutdown-grace`, `filename` on media,
  `mode` on resume, 404 `not_found`, 405, 413 (32 MiB bodies), list
  `limit` 1–500 (default 50), comma-separated `status`.
- On shutdown, waiting requests answer at once and event streams end, so the
  grace period is only for other requests.

### Verification

- `cabal build all --enable-tests` under `-Wall -Werror`: clean.
- `agents-tests`: 941 tests pass (one new runner test).
- `agents-server-tests`: 7 tests pass; 30 repeated runs plus 10 with
  `+RTS -N4`, no failures.
- Smoke tests of the binary against a fake OpenAI endpoint:
  - health, agents, and a created session answered by the LLM;
  - a deferred call completed over HTTP, with the events stream;
  - a restart on the same database, with the session still there;
  - SIGTERM with an open events stream: exit 0 in 0.01 s;
  - the API key reached the LLM but not the log.
- The guide's Haskell snippet typechecks.

## Phase 7 — Authentication and owners ✅ COMPLETE

- Spec: new "Milestone 2" section planning Phases 7–12, with the defaults
  taken recorded as decisions 5–10.
- `SessionStore`: `SessionQuery.sqOwner` (SQLite, file, composite), and
  migration 3 adding an `(owner, updated_at)` index.
- `Host.Runner`: `createSessionAs` (with an owner) and `sessionOwner`, which
  returns the owner of the root session.
- `agents-server`:
  - `AgentsServer.Auth`: loads the tokens file (`sha256` or plain `token`
    entries), compares tokens by SHA-256 digest, parses `Bearer` headers.
  - `--auth-tokens`: 401 `unauthorized` without a valid token (`/healthz`
    stays open). Every session endpoint checks the owner, answering 404 for
    another owner's session. Continuations check the owner of the token's
    session, answering 404 `unknown_token`. Listing filters by owner, or by
    an owned `parent`.
  - `server.started` logs `authentication: bearer|none`.
- `docs/agents-server.md`: an Authentication section.

### Not done here

- Per-owner API keys and a default isolation policy (see "Remaining later
  work" in the spec).

### Verification

- `cabal build all --enable-tests` under `-Wall -Werror`: clean.
- `agents-tests`: 941 pass. The delete test now also checks that a
  sub-session has its root's owner. The migrations test expects
  migrations 1–3.
- `agents-server-tests`: 9 pass, including the two-owner flow and loading a
  tokens file.

## Phase 8 — MCP over HTTP ✅ COMPLETE

- `AgentsServer.Mcp`: the Streamable HTTP transport, answering with JSON,
  one `ask_<slug>` tool per root agent, runs through the session runner.
- `AgentsServer.Api`:
  - routes `POST /mcp` (405 for other methods);
  - refuses non-loopback `Origin`s when authentication is off (403
    `forbidden_origin`), as the MCP transport requires against DNS rebinding;
  - `waitForRun` is shared by the REST and MCP handlers.
- `docs/agents-server.md`: an MCP over HTTP section, and the origin rule.

### Verification

- `agents-server-tests`: 12 pass. New tests: an MCP flow
  (initialize with version negotiation, the notification's 202, tools/list,
  tools/call with `_meta.session_id`, unknown tool, unknown method, batch,
  GET 405), a deferred call reported through MCP then completed over REST,
  and origin checks with and without authentication.
- The official Python MCP SDK (2.2.0, `streamable_http_client`) against the
  binary and a fake OpenAI endpoint: initialize (2025-06-18), list tools,
  call `ask_weather`, which reported the deferred call and its token.

## Phase 9 — TUI out of the core library ✅ COMPLETE

- `git mv` of 25 modules from `src/` to `tui/`: `TUI.*` except
  `ToolCallActivity`, plus `CLI.TUI`, `CLI.Config`, and `CLI`.
- `agents.cabal`: new public library `agents-tui`. `agents-lib` loses
  those modules and `brick`, `vty`, `text-zipper`, `data-clist`.
  `agents-exe` adds `agents-tui`.
- `docs/architecture.md`: a "Libraries and executables" table.

### Verification

- `cabal build all --enable-tests`: clean. The build plan shows brick, vty,
  and text-zipper only under `agents-tui`, and nowhere in `agents-lib`'s
  transitive closure.
- `cabal test all`: 941 + 12 pass. `agents-exe --help` runs.

## Phase 10 — Postgres backend ✅ COMPLETE

- `agents-postgres` library and `agents-postgres-tests` suite (see the spec).
- `Host.withHostStores` / `HostStores`; `withHost` uses it.
- `agents-server --db postgresql://…`; `redactDatabase` for the log.
- Docs: a Postgres section in `docs/agents-server.md`, and the library in
  the table in `docs/architecture.md`.

### Verification

- `agents-postgres-tests`: 5 pass against a throwaway Postgres 16 cluster.
- Smoke test of the binary on a Postgres 16 cluster:
  - created a session blocked on a deferred call, restarted the server, and
    completed the call: the session reached `idle`, with consistent rows;
  - the URL's password is absent from the log, which holds only JSON lines.

## Phase 11 — Token streaming ✅ COMPLETE

- `LLMs/OpenAIStream.hs` (new), `HttpClient.postStream`,
  `OpenAI.callLLMPayloadStreaming` and `withOverloadedRetry`,
  `OpenAICompletionConfig.cfgOnTextDelta`, `AgentDeps.adOnTextDelta`,
  `HostConfig.hcStreamTokens`, `SessionEvent.TextDelta`,
  `agents-server --stream-tokens`.
- Docs: a "Streaming answers" section and the `text.delta` event in
  `docs/agents-server.md`.

### Verification

- `agents-tests`: 5 new `OpenAIStreamTests`.
- `agents-server-tests`: 13 pass, including streaming against a fake
  streaming endpoint.
- The non-streaming path through the binary still works (fake endpoint,
  deferred call reported).

## Phase 12 — Agents from the database ✅ COMPLETE

- `AgentStore` (new), `AgentTree.loadAgentTreeFromConfig`, stored agents in
  `Host`, `mkPostgresAgentStore`, `/v1/agents/:slug` endpoints,
  `--admin-owners`. See the spec for the differences from the plan.
- Docs: a "Storing agents" section in `docs/agents-server.md`.

### Verification

- `agents-tests`: 947 pass (a new runner test for stored agents).
- `agents-server-tests`: 15 pass.
- `agents-postgres-tests`: 6 pass.
- Smoke test of the binary with authentication:
  - a non-admin PUT is refused (403); an admin PUT stores an agent whose MCP
    server is `agents-exe mcp-server`, and its tool
    (`mcp_weather_ask_weather_000`) is listed;
  - after a restart the agent is reloaded from SQLite with its tool;
  - no tokens in the logs, and no MCP server process left after shutdown.
