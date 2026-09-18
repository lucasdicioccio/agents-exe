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

- `cabal build all --enable-tests`: no new warnings.
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
  `-Wall -Werror` since the library was made public. A forced full rebuild
  shows no warnings.

### Not done here

- The search index (`Session/Search`) stays on the file store (CLI-only).
- `PRAGMA` settings are left to `withHost` (Phase 5).

### Verification

- Forced full rebuild of the library with `-Wall -Werror`: no warnings.
- `agents-tests`: 923 tests pass, including 21 new `SessionMetadataTests`.
