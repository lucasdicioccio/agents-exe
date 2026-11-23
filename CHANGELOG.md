# Revision history for agents

## 0.2.0.0 -- Unreleased

### New Features

#### agents-server: favicon
- `agents-server` and `agents-exe serve` serve the project's icon (the website's `favicon.png`, embedded in the binary) at `GET /favicon.ico` and `GET /favicon.png`, without a token; the chat page links it with `<link rel="icon">`
- The icon (website and server) is now a 64x64 reduction of the agents-exe logo (`logo-01.png`, the head on a transparent background) instead of the salmon icon

#### Bash tools v2: runtime for oneshot tools
- A v2 bash tool may declare a `runtime` object: `envdir` (read the daemontools way under the operator's `--envdir-root`), `environment`, `rundir`, `timeout.start` (the call's deadline) and `timeout.stop`, `cancel` (`SIGTERM` or `SIGINT`), `run_as` (enforced with `--run-as-mechanism setpriv|sudo|runuser`) and `sandbox` (`fs.read`/`fs.write`, `net`, `tmp`; enforced with `--sandbox bwrap|auto`)
- A tool that declares what the operator did not configure fails to load, naming the field; so does an unknown `runtime` field. Still refused at load: `timeout.idle`, `on_exit`, `cancel` by `send` (service only) and `--sandbox landlock` (not implemented yet)
- Library: `ScriptInfo.scriptRuntime` is now a parsed `ScriptRuntime`; new module `System.Agents.Tools.ScriptRuntime`; `loadScriptWith` and `runValueWith` in `System.Agents.Tools.Bash` take the operator configuration explicitly; `RunScriptError` has `ScriptRuntimeError` and `ScriptTimeoutError`
- See `documentation/binary-tool.md` (Protocol v2: runtime) and `todos/bash-tools-v2.md`

#### Bash tools v2: protocol 2 and framed oneshot output
- A bash tool whose `describe` says `"protocol": 2` may declare a `process` object; its output is cut into events (`output.framing`: `jsonl` by default, `lines`, `chunks`) and, under `jsonl`, validated against the declared `output.event` JSON Schema. The model receives a JSON array of events; a line that does not parse or match is kept as `{"invalid": {"raw", "error"}}`
- Each event is reported as progress as it arrives (`{"stream": "stdout", "event": …, "events": n}`)
- Not implemented yet, and refused at load rather than ignored: the `stream` and `service` modes, `process.input`, `runtime`. v1 tools are unchanged
- Library: `ScriptInfo` has four more fields (`scriptProtocol`, `scriptProcess`, `scriptRuntime`, `scriptCheck`); new module `System.Agents.Tools.ScriptProcess`; `runProcessReportingEvents` in `System.Agents.Tools.Bash`
- See `documentation/binary-tool.md` (Protocol v2) and `todos/bash-tools-v2.md`

#### Agent templates: `sqlite-toolbox`
- The `agents` template library builds a SQLite toolbox entry: `sqlite-toolbox({name, versioning, description, activation})`, with `sqlite-read-only(path)`, `sqlite-read-write(path)` or `sqlite-versioned({basePath, shared, storageRoot, retention})` as its `versioning`
- `name`, `versioning`, `basePath` and `shared` are required; `description` defaults to "SQLite database"
- See `documentation/agent-templates.md`

#### Versioned SQLite: documentation and behavioural tests
- `documentation/tools.md` (SQLite Tools) now describes `SqliteVersioned` in full: the access modes, the sharing scopes, where snapshots are stored, version handles and `restore_from`, and promotion
- Tests cover write, handle, restore and branch, promotion, and what each sharing scope sees (`test/SqliteVersionedBehaviourTests.hs`)

#### Versioned SQLite: snapshot retention policy
- A `SqliteVersioned` config takes an optional `retention`: `{"maxSnapshots": N, "maxAgeDays": D}`. After each write the toolbox deletes the snapshots beyond the newest N, or older than D days, across all sessions; the snapshot just written and the live heads are kept
- Default (no `retention`): every snapshot is kept, as before
- Library: `SqliteVersioned` has a fourth field `vRetention :: SqliteRetention` (`keepAllSnapshots` for the old behavior); `pruneSnapshots`, `scanSnapshotsTimed` and two `Trace` constructors in `System.Agents.Tools.SqliteToolbox`
- See `documentation/tools.md` (SQLite Tools)

#### Versioned SQLite: list the stored versions
- A `SqliteVersioned` toolbox exposes `sqlite_{name}_versions`: the stored snapshots, most recent first, each with a `versionHandle` to pass as `restore_from` and `is_head` for the current head of the caller's sharing scope. Options: `all_sessions` (default: the calling session only), `limit` (default 20)
- Read from the snapshot directory, so snapshots written before a restart are listed too
- Library: `listVersions`, `scanSnapshots`, `ListVersionsInput`, `ListVersionsResult` in `System.Agents.Tools.SqliteToolbox`
- See `documentation/tools.md` (SQLite Tools)

#### Mailbox extensions
- `send-to-tool-call`, a System Toolbox capability: the model writes to the sub-agent that one of its running `prompt_agent_<slug>` calls started, naming the call (`tool_call_id`) instead of a session. Under the runner (TUI, `agents-server`) a sub-agent call now records its child session, so its placeholder shows `childSessionId` there too; in `run`, an in-tool sub-agent is addressable by mail while it runs
- Notify progress level: a tool that reports `{"level": "notify", "message": "…"}` (a bash tool: a `::notify:: …` line on standard error) is also delivered to the session as mail, at most 16 times per call. It wakes a session that idles on its background calls, or `wait`, while the call goes on running
- A session that idles on background calls now reads its mail when it arrives (a message from the user or another session), instead of when a call ends
- Wrappers match on arguments: `"match": {"tool": "bash_*", "args": {"path": "command", "glob": "rm *"}}`, with `equals`, `glob`, `exists`, `all`, `any`, `not`. A predicate that cannot be read is a loading error
- Library: `WrapperMatch` is a record with a second field `wmArgs`; `MailBody` has a `ToolCallNotice` constructor (JSON tag `toolCallNotice`); `SystemToolCapability` has `SystemToolSendToToolCall`
- See `documentation/async-tool-calls.md` (Wrappers, Tool-call capabilities, Progress)

#### Tunable `spectate` display
- `agents-exe spectate --panels SPEC` chooses the panels shown, their places and sizes (`tree:60+tools:40/45,text/55` is the default screen; `tree,tools,text` is three columns); `--refresh SECONDS` sets the time between two refreshes
- Keys change the same things while running, as in `top`: `1` `2` `3` show or hide a panel, `Tab` picks one, `<` `>` move it, `[` `]` and `-` `+` resize it, `s` restacks it, `d` `D` change the refresh interval, `0` goes back to the layout at start, `?` lists the keys and the flags that reproduce the screen
- `W` saves the layout to `~/.config/agents-exe/spectate-layout` (`--layout-file` for another file), read at the next start; the flags go over it
- The screen is now redrawn once per refresh interval rather than on every event
- See `documentation/cli-commands.md` (spectate)

#### Background tool calls run again after a restart (opt-in)
- A new decorator, `{"tag": "rerunOnRestart", "times": 1}`, set in a `wrappers` rule of `toolCallPolicyConfig`, marks a tool as safe to run twice
- A background call of such a tool that was running when its process went away is started again by the next process, from the beginning, with the same arguments and `tool_call_id`, at most `times` times; its late result says it ran again
- Every other call is reported as orphaned, as before: nothing runs twice unless asked
- `agents-server` and the TUI resume the sessions concerned at startup (log line `sessions.resumed_for_reruns`), and a graceful shutdown leaves them stored as `running` instead of cancelling their calls
- A call is run again, never resumed where it stopped; a cancelled call is not started again
- Library: `Decorator` has a `WithRerunOnRestart` constructor; `HostTrace` has `HostResumedForReruns`
- See `documentation/async-tool-calls.md` (Across a restart)

#### Terminal UI screenshots and end-to-end tests
- `agents-tui-e2e`, an opt-in test-suite (`cabal test agents-tui-e2e -ftui-e2e`), drives the real `agents-exe tui` in a pseudo-terminal with [tuispec](https://github.com/Tritlo/tuispec) against a scripted OpenAI-compatible endpoint: launch, send a message and read the reply, answer a deferred call from the Pending panel
- Each step is compared with a baseline under `test/tui-snapshots/`; `AGENTS_TUI_E2E_PNG=1` renders the screenshots `documentation/tui.md` now shows
- See `documentation/tui.md` (Screenshots and end-to-end tests)

#### `agents-exe new agent` bootstrap defaults
- A new agent can read, edit and list files under `./` (a `workspace` file sandbox shared by its developer and system toolboxes) and has a read-write SQLite memory (`./{slug}-memory.sqlite`); before, its file sandbox denied everything
- After creating the agent, reports whether `agents-exe.cfg.json` loads it and offers to add it to `agentsFiles` (`--add-to-config`, `--no-add-to-config`)
- A `DirectoryRecursive` predicate written with a trailing slash (`./`, `./src/`) now allows the directory itself, not only its contents
- The system toolbox's `list-directory` now honours a configured `FileSandbox` even when `attach-file` is not enabled; without a sandbox it stays unrestricted
- `documentation/cli-commands.md` documents the `config` command

#### `agents-exe spectate`
- A read-only live dashboard of a running `agents-exe serve`/`agents-server` (`--attach URL|PATH`): the session and sub-agent tree, the tool calls running and recently finished, and the text of one session
- Library: the model is `System.Agents.Spectate`, a fold over the runner's events
- See `documentation/cli-commands.md` (spectate) and `documentation/agents-server.md` (Watching with spectate)

#### `agents-server`: several servers on one Postgres database
- A session's run holds a lease (`run_owner`, `run_lease_until`, Postgres migration 4), renewed while the run lasts; another server takes the session over once its owner stops renewing
- While another server holds the lease the session is busy: messages and completed calls go to its mail, resume and delete are refused, cancel posts `StopRun`
- Library: `System.Agents.Host.Coordination` (`noCoordination` for a single process)
- See `documentation/agents-server.md` (Several servers on one database)

#### `agents-server`: stored agents with bash tool files and helpers
- `PUT /v1/agents/:slug` stores the files of an agent's bash tools (a `files` object; `files` column, migration 2), written to a directory of their own at each load
- `extraAgents` of a stored agent name other stored agents by slug; an unknown helper, a cycle, or a path that leaves the agent's directory is refused, and a helper in use cannot be deleted
- See `documentation/agents-server.md` (Storing agents)

#### `agents-server`: API keys per owner, isolated tool calls
- `--owner-api-keys OWNER=FILE` (repeatable, with `--auth-tokens`): an owner's sessions, sub-sessions included, use that owner's keys file
- `--isolate-tools docker:IMAGE|process:PATH`: bash and MCP tool calls run in a worker, with no in-process fallback; off by default
- A helper that is not also a root agent runs as a session of its own
- See `documentation/agents-server.md` (API keys per owner, Isolating tool calls)

#### Named file sandboxes
- An agent may declare file sandboxes once, by name, in `fileSandboxes`; a builtin toolbox (System, Developer, Lua) refers to one with `"FileSandbox": {"ref": "<name>"}`
- The inline `FileSandbox` form is unchanged; an undeclared name is a loading error, also reported by `validate-agent`
- `sandbox-ref(name)` in the `agents` template library
- See `documentation/tools.md` (Named Sandboxes)

#### Agent templates
- An agent file may be a tramaj program (`.tramaj`), evaluated once at load to the JSON an agent file holds
- `$ctx` is the process parameters (`--set`, `--pin`, `--params-file`); a template may only read process-scope, non-secret parameters the agent declares
- `tramajLibraries` in `agents-exe.cfg.json` lists directories of shared libraries; a built-in `agents` library builds sandboxes and toolboxes
- `agents-exe check --show-config` prints the evaluated JSON
- See `documentation/agent-templates.md`

#### `agents-exe paths` Command
- Added new diagnostic command `agents-exe paths` to show all important configuration paths
- Displays config file location, agent files, API keys file, and session storage directory
- Supports `--json` flag for machine-readable output
- Helps users debug configuration issues when running outside a project

#### Session Storage Fix
- Fixed bug where conversation JSON files were empty when running outside a project
- Sessions are now properly stored in `~/.config/agents-exe/sessions/` when no project config is found
- The sessions directory is created automatically on first use

#### `--agent <slug>` Selection
- Added `--agent <agent-slug>` (or `-a <slug>`) command line argument
- Allows selecting agents by their slug instead of full file paths
- Example: `agents-exe run --agent openai-assistant -p "Hello"`
- Provides helpful error messages listing available agents when slug is not found

#### `--alias <name>` Prompts
- Added `--alias <name>` command line argument for well-known prompts
- Predefined aliases available:
  - `translate`: Translate text to English
  - `summarize`: Summarize text content
  - `code-review`: Review code for issues
  - `explain`: Explain code in plain English
- Aliases can be configured in `agents-exe.cfg.json`
- Supports template variables: `{{content}}`, `{{language}}`, `{{filename}}`
- Auto-detects programming language from file extension

### Bug Fixes

- The Lua toolbox's `MaxMemoryMB` is now enforced: a script that allocates past the limit fails with "Lua script exceeded the memory limit of N MB" instead of growing until the host runs out of memory
- Background tool calls: a result is sent to the model once. In a session with a mailbox (TUI, one-shot, server) each finished call was also rendered as mail, so its result appeared a second time; and a result the model had read with `get-tool-call-status` was repeated by the delivery notice, which now says `result already read with get-tool-call-status`
- `cancel-tool-call` (and `CancelCalls` mail) stops a call's thread even when the session was resumed with a newly built agent; before, it only marked the call cancelled and the thread kept running
- `Session.Loop.run` and `runWithProgress` no longer loop forever on a turn that only waits on deferred calls: they throw `BlockedOnDeferredCalls`, which carries the session
- `runAsyncKeepingAgent` / `runAsyncWithProgressKeepingAgent` return the agent holding the engine that owns the calls still running at a pause
- See `documentation/async-tool-calls.md`
- TUI: text sent twice within a second while a session is busy is all kept in the draft; before, each send appended to the copy of the draft the conversation list had last drawn, so the earlier paragraph could be lost, `Ctrl+D` could answer "No draft to clear" for a draft just made, and a message sent right after a run stopped could stay in a draft that nothing posted
- TUI: token counts of 10 and more are shown with their digits in order (`12 tokens`, `1,234`), not reversed within each group of three (`21`, `1,432`)
- TUI: the Help tab says how a run paused with `Ctrl+B` is resumed (`Ctrl+B` again); it named `Ctrl+C`, which does not resume
- Versioned SQLite toolboxes: a `restore_from` handle that names no snapshot (missing, malformed, or for another toolbox) is an error for both the query and the promote tool; before, the call quietly ran on, or promoted, the base database

### Improvements

- Better experience when running `agents-exe` outside of a project directory
- Default configuration structure is automatically created in `~/.config/agents-exe/`
- Example agents (openai-assistant, mistral-assistant, ollama-assistant, orchestrator) created automatically
- Background tool calls are traced: `tool_call.queued`, `.started`, `.progressed`, `.completed`, `.failed`, `.timed_out` and `.cancelled` lines, each with the session id and the call id, in the server's JSON log and in the `--log-json-file` / `--log-file` of `tui`, `run` and `mcp-server`. See `documentation/async-tool-calls.md` (Logs)
- Library: `Agent` has a `ctxAsyncTracer` field, and `mkAsyncEngine` / `mkAsyncEngineSharing` take a `Tracer IO AsyncTrace` as first argument (`Prod.Tracer.silent` for none); `AgentFactory.Trace` and `MCP.Server.Trace` have a new `AsyncToolCallTrace` constructor

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.

