# agents-exe feature test agents

This directory contains a curated set of official agent configurations whose
purpose is to **empirically exercise** the major toolbox and orchestration
features of `agents-exe`.  Each agent is focused on one feature area and is
meant to be invoked from the project root with a command like:

```bash
agents-exe tui --agent-file agents/test/file-system-tester.json
```

or, if you have configured an `agents-exe.cfg.json` that points at this
directory:

```bash
agents-exe tui --agent file-system-tester
```

## Agent catalogue

| Agent | Feature under test | Key toolboxes |
|-------|--------------------|---------------|
| `file-system-tester` | File / system toolboxes | `SystemToolbox`, `DeveloperToolbox`, bash read/write tools |
| `sqlite-tester` | SQLite toolboxes | `SqliteToolbox` (direct read-write + versioned) |
| `progressive-disclosure-tester` | Progressive disclosure / activation | bash tools with `on-demand` activation |
| `openapi-tester` | OpenAPI → tool mapping | `OpenAPIToolbox` against a local spec + mock server |
| `postgrest-tester` | PostgREST → tool mapping | `PostgRESToolbox` against a local spec + mock server |
| `lua-tester` | Lua toolbox & tool exposition | `LuaToolbox` calling `tools.list()` and other tools |
| `lua-rlm-recursion-tester` | Symbolic recursive tool calls in Lua | `LuaToolbox` calling itself, `SqliteToolbox` as external memory |
| `introspection-tester` | Session introspection | `SystemToolbox` session capabilities |

## Common model configuration

All agents use the same provider block as the production agents in
`agents/`:

```json
"flavor": "KimiV1",
"modelUrl": "https://api.moonshot.ai/v1",
"apiKeyId": "kimi",
"modelName": "kimi-k2.7-code"
```

Replace these values if you want to run the tests against a different
OpenAI-compatible endpoint.

## How to use a test agent

1. Make sure `agents-exe` is built:

   ```bash
   cabal build agents-exe
   ```

2. Start the mock API server if you plan to test OpenAPI or PostgREST
   mappings (see below).

3. Run the agent and ask it to perform its smoke test.  The system prompt of
   each agent tells it exactly what to exercise.  A typical invocation:

   ```bash
   agents-exe tui --agent-file agents/test/sqlite-tester.json
   # then type: "run the sqlite smoke test"
   ```

4. The agent should report:
   - which tools it sees in its context,
   - the exact tool calls it made,
   - the results returned,
   - any errors encountered.

## Mock server for OpenAPI / PostgREST tests

The agents `openapi-tester` and `postgrest-tester` load their specs from
local files but issue real HTTP calls against a tiny Python mock server.
Start it from the repository root:

```bash
python3 agents/test/fixtures/mock-server.py
```

By default it binds two ports:

- `8765` — serves the OpenAPI sample API (`/openapi.json`, `/items`, `/items/{id}`)
- `3000` — serves the PostgREST-style sample API (`/`, `/tasks`, `/tasks/{id}`)

You can also start only one of the two:

```bash
python3 agents/test/fixtures/mock-server.py --openapi-port 8765
python3 agents/test/fixtures/mock-server.py --postgrest-port 3000
```

The mock server uses only the Python standard library (`http.server`) so no
third-party packages are required.

## Adding new test agents

When adding a new test agent:

1. Keep it focused on a single feature.
2. Re-use existing tool directories where possible so no new executable
   tools need to be created.
3. Put any required static fixtures under `agents/test/fixtures/`.
4. Document the agent in the table above and mention any external
   prerequisites (running servers, environment variables, etc.).
5. Run `agents-exe check --agent-file agents/test/<new-agent>.json` to verify the
   configuration loads before trying to chat with it.
6. Add the agent to `test/OfficialAgentsTests.hs` so CI ensures the JSON
   remains parseable.

## Notes on empirical testing

These agents are intentionally **not** unit tests.  They are meant to be run
against a real LLM so that we can observe:

- whether the LLM receives correct tool schemas,
- whether it produces valid tool-call payloads,
- whether the tool runtime returns sensible results,
- how activation / progressive disclosure behaves across turns,
- whether introspection capabilities expose the expected session history.

Because they hit real services, results may vary by model provider and by
the current project state (e.g. existing sessions, files on disk).  Always
review the tool outputs rather than trusting the LLM summary alone.

