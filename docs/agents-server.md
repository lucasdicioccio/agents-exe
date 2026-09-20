# agents-server: agents over HTTP

`agents-server` runs agents as a long-lived HTTP service. Sessions live in a
SQLite database rather than in files, so a web application can start
conversations, follow them live, complete deferred tool calls from its own
workers, and pick sessions up again after a restart.

It uses the same agent files as `agents-exe`, and the same durable-session
machinery as the `session` CLI commands (see
[durable-workflows-howto.md](durable-workflows-howto.md)): an agent can pause on
deferred tool calls, and the server resumes it when the results arrive.

> **Authentication is off by default.** Without `--auth-tokens`, anyone who
> can reach the server can run its agents, with its API keys. It binds to
> `127.0.0.1` by default. See [Authentication](#authentication) before exposing
> it.

---

## Running the server

```bash
cabal build agents-server
cabal run agents-server -- \
    --agent-file ./weather.json \
    --api-keys ./secrets/keys.json \
    --db ./agents-server.db \
    --port 8080
```

| Flag | Default | Meaning |
|---|---|---|
| `--agent-file FILE` | (required) | A root agent file. Repeat it to serve several agents; each is addressed by its `slug`, which must be unique. |
| `--api-keys FILE` | (required) | The API keys file, as for `agents-exe`. |
| `--db FILE\|URL` | `agents-server.db` | Where sessions and continuation tokens live: a SQLite file, or a `postgresql://` URL (see [Postgres](#postgres)). Created or migrated on start. |
| `--bind HOST` | `127.0.0.1` | Address to listen on. |
| `--port PORT` | `8080` | Port to listen on. |
| `--live-session-ttl SECONDS` | `900` | How long an idle session keeps its in-memory state (including background tool calls) before it is dropped. It is reloaded from the database on next use. |
| `--shutdown-grace SECONDS` | `10` | How long open requests get to finish on shutdown. |
| `--auth-tokens FILE` | (none) | Bearer tokens and their owners. See [Authentication](#authentication). |
| `--admin-owners OWNER,…` | (none) | Owners allowed to store and delete agents over the API. Needs `--auth-tokens`. See [Storing agents](#storing-agents). |
| `--stream-tokens` | off | Stream LLM answers: the events stream gets `text.delta` events as the text arrives. See [Streaming answers](#streaming-answers). |
| `--no-ui` | off | Do not serve the chat page at `/`. See [Finding your way around](#finding-your-way-around). |

Sub-agents work as they do elsewhere: their sessions are stored in the same
database, linked to the parent session.

### Finding your way around

The server describes itself, so a client that knows only its URL can start
using it:

| Path | What it is |
|---|---|
| `GET /` | A small chat page, for a person at this machine. Served on a loopback bind, or when `--auth-tokens` is on; `--no-ui` turns it off. |
| `GET /openapi.json` | The whole API as an OpenAPI 3 document, generated from the routes. |
| `GET /healthz` | Liveness and counters. |

None of the three needs a bearer token: they describe the server, not its
sessions or its agents.

The OpenAPI document is the machine-readable reference, and it is generated
from the same servant types that route the requests, so it cannot drift from
the server. An agents-exe agent can use it directly as a toolbox:

```json
"openApiToolboxes": [
  {"tag": "OpenAPIServer", "contents": {"SpecUrl": "http://127.0.0.1:8080/openapi.json"}}
]
```

The chat page is one self-contained HTML document with no build step and no
assets. It starts sessions, follows their event streams, offers a box to
complete deferred tool calls, and attaches files, so it doubles as a worked
example of the API.

**Attachments.** *Attach* adds files to the next message, up to 20 MB in
total (the body limit is 32 MiB and base64 adds a third). They are sent as
the `media` field described under
[API reference](#api-reference), and the agent's model must accept them.

A PDF is sent as a `file` content part; **everything else is sent as an
image**. So images work, and a text, CSV or JSON attachment reaches the
provider as a malformed image and is refused (`invalid image input`, or the
provider's equivalent). Attach images and PDFs.

### An agent for the examples

The examples below use an agent that defers every tool call to an external
worker:

```json
{
  "tag": "OpenAIAgentDescription",
  "contents": {
    "slug": "weather",
    "apiKeyId": "openai",
    "flavor": "OpenAIv1",
    "modelUrl": "https://api.openai.com/v1",
    "modelName": "gpt-4o-mini",
    "announce": "answers weather questions",
    "systemPrompt": ["You answer weather questions. Use get_weather."],
    "builtinToolboxes": [],
    "mcpServers": [],
    "executionMode": "asynchronous",
    "toolCallPolicyConfig": {
      "default": {"tag": "defer", "reason": "weather service"},
      "rules": []
    }
  }
}
```

Agents without a `toolCallPolicyConfig` run all their tools in the server
process and never wait on the outside world. The server always runs agents
in asynchronous mode.

---

## A first conversation

Ask a question and wait for the run to stop:

```bash
curl -s -X POST 'localhost:8080/v1/sessions?wait=true' \
     -d '{"agent": "weather", "prompt": "Weather in Paris?"}'
```

The LLM called `get_weather`, which the policy defers, so the session is
waiting on the outside world (turns elided):

```json
{
  "session_id": "4ed40545-d192-4230-aed0-39cd09117123",
  "agent": "weather",
  "parent_session_id": null,
  "owner": null,
  "status": "waiting_external",
  "status_detail": null,
  "version": 5,
  "created_at": "2026-09-19T07:31:55.703930717Z",
  "updated_at": "2026-09-19T07:31:55.713658207Z",
  "session": {"turns": ["…"]},
  "pending": [
    {
      "tool_call_id": "017bb633-3306-4f95-8512-534427a4c030",
      "continuation_token": "0dd070c1-eada-4669-9755-bb75a2783df1",
      "tool": "get_weather",
      "disposition": {"tag": "defer", "reason": "weather service"},
      "call": {
        "id": "call_1",
        "type": "function",
        "function": {"name": "get_weather", "arguments": "{\"city\": \"Paris\"}"}
      }
    }
  ]
}
```

A worker computes the result and posts it with the continuation token. The
session resumes on its own; with `wait=true` the answer holds the LLM's
final turn:

```bash
curl -s -X POST 'localhost:8080/v1/continuations/0dd070c1-eada-4669-9755-bb75a2783df1?wait=true' \
     -d '{"result": "sunny, 24°C"}'
# 200, "status": "idle", "pending": []
```

Continue the conversation with another message:

```bash
curl -s -X POST 'localhost:8080/v1/sessions/4ed40545-…/messages?wait=true' \
     -d '{"prompt": "And tomorrow?"}'
```

---

## Sessions and runs

A **session** is one conversation. A **run** steps it in the background: it
calls the LLM, runs or defers tool calls, and stops when the LLM answers,
when only deferred calls remain, or after one step in `step` mode. A session
has at most one run at a time.

Each session has a `status`:

| Status | Meaning | What moves it on |
|---|---|---|
| `ready` | Can progress: a prompt or tool results are waiting for the LLM. | `resume` |
| `running` | A run is active. | Wait for it, or `cancel` it. |
| `waiting_external` | Only deferred calls remain. | Post their results to `/v1/continuations/:token`. |
| `idle` | The LLM answered. | Post a new message. |
| `failed` | The last run failed; `status_detail` says why. | `resume` retries from the last stored version. |

Every stored change increments the session's `version`.

### Waiting for a run

Endpoints that can start a run (create, messages, resume, continuations)
take two query parameters:

* `wait=true|false` (default `false`). Without waiting, the server answers
  as soon as the run has started. With `wait=true`, it answers when the run
  stops.
* `timeout=SECONDS` (default `120`, at most `600`), used with `wait=true`. When
  it expires, the server answers with the current state (`status: "running"`)
  and the run carries on.

The answer is always the session as it is at that moment, including its
pending deferred calls. Creation answers `201`. The other endpoints answer
`202` while a run is still going and `200` otherwise. A client that
disconnects while waiting does not cancel the run.

How far a run goes is set in the body: `run` on create and messages (`none`,
`step`, or `until_blocked`, the default), `mode` on resume (`step` or
`until_blocked`). With `run: "none"`, the session is stored as `ready` and
nothing runs until `resume`.

### Deferred results arriving during a run

A result posted while a run is active is checked and queued, and the run
applies it before its next step. If the result makes the session able to
progress, a run about to stop keeps going instead. Posting with
`"resume": false` stores the result without starting a run; `resume` later.
While a run is active the result is queued for it, and `resume` is ignored:
the run applies it either way.

---

## Following a session live

`GET /v1/sessions/:id/events` is a
[server-sent events](https://html.spec.whatwg.org/multipage/server-sent-events.html)
stream. It first sends a `snapshot` of the session's metadata, then one event
per change:

| Event | Data |
|---|---|
| `snapshot` | Session metadata (the object above without `session` and `pending`). |
| `session.updated` | Session metadata after a stored change, plus `head_turn`: the newest turn. |
| `run.started` | `{session_id, mode}` |
| `calls.deferred` | `{session_id, calls}`: the run stopped on these deferred calls. |
| `run.stopped` | `{session_id, status}` |
| `session.failed` | `{session_id, message}`, followed by `run.stopped` with status `failed`. |
| `text.delta` | `{session_id, text}`: the next piece of the LLM's answer, with `--stream-tokens` only. |

A typical run, from a `resume`:

```
event: snapshot
data: {"session_id":"4ed4…","status":"ready","version":1,…}

event: session.updated
data: {"session_id":"4ed4…","status":"running","version":2,"head_turn":{…},…}

event: run.started
data: {"mode":"until_blocked","session_id":"4ed4…"}

event: session.updated
data: {"session_id":"4ed4…","status":"running","version":3,"head_turn":{…},…}

event: calls.deferred
data: {"session_id":"4ed4…","calls":[{"continuation_token":"0dd0…",…}]}

event: run.stopped
data: {"session_id":"4ed4…","status":"waiting_external"}
```

### Streaming answers

With `--stream-tokens`, the server asks the LLM for a streamed answer
(`"stream": true`) and forwards each piece of text as a `text.delta` event,
before the answer is stored. Concatenating a step's deltas gives the text of
the LLM turn that the following `session.updated` carries. Tool calls
are not streamed: they appear in the stored turn as usual. Sub-agents do not
stream.

The option applies to every agent of the server. It needs an endpoint that
supports streaming: OpenAI and most OpenAI-compatible APIs do. For the
`OpenAIv1` flavor the server also asks for token usage in the last chunk
(`stream_options.include_usage`).

The stream stays open across runs. It sends a `: keepalive` comment after 15
seconds without events. Events are not replayed: a client that reconnects
gets a new snapshot and continues from there.

---

## API reference

All bodies are JSON. Errors are `{"error": "<code>", "message": "<text>"}`.

| Method and path | Body | Success | Errors |
|---|---|---|---|
| `GET /` | | `200 text/html` chat page | 404 when the page is off |
| `GET /openapi.json` | | `200` OpenAPI 3 document | |
| `GET /healthz` | | `200 {ok, live_sessions, active_runs}` | |
| `POST /mcp` | JSON-RPC message or batch | `200` JSON-RPC answer, or `202` | see [MCP over HTTP](#mcp-over-http) |
| `GET /v1/agents` | | `200 [{slug, description, tools, source, …}]` | |
| `GET /v1/agents/:slug` | | `200` agent | 404 `unknown_agent` |
| `PUT /v1/agents/:slug` | agent configuration | `201` (new) or `200` agent | 403 `agent_edits_disabled` / `forbidden`, 400 `agent_uses_files` / `agent_failed_to_load` / `bad_request`, 409 `agent_defined_by_file` |
| `DELETE /v1/agents/:slug` | | `200 {deleted}` | 403, 404 `unknown_agent`, 409 `agent_defined_by_file` |
| `POST /v1/sessions?wait=&timeout=` | `{agent, prompt, media?, run?}` | `201` session, with a `Location` header | 404 `unknown_agent`, 400 `bad_request` |
| `GET /v1/sessions?agent=&status=&parent=&limit=&before=` | | `200 {sessions, next_before}` | 400 `bad_request` |
| `GET /v1/sessions/:id` | | `200` session | 404 `unknown_session` |
| `POST /v1/sessions/:id/messages?wait=&timeout=` | `{prompt, media?, run?}` | `202` or `200` session | 404, 409 `run_in_progress`, 409 `not_accepting_messages` |
| `POST /v1/sessions/:id/resume?wait=&timeout=` | `{mode?}` or no body | `202` or `200` session | 404, 409 `run_in_progress` |
| `POST /v1/sessions/:id/cancel` | | `200` session metadata | 404, 409 `no_active_run` |
| `GET /v1/sessions/:id/pending` | | `200 {calls}` | 404 |
| `GET /v1/sessions/:id/events` | | `200 text/event-stream` | 404 |
| `POST /v1/continuations/:token?wait=&timeout=` | `{result, resume?}` | `202` or `200` session | 404 `unknown_token`, 409 `token_already_completed`, 409 `conflict` |
| `DELETE /v1/sessions/:id?dry_run=` | | `200 {sessions, continuations, dry_run}` | 404, 409 `run_in_progress` |

Any endpoint that reads a body or a query parameter can answer
`400 bad_request`; the table names it only where it is the usual outcome.
Other errors: `401 unauthorized` when authentication is on (with a
`WWW-Authenticate: Bearer` header),
`403 forbidden_origin` when it is off (see [Authentication](#authentication)),
`404 not_found` for an unknown path, `405 method_not_allowed`,
`413 payload_too_large` for bodies over 32 MiB, and `500 internal_error`.

**Messages** (`prompt`, `media`). `media` is a list of
`{"mime": "image/png", "base64": "…", "filename": "optional"}`.

**Results** (`result`). A JSON string is a text result. Other forms are
`{"type": "text", "content": "…"}`, `{"type": "json", "content": <any>}`,
`{"type": "media", "mimeType": "…", "base64Data": "…"}`, and
`{"type": "mixed", "parts": [...]}`.

**Listing.** Sessions come newest first (by `updated_at`). Filters:
`agent=<slug>`, `status=<s1,s2,…>`, `parent=<session id>` (the sub-sessions
of a session; another owner's session answers `404 unknown_session`). `limit` is 1 to 500 (default 50). When a page is full,
`next_before` holds the `updated_at` of its last session: pass it as
`before=` to get the next page.

**Cancelling** stops the active run and its background tool calls. The
session is stored with the status its turns imply, usually `ready`. The
cancelled calls are reported to the LLM on the next run.

**Deleting** removes the session, all its sub-sessions, and their
continuation tokens. It is refused while a run is active on any of them or on
a parent session. `dry_run=true` answers with what would be removed and
changes nothing.

### Postgres

`--db postgresql://user:password@host:5432/agents` stores sessions in
Postgres instead of SQLite. The server creates its tables on start
(`sessions`, `tool_continuations`, `agents`, `schema_migrations`); the
database must exist and the user must be allowed to create tables. The logs show the URL
without its user and password.

Several servers may share one Postgres database: every write is versioned,
so a conflicting write is detected and refused (`409 conflict`). Runs are
not coordinated between servers, though: route all requests for a session to
the same server.

---

## Storing agents

Besides `--agent-file`, agents can live in the server's database and be
created, replaced, and deleted over the API. Storing agents is off unless
`--admin-owners` names the owners allowed to do it, which needs
`--auth-tokens`: an agent definition can start MCP servers, which are
commands run on the server's machine.

```bash
curl -X PUT localhost:8080/v1/agents/helper -H 'Authorization: Bearer <admin token>' -d '{
  "apiKeyId": "openai", "flavor": "OpenAIv1",
  "modelUrl": "https://api.openai.com/v1", "modelName": "gpt-4o-mini",
  "announce": "a helpful assistant", "systemPrompt": ["You help."],
  "builtinToolboxes": [], "mcpServers": []
}'
```

* The body is what goes under `contents` in an agent file. The slug comes
  from the path.
* A stored agent cannot use anything that refers to files:
  `toolDirectory`, `bashToolboxes`, `openApiToolboxes`,
  `postgrestToolboxes`, `extraAgents`, `skillSources`, `autoEnableSkills`
  (`400 agent_uses_files`). Builtin toolboxes and MCP servers work, as do the
  execution mode and tool-call policy.
* The agent is loaded before it is stored: if an MCP server fails to start,
  the answer is `400 agent_failed_to_load` and nothing is stored.
* A slug used by an agent file cannot be stored (`409
  agent_defined_by_file`). If an agent file with the slug of a stored agent
  appears later, the file wins, and the stored agent is skipped at startup
  (logged as `agents.stored_skipped`).
* `GET /v1/agents` lists both kinds, with `source: "file"` or `"database"`.
  Stored agents also show `config`, `updated_at`, and `updated_by`.
* New sessions use a replaced agent at once. Sessions already in memory keep
  the version they built until they are idle long enough to be dropped
  (`--live-session-ttl`). Sessions of a deleted agent stay, but runs on them
  fail with `unknown_agent` until an agent with that slug exists again.
* MCP servers started for a stored agent keep running when the agent is
  replaced or deleted, until the server stops.

---

## Authentication

With `--auth-tokens tokens.json`, every endpoint except `/healthz` needs an
`Authorization: Bearer <token>` header, and each caller only sees their own
sessions. The file maps tokens to owners:

```json
{"tokens": [
  {"owner": "alice", "sha256": "42f5d7b6be1957766e51c84756eb7a7c19a0e690a75d628238e2ab78f0ba9d19"},
  {"owner": "bob", "token": "a-long-random-token"}
]}
```

`sha256` is the hex SHA-256 of the token (`printf %s TOKEN | sha256sum`), so
the file need not hold the token itself. `token` holds it in plain text.
Several tokens may share an owner. The file is read at startup.

* A missing or unknown token answers `401 unauthorized`.
* A session belongs to the caller who created it. Sub-sessions, created by
  sub-agents, belong to the owner of their root session.
* Another owner's session answers `404`, exactly as a session that does not
  exist, including its continuation tokens (`404 unknown_token`).
* `GET /v1/sessions` lists the caller's own sessions. Sub-sessions are
  listed through their parent: `?parent=<session id>`.
* Sessions created while authentication was off have no owner, and no caller
  sees them once it is on.
* `GET /v1/sessions/:id/events` also accepts the token as an `access_token`
  query parameter, because a browser's `EventSource` cannot set headers. No
  other endpoint does, and the request log records no query strings.

All owners share the agents and the API keys of the server.

**Browser origins.** Without `--auth-tokens`, requests carrying an `Origin`
header that is not `localhost`, `127.0.0.1`, or `[::1]` answer
`403 forbidden_origin`. This stops a web page from reaching a local server
through DNS rebinding. Clients that send no `Origin` (curl, servers, MCP
clients) are not affected. With authentication on, origins are not checked.
`/healthz`, `/openapi.json` and `/` are answered before the check, so a
monitor or a documentation browser reaches them from anywhere.

---

## MCP over HTTP

`POST /mcp` serves the agents to MCP clients over the
[Streamable HTTP transport](https://modelcontextprotocol.io/specification/2025-06-18/basic/transports#streamable-http).
Point a client at `http://127.0.0.1:8080/mcp`; with `--auth-tokens`, it must
send the same bearer token as REST clients.

* Each root agent is one tool, `ask_<slug>`, with a single string argument
  `prompt`.
* A call creates a session, owned by the caller, and waits for its run (up to
  120 seconds). The result is the agent's final answer. `_meta.session_id`
  names the session, which is also visible through the REST API.
* If the run stops on deferred tool calls, the result (not an error) says
  so and lists the pending calls with their continuation tokens: complete
  them through `POST /v1/continuations/:token`. If the run is still going when
  the wait ends, the result gives the session id to follow.
* A failed run returns a result with `isError: true`.
* A body that is not a JSON-RPC message, or an empty batch, answers `400`
  with a JSON-RPC error (`-32700`, `-32600`). An unknown method answers
  `-32601`, and an unknown tool or bad arguments `-32602`, both inside a
  `200`.
* Every request gets a plain JSON response. Notifications answer `202`.
  The server sends no requests or notifications of its own, so `GET /mcp`
  answers `405`. MCP sessions (`Mcp-Session-Id`) are not used.
* Supported protocol versions: `2025-06-18`, `2025-03-26`, `2024-11-05`.

For the stdio MCP server that runs agents without storing sessions, see
[mcp.md](mcp.md).

---

## Restarts and shutdown

On `SIGTERM` or `SIGINT`, the server:

1. stops accepting connections;
2. ends event streams and answers waiting requests with the current state;
3. gives other open requests the grace period (`--shutdown-grace`);
4. cancels active runs, storing their sessions;
5. closes the database.

On start, sessions left `running` by a crash are stored with the status their
turns imply. Their interrupted step is lost, and background tool calls they
were running are reported to the LLM as orphaned on the next run. Nothing is
resumed automatically: `resume` them.

Background tool calls (`runAsync`) live in the server process. They survive
between runs of a session, but not a restart. A session is not dropped at
the TTL while one of its background calls is still running: it is kept until
they finish. Deferred calls are the durable kind:
their tokens stay valid across restarts.

---

## Logs

The server writes one JSON object per line on stderr, with `ts`, `kind`,
and, when known, `session_id`:

| `kind` | Fields |
|---|---|
| `server.started` | `bind`, `port`, `agents`, `admin_owners`, `database`, `authentication` (`bearer` or `none`), `ui`, and `warning` when authentication is off |
| `http.request` | `method`, `path`, `status`, `ms` (when the response starts) |
| `run.started`, `session.updated`, `calls.deferred`, `run.stopped`, `session.failed` | `session_id` |
| `llm.request` / `llm.response` | `bytes`, token counts |
| `llm.http` | `method`, `host`, `path`, `status` |
| `sessions.recovered` | `session_ids` |
| `agents.stored_skipped` | `slug`, `reason`: a stored agent hidden by an agent file |
| `agent_tree`, `tool`, `tool.portal` | `event`: what the agent loader and the tools reported |
| `llm.backoff` | `attempt`, `delay_seconds` |
| `server.signal`, `server.stopping`, `server.stopped`, `server.failed` | |

Prompts, LLM payloads, HTTP headers, and API keys are never logged.

---

## Embedding the runner in your own program

The server is a thin layer over two library modules, which a Haskell
program can use directly:

* `System.Agents.Host.withHost` loads agent files and opens the database.
* `System.Agents.Host.Runner` provides `createSession`, `postMessage`,
  `resume`, `completeCall`, `cancelRun`, `awaitRun`, `deleteSession`, and
  `subscribe` / `subscribeSTM` for events.

```haskell
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

import Prod.Tracer (silent)
import System.Agents.Host
import System.Agents.Host.Runner
import System.Agents.SessionStore (SessionMeta (..))

main :: IO ()
main = do
    let cfg = defaultHostConfig ["weather.json"] "keys.json" "agents.db"
    withHost cfg silent $ \host ->
        withSessionRunner host $ \runner -> do
            Right meta <- createSession runner "weather" (NewMessage "Weather in Paris?" []) (Just UntilBlocked)
            Right (stopped, _) <- awaitRun runner meta.smSessionId 120
            print stopped.smStatus
```

The HTTP layer itself is in `examples/agents-server/src/AgentsServer/Api.hs`.

## Not yet supported

* Per-owner API keys: all owners share the server's keys.
* Coordinating runs between several servers sharing a Postgres database.

See `todos/web-server-embedding.md` for the design and the planned work.
