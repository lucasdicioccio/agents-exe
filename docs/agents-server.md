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
| `--db FILE` | `agents-server.db` | SQLite database for sessions and continuation tokens. Created and migrated on start. |
| `--bind HOST` | `127.0.0.1` | Address to listen on. |
| `--port PORT` | `8080` | Port to listen on. |
| `--live-session-ttl SECONDS` | `900` | How long an idle session keeps its in-memory state (including background tool calls) before it is dropped. It is reloaded from the database on next use. |
| `--shutdown-grace SECONDS` | `10` | How long open requests get to finish on shutdown. |
| `--auth-tokens FILE` | (none) | Bearer tokens and their owners. See [Authentication](#authentication). |

Sub-agents work as they do elsewhere: their sessions are stored in the same
database, linked to the parent session.

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

The stream stays open across runs. It sends a `: keepalive` comment after 15
seconds without events. Events are not replayed: a client that reconnects
gets a new snapshot and continues from there.

---

## API reference

All bodies are JSON. Errors are `{"error": "<code>", "message": "<text>"}`.

| Method and path | Body | Success | Errors |
|---|---|---|---|
| `GET /healthz` | | `200 {ok, live_sessions, active_runs}` | |
| `GET /v1/agents` | | `200 [{slug, description, tools}]` | |
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

Other errors: `401 unauthorized` when authentication is on,
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
of a session). `limit` is 1 to 500 (default 50). When a page is full,
`next_before` holds the `updated_at` of its last session: pass it as
`before=` to get the next page.

**Cancelling** stops the active run and its background tool calls. The
session is stored with the status its turns imply, usually `ready`. The
cancelled calls are reported to the LLM on the next run.

**Deleting** removes the session, all its sub-sessions, and their
continuation tokens. It is refused while a run is active on any of them or on
a parent session. `dry_run=true` answers with what would be removed and
changes nothing.

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

All owners share the agents and the API keys of the server.

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
between runs of a session, but not a restart, and not the session being idle
for longer than `--live-session-ttl`. Deferred calls are the durable kind:
their tokens stay valid across restarts.

---

## Logs

The server writes one JSON object per line on stderr, with `ts`, `kind`,
and, when known, `session_id`:

| `kind` | Fields |
|---|---|
| `server.started` | `bind`, `port`, `agents`, `database`, `authentication` (`bearer` or `none`) |
| `http.request` | `method`, `path`, `status`, `ms` (when the response starts) |
| `run.started`, `session.updated`, `calls.deferred`, `run.stopped`, `session.failed` | `session_id` |
| `llm.request` / `llm.response` | `bytes`, token counts |
| `llm.http` | `method`, `host`, `path`, `status` |
| `sessions.recovered` | `session_ids` |
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
* Several server processes sharing one database: each process serialises
  its own writes, and version checks detect the others, but runs are not
  coordinated between processes.
* Streaming LLM tokens.
* MCP over HTTP.

See `todos/web-server-embedding.md` for the design and the planned work.
