{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- | The OpenAPI document served at @\/openapi.json@.

It is derived from 'DocumentedAPI', so the paths, parameters and bodies
cannot drift from the routes. What generation cannot know is added here:
the orientation text, the bearer scheme, the error answers every endpoint
shares, and the two endpoints OpenAPI cannot express (the event stream and
the MCP transport).

The point of serving this is that a client which has never heard of
agents-exe can learn the whole protocol from the server itself. agents-exe
agents in particular can consume it directly as an @openApiToolboxes@
@SpecUrl@.
-}
module AgentsServer.OpenApi (
    apiDocument,
    documentedPaths,
) where

import Control.Lens ((%~), (&), (.~), (?~))
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import qualified Data.HashMap.Strict.InsOrd as InsOrd
import Data.OpenApi
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Servant.OpenApi (toOpenApi)

import AgentsServer.Routes (DocumentedAPI)
import AgentsServer.Types (ErrorBody)

-- | The document, with the server's own base URL when one is known.
apiDocument :: Maybe Text -> OpenApi
apiDocument baseUrl =
    toOpenApi (Proxy @DocumentedAPI)
        & info . title .~ "agents-server"
        & info . version .~ "1"
        & info . description ?~ orientation
        & servers .~ [Server base Nothing mempty | Just base <- [baseUrl]]
        & components . securitySchemes .~ SecurityDefinitions [("bearerAuth", bearerScheme)]
        & components . schemas %~ InsOrd.insert "ErrorBody" (toSchema (Proxy @ErrorBody))
        & security .~ [SecurityRequirement [("bearerAuth", [])]]
        & allOperations . responses . responses %~ (<> sharedErrors)
        & paths %~ InsOrd.union handWritten

{- | What a client needs to know before reading the endpoints: this is the
first thing an unfamiliar agent will read.
-}
orientation :: Text
orientation =
    "Run AI agents over HTTP.\n\n\
    \An **agent** is a named assistant this server can run; `GET /v1/agents` lists them. \
    \A **session** is one conversation with an agent. A **run** steps a session in the \
    \background: it calls the model, runs or defers tool calls, and stops when the agent \
    \answers or when only deferred calls remain.\n\n\
    \To get an answer: `POST /v1/sessions` with an agent slug and a prompt, passing \
    \`?wait=true` to be answered when the run stops. The session's `status` says what \
    \happened: `idle` means the agent answered (read the last turn of `session`), \
    \`waiting_external` means it is blocked on the deferred calls listed in `pending`.\n\n\
    \A **deferred call** is a tool the server will not run itself: your worker computes \
    \the result and posts it to `POST /v1/continuations/{token}` using the \
    \`continuation_token` from `pending`. The session then resumes on its own. Tokens stay \
    \valid across restarts.\n\n\
    \To follow a session as it goes, open the server-sent event stream at \
    \`GET /v1/sessions/{id}/events`.\n\n\
    \When the server is started with bearer tokens, every endpoint but `/healthz` needs an \
    \`Authorization: Bearer <token>` header, and each caller sees only their own sessions."

bearerScheme :: SecurityScheme
bearerScheme =
    SecurityScheme
        (SecuritySchemeHttp (HttpSchemeBearer Nothing))
        (Just "Sent only when the server was started with --auth-tokens; open otherwise.")

-- | The answers every endpoint can give, on top of its own.
sharedErrors :: InsOrdHashMap Int (Referenced Response)
sharedErrors =
    InsOrd.fromList
        [ (400, errorResponse "The body or a query parameter could not be read.")
        , (401, errorResponse "No valid bearer token, when the server requires one.")
        , (403, errorResponse "Refused: a cross-origin request, or an edit the caller may not make.")
        , (404, errorResponse "No such session, token, agent, or path. Also another owner's session.")
        , (409, errorResponse "The session is not in a state that allows this, or another writer won.")
        , (413, errorResponse "The body is larger than 32 MiB.")
        , (500, errorResponse "The server failed.")
        ]

errorResponse :: Text -> Referenced Response
errorResponse d =
    Inline $
        mempty
            & description .~ d
            & content .~ [("application/json", mempty & schema ?~ Ref (Reference "ErrorBody"))]

-------------------------------------------------------------------------------
-- The two endpoints OpenAPI cannot express
-------------------------------------------------------------------------------

-- | Paths added by hand; see the module header for why.
handWritten :: InsOrdHashMap FilePath PathItem
handWritten =
    InsOrd.fromList
        [ ("/v1/sessions/{id}/events", mempty & get ?~ eventsOperation)
        , ("/v1/events", mempty & get ?~ allEventsOperation)
        , ("/mcp", mempty & post ?~ mcpOperation)
        ]

-- | The paths this module documents without generating them.
documentedPaths :: [FilePath]
documentedPaths = InsOrd.keys handWritten

eventsOperation :: Operation
eventsOperation =
    mempty
        & summary ?~ "Follow a session as it runs"
        & description
            ?~ "A server-sent event stream. Each frame carries an `id:` (the event's \
               \sequence number). It opens with a `snapshot` of the session's metadata (unless \
               \a replay from `Last-Event-ID`/`after` is available -- see below), then sends \
               \one event per change: `session.updated` (metadata plus `head_turn`, the newest \
               \turn), `run.started`, `calls.deferred` (the run stopped on these deferred \
               \calls), `run.stopped`, `session.failed`, and, when the server runs with \
               \--stream-tokens, `text.delta` as the answer is written. A `: keepalive` \
               \comment is sent after 15 seconds of silence. A reconnecting client sends \
               \`Last-Event-ID` (set automatically by `EventSource`) or `?after=<seq>`: when \
               \that sequence number is still in the server's event ring (the last 4096 events \
               \by default), the missed events replay before the stream goes live, with no gap \
               \or duplicate; otherwise the stream falls back to a fresh `snapshot`, as if \
               \reconnecting for the first time."
        & parameters .~ [Inline sessionIdParam, Inline afterParam]
        & responses . responses
            .~ [
                   ( 200
                   , Inline $
                        mempty
                            & description .~ "The stream, until the client disconnects or the server stops."
                            & content .~ [("text/event-stream", mempty)]
                   )
               ]

allEventsOperation :: Operation
allEventsOperation =
    mempty
        & summary ?~ "Follow every session, or one owner's, across the server"
        & description
            ?~ "Like `GET /v1/sessions/{id}/events`, without a `snapshot` (there is no single \
               \session to snapshot) but with the same `Last-Event-ID`/`after` replay, and two \
               \events besides the session ones: `session.created` and `session.deleted`. \
               \`?scope=owner` (the default when the caller has an owner) is that caller's own \
               \sessions; `?scope=all` is every session, and needs authentication off or the \
               \caller to be an admin owner (`--admin-owners`)."
        & parameters .~ [Inline afterParam, Inline scopeParam]
        & responses . responses
            .~ [
                   ( 200
                   , Inline $
                        mempty
                            & description .~ "The stream, until the client disconnects or the server stops."
                            & content .~ [("text/event-stream", mempty)]
                   )
               , (403, errorResponse "`scope=all` without authentication off or an admin owner.")
               ]

mcpOperation :: Operation
mcpOperation =
    mempty
        & summary ?~ "Model Context Protocol, over the Streamable HTTP transport"
        & description
            ?~ "Serves each agent as one MCP tool, `ask_<slug>`, taking a `prompt`. The body \
               \is a JSON-RPC message or a batch; every request is answered with plain JSON, \
               \and notifications answer 202. A call creates a session and waits for its run \
               \(up to 120 seconds); its `_meta.session_id` names the session, which stays \
               \visible through the REST endpoints. Supported protocol versions: 2025-06-18, \
               \2025-03-26, 2024-11-05."
        & requestBody
            ?~ Inline
                ( mempty
                    & description ?~ "A JSON-RPC 2.0 message, or an array of them."
                    & content .~ [("application/json", mempty)]
                )
        & responses . responses
            .~ [ (200, Inline (mempty & description .~ "The JSON-RPC answer." & content .~ [("application/json", mempty)]))
               , (202, Inline (mempty & description .~ "Accepted; the message needed no answer."))
               , (400, errorResponse "The body is not a JSON-RPC message.")
               ]

sessionIdParam :: Param
sessionIdParam =
    mempty
        & name .~ "id"
        & in_ .~ ParamPath
        & required ?~ True
        & schema ?~ Inline (mempty & type_ ?~ OpenApiString & format ?~ "uuid")

afterParam :: Param
afterParam =
    mempty
        & name .~ "after"
        & in_ .~ ParamQuery
        & description ?~ "Replay events after this sequence number (an alternative to the `Last-Event-ID` header, which `EventSource` sets on its own)."
        & required ?~ False
        & schema ?~ Inline (mempty & type_ ?~ OpenApiInteger)

scopeParam :: Param
scopeParam =
    mempty
        & name .~ "scope"
        & in_ .~ ParamQuery
        & description ?~ "\"owner\" (the caller's own sessions, the default) or \"all\" (every session; needs authentication off or an admin owner)."
        & required ?~ False
        & schema ?~ Inline (mempty & type_ ?~ OpenApiString & enum_ ?~ ["owner", "all"])
