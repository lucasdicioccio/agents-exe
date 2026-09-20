{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{- | The HTTP API as a servant type.

This is the single description of the endpoints: @\/openapi.json@ is derived
from it (see "AgentsServer.OpenApi"), so the published document cannot drift
from the routes.

Two endpoints are deliberately absent from 'DocumentedAPI' and described by
hand instead, because OpenAPI models neither of them well:

* @GET \/v1\/sessions\/:id\/events@ is a @text\/event-stream@ whose frames
  are a sum of six event kinds;
* @POST \/mcp@ is JSON-RPC, whose shape is set by the MCP specification.
-}
module AgentsServer.Routes (
    DocumentedAPI,
    HealthAPI,
    AgentsAPI,
    SessionsAPI,
    ContinuationsAPI,
    WaitParams,
) where

import Data.Text (Text)
import Data.Time (UTCTime)
import Servant.API

import AgentsServer.Types
import System.Agents.Session.Base (ContinuationToken, SessionId)

{- | Every endpoint whose shape OpenAPI can express. The order matches the
reference table of @docs\/agents-server.md@.
-}
type DocumentedAPI =
    HealthAPI
        :<|> AgentsAPI
        :<|> SessionsAPI
        :<|> ContinuationsAPI

{- | How far a request waits for the run it starts. Absent @wait@ answers as
soon as the run has started.
-}
type WaitParams rest =
    QueryParam "wait" Bool
        :> QueryParam "timeout" Double
        :> rest

type HealthAPI =
    Summary "Liveness, and how much the server is doing"
        :> Description "The only endpoint that needs no bearer token."
        :> "healthz"
        :> Get '[JSON] HealthBody

type AgentsAPI =
    "v1"
        :> "agents"
        :> ( Summary "The agents this server can run"
                :> Description "Both agents loaded from files and agents stored in the database."
                :> Get '[JSON] [AgentBody]
                :<|> Capture "slug" Text
                    :> ( Summary "One agent"
                            :> Get '[JSON] AgentBody
                            :<|> Summary "Store or replace an agent"
                                :> Description
                                    "The body is what goes under `contents` in an agent file; the slug \
                                    \comes from the path. Needs an owner named by --admin-owners. A \
                                    \stored agent cannot use fields that refer to files."
                                :> ReqBody '[JSON] RawJson
                                :> Put '[JSON] AgentBody
                            :<|> Summary "Delete a stored agent"
                                :> Delete '[JSON] DeletedBody
                       )
           )

type SessionsAPI =
    "v1"
        :> "sessions"
        :> ( Summary "Start a conversation"
                :> Description
                    "Creates a session and, unless `run` is \"none\", starts a run. The answer \
                    \carries a Location header naming the new session."
                :> WaitParams
                    ( ReqBody '[JSON] CreateSessionBody
                        :> PostCreated '[JSON] SessionBody
                    )
                :<|> Summary "List sessions, newest first"
                    :> Description
                        "Only the caller's own sessions when authentication is on. Sub-sessions \
                        \are listed through their parent."
                    :> QueryParam "agent" Text
                    :> QueryParam "status" Text
                    :> QueryParam "parent" SessionId
                    :> QueryParam "limit" Int
                    :> QueryParam "before" UTCTime
                    :> Get '[JSON] SessionListBody
                :<|> Capture "id" SessionId
                    :> ( Summary "One session, with its conversation and pending calls"
                            :> Get '[JSON] SessionBody
                            :<|> Summary "Delete a session and everything below it"
                                :> Description
                                    "Removes the session, its sub-sessions, and their continuation \
                                    \tokens. Refused while a run is active."
                                :> QueryParam "dry_run" Bool
                                :> Delete '[JSON] DeletePlanBody
                            :<|> Summary "Send another message"
                                :> "messages"
                                :> WaitParams
                                    ( ReqBody '[JSON] MessageBody
                                        :> Post '[JSON] SessionBody
                                    )
                            :<|> Summary "Step a session that can progress"
                                :> "resume"
                                :> WaitParams
                                    ( ReqBody '[JSON] ResumeBody
                                        :> Post '[JSON] SessionBody
                                    )
                            :<|> Summary "Stop the active run and its background calls"
                                :> "cancel"
                                :> Post '[JSON] SessionMetaBody
                            :<|> Summary "The deferred calls this session waits on"
                                :> "pending"
                                :> Get '[JSON] PendingBody
                       )
           )

type ContinuationsAPI =
    Summary "Complete a deferred tool call"
        :> Description
            "The session resumes on its own unless `resume` is false. The token comes from \
            \a session's `pending` calls, or from a calls.deferred event."
        :> "v1"
        :> "continuations"
        :> Capture "token" ContinuationToken
        :> WaitParams
            ( ReqBody '[JSON] ContinuationBody
                :> Post '[JSON] SessionBody
            )
