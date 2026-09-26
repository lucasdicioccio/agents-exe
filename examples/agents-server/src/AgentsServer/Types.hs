{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | The bodies of the HTTP API, as types.

These carry both the JSON encoding the API has always used and an OpenAPI
schema, so that @\/openapi.json@ describes the real shapes rather than
free-form objects. A client that has never heard of agents-exe can read the
document and drive the server from it.

The conversation itself (the @session@ field of 'SessionBody') stays
opaque: its turn tree is large and belongs to the library, not to the HTTP
protocol. Everything a caller must send or match on has a full schema.
-}
module AgentsServer.Types (
    -- * Requests
    CreateSessionBody (..),
    MessageBody (..),
    ResumeBody (..),
    ContinuationBody (..),
    SetParamsBody (..),
    MediaItem (..),

    -- * Responses
    ErrorBody (..),
    HealthBody (..),
    AgentBody (..),
    SessionMetaBody (..),
    SessionBody (..),
    SessionListBody (..),
    PendingBody (..),
    DeletePlanBody (..),
    DeletedBody (..),
    DeferredCallBody (..),
    MailPostBody (..),
    MailListBody (..),
    ForkBody (..),
    RawJson (..),

    -- * Encoding shared by the two
    bodyOptions,
    bodySchemaOptions,
) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Char (isUpper, toLower)
import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import qualified Data.HashMap.Strict.InsOrd as InsOrd
import Data.OpenApi (
    NamedSchema (..),
    OpenApiType (..),
    SchemaOptions (..),
    ToSchema (..),
    declareSchemaRef,
    defaultSchemaOptions,
    genericDeclareNamedSchema,
 )
import qualified Data.OpenApi as OpenApi
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))

import System.Agents.Session.Base (ContinuationToken (..), SessionId (..))

-------------------------------------------------------------------------------
-- Field naming
-------------------------------------------------------------------------------

{- | Drop a record's prefix and snake_case the rest: @csParentSessionId@
becomes @parent_session_id@.
-}
fieldName :: Int -> String -> String
fieldName n = snake . drop n
  where
    snake [] = []
    snake (c : cs) = toLower c : concatMap sep cs
    sep c
        | isUpper c = ['_', toLower c]
        | otherwise = [c]

-- | Aeson encoding of a body whose fields carry an @n@-character prefix.
bodyOptions :: Int -> Aeson.Options
bodyOptions n = Aeson.defaultOptions{Aeson.fieldLabelModifier = fieldName n}

-- | The matching OpenAPI naming, so schema and encoding cannot disagree.
bodySchemaOptions :: Int -> SchemaOptions
bodySchemaOptions n = defaultSchemaOptions{fieldLabelModifier = fieldName n}

{- | A JSON value this protocol carries but does not model: a stored agent's
configuration, a tool call's arguments. An empty schema accepts any value.
-}
newtype RawJson = RawJson {unRawJson :: Aeson.Value}
    deriving (Show, Eq)

instance Aeson.ToJSON RawJson where
    toJSON = unRawJson

instance Aeson.FromJSON RawJson where
    parseJSON = pure . RawJson

instance ToSchema RawJson where
    declareNamedSchema _ = pure $ NamedSchema Nothing mempty

-- | A free-form JSON object, described rather than modelled.
opaqueObject :: Text -> OpenApi.Schema
opaqueObject description =
    mempty
        { OpenApi._schemaType = Just OpenApiObject
        , OpenApi._schemaDescription = Just description
        }

{- | Describe a generated schema's fields. openapi3 does not read Haddock,
and a document whose fields carry no explanation is only half of one.
-}
withFieldDocs :: [(Text, OpenApi.Schema -> OpenApi.Schema)] -> NamedSchema -> NamedSchema
withFieldDocs edits (NamedSchema n s) =
    NamedSchema n s{OpenApi._schemaProperties = foldl' apply (OpenApi._schemaProperties s) edits}
  where
    apply props (field, f) = InsOrd.adjust (inline f) field props
    inline f (OpenApi.Inline sc) = OpenApi.Inline (f sc)
    inline _ referenced = referenced

-- | Explain a field.
says :: Text -> OpenApi.Schema -> OpenApi.Schema
says d sc = sc{OpenApi._schemaDescription = Just d}

-- | Explain a field and list the values it accepts.
oneOfValues :: Text -> [Text] -> OpenApi.Schema -> OpenApi.Schema
oneOfValues d values sc =
    (says d sc){OpenApi._schemaEnum = Just (map Aeson.String values)}

{- | Session ids travel as UUID strings. Orphan instances: 'SessionId' is a
library type, and only the HTTP layer needs to describe it.
-}
instance ToSchema SessionId where
    declareNamedSchema _ = pure $ NamedSchema (Just "SessionId") uuidSchema

instance OpenApi.ToParamSchema SessionId where
    toParamSchema _ =
        mempty
            { OpenApi._schemaType = Just OpenApiString
            , OpenApi._schemaFormat = Just "uuid"
            }

uuidSchema :: OpenApi.Schema
uuidSchema =
    mempty
        { OpenApi._schemaType = Just OpenApiString
        , OpenApi._schemaFormat = Just "uuid"
        , OpenApi._schemaDescription = Just "A session id."
        }

instance FromHttpApiData SessionId where
    parseUrlPiece = fmap SessionId . parseUrlPiece

instance ToHttpApiData SessionId where
    toUrlPiece (SessionId uuid) = toUrlPiece uuid

instance ToSchema ContinuationToken where
    declareNamedSchema _ =
        pure $
            NamedSchema (Just "ContinuationToken") $
                uuidSchema{OpenApi._schemaDescription = Just "A token naming one deferred tool call."}

instance OpenApi.ToParamSchema ContinuationToken where
    toParamSchema _ =
        mempty
            { OpenApi._schemaType = Just OpenApiString
            , OpenApi._schemaFormat = Just "uuid"
            }

instance FromHttpApiData ContinuationToken where
    parseUrlPiece = fmap ContinuationToken . parseUrlPiece

instance ToHttpApiData ContinuationToken where
    toUrlPiece (ContinuationToken uuid) = toUrlPiece uuid

-------------------------------------------------------------------------------
-- Requests
-------------------------------------------------------------------------------

-- | The @params@ field several request bodies share: parameter name to value.
paramsDoc :: OpenApi.Schema -> OpenApi.Schema
paramsDoc =
    says
        "Parameter values, by name (see the agent's `params` in GET /v1/agents). \
        \A null value clears a session-scope value. Session-scope values are kept \
        \with the session; secret ones only in memory. A session token cannot set them."

-- | An image or other attachment sent with a prompt.
data MediaItem = MediaItem
    { miMime :: Text
    , miBase64 :: Text
    , miFilename :: Maybe Text
    }
    deriving (Show, Eq, Generic)

instance Aeson.ToJSON MediaItem where
    toJSON = Aeson.genericToJSON (bodyOptions 2)

instance Aeson.FromJSON MediaItem where
    parseJSON = Aeson.genericParseJSON (bodyOptions 2)

instance ToSchema MediaItem where
    declareNamedSchema = genericDeclareNamedSchema (bodySchemaOptions 2)

-- | @POST \/v1\/sessions@.
data CreateSessionBody = CreateSessionBody
    { csAgent :: Text
    -- ^ The slug of the agent to run; see @GET \/v1\/agents@.
    , csPrompt :: Maybe Text
    -- ^ Absent (and no 'csMedia'): create an idle session with no turn at
    -- all, ready for a later message or 'resume' (G2).
    , csMedia :: Maybe [MediaItem]
    , csRun :: Maybe Text
    -- ^ @none@, @step@, or @until_blocked@ (the default).
    , csParent :: Maybe SessionId
    {- ^ Record the new session as a child of this one (lineage only:
    the child does not report back to it). The caller must be able to see
    the parent; the child is then listed through it (@?parent=@). This is
    what 'System.Agents.Host.Client.createSessionAsChild' and
    'System.Agents.Host.Client.spawnSession' send over HTTP.
    -}
    , csParams :: Maybe (Map Text RawJson)
    , csSeal :: Maybe Bool
    -- ^ Seal the session: its conversation is no longer readable through the API.
    , csSessionToken :: Maybe Bool
    -- ^ Mint a token scoped to this session; shown once, in the answer's @session_token@.
    }
    deriving (Show, Eq, Generic)

instance Aeson.ToJSON CreateSessionBody where
    toJSON = Aeson.genericToJSON (bodyOptions 2)

instance Aeson.FromJSON CreateSessionBody where
    parseJSON = Aeson.genericParseJSON (bodyOptions 2)

instance ToSchema CreateSessionBody where
    declareNamedSchema p =
        withFieldDocs
            [ ("agent", says "The slug of the agent to run; GET /v1/agents lists them.")
            , ("prompt", says "What to ask the agent. Absent: create an idle session with no turn yet.")
            , ("run", oneOfValues "How far the run should go." ["none", "step", "until_blocked"])
            , ("parent", says "Record the new session as a child of this session (lineage only). The caller must be able to see it.")
            , ("params", paramsDoc)
            , ("seal", says "Default false. Seal the session so its conversation cannot be read back through the API.")
            , ("session_token", says "Default false. Mint a bearer token limited to this session; the answer carries it once, as `session_token`. Needs --auth-tokens.")
            ]
            <$> genericDeclareNamedSchema (bodySchemaOptions 2) p

-- | @POST \/v1\/sessions\/:id\/messages@.
data MessageBody = MessageBody
    { mbPrompt :: Text
    , mbMedia :: Maybe [MediaItem]
    , mbRun :: Maybe Text
    , mbInterrupt :: Maybe Bool
    , mbParams :: Maybe (Map Text RawJson)
    }
    deriving (Show, Eq, Generic)

instance Aeson.ToJSON MessageBody where
    toJSON = Aeson.genericToJSON (bodyOptions 2)

instance Aeson.FromJSON MessageBody where
    parseJSON = Aeson.genericParseJSON (bodyOptions 2)

instance ToSchema MessageBody where
    declareNamedSchema p =
        withFieldDocs
            [ ("prompt", says "The next message in this conversation.")
            , ("run", oneOfValues "How far the run should go." ["none", "step", "until_blocked"])
            ,
                ( "interrupt"
                , says
                    "Only against a running session (default false): instead of 409 \
                    \not_accepting_messages, detach its attached tool calls (and, with \
                    \the agent's interruptCompletions on, cancel an in-flight LLM call) \
                    \and ask the model again with this message folded in."
                )
            , ("params", paramsDoc)
            ]
            <$> genericDeclareNamedSchema (bodySchemaOptions 2) p

-- | @POST \/v1\/sessions\/:id\/resume@.
data ResumeBody = ResumeBody
    { rbMode :: Maybe Text
    -- ^ @step@ or @until_blocked@ (the default).
    , rbParams :: Maybe (Map Text RawJson)
    }
    deriving (Show, Eq, Generic)

instance Aeson.ToJSON ResumeBody where
    toJSON = Aeson.genericToJSON (bodyOptions 2)

instance Aeson.FromJSON ResumeBody where
    parseJSON = Aeson.genericParseJSON (bodyOptions 2)

instance ToSchema ResumeBody where
    declareNamedSchema p =
        withFieldDocs
            [ ("mode", oneOfValues "One step, or until the run is blocked (the default)." ["step", "until_blocked"])
            , ("params", paramsDoc)
            ]
            <$> genericDeclareNamedSchema (bodySchemaOptions 2) p

{- | @POST \/v1\/continuations\/:token@. A JSON string is a text result;
an object is one of the tagged forms (@text@, @json@, @media@, @mixed@).
-}
data ContinuationBody = ContinuationBody
    { cbResult :: Aeson.Value
    , cbResume :: Maybe Bool
    -- ^ Start a run once the result is stored. Defaults to true.
    , cbParams :: Maybe (Map Text RawJson)
    }
    deriving (Show, Eq, Generic)

instance Aeson.ToJSON ContinuationBody where
    toJSON = Aeson.genericToJSON (bodyOptions 2)

instance Aeson.FromJSON ContinuationBody where
    parseJSON = Aeson.genericParseJSON (bodyOptions 2)

instance ToSchema ContinuationBody where
    declareNamedSchema _ = do
        boolRef <- declareSchemaRef (Proxy :: Proxy Bool)
        pure $
            NamedSchema (Just "ContinuationBody") $
                mempty
                    { OpenApi._schemaType = Just OpenApiObject
                    , OpenApi._schemaRequired = ["result"]
                    , OpenApi._schemaDescription =
                        Just "The result of a deferred tool call. A JSON string is a text result; an object is one of the tagged forms (text, json, media, mixed)."
                    , OpenApi._schemaProperties =
                        [ ("result", OpenApi.Inline (opaqueObject "The tool's result, as a string or a tagged object."))
                        , ("resume", boolRef)
                        , ("params", OpenApi.Inline (paramsDoc (opaqueObject "")))
                        ]
                    }

{- | @POST \/v1\/sessions\/:id\/mail@ (G6): generic mail to a session,
generalizing @POST .../messages@'s @interrupt@. @body@ is any
@System.Agents.Session.Base.MailBody@, tagged JSON (@{tag, ...}@); see the
"Mail" section of @docs\/agents-server.md@ for the exact shapes
(@userMessage@, @agentMessage@, @control@, ...). The answer is the mail's
@Receipt@: @{id, seq, duplicate}@.
-}
data MailPostBody = MailPostBody
    { mpBody :: RawJson
    , mpPriority :: Maybe Text
    -- ^ @normal@ (the default) or @interrupt@.
    }
    deriving (Show, Eq, Generic)

instance Aeson.ToJSON MailPostBody where
    toJSON = Aeson.genericToJSON (bodyOptions 2)

instance Aeson.FromJSON MailPostBody where
    parseJSON = Aeson.genericParseJSON (bodyOptions 2)

instance ToSchema MailPostBody where
    declareNamedSchema p =
        withFieldDocs
            [
                ( "body"
                , says
                    "A MailBody, tagged JSON ({tag, ...}); see the Mail section of \
                    \documentation/agents-server.md for the exact shapes (userMessage, \
                    \agentMessage, control, ...)."
                )
            , ("priority", oneOfValues "Whether this mail may pre-empt a wait." ["normal", "interrupt"])
            ]
            <$> genericDeclareNamedSchema (bodySchemaOptions 2) p

-- | @GET \/v1\/sessions\/:id\/mail@: this session's mail, oldest first.
newtype MailListBody = MailListBody
    { mlMail :: [RawJson]
    -- ^ Each is an Envelope: @{id, seq, from, priority, hops, sentAt, body}@.
    }
    deriving (Show, Eq, Generic)

instance Aeson.ToJSON MailListBody where
    toJSON = Aeson.genericToJSON (bodyOptions 2)

instance Aeson.FromJSON MailListBody where
    parseJSON = Aeson.genericParseJSON (bodyOptions 2)

instance ToSchema MailListBody where
    declareNamedSchema p =
        withFieldDocs
            [
                ( "mail"
                , says
                    "Each is an Envelope: {id, seq, from, priority, hops, sentAt, body}. \
                    \See the Mail section of documentation/agents-server.md."
                )
            ]
            <$> genericDeclareNamedSchema (bodySchemaOptions 2) p

{- | @POST \/v1\/sessions\/:id\/fork@ (G6). @at_turn@ is a 0-based index into
the source session's turns, newest first (as the TUI counts them); absent,
the whole session is copied. @agent@ rebinds the fork to another agent,
which also covers "continue with another agent" (fork with no @at_turn@,
or @at_turn: 0@, and an @agent@).
-}
data ForkBody = ForkBody
    { fbAtTurn :: Maybe Int
    , fbAgent :: Maybe Text
    , fbParams :: Maybe (Map Text RawJson)
    }
    deriving (Show, Eq, Generic)

instance Aeson.ToJSON ForkBody where
    toJSON = Aeson.genericToJSON (bodyOptions 2)

instance Aeson.FromJSON ForkBody where
    parseJSON = Aeson.genericParseJSON (bodyOptions 2)

instance ToSchema ForkBody where
    declareNamedSchema p =
        withFieldDocs
            [
                ( "at_turn"
                , says
                    "A 0-based turn index, newest first. Absent copies the whole \
                    \session; 0 forks at the head (e.g. to continue with another agent)."
                )
            , ("agent", says "Rebind the fork to another agent's slug; absent keeps the source's agent.")
            , ("params", paramsDoc)
            ]
            <$> genericDeclareNamedSchema (bodySchemaOptions 2) p

-- | @PUT \/v1\/sessions\/:id\/params@: set session-scope values without starting a run.
newtype SetParamsBody = SetParamsBody
    { spParams :: Map Text RawJson
    }
    deriving (Show, Eq, Generic)

instance Aeson.ToJSON SetParamsBody where
    toJSON = Aeson.genericToJSON (bodyOptions 2)

instance Aeson.FromJSON SetParamsBody where
    parseJSON = Aeson.genericParseJSON (bodyOptions 2)

instance ToSchema SetParamsBody where
    declareNamedSchema p =
        withFieldDocs [("params", paramsDoc)]
            <$> genericDeclareNamedSchema (bodySchemaOptions 2) p

-------------------------------------------------------------------------------
-- Responses
-------------------------------------------------------------------------------

-- | Every error answer.
data ErrorBody = ErrorBody
    { ebError :: Text
    -- ^ A stable code, such as @unknown_session@.
    , ebMessage :: Text
    }
    deriving (Show, Eq, Generic)

instance Aeson.ToJSON ErrorBody where
    toJSON = Aeson.genericToJSON (bodyOptions 2)

instance Aeson.FromJSON ErrorBody where
    parseJSON = Aeson.genericParseJSON (bodyOptions 2)

instance ToSchema ErrorBody where
    declareNamedSchema = genericDeclareNamedSchema (bodySchemaOptions 2)

-- | @GET \/healthz@.
data HealthBody = HealthBody
    { hbOk :: Bool
    , hbLiveSessions :: Int
    , hbActiveRuns :: Int
    }
    deriving (Show, Eq, Generic)

instance Aeson.ToJSON HealthBody where
    toJSON = Aeson.genericToJSON (bodyOptions 2)

instance Aeson.FromJSON HealthBody where
    parseJSON = Aeson.genericParseJSON (bodyOptions 2)

instance ToSchema HealthBody where
    declareNamedSchema = genericDeclareNamedSchema (bodySchemaOptions 2)

{- | One agent, as @GET \/v1\/agents@\/@GET \/v1\/agents\/:slug@ answer it
(@todos/os-as-standalone-server.md@ Phase 3a: an 'AgentDescriptor', model,
system prompt, tool activation and helpers included, not just slug,
description, tool names and source). 'abTools' and 'abParameters' are kept
as 'RawJson' here (each element is a tool\/parameter object -- see
@documentation/agents-server.md@'s API reference) rather than given their own
schema types, since this module only documents the wire shape, not the
library's own 'System.Agents.Protocol.ToolDescriptor'\/'System.Agents.Protocol.AgentParameter'.
-}
data AgentBody = AgentBody
    { abSlug :: Text
    , abDescription :: Text
    , abModel :: Text
    , abSystemPrompt :: [Text]
    , abTools :: [RawJson]
    , abParameters :: [RawJson]
    , abHelpers :: [Text]
    , abSource :: Text
    -- ^ @file@ or @database@.
    , abUpdatedAt :: Maybe UTCTime
    , abUpdatedBy :: Maybe Text
    , abConfig :: Maybe RawJson
    -- ^ The stored configuration, for @database@ agents.
    }
    deriving (Show, Eq, Generic)

instance Aeson.ToJSON AgentBody where
    toJSON = Aeson.genericToJSON (bodyOptions 2){Aeson.omitNothingFields = True}

instance Aeson.FromJSON AgentBody where
    parseJSON = Aeson.genericParseJSON (bodyOptions 2)

instance ToSchema AgentBody where
    declareNamedSchema p =
        withFieldDocs
            [ ("slug", says "Names this agent in POST /v1/sessions and in the MCP tool ask_<slug>.")
            , ("description", says "What the agent announces about itself.")
            , ("model", says "The model name configured for this agent.")
            , ("system_prompt", says "The agent's system prompt, one string per line.")
            , ("tools", says "The tools it can call, each {name, description, activation}.")
            , ("parameters", says "The agent's declared parameters (name, secret, scope, required, bound, pinned).")
            , ("helpers", says "Slugs of this agent's sub-agents (helpers), if any.")
            , ("source", oneOfValues "Where the definition comes from." ["file", "database"])
            , ("config", says "The stored configuration, for database agents only.")
            ]
            <$> genericDeclareNamedSchema (bodySchemaOptions 2) p

-- | A deferred call waiting for a result.
data DeferredCallBody = DeferredCallBody
    { dcToolCallId :: Text
    , dcContinuationToken :: Maybe Text
    -- ^ Post the result to @POST \/v1\/continuations\/{token}@.
    , dcTool :: Text
    , dcDisposition :: RawJson
    , dcCall :: RawJson
    -- ^ The call as the LLM made it, with its arguments.
    }
    deriving (Show, Eq, Generic)

instance Aeson.ToJSON DeferredCallBody where
    toJSON = Aeson.genericToJSON (bodyOptions 2)

instance Aeson.FromJSON DeferredCallBody where
    parseJSON = Aeson.genericParseJSON (bodyOptions 2)

instance ToSchema DeferredCallBody where
    declareNamedSchema p =
        withFieldDocs
            [ ("continuation_token", says "Post the result to POST /v1/continuations/{token}.")
            , ("tool", says "The tool the agent wanted to call.")
            , ("call", says "The call as the model made it, with its arguments.")
            , ("disposition", says "Why the server did not run it itself.")
            ]
            <$> genericDeclareNamedSchema (bodySchemaOptions 2) p

-- | A session's metadata: what @snapshot@ events and listings carry.
data SessionMetaBody = SessionMetaBody
    { smbSessionId :: SessionId
    , smbAgent :: Maybe Text
    , smbParentSessionId :: Maybe SessionId
    , smbOwner :: Maybe Text
    , smbStatus :: Text
    -- ^ @ready@, @running@, @waiting_external@, @idle@, @paused@, or @failed@.
    , smbStatusDetail :: Maybe Text
    , smbVersion :: Int
    -- ^ Incremented on every stored change.
    , smbCreatedAt :: UTCTime
    , smbUpdatedAt :: UTCTime
    }
    deriving (Show, Eq, Generic)

instance Aeson.ToJSON SessionMetaBody where
    toJSON = Aeson.genericToJSON (bodyOptions 3)

instance Aeson.FromJSON SessionMetaBody where
    parseJSON = Aeson.genericParseJSON (bodyOptions 3)

instance ToSchema SessionMetaBody where
    declareNamedSchema p =
        withFieldDocs
            [ ("status", oneOfValues statusHelp ["ready", "running", "waiting_external", "idle", "paused", "failed"])
            , ("status_detail", says "Why the last run failed.")
            , ("version", says "Incremented on every stored change; a conflicting write answers 409.")
            , ("owner", says "Who the session belongs to, when the server authenticates callers.")
            , ("parent_session_id", says "Set when a sub-agent created this session as a tool call.")
            ]
            <$> genericDeclareNamedSchema (bodySchemaOptions 3) p

statusHelp :: Text
statusHelp =
    "ready: can progress, call resume. running: a run is active. waiting_external: \
    \blocked on the deferred calls in `pending`. idle: the agent answered. paused: \
    \stopped by pause, call resume. failed: the last run failed."

{- | A session's metadata, its conversation, and its pending deferred calls:
what the session endpoints answer.
-}
data SessionBody = SessionBody
    { sbMeta :: SessionMetaBody
    -- ^ Flattened into the object, not nested.
    , sbSession :: Aeson.Value
    , sbPending :: [DeferredCallBody]
    }
    deriving (Show, Eq, Generic)

instance Aeson.ToJSON SessionBody where
    toJSON b = case Aeson.toJSON b.sbMeta of
        Aeson.Object o ->
            Aeson.Object $
                KeyMap.insert "session" b.sbSession $
                    KeyMap.insert "pending" (Aeson.toJSON b.sbPending) o
        other -> other

instance ToSchema SessionBody where
    declareNamedSchema _ = do
        NamedSchema _ metaSchema <- declareNamedSchema (Proxy :: Proxy SessionMetaBody)
        pendingRef <- declareSchemaRef (Proxy :: Proxy [DeferredCallBody])
        pure $
            NamedSchema (Just "Session") $
                metaSchema
                    { OpenApi._schemaDescription =
                        Just "A session's metadata, its conversation, and the deferred calls it is waiting on."
                    , OpenApi._schemaProperties =
                        OpenApi._schemaProperties metaSchema
                            <> [ ("session", OpenApi.Inline (opaqueObject "The conversation: a turn tree. See documentation/sessions.md."))
                               , ("pending", pendingRef)
                               ]
                    }

-- | @GET \/v1\/sessions@.
data SessionListBody = SessionListBody
    { slbSessions :: [SessionMetaBody]
    , slbNextBefore :: Maybe UTCTime
    -- ^ Pass as @before@ to get the next page; absent on the last one.
    }
    deriving (Show, Eq, Generic)

instance Aeson.ToJSON SessionListBody where
    toJSON = Aeson.genericToJSON (bodyOptions 3)

instance Aeson.FromJSON SessionListBody where
    parseJSON = Aeson.genericParseJSON (bodyOptions 3)

instance ToSchema SessionListBody where
    declareNamedSchema = genericDeclareNamedSchema (bodySchemaOptions 3)

-- | @GET \/v1\/sessions\/:id\/pending@.
newtype PendingBody = PendingBody
    { pbCalls :: [DeferredCallBody]
    }
    deriving (Show, Eq, Generic)

instance Aeson.ToJSON PendingBody where
    toJSON = Aeson.genericToJSON (bodyOptions 2)

instance Aeson.FromJSON PendingBody where
    parseJSON = Aeson.genericParseJSON (bodyOptions 2)

instance ToSchema PendingBody where
    declareNamedSchema = genericDeclareNamedSchema (bodySchemaOptions 2)

-- | @DELETE \/v1\/sessions\/:id@: what was removed, or would be.
data DeletePlanBody = DeletePlanBody
    { dpbSessions :: [SessionId]
    , dpbContinuations :: Int
    , dpbDryRun :: Bool
    }
    deriving (Show, Eq, Generic)

instance Aeson.ToJSON DeletePlanBody where
    toJSON = Aeson.genericToJSON (bodyOptions 3)

instance Aeson.FromJSON DeletePlanBody where
    parseJSON = Aeson.genericParseJSON (bodyOptions 3)

instance ToSchema DeletePlanBody where
    declareNamedSchema = genericDeclareNamedSchema (bodySchemaOptions 3)

-- | @DELETE \/v1\/agents\/:slug@.
newtype DeletedBody = DeletedBody
    { dbDeleted :: Text
    }
    deriving (Show, Eq, Generic)

instance Aeson.ToJSON DeletedBody where
    toJSON = Aeson.genericToJSON (bodyOptions 2)

instance Aeson.FromJSON DeletedBody where
    parseJSON = Aeson.genericParseJSON (bodyOptions 2)

instance ToSchema DeletedBody where
    declareNamedSchema = genericDeclareNamedSchema (bodySchemaOptions 2)
