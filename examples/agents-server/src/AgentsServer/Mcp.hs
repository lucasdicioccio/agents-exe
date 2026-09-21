{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | MCP over HTTP (the Streamable HTTP transport), answering every request
with plain JSON.

Each root agent is one tool, @ask_<slug>@, taking a @prompt@. A call creates
a session through the runner and waits for its run. The result is the
agent's answer; a session that stops on deferred tool calls, or is still
running when the wait ends, is reported with its id and pending
continuation tokens, so that the client can finish through the REST API.
-}
module AgentsServer.Mcp (
    McpContext (..),
    handleMcp,
    toolNameFor,
) where

import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as LByteString
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.UUID as UUID
import qualified Data.Vector as Vector
import Network.HTTP.Types (Status, status200, status202, status400)

import System.Agents.AgentTree (OSAgentNode (..))
import qualified System.Agents.Base as Base
import System.Agents.Host.Runner
import qualified System.Agents.MCP.Base as Mcp
import System.Agents.Session.Base (LlmResponse (..), LlmTurnContent (..), Session (..), SessionId (..), SessionStatus (..), Turn (..), pendingDeferredCalls)
import System.Agents.SessionStore (SessionMeta (..))
import System.Agents.Tools.Params.Types (ParamName)

data McpContext = McpContext
    { mcRunner :: SessionRunner
    , mcAgents :: Map Text OSAgentNode
    , mcOwner :: Maybe Text
    -- ^ Owner of the sessions that tool calls create.
    , mcWait :: SessionId -> IO ()
    -- ^ Wait for a session's run to stop, within the server's limits.
    , mcHeaderParams :: Map ParamName Aeson.Value
    {- ^ Parameter values from this request's @Agents-Param-<name>@ headers
    (@todos/tool-partial-application.md@, Phase 5). Every value is a
    string. A @tools/call@'s @_meta.\"agents-exe\/params\"@ overrides these
    by name; each call is one run, so session and message scope coincide
    here.
    -}
    }

{- | Answer the body of a @POST /mcp@: a JSON-RPC message or a batch.
'Nothing' when no message needs an answer (notifications, responses).
-}
handleMcp :: McpContext -> LByteString.ByteString -> IO (Status, Maybe Aeson.Value)
handleMcp ctx raw = case Aeson.eitherDecode raw of
    Left err -> pure (status400, Just (rpcError Aeson.Null (-32700) ("parse error: " <> Text.pack err)))
    Right (Aeson.Array batch)
        | Vector.null batch -> pure (status400, Just (rpcError Aeson.Null (-32600) "empty batch"))
        | otherwise -> do
            answers <- catMaybes <$> mapM (handleMessage ctx) (toList batch)
            pure $ if null answers then (status202, Nothing) else (status200, Just (Aeson.toJSON answers))
    Right message -> maybe (status202, Nothing) (\answer -> (status200, Just answer)) <$> handleMessage ctx message

handleMessage :: McpContext -> Aeson.Value -> IO (Maybe Aeson.Value)
handleMessage ctx = \case
    Aeson.Object o -> case (KeyMap.lookup "method" o, KeyMap.lookup "id" o) of
        (Just (Aeson.String method), Just rid) ->
            Just . either (\(code, msg) -> rpcError rid code msg) (rpcResult rid)
                <$> request ctx method (maybe (Aeson.Object mempty) id (KeyMap.lookup "params" o))
        -- A notification.
        (Just (Aeson.String _), Nothing) -> pure Nothing
        -- A response: the server sends no requests, so there is nothing to match.
        (Nothing, Just _) -> pure Nothing
        _ -> pure $ Just $ rpcError Aeson.Null (-32600) "invalid request"
    _ -> pure $ Just $ rpcError Aeson.Null (-32600) "invalid request"

type RpcError = (Int, Text)

request :: McpContext -> Text -> Aeson.Value -> IO (Either RpcError Aeson.Value)
request ctx method params = case method of
    "initialize" -> pure $ Right $ Aeson.toJSON $ initializeResult params
    "ping" -> pure $ Right $ Aeson.object []
    "tools/list" -> pure $ Right $ Aeson.toJSON $ Mcp.ListToolsResult (map (uncurry agentTool) (Map.toList ctx.mcAgents)) Nothing
    "resources/list" -> pure $ Right $ Aeson.object ["resources" .= ([] :: [Aeson.Value])]
    "prompts/list" -> pure $ Right $ Aeson.object ["prompts" .= ([] :: [Aeson.Value])]
    "tools/call" -> callTool ctx params
    _ -> pure $ Left (-32601, "method not found: " <> method)

-------------------------------------------------------------------------------
-- Initialization and tools
-------------------------------------------------------------------------------

supportedVersions :: [Text]
supportedVersions = [latestVersion, "2025-03-26", "2024-11-05"]

-- | Answer with the client's protocol version when supported, else the latest.
initializeResult :: Aeson.Value -> Mcp.InitializeResult
initializeResult params =
    Mcp.InitializeResult
        version
        (Mcp.ServerCapabilities Nothing Nothing [])
        (Mcp.Implementation "agents-server" "0.1.0")
        (Just "Each tool asks one agent. Sessions are stored on the server; deferred tool calls are completed through its REST API.")
  where
    requested = Aeson.parseMaybe (Aeson.withObject "params" (.: "protocolVersion")) params
    version = case requested of
        Just v | v `elem` supportedVersions -> v
        _ -> latestVersion

latestVersion :: Text
latestVersion = "2025-06-18"

toolNameFor :: Text -> Text
toolNameFor slug = "ask_" <> slug

agentTool :: Text -> OSAgentNode -> Mcp.Tool
agentTool slug node =
    Mcp.Tool
        (toolNameFor slug)
        (Just (Base.announce node.osNodeConfig))
        ( Mcp.InputSchema
            (Just ["prompt"])
            ( Just $
                KeyMap.fromList
                    [
                        ( "prompt"
                        , Aeson.object
                            [ "type" .= ("string" :: Text)
                            , "description" .= ("What to ask the agent" :: Text)
                            ]
                        )
                    ]
            )
        )

callTool :: McpContext -> Aeson.Value -> IO (Either RpcError Aeson.Value)
callTool ctx params = case Aeson.parseMaybe parseCall params of
    Nothing -> pure $ Left (-32602, "tools/call needs a tool name and a string argument \"prompt\"")
    Just (name, prompt, metaParams) -> case [slug | slug <- Map.keys ctx.mcAgents, toolNameFor slug == name] of
        [] -> pure $ Left (-32602, "unknown tool: " <> name)
        (slug : _) ->
            createSessionAs ctx.mcRunner ctx.mcOwner slug (NewMessage prompt []) (Just UntilBlocked) (Map.union metaParams ctx.mcHeaderParams) >>= \case
                Left err -> pure $ Right $ toolResult True Nothing [Text.pack (show err)]
                Right meta -> do
                    let sid = meta.smSessionId
                    ctx.mcWait sid
                    Right . outcome sid <$> getSession ctx.mcRunner sid
  where
    parseCall = Aeson.withObject "params" $ \o -> do
        name <- o .: "name"
        args <- o .: "arguments"
        prompt <- Aeson.withObject "arguments" (.: "prompt") args
        metaParams <- parseMetaParams o
        pure (name, prompt, metaParams)

{- | @_meta.\"agents-exe\/params\"@ on a @tools/call@ request
(@todos/tool-partial-application.md@, Phase 5): any JSON value per
parameter, unlike the always-string HTTP headers.
-}
parseMetaParams :: Aeson.Object -> Aeson.Parser (Map ParamName Aeson.Value)
parseMetaParams o =
    o .:? "_meta" >>= \case
        Nothing -> pure Map.empty
        Just meta -> Aeson.withObject "_meta" metaObject meta
  where
    metaObject m = case KeyMap.lookup "agents-exe/params" m of
        Nothing -> pure Map.empty
        Just (Aeson.Object obj) -> pure $ Map.fromList [(Key.toText k, v) | (k, v) <- KeyMap.toList obj]
        Just _ -> fail "_meta.\"agents-exe/params\" must be an object"

-- | The tool result for a session after its run stopped, or the wait ended.
outcome :: SessionId -> Maybe (Session, SessionMeta) -> Aeson.Value
outcome sid = \case
    Nothing -> toolResult True (Just sid) ["the session was deleted"]
    Just (sess, meta) -> case meta.smStatus of
        StatusIdle -> toolResult False (Just sid) [finalAnswer sess]
        StatusFailed -> toolResult True (Just sid) ["the agent failed: " <> maybe "unknown error" id meta.smStatusDetail]
        StatusWaitingExternal ->
            toolResult
                False
                (Just sid)
                [ "Session "
                    <> showId sid
                    <> " is waiting on deferred tool calls. Complete each with POST /v1/continuations/<continuation_token>; the session then resumes."
                , Text.decodeUtf8 (LByteString.toStrict (Aeson.encode (pendingDeferredCalls sess)))
                ]
        _ ->
            toolResult
                False
                (Just sid)
                ["Session " <> showId sid <> " is still running. Follow it with GET /v1/sessions/" <> showId sid <> "."]

-- | The text of the newest LLM answer.
finalAnswer :: Session -> Text
finalAnswer sess =
    maybe "" id $ listToMaybe [t | LlmTurn content _ <- sess.turns, Just t <- [content.llmResponse.responseText]]

{- | A @tools/call@ result. Written by hand rather than with
'Mcp.CallToolResult', whose text content always carries
@"annotations": null@, which strict clients reject.
-}
toolResult :: Bool -> Maybe SessionId -> [Text] -> Aeson.Value
toolResult isError sid texts =
    Aeson.object $
        [ "content" .= [Aeson.object ["type" .= ("text" :: Text), "text" .= t] | t <- texts]
        , "isError" .= isError
        ]
            <> ["_meta" .= Aeson.object ["session_id" .= s] | Just s <- [sid]]

-------------------------------------------------------------------------------
-- JSON-RPC
-------------------------------------------------------------------------------

rpcResult :: Aeson.Value -> Aeson.Value -> Aeson.Value
rpcResult rid result = Aeson.object ["jsonrpc" .= ("2.0" :: Text), "id" .= rid, "result" .= result]

rpcError :: Aeson.Value -> Int -> Text -> Aeson.Value
rpcError rid code msg =
    Aeson.object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "id" .= rid
        , "error" .= Aeson.object ["code" .= code, "message" .= msg]
        ]

showId :: SessionId -> Text
showId (SessionId uuid) = UUID.toText uuid
