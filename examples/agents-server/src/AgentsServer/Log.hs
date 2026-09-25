{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | JSON-line logs on stderr.

Each line is one object with @ts@, @kind@, and, when known, @session_id@.
Traces are summarised field by field rather than 'show'n: LLM payloads,
HTTP headers, and API keys are never printed.
-}
module AgentsServer.Log (
    Logger,
    newHandleLogger,
    silentLogger,
    logLine,
    hostTraceLogger,
    requestLogger,
) where

import Control.Concurrent.MVar (newMVar, withMVar)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Pair)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy.Char8 as LChar8
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (diffUTCTime, getCurrentTime)
import qualified Network.HTTP.Client as HttpClient
import Network.HTTP.Types (statusCode)
import Network.Wai (Middleware, rawPathInfo, requestMethod, responseStatus)
import Prod.Tracer (Tracer (..))
import System.IO (Handle, hFlush)

import qualified System.Agents.AgentFactory as AgentFactory
import qualified System.Agents.AgentTree.OneShotTool as OneShotTool
import System.Agents.Host (HostTrace (..))
import qualified System.Agents.HttpClient as HttpClient
import qualified System.Agents.LLMs.OpenAI as OpenAI

-- | Writes one JSON object per line.
newtype Logger = Logger ([Pair] -> IO ())

-- | A logger writing to a handle (stderr for the server), one line at a time.
newHandleLogger :: Handle -> IO Logger
newHandleLogger h = do
    lock <- newMVar ()
    pure $ Logger $ \pairs -> do
        now <- getCurrentTime
        let line = Aeson.encode (Aeson.object (("ts" .= now) : pairs))
        withMVar lock $ \_ -> LChar8.hPutStrLn h line >> hFlush h

silentLogger :: Logger
silentLogger = Logger (const (pure ()))

logLine :: Logger -> Text -> [Pair] -> IO ()
logLine (Logger write) kind pairs = write (("kind" .= kind) : pairs)

-- | Host traces, summarised.
hostTraceLogger :: Logger -> Tracer IO HostTrace
hostTraceLogger logger = Tracer $ \case
    HostRunnerTrace kind sid -> logLine logger kind ["session_id" .= sid]
    HostRecoveredSessions sids -> logLine logger "sessions.recovered" ["session_ids" .= sids]
    HostRecoveredParamsRequired sid names -> logLine logger "sessions.params_required" ["session_id" .= sid, "params" .= names]
    HostStoredAgentSkipped slug reason -> logLine logger "agents.stored_skipped" ["slug" .= slug, "reason" .= reason]
    HostAgentTrace t -> agentTrace [] t
    HostSubAgentTrace (OneShotTool.OneShotTrace t) -> agentTrace ["sub_agent" .= True] t
    HostTreeTrace t -> logLine logger "agent_tree" ["event" .= constructorName t]
  where
    agentTrace :: [Pair] -> AgentFactory.Trace -> IO ()
    agentTrace extra = \case
        AgentFactory.OpenAITrace t -> openAITrace extra t
        AgentFactory.ToolRegistrationTrace t -> logLine logger "tool" (("event" .= constructorName t) : extra)
        AgentFactory.ToolPortalTrace t -> logLine logger "tool.portal" (("event" .= constructorName t) : extra)

    openAITrace :: [Pair] -> OpenAI.Trace -> IO ()
    openAITrace extra = \case
        OpenAI.CallChatCompletion _payload bytes estimated ->
            logLine logger "llm.request" (["bytes" .= bytes, "estimated_tokens" .= estimated] <> extra)
        OpenAI.GotChatCompletion _payload bytes usage ->
            logLine logger "llm.response" (["bytes" .= bytes] <> usagePairs usage <> extra)
        OpenAI.OverloadedBackoff attempt delay ->
            logLine logger "llm.backoff" (["attempt" .= attempt, "delay_seconds" .= delay] <> extra)
        -- Method, host, path, and status only: headers carry the API key.
        OpenAI.HttpClientTrace (HttpClient.RunRequest req rsp) ->
            logLine
                logger
                "llm.http"
                ( [ "method" .= decode (HttpClient.method req)
                  , "host" .= decode (HttpClient.host req)
                  , "path" .= decode (HttpClient.path req)
                  , "status" .= statusCode (HttpClient.responseStatus rsp)
                  ]
                    <> extra
                )

    usagePairs :: Maybe OpenAI.TokenUsage -> [Pair]
    usagePairs = \case
        Nothing -> []
        Just u ->
            [ "prompt_tokens" .= u.tokenPromptTokens
            , "completion_tokens" .= u.tokenCompletionTokens
            , "cached_tokens" .= u.tokenCachedTokens
            ]

-- | One line per request, when the response starts: method, path, status, time.
requestLogger :: Logger -> Middleware
requestLogger logger app req respond = do
    start <- getCurrentTime
    app req $ \rsp -> do
        end <- getCurrentTime
        logLine
            logger
            "http.request"
            [ "method" .= decode (requestMethod req)
            , "path" .= decode (rawPathInfo req)
            , "status" .= statusCode (responseStatus rsp)
            , "ms" .= (round (realToFrac (diffUTCTime end start) * 1000 :: Double) :: Int)
            ]
        respond rsp

-- | The constructor of a trace, without its (possibly large) arguments.
constructorName :: (Show a) => a -> Text
constructorName = Text.pack . takeWhile (/= ' ') . show

decode :: Char8.ByteString -> Text
decode = Text.decodeUtf8Lenient
