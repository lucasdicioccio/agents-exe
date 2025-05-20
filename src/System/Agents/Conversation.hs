{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Agents.Conversation where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Either as Either
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Prod.Tracer (Tracer (..), contramap)

import qualified System.Agents.Agent as Agent
import qualified System.Agents.FileLoader as FileLoader
import qualified System.Agents.LLMs.OpenAI as OpenAI

import System.Agents.ApiKeys

data Trace
    = AgentTrace Agent.Trace
    | DataLoadingTrace FileLoader.Trace
    deriving (Show)

data Props
    = Props
    { apiKeysFile :: FilePath
    , mainAgentFile :: FilePath
    , helperAgentsDir :: FilePath
    , interactiveTracer :: Tracer IO Trace
    , rawLogFile :: FilePath
    }

data AgentInfo = AgentInfo
    { agentDescription :: FileLoader.AgentDescription
    , agentRuntime :: Agent.Runtime
    , agentSibling :: FileLoader.Agents
    , agentSiblingRuntimes :: [Agent.Runtime]
    }

data Continue
    = LoadingErrors (NonEmpty.NonEmpty FileLoader.InvalidAgentError)
    | OtherErrors (NonEmpty.NonEmpty String)
    | Initialized AgentInfo

withAgentRuntime :: Props -> (Continue -> IO a) -> IO a
withAgentRuntime props continue = do
    let tracer = props.interactiveTracer
    loadedBoss <- FileLoader.loadJsonFile (contramap DataLoadingTrace tracer) props.mainAgentFile
    case loadedBoss of
        Left err ->
            continue $ LoadingErrors (NonEmpty.singleton err)
        Right mainAgentDescription -> do
            (loadedAgents, errs) <- FileLoader.loadDirectory (contramap DataLoadingTrace tracer) props.helperAgentsDir
            case NonEmpty.nonEmpty errs of
                Just xs -> continue $ LoadingErrors xs
                Nothing -> do
                    keys <- readOpenApiKeysFile props.apiKeysFile
                    agentRuntimes <- traverse (initAgent tracer keys id []) loadedAgents.agents
                    let (koRuntimes, okRuntimes) = Either.partitionEithers agentRuntimes
                    case NonEmpty.nonEmpty koRuntimes of
                        Just xs -> continue $ OtherErrors xs
                        Nothing -> do
                            mainAgent <-
                                initAgent
                                    tracer
                                    keys
                                    (augmentMainAgentPromptWithSubAgents okRuntimes)
                                    okRuntimes
                                    mainAgentDescription
                            case mainAgent of
                                Left err -> continue $ OtherErrors (NonEmpty.singleton err)
                                Right mainRt -> continue $ Initialized (AgentInfo mainAgentDescription mainRt loadedAgents okRuntimes)

type PromptModifier = Text -> Text

initAgent ::
    Tracer IO Trace ->
    [(Text, OpenAI.ApiKey)] ->
    PromptModifier ->
    [Agent.Runtime] ->
    FileLoader.AgentDescription ->
    IO (Either String Agent.Runtime)
initAgent _ _ _ _ (FileLoader.Unspecified _) = pure $ Left "unspecified agent unsupported"
initAgent tracer keys modifyPrompt helperAgents (FileLoader.OpenAIAgentDescription desc) = do
    case (lookup desc.apiKeyId keys, OpenAI.parseFlavor desc.flavor) of
        (_, Nothing) ->
            pure $ Left ("could not parse flavor " <> Text.unpack desc.flavor)
        (Nothing, _) ->
            pure $ Left ("could not find key " <> Text.unpack desc.apiKeyId)
        (Just key, Just flavor) ->
            Agent.newRuntime
                desc.slug
                desc.announce
                (contramap AgentTrace tracer)
                key
                ( OpenAI.Model
                    flavor
                    (OpenAI.ApiBaseUrl desc.modelUrl)
                    desc.modelName
                    ( OpenAI.SystemPrompt $
                        modifyPrompt $
                            Text.unlines desc.systemPrompt
                    )
                )
                desc.toolDirectory
                [Agent.turnAgentRuntimeIntoIOTool rt | rt <- helperAgents]

augmentMainAgentPromptWithSubAgents :: [Agent.Runtime] -> Text -> Text
augmentMainAgentPromptWithSubAgents [] base = base
augmentMainAgentPromptWithSubAgents agents base =
    Text.unlines
        [ base
        , ""
        , "==="
        , "The helper agents you can query using json tools are as follows:"
        , Text.unlines $
            Maybe.mapMaybe declareAgent agents
        , ""
        , "If an helper agent fails, do not retry and abdicate"
        ]
  where
    declareAgent :: Agent.Runtime -> Maybe Text
    declareAgent desc =
        Just $ Text.unwords ["*", desc.agentSlug, ":", desc.agentAnnounce]

readApiKeys :: FilePath -> IO (Maybe ApiKeys)
readApiKeys path =
    Aeson.decode <$> LByteString.readFile path

flattenOpenAIKeys :: ApiKeys -> [(Text, OpenAI.ApiKey)]
flattenOpenAIKeys (ApiKeys keys) =
    [(k.apiKeyId, OpenAI.ApiKey $ Text.encodeUtf8 k.apiKeyValue) | k <- keys]

readOpenApiKeysFile :: FilePath -> IO [(Text, OpenAI.ApiKey)]
readOpenApiKeysFile path =
    maybe [] flattenOpenAIKeys <$> readApiKeys path

traceWaitingOpenAIRateLimits :: OpenAI.ApiLimits -> (OpenAI.WaitAction -> IO ()) -> Tracer IO Trace
traceWaitingOpenAIRateLimits lims onWait = Tracer f
  where
    f (AgentTrace (Agent.AgentTrace_Conversation _ _ _ (Agent.LLMTrace _ tr))) =
        runTracer (OpenAI.waitRateLimit lims onWait) tr
    f _ = pure ()
