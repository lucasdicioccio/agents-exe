{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Agents.Agent where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as CByteString
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Either as Either
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import Data.Semigroup (sconcat)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Prod.Tracer (Tracer (..), contramap)

import System.Agents.Base (Agent, AgentDescription (..), AgentId, AgentSlug, newConversationId)
import qualified System.Agents.FileLoader as FileLoader
import qualified System.Agents.LLMs.OpenAI as LLM
import qualified System.Agents.LLMs.OpenAI as OpenAI
import System.Agents.Runtime (Runtime (..))
import qualified System.Agents.Runtime as Runtime
import System.Agents.ToolRegistration
import qualified System.Agents.Tools.IO as IOTools

import System.Agents.ApiKeys

readOpenApiKeysFile :: FilePath -> IO LoadedApiKeys
readOpenApiKeysFile path =
    maybe [] flattenOpenAIKeys <$> readApiKeys path
  where
    flattenOpenAIKeys :: ApiKeys -> [(Text, OpenAI.ApiKey)]
    flattenOpenAIKeys (ApiKeys keys) =
        [(k.apiKeyId, OpenAI.ApiKey $ Text.encodeUtf8 k.apiKeyValue) | k <- keys]

    readApiKeys :: FilePath -> IO (Maybe ApiKeys)
    readApiKeys path =
        Aeson.decode <$> LByteString.readFile path

data Trace
    = AgentTrace Runtime.Trace
    | DataLoadingTrace FileLoader.Trace
    deriving (Show)

data Props
    = Props
    { apiKeys :: LoadedApiKeys
    , rootAgentFile :: FilePath
    , interactiveTracer :: Tracer IO Trace
    }

data AgentTree = AgentTree
    { agentBase :: Agent
    , agentRuntime :: Runtime.Runtime
    , agentChildren :: [AgentTree]
    }

data AgentTree2 = AgentTree2
    { agentBase2 :: Agent
    , agentChildren2 :: [AgentTree2]
    }

agentLeaf :: Agent -> Runtime.Runtime -> AgentTree
agentLeaf a rt =
    AgentTree a rt []

extractChildrenAgents :: FileLoader.Agents -> [Agent]
extractChildrenAgents xs = [a | (AgentDescription a) <- xs.agents]

data LoadingError
    = AgentLoadingError FileLoader.InvalidAgentError
    | OtherError String
    deriving (Show)

data LoadAgentResult
    = Errors (NonEmpty.NonEmpty LoadingError)
    | Initialized AgentTree

loadAgentTreeConfig :: Props -> IO (Either (NonEmpty.NonEmpty LoadingError) AgentTree2)
loadAgentTreeConfig props = do
    let tracer = props.interactiveTracer
    boss <- FileLoader.loadJsonFile (contramap DataLoadingTrace tracer) props.rootAgentFile
    case boss of
        Left err ->
            pure $ Left (NonEmpty.singleton (AgentLoadingError err))
        Right (AgentDescription agent) -> do
            subConfigs <- FileLoader.listJsonDirectory agent.toolDirectory
            let propz = [props{rootAgentFile = c} | c <- subConfigs]
            (kos, oks) <- fmap Either.partitionEithers $ traverse loadAgentTreeConfig propz
            case NonEmpty.nonEmpty kos of
                Just errs -> do
                    pure $ Left $ sconcat errs
                Nothing -> do
                    pure $ Right $ AgentTree2 agent oks

loadAgentTree :: Props -> AgentTree2 -> IO (Either (NonEmpty.NonEmpty LoadingError) AgentTree)
loadAgentTree props tree = do
    let tracer = props.interactiveTracer
    agentRuntimes <- traverse (loadAgentTree props) tree.agentChildren2
    let (kos, oks) = Either.partitionEithers agentRuntimes
    case NonEmpty.nonEmpty kos of
        Just errs -> pure $ Left $ sconcat errs
        Nothing -> do
            let okRuntimes = fmap agentRuntime oks
            rt <-
                initAgent
                    tracer
                    props.apiKeys
                    (augmentMainAgentPromptWithSubAgents okRuntimes)
                    okRuntimes
                    (AgentDescription tree.agentBase2)
            case rt of
                Left err ->
                    pure $ Left $ NonEmpty.singleton $ OtherError err
                Right agentRt ->
                    pure $ Right $ AgentTree tree.agentBase2 agentRt oks

loadAgentRuntime :: Props -> IO LoadAgentResult
loadAgentRuntime props = do
    cfgs <- loadAgentTreeConfig props
    case cfgs of
        Left errs -> pure $ Errors errs
        Right cfg -> do
            tree <- loadAgentTree props cfg
            case tree of
                Left errs -> pure $ Errors errs
                Right ok -> pure $ Initialized ok

withAgentRuntime :: Props -> (LoadAgentResult -> IO a) -> IO a
withAgentRuntime props continue = do
    loadAgentRuntime props >>= continue

type PromptModifier = Text -> Text

initAgent ::
    Tracer IO Trace ->
    [(Text, OpenAI.ApiKey)] ->
    PromptModifier ->
    [Runtime.Runtime] ->
    AgentDescription ->
    IO (Either String Runtime.Runtime)
initAgent tracer keys modifyPrompt helperAgents (AgentDescription desc) = do
    case (lookup desc.apiKeyId keys, OpenAI.parseFlavor desc.flavor) of
        (_, Nothing) ->
            pure $ Left ("could not parse flavor " <> Text.unpack desc.flavor)
        (Nothing, _) ->
            pure $ Left ("could not find key " <> Text.unpack desc.apiKeyId)
        (Just key, Just flavor) ->
            Runtime.newRuntime
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
                [turnAgentRuntimeIntoIOTool rt | rt <- helperAgents]

augmentMainAgentPromptWithSubAgents :: [Runtime.Runtime] -> Text -> Text
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
    declareAgent :: Runtime.Runtime -> Maybe Text
    declareAgent desc =
        Just $ Text.unwords ["*", desc.agentSlug, ":", desc.agentAnnounce]

type LoadedApiKeys = [(Text, OpenAI.ApiKey)]

traceWaitingOpenAIRateLimits :: OpenAI.ApiLimits -> (OpenAI.WaitAction -> IO ()) -> Tracer IO Trace
traceWaitingOpenAIRateLimits lims onWait = Tracer f
  where
    f (AgentTrace (Runtime.AgentTrace_Conversation _ _ _ (Runtime.LLMTrace _ tr))) =
        runTracer (OpenAI.waitRateLimit lims onWait) tr
    f _ = pure ()

-------------------------------------------------------------------------------
data PromptOtherAgent
    = PromptOtherAgent Text

instance Aeson.FromJSON PromptOtherAgent where
    parseJSON = Aeson.withObject "PromptOtherAgent" $ \v -> do
        PromptOtherAgent
            <$> v Aeson..: "what"

turnAgentRuntimeIntoIOTool ::
    Runtime -> AgentSlug -> AgentId -> ToolRegistration
turnAgentRuntimeIntoIOTool rt callerSlug callerId =
    registerIOScriptInLLM io props
  where
    props =
        [ LLM.ParamProperty
            { LLM.propertyKey = "what"
            , LLM.propertyType = "string"
            , LLM.propertyDescription = "the prompt to the other agent"
            }
        ]
    io =
        IOTools.IOScript
            ( IOTools.IOScriptDescription
                ("prompt_agent_" <> rt.agentSlug)
                ("aks a prompt to the expert agent: " <> rt.agentSlug)
            )
            (\conversationId (PromptOtherAgent txt) -> runSubAgent conversationId txt)

    runSubAgent conversationId txt = do
        subConversationId <- newConversationId
        Runtime.handleConversation
            (nestTracer conversationId rt)
            conversationFunctions
            subConversationId
            txt

    nestTracer conversationId childRuntime =
        childRuntime
            { agentTracer =
                contramap
                    ( Runtime.AgentTrace_Conversation
                        callerSlug
                        callerId
                        conversationId
                        . Runtime.ChildrenTrace
                    )
                    childRuntime.agentTracer
            }

    conversationFunctions =
        Runtime.ConversationFunctions
            (pure Nothing)
            (\_hist -> pure ())
            (\err -> pure $ "sorry I got an error: " <> (CByteString.pack err))
            (\done -> pure $ maybe "i could not find an answer" Text.encodeUtf8 $ LLM.locateResponseText done)
