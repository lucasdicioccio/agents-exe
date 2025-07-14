{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Agents.Runtime.Runtime (
    Runtime (..),
    addTracer,
    newRuntime,
    triggerRefreshTools,
) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.STM (STM, atomically, readTVar)
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVarIO, takeTMVar, tryPutTMVar)
import qualified Data.Aeson.Types as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as CByteString
import Data.Foldable (toList)
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID
import qualified Prod.Background as Background
import Prod.Tracer (Tracer, contramap, runTracer, traceBoth)
import qualified System.Agents.HttpClient as HttpClient

import System.Agents.Base
import qualified System.Agents.LLMs.OpenAI as LLM
import System.Agents.ToolRegistration
import System.Agents.Tools
import qualified System.Agents.Tools.Bash as BashTools
import qualified System.Agents.Tools.BashToolbox as BashToolbox
import qualified System.Agents.Tools.IO as IOTools
import qualified System.Agents.Tools.McpToolbox as McpTools

import System.Agents.Runtime.Trace

-------------------------------------------------------------------------------

data Runtime
    = Runtime
    { agentSlug :: AgentSlug
    , agentId :: AgentId
    , agentAnnounce :: AgentAnnounce
    , agentTracer :: Tracer IO Trace
    , agentAuthenticatedHttpClientRuntime :: HttpClient.Runtime
    , agentModel :: LLM.Model
    , agentTools :: IO [ToolRegistration]
    , agentTriggerRefreshTools :: STM Bool
    }

{- | Adds an extra tracer to the runtime, hence returning a modified Runtime.

In current implementation (arbitrary) the extra tracer is ran before the
already-in-place one.
-}
addTracer :: Runtime -> (Tracer IO Trace) -> Runtime
addTracer rt t = rt{agentTracer = traceBoth t rt.agentTracer}

triggerRefreshTools :: Runtime -> IO Bool
triggerRefreshTools rt = atomically $ rt.agentTriggerRefreshTools

-- todo: directly ask for effects returning registrations
type ToolboxDirectory = FilePath
type IOToolBuilder = AgentSlug -> AgentId -> ToolRegistration
type McpToolConfig = McpTools.Toolbox

newRuntime ::
    AgentSlug ->
    AgentAnnounce ->
    Tracer IO Trace ->
    LLM.ApiKey ->
    LLM.Model ->
    ToolboxDirectory ->
    [IOToolBuilder] ->
    [McpToolConfig] ->
    IO (Either String Runtime)
newRuntime slug announce tracer apiKey model tooldir mkIoTools mcpToolboxes = do
    uid <- newAgentId
    let ioTools = [mk slug uid | mk <- mkIoTools]
    toolz <- BashToolbox.initializeBackroundToolbox (contramap (AgentTrace_Loading slug uid) tracer) tooldir
    case toolz of
        Left err -> pure $ Left (show err)
        Right toolbox -> do
            let auth = HttpClient.BearerToken $ Text.decodeUtf8 $ LLM.revealApiKey apiKey
            httpRt <- HttpClient.newRuntime auth
            let appendIOTools xs = ioTools <> xs
            let registerTools xs = fmap registerBashToolInLLM xs
            let bkgToolsWithIOTools = fmap (appendIOTools . registerTools) toolbox.tools

            let readTools = (<>) <$> Background.readBackgroundVal bkgToolsWithIOTools <*> readMcpToolsRegistrations
            let rt = Runtime slug uid announce tracer httpRt model readTools toolbox.triggerReload
            pure $ Right rt
  where
    readMcpToolsRegistrations :: IO [ToolRegistration]
    readMcpToolsRegistrations = do
        lists <- traverse (atomically . readTVar . McpTools.toolsList) $ mcpToolboxes
        let reg tb tds = [registerMcpToolInLLM tb td | td <- tds]
        pure $ mconcat $ zipWith reg mcpToolboxes lists

data ConversationFunctions r
    = ConversationFunctions
    { waitAdditionalQuery :: IO (Maybe Text)
    , onProgress :: LLM.History -> IO ()
    , onError :: String -> IO r
    , onDone :: LLM.History -> IO r
    }

data ContinueD
    = PromptMore !PingPongQuery !LLM.History
    | OnDone !LLM.History
    | OnError !String

type ContinueFunction r = ContinueD -> IO r

handleConversation :: forall r. Runtime -> ConversationFunctions r -> ConversationId -> Text -> IO r
handleConversation rt functions conversationId startingPrompt = do
    runTracer
        rt.agentTracer
        (AgentTrace_Conversation rt.agentSlug rt.agentId conversationId NewConversation)
    go conversationId
  where
    go cId =
        let step :: LLM.History -> PingPongQuery -> IO r
            step = stepWith cId rt functions continue

            continue :: ContinueFunction r
            continue (PromptMore q h) = step h q
            continue (OnError err) = functions.onError err
            continue (OnDone h) = functions.onDone h
         in step Seq.empty (SomeQueryToAnswer startingPrompt)

stepWith ::
    ConversationId ->
    Runtime ->
    ConversationFunctions r ->
    ContinueFunction r ->
    LLM.History ->
    PingPongQuery ->
    IO r
stepWith conversationId rt _ next hist NoQuery = do
    let memoTracer = contramap (AgentTrace_Memorize rt.agentSlug rt.agentId conversationId) rt.agentTracer
    stepUUID <- newStepId
    runTracer memoTracer (InteractionDone hist stepUUID)
    next $ OnDone hist
stepWith conversationId rt@(Runtime _ _ _ tracer httpRt model tools _) functions next hist pendingQuery = do
    let convTracer = contramap (AgentTrace_Conversation rt.agentSlug rt.agentId conversationId) tracer
    let memoTracer = contramap (AgentTrace_Memorize rt.agentSlug rt.agentId conversationId) tracer
    let query = getQueryToAnswer pendingQuery
    registeredTools <- tools
    let llmTools = fmap declareTool registeredTools
    let payload = LLM.simplePayload model llmTools hist query
    stepUUID <- newStepId
    runTracer memoTracer (Calling pendingQuery hist stepUUID)
    llmResponse <- LLM.callLLMPayload (contramap (LLMTrace stepUUID) convTracer) httpRt model.modelBaseUrl payload
    case Aeson.parseEither LLM.parseLLMResponse =<< llmResponse of
        Right rsp -> do
            let hist02 = hist <> Seq.singleton (LLM.PromptAnswered query rsp)
            functions.onProgress hist02
            runTracer memoTracer (GotResponse pendingQuery hist02 stepUUID rsp)
            case Maybe.fromMaybe [] rsp.rspToolCalls of
                [] -> do
                    runTracer convTracer WaitingForPrompt
                    nextQuery <- functions.waitAdditionalQuery
                    next $
                        PromptMore
                            (maybe NoQuery SomeQueryToAnswer nextQuery)
                            hist02
                toolcalls -> do
                    responses <- mapConcurrently (llmCallTool conversationId convTracer registeredTools) toolcalls
                    let toolResults = fmap LLM.ToolCalled $ yankResults responses
                    let hist03 = hist02 <> Seq.fromList toolResults
                    next $
                        PromptMore
                            GaveToolAnswers
                            hist03
        Left err ->
            next $ OnError err

llmCallTool ::
    ConversationId ->
    Tracer IO ConversationTrace ->
    [ToolRegistration] ->
    LLM.ToolCall ->
    IO (CallResult LLM.ToolCall)
llmCallTool conversationId tracer registrations call =
    let
        script =
            Maybe.listToMaybe $
                Maybe.mapMaybe (\r -> r.findTool call) registrations
        args = call.toolCallFunction.toolCallFunctionArgs
        spec = (,) <$> script <*> args
     in
        case spec of
            Nothing -> pure $ ToolNotFound call
            Just (t, v) -> do
                toolcallUUID <- newStepId
                ret <- t.toolRun (contramap (RunToolTrace toolcallUUID) tracer) conversationId v
                pure $ mapCallResult (const call) ret

-- TODO: improve on message handling here so that yankResults or default values are agent-specific
yankResults :: [CallResult call] -> [(call, ByteString)]
yankResults xs = fmap (\x -> (extractCall x, f x)) xs
  where
    f :: CallResult c -> ByteString
    f (ToolNotFound _) = "the tool was not found"
    f (BashToolError _ err) = CByteString.unlines ["the tool errored with:", CByteString.pack $ show err]
    f (IOToolError _ err) = CByteString.unlines ["the tool errored with:", CByteString.pack $ show err]
    f (ToolSuccess _ v) = v
