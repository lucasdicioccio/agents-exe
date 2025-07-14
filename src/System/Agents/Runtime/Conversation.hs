{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Agents.Runtime.Conversation (
    ConversationFunctions (..),
    handleConversation,
) where

import Control.Concurrent.Async (mapConcurrently)
import qualified Data.Aeson.Types as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as CByteString
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Prod.Tracer (Tracer, contramap, runTracer)

import System.Agents.Base
import qualified System.Agents.LLMs.OpenAI as LLM
import System.Agents.ToolRegistration
import System.Agents.Tools

import System.Agents.Runtime.Runtime
import System.Agents.Runtime.Trace

-------------------------------------------------------------------------------

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
    let payload = LLM.renderPayload model llmTools hist query
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
