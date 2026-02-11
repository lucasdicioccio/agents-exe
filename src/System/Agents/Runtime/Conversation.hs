{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Agents.Runtime.Conversation (
    ConversationFunctions (..),
    handleConversation,
) where

import Control.Concurrent.Async (mapConcurrently)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Prod.Tracer (Tracer, contramap, runTracer)

-------------------------------------------------------------------------------
import System.Agents.Base
import qualified System.Agents.LLMs.OpenAI as OpenAI
import qualified System.Agents.MCP.Base as Mcp
import System.Agents.ToolRegistration
import System.Agents.Tools
import System.Agents.Tools.Base

-------------------------------------------------------------------------------
import System.Agents.Runtime.Base
import System.Agents.Runtime.Runtime
import System.Agents.Runtime.Trace

-------------------------------------------------------------------------------

data ConversationFunctions r
    = ConversationFunctions
    { waitAdditionalQuery :: IO (Maybe Text)
    , onProgress :: OpenAI.History -> IO ()
    , onError :: String -> IO r
    , onDone :: OpenAI.History -> IO r
    }

data ContinueD
    = PromptMore !PingPongQuery !OpenAI.History
    | OnDone !OpenAI.History
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
        let step :: OpenAI.History -> PingPongQuery -> IO r
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
    OpenAI.History ->
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
    let payload = OpenAI.renderPayload model llmTools hist query
    stepUUID <- newStepId
    runTracer memoTracer (Calling pendingQuery hist stepUUID)
    llmResponse <- OpenAI.callLLMPayload (contramap (LLMTrace stepUUID) convTracer) httpRt model.modelBaseUrl payload
    case Aeson.parseEither OpenAI.parseLLMResponse =<< llmResponse of
        Right rsp -> do
            let hist02 = hist <> Seq.singleton (OpenAI.PromptAnswered query rsp)
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
                    let toolResults = fmap OpenAI.ToolCalled $ yankResults responses
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
    OpenAI.ToolCall ->
    IO (CallResult OpenAI.ToolCall)
llmCallTool conversationId tracer registrations call =
    let
        script =
            Maybe.listToMaybe $
                Maybe.mapMaybe (\r -> r.findTool call) registrations
        targs = call.toolCallFunction.toolCallFunctionArgs
        spec = (,) <$> script <*> targs
     in
        case spec of
            Nothing -> pure $ ToolNotFound call
            Just (t, v) -> do
                toolcallUUID <- newStepId
                ret <- t.toolRun (contramap (RunToolTrace toolcallUUID) tracer) conversationId v
                pure $ mapCallResult (const call) ret

yankResults :: [CallResult call] -> [(call, OpenAI.ToolResponse)]
yankResults xs = fmap (\x -> (extractCall x, f x)) xs
  where
    f :: CallResult c -> OpenAI.ToolResponse
    f (ToolNotFound _) =
        OpenAI.ToolNotFound
    f (BashToolError _ err) =
        OpenAI.ToolFailure $ Text.pack $ show err
    f (McpToolError _ err) =
        OpenAI.ToolFailure $ (Text.unlines ["tool-error", Text.pack (show err)])
    f (McpToolResult _ res) =
        case res.isError of
            (Just True) -> OpenAI.ToolFailure (Text.unlines ["result-is-error:", aesonBlobify res])
            _ -> interpretMcpContents res.content
    f (IOToolError _ err) =
        OpenAI.ToolFailure $ Text.pack $ show err
    f (BlobToolSuccess _ v) =
        OpenAI.TextToolResponse (NonEmpty.singleton $ Text.decodeUtf8 v)

    aesonBlobify :: (Aeson.ToJSON a) => a -> Text
    aesonBlobify = Text.decodeUtf8 . LByteString.toStrict . Aeson.encode

    interpretMcpContents :: [Mcp.Content] -> OpenAI.ToolResponse
    interpretMcpContents items =
        case NonEmpty.nonEmpty items of
            Nothing -> OpenAI.ToolFailure "tool gave no contents"
            Just ys -> OpenAI.TextToolResponse (fmap interpretMcpContent ys)

    interpretMcpContent :: Mcp.Content -> Text
    interpretMcpContent (Mcp.TextContent impl) = impl.text
    interpretMcpContent (Mcp.ImageContent impl) = impl.data_ -- todo: consider giving the whole json payload
    interpretMcpContent (Mcp.EmbeddedResourceContent (Mcp.TextResourceContents impl)) = impl.text
    interpretMcpContent (Mcp.EmbeddedResourceContent (Mcp.BlobResourceContents impl)) = impl.blob

-------------------------------------------------------------------------------
