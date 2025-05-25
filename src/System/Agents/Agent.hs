{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Agents.Agent where

import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.STM (STM, atomically)
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
import Prod.Tracer (Tracer, contramap, runTracer)
import qualified System.Agents.HttpClient as HttpClient

import System.Agents.Base
import qualified System.Agents.LLMs.OpenAI as LLM
import System.Agents.Tools
import qualified System.Agents.Tools.Bash as BashTools
import qualified System.Agents.Tools.IO as IOTools

-------------------------------------------------------------------------------

data Trace
    = AgentTrace_Loading !AgentSlug !AgentId !LoadingTrace
    | AgentTrace_Memorize !AgentSlug !AgentId !ConversationId !MemorizeTrace
    | AgentTrace_Conversation !AgentSlug !AgentId !ConversationId !ConversationTrace
    deriving (Show)

traceAgentSlug :: Trace -> AgentSlug
traceAgentSlug (AgentTrace_Loading slug _ _) = slug
traceAgentSlug (AgentTrace_Conversation slug _ _ _) = slug
traceAgentSlug (AgentTrace_Memorize slug _ _ _) = slug

traceAgentId :: Trace -> AgentId
traceAgentId (AgentTrace_Loading _ aId _) = aId
traceAgentId (AgentTrace_Conversation _ aId _ _) = aId
traceAgentId (AgentTrace_Memorize _ aId _ _) = aId

traceConversationId :: Trace -> Maybe ConversationId
traceConversationId (AgentTrace_Loading _ _ _) = Nothing
traceConversationId (AgentTrace_Conversation _ _ cId _) = Just cId
traceConversationId (AgentTrace_Memorize _ _ cId _) = Just cId

data ConversationTrace
    = -- | useful for recording an edge between a pair of parent/child conversation
      NewConversation
    | LLMTrace !StepId !LLM.Trace
    | RunToolTrace !StepId !ToolTrace
    | ChildrenTrace !Trace
    deriving (Show)

data LoadingTrace
    = BashToolsLoadingTrace !BashTools.LoadTrace
    | ReloadToolsTrace !(Background.Track [BashTools.ScriptDescription])
    deriving (Show)

data MemorizeTrace
    = Calling !PendingQuery !LLM.History !StepId
    | GotResponse !PendingQuery !LLM.History !StepId !LLM.Response
    | InteractionDone !LLM.History !StepId
    deriving (Show)

-------------------------------------------------------------------------------

type ToolRuntimeArg = ConversationId
type ToolRegistration = Registration ToolRuntimeArg LLM.Tool LLM.ToolCall

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

triggerRefreshTools :: Runtime -> IO Bool
triggerRefreshTools rt = atomically $ rt.agentTriggerRefreshTools

type BackgroundTools =
    ( Background.BackgroundVal [BashTools.ScriptDescription]
    , STM Bool
    )

backgroundToolDir ::
    Tracer IO LoadingTrace ->
    FilePath ->
    IO (Either String BackgroundTools)
backgroundToolDir tracer tooldir = do
    (startingTools, errs) <- loadToolsOnce
    if null errs
        then do
            lock <- newEmptyTMVarIO
            bkgTools <-
                Background.background
                    (contramap ReloadToolsTrace tracer)
                    ()
                    startingTools
                    (const (reloadToolsOnTrigger lock))
            let triggerReloadTools = tryPutTMVar lock ()
            pure $ Right (bkgTools, triggerReloadTools)
        else do
            pure $ Left (unlines ("errors when loading tools:" : fmap show errs))
  where
    loadToolsOnce :: IO ([BashTools.ScriptDescription], [BashTools.InvalidScriptError])
    loadToolsOnce = do
        (scripts, errs) <- BashTools.loadDirectory (contramap BashToolsLoadingTrace tracer) tooldir
        pure (scripts.scriptDescriptions, errs)

    -- returns an extra '()' as a state for Background.background
    reloadToolsOnTrigger :: TMVar () -> IO ([BashTools.ScriptDescription], ())
    reloadToolsOnTrigger lock = do
        _ <- atomically $ takeTMVar lock
        tools <- reloadTools
        pure (tools, ())

    reloadTools :: IO [BashTools.ScriptDescription]
    reloadTools = do
        (tools, _) <- loadToolsOnce
        pure tools

newRuntime ::
    AgentSlug ->
    AgentAnnounce ->
    Tracer IO Trace ->
    LLM.ApiKey ->
    LLM.Model ->
    FilePath ->
    [AgentSlug -> AgentId -> ToolRegistration] ->
    IO (Either String Runtime)
newRuntime slug announce tracer apiKey model tooldir mkIoTools = do
    uid <- newAgentId
    let ioTools = [mk slug uid | mk <- mkIoTools]
    toolz <- backgroundToolDir (contramap (AgentTrace_Loading slug uid) tracer) tooldir
    case toolz of
        Left err -> pure $ Left err
        Right (bkgTools, triggerReloadTools) -> do
            let auth = HttpClient.BearerToken $ Text.decodeUtf8 $ LLM.revealApiKey apiKey
            httpRt <- HttpClient.newRuntime auth
            let appendIOTools xs = ioTools <> xs
            let registerTools xs = fmap registerBashToolInLLM xs
            let bkgToolsWithIOTools = fmap (appendIOTools . registerTools) bkgTools
            let readTools = Background.readBackgroundVal bkgToolsWithIOTools
            let rt = Runtime slug uid announce tracer httpRt model readTools triggerReloadTools
            pure $ Right rt

data AgentFunctions r
    = AgentFunctions
    { waitAdditionalQuery :: IO (Maybe Text)
    , onError :: String -> IO r
    , onDone :: LLM.History -> IO r
    }

data PendingQuery
    = SomeQuery Text
    | GaveToolAnswers
    | Done
    deriving (Show)

getQuery :: PendingQuery -> Maybe Text
getQuery (SomeQuery t) = Just t
getQuery _ = Nothing

handleConversation :: forall r. Runtime -> AgentFunctions r -> Text -> IO r
handleConversation rt functions startingPrompt = do
    conversationId <- newConversationId
    runTracer
        rt.agentTracer
        (AgentTrace_Conversation rt.agentSlug rt.agentId conversationId NewConversation)
    go conversationId
  where
    go conversationId =
        let step :: LLM.History -> PendingQuery -> IO r
            step = stepWith conversationId rt functions continue

            continue :: ContinueFunction r
            continue (PromptMore q h) = step h q
            continue (OnError err) = functions.onError err
            continue (OnDone h) = functions.onDone h
         in step Seq.empty (SomeQuery startingPrompt)

type ContinueFunction r = ContinueD -> IO r

data ContinueD
    = PromptMore !PendingQuery !LLM.History
    | OnDone !LLM.History
    | OnError !String

type StepId = UUID

stepWith ::
    ConversationId ->
    Runtime ->
    AgentFunctions r ->
    ContinueFunction r ->
    LLM.History ->
    PendingQuery ->
    IO r
stepWith conversationId rt _ next hist Done = do
    let infoTracer = contramap (AgentTrace_Memorize rt.agentSlug rt.agentId conversationId) rt.agentTracer
    stepUUID <- UUID.nextRandom
    runTracer infoTracer (InteractionDone hist stepUUID)
    next $ OnDone hist
stepWith conversationId rt@(Runtime _ _ _ tracer httpRt model tools _) functions next hist pendingQuery = do
    let convTracer = contramap (AgentTrace_Conversation rt.agentSlug rt.agentId conversationId) tracer
    let infoTracer = contramap (AgentTrace_Memorize rt.agentSlug rt.agentId conversationId) tracer
    let query = getQuery pendingQuery
    registeredTools <- tools
    let llmTools = fmap declareTool registeredTools
    let payload = LLM.simplePayload model llmTools hist query
    stepUUID <- UUID.nextRandom
    runTracer infoTracer (Calling pendingQuery hist stepUUID)
    llmResponse <- LLM.callLLMPayload (contramap (LLMTrace stepUUID) convTracer) httpRt model.modelBaseUrl payload
    case Aeson.parseEither LLM.parseLLMResponse =<< llmResponse of
        Right rsp -> do
            let hist02 = hist <> Seq.singleton (LLM.PromptAnswered query rsp)
            runTracer infoTracer (GotResponse pendingQuery hist02 stepUUID rsp)
            case Maybe.fromMaybe [] rsp.rspToolCalls of
                [] -> do
                    nextQuery <- functions.waitAdditionalQuery
                    next $
                        PromptMore
                            (maybe Done SomeQuery nextQuery)
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
                toolcallUUID <- UUID.nextRandom
                ret <- t.toolRun (contramap (RunToolTrace toolcallUUID) tracer) conversationId v
                pure $ mapCallResult (const call) ret

registerBashToolInLLM ::
    BashTools.ScriptDescription ->
    ToolRegistration
registerBashToolInLLM script =
    let
        bash2LLMName :: BashTools.ScriptDescription -> LLM.ToolName
        bash2LLMName bash = LLM.ToolName (mconcat ["bash_", bash.scriptInfo.scriptSlug])

        matchName :: BashTools.ScriptDescription -> LLM.ToolCall -> Bool
        matchName bash call = bash2LLMName bash == call.toolCallFunction.toolCallFunctionName

        mapToolDescriptionBash2LLM :: BashTools.ScriptDescription -> LLM.Tool
        mapToolDescriptionBash2LLM bash =
            LLM.Tool
                { LLM.toolName = bash2LLMName bash
                , LLM.toolDescription = bash.scriptInfo.scriptDescription
                , LLM.toolParamProperties = fmap mapArg bash.scriptInfo.scriptArgs
                }

        mapArg :: BashTools.ScriptArg -> LLM.ParamProperty
        mapArg arg =
            LLM.ParamProperty
                { LLM.propertyKey = arg.argName
                , LLM.propertyType = arg.argBackingTypeString
                , LLM.propertyDescription = arg.argDescription
                }

        tool :: Tool ToolRuntimeArg ()
        tool = bashTool script

        find :: LLM.ToolCall -> Maybe (Tool ToolRuntimeArg LLM.ToolCall)
        find call = if matchName script call then Just (mapToolCall (const call) tool) else Nothing
     in
        Registration tool (mapToolDescriptionBash2LLM script) find

{- | registers an IO Script, since we have not yet decided on a way to capture the
shape of the tool for IOScript (ideally some generics or something like Data.Aeson.Encoding) we take the whole Tool definition
-}
registerIOScriptInLLM ::
    (Aeson.FromJSON a) =>
    IOTools.IOScript ToolRuntimeArg a ByteString ->
    [LLM.ParamProperty] ->
    ToolRegistration
registerIOScriptInLLM script llmProps =
    let
        io2LLMName :: IOTools.IOScript ToolRuntimeArg a b -> LLM.ToolName
        io2LLMName io = LLM.ToolName (mconcat ["io_", io.description.ioSlug])

        matchName :: IOTools.IOScript ToolRuntimeArg a b -> LLM.ToolCall -> Bool
        matchName io call = io2LLMName io == call.toolCallFunction.toolCallFunctionName

        tool :: Tool ToolRuntimeArg ()
        tool = ioTool script

        find :: LLM.ToolCall -> Maybe (Tool ToolRuntimeArg LLM.ToolCall)
        find call = if matchName script call then Just (mapToolCall (const call) tool) else Nothing

        llmTool :: LLM.Tool
        llmTool =
            LLM.Tool
                { LLM.toolName = io2LLMName script
                , LLM.toolDescription = script.description.ioDescription
                , LLM.toolParamProperties = llmProps
                }
     in
        Registration tool llmTool find

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

    runSubAgent conversationId txt =
        handleConversation (nestTracer conversationId rt) agentFunctions txt

    nestTracer conversationId childRuntime =
        childRuntime
            { agentTracer =
                contramap
                    ( AgentTrace_Conversation
                        callerSlug
                        callerId
                        conversationId
                        . ChildrenTrace
                    )
                    childRuntime.agentTracer
            }

    agentFunctions =
        AgentFunctions
            (pure Nothing)
            (\err -> pure $ "sorry I got an error: " <> (CByteString.pack err))
            (\done -> pure $ maybe "i could not find an answer" Text.encodeUtf8 $ locateResponse done)

    locateResponse :: LLM.History -> Maybe Text
    locateResponse hist = do
        rsp <-
            Maybe.listToMaybe $
                Maybe.mapMaybe viewResponse $
                    toList $
                        Seq.reverse hist
        rsp.rspContent
      where
        viewResponse :: LLM.HistoryItem -> Maybe LLM.Response
        viewResponse (LLM.PromptAnswered _ rsp) = Just rsp
        viewResponse _ = Nothing

-- TODO: improve on message handling here so that yankResults or default values are agent-specific
yankResults :: [CallResult call] -> [(call, ByteString)]
yankResults xs = fmap (\x -> (extractCall x, f x)) xs
  where
    f :: CallResult c -> ByteString
    f (ToolNotFound _) = "the tool was not found"
    f (BashToolError _ err) = CByteString.unlines ["the tool errored with:", CByteString.pack $ show err]
    f (IOToolError _ err) = CByteString.unlines ["the tool errored with:", CByteString.pack $ show err]
    f (ToolSuccess _ v) = v
