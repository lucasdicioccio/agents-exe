{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Agents.OpenAI where

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
import qualified Prod.Background as Background
import Prod.Tracer (Tracer, contramap, runTracer)
import qualified System.Agents.HttpClient as HttpClient

import System.Agents.Base
import qualified System.Agents.LLMs.OpenAI as OpenAI
import System.Agents.Tools
import qualified System.Agents.Tools.Bash as BashTools
import qualified System.Agents.Tools.IO as IOTools

-------------------------------------------------------------------------------

data Trace
    = OpenAITrace !OpenAI.Trace
    | StepTrace !TraceStep
    | BashToolsLoadingTrace !BashTools.LoadTrace
    | ReloadToolsTrace !(Background.Track [BashTools.ScriptDescription])
    | RunToolTrace !ToolTrace
    | ChildrenTrace !AgentSlug Trace
    deriving (Show)

data TraceStep
    = GotResponse !OpenAI.Response
    deriving (Show)

-------------------------------------------------------------------------------

data Runtime
    = Runtime
    { agentSlug :: AgentSlug
    , agentAnnounce :: AgentAnnounce
    , agentTracer :: Tracer IO Trace
    , agentAuthenticatedHttpClientRuntime :: HttpClient.Runtime
    , agentModel :: OpenAI.Model
    , agentTools :: IO [Registration OpenAI.Tool OpenAI.ToolCall]
    , agentTriggerRefreshTools :: STM Bool
    }

triggerRefreshTools :: Runtime -> IO Bool
triggerRefreshTools rt = atomically $ rt.agentTriggerRefreshTools

type BackgroundTools =
    ( Background.BackgroundVal [BashTools.ScriptDescription]
    , STM Bool
    )

backgroundToolDir ::
    Tracer IO Trace ->
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
    OpenAI.ApiKey ->
    OpenAI.Model ->
    FilePath ->
    [Registration OpenAI.Tool OpenAI.ToolCall] ->
    IO (Either String Runtime)
newRuntime slug announce tracer apiKey model tooldir ioTools = do
    toolz <- backgroundToolDir tracer tooldir
    case toolz of
        Left err -> pure $ Left err
        Right (bkgTools, triggerReloadTools) -> do
            let auth = HttpClient.BearerToken $ Text.decodeUtf8 $ OpenAI.revealApiKey apiKey
            httpRt <- HttpClient.newRuntime auth
            let appendIOTools xs = ioTools <> xs
            let registerTools xs = fmap registerBashToolInOpenAI xs
            let bkgToolsWithIOTools = fmap (appendIOTools . registerTools) bkgTools
            let readTools = Background.readBackgroundVal bkgToolsWithIOTools
            let rt = Runtime slug announce tracer httpRt model readTools triggerReloadTools
            pure $ Right rt

data AgentFunctions r
    = AgentFunctions
    { waitAdditionalQuery :: IO (Maybe Text)
    , onError :: String -> IO r
    , onDone :: OpenAI.History -> IO r
    }

data PendingQuery
    = SomeQuery Text
    | GaveToolAnswers
    | Done

getQuery :: PendingQuery -> Maybe Text
getQuery (SomeQuery t) = Just t
getQuery _ = Nothing

openAIAgent :: forall r. Runtime -> AgentFunctions r -> Text -> IO r
openAIAgent rt functions startingPrompt =
    let step :: OpenAI.History -> PendingQuery -> IO r
        step = stepWith rt functions continue

        continue :: ContinueFunction r
        continue (PromptMore q h) = step h q
        continue (OnError err) = functions.onError err
        continue (OnDone h) = functions.onDone h
     in step Seq.empty (SomeQuery startingPrompt)

type ContinueFunction r = ContinueD -> IO r

data ContinueD
    = PromptMore !PendingQuery !OpenAI.History
    | OnDone !OpenAI.History
    | OnError !String

stepWith ::
    Runtime ->
    AgentFunctions r ->
    ContinueFunction r ->
    OpenAI.History ->
    PendingQuery ->
    IO r
stepWith _ _ next hist Done = next $ OnDone hist
stepWith (Runtime _ _ tracer httpRt model tools _) functions next hist pendingQuery = do
    let query = getQuery pendingQuery
    registeredTools <- tools
    let openAITools = fmap declareTool registeredTools
    let payload = OpenAI.simplePayload model openAITools hist query
    llmResponse <- OpenAI.callLLMPayload (contramap OpenAITrace tracer) httpRt payload
    case Aeson.parseEither OpenAI.parseLLMResponse =<< llmResponse of
        Right rsp -> do
            runTracer tracer (StepTrace $ GotResponse rsp)
            case Maybe.fromMaybe [] rsp.rspToolCalls of
                [] -> do
                    nextQuery <- functions.waitAdditionalQuery
                    next $
                        PromptMore
                            (maybe Done SomeQuery nextQuery)
                            (hist <> Seq.singleton (OpenAI.PromptAnswered query rsp))
                toolcalls -> do
                    responses <- mapConcurrently (openAICallTool tracer registeredTools) toolcalls
                    let toolResults = fmap OpenAI.ToolCalled $ yankResults responses
                    next $
                        PromptMore
                            GaveToolAnswers
                            (hist <> Seq.singleton (OpenAI.PromptAnswered query rsp) <> Seq.fromList toolResults)
        Left err ->
            next $ OnError err

openAICallTool ::
    Tracer IO Trace ->
    [Registration OpenAI.Tool OpenAI.ToolCall] ->
    OpenAI.ToolCall ->
    IO (CallResult OpenAI.ToolCall)
openAICallTool tracer registrations call =
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
                ret <- t.toolRun (contramap RunToolTrace tracer) v
                pure $ mapCallResult (const call) ret

registerBashToolInOpenAI ::
    BashTools.ScriptDescription ->
    Registration OpenAI.Tool OpenAI.ToolCall
registerBashToolInOpenAI script =
    let
        bash2OpenAIName :: BashTools.ScriptDescription -> OpenAI.ToolName
        bash2OpenAIName bash = OpenAI.ToolName (mconcat ["bash_", bash.scriptInfo.scriptSlug])

        matchName :: BashTools.ScriptDescription -> OpenAI.ToolCall -> Bool
        matchName bash call = bash2OpenAIName bash == call.toolCallFunction.toolCallFunctionName

        mapToolDescriptionBash2OpenAI :: BashTools.ScriptDescription -> OpenAI.Tool
        mapToolDescriptionBash2OpenAI bash =
            OpenAI.Tool
                { OpenAI.toolName = bash2OpenAIName bash
                , OpenAI.toolDescription = bash.scriptInfo.scriptDescription
                , OpenAI.toolParamProperties = fmap mapArg bash.scriptInfo.scriptArgs
                }

        mapArg :: BashTools.ScriptArg -> OpenAI.ParamProperty
        mapArg arg =
            OpenAI.ParamProperty
                { OpenAI.propertyKey = arg.argName
                , OpenAI.propertyType = arg.argBackingTypeString
                , OpenAI.propertyDescription = arg.argDescription
                }

        tool :: Tool ()
        tool = bashTool script

        find :: OpenAI.ToolCall -> Maybe (Tool OpenAI.ToolCall)
        find call = if matchName script call then Just (mapToolCall (const call) tool) else Nothing
     in
        Registration tool (mapToolDescriptionBash2OpenAI script) find

{- | registers an IO Script, since we have not yet decided on a way to capture the
shape of the tool for IOScript (ideally some generics or something like Data.Aeson.Encoding) we take the whole Tool definition
-}
registerIOScriptInOpenAI ::
    (Aeson.FromJSON a) =>
    IOTools.IOScript a ByteString ->
    [OpenAI.ParamProperty] ->
    Registration OpenAI.Tool OpenAI.ToolCall
registerIOScriptInOpenAI script openAIProps =
    let
        io2OpenAIName :: IOTools.IOScript a b -> OpenAI.ToolName
        io2OpenAIName io = OpenAI.ToolName (mconcat ["io_", io.description.ioSlug])

        matchName :: IOTools.IOScript a b -> OpenAI.ToolCall -> Bool
        matchName io call = io2OpenAIName io == call.toolCallFunction.toolCallFunctionName

        tool :: Tool ()
        tool = ioTool script

        find :: OpenAI.ToolCall -> Maybe (Tool OpenAI.ToolCall)
        find call = if matchName script call then Just (mapToolCall (const call) tool) else Nothing

        openAITool :: OpenAI.Tool
        openAITool =
            OpenAI.Tool
                { OpenAI.toolName = io2OpenAIName script
                , OpenAI.toolDescription = script.description.ioDescription
                , OpenAI.toolParamProperties = openAIProps
                }
     in
        Registration tool openAITool find

-------------------------------------------------------------------------------
data PromptOtherAgent
    = PromptOtherAgent Text

instance Aeson.FromJSON PromptOtherAgent where
    parseJSON = Aeson.withObject "PromptOtherAgent" $ \v -> do
        PromptOtherAgent
            <$> v Aeson..: "what"

turnAgentRuntimeIntoIOTool :: AgentSlug -> Runtime -> Registration OpenAI.Tool OpenAI.ToolCall
turnAgentRuntimeIntoIOTool callerSlug rt =
    registerIOScriptInOpenAI io props
  where
    props =
        [ OpenAI.ParamProperty
            { OpenAI.propertyKey = "what"
            , OpenAI.propertyType = "string"
            , OpenAI.propertyDescription = "the prompt to the other agent"
            }
        ]
    io =
        IOTools.IOScript
            ( IOTools.IOScriptDescription
                ("prompt_agent_" <> rt.agentSlug)
                ("aks a prompt to the expert agent: " <> rt.agentSlug)
            )
            (\(PromptOtherAgent txt) -> runSubAgent txt)

    runSubAgent txt =
        openAIAgent (nestTracer rt) agentFunctions txt

    nestTracer baseRuntime =
        baseRuntime{agentTracer = contramap (ChildrenTrace callerSlug) baseRuntime.agentTracer}

    agentFunctions =
        AgentFunctions
            (pure Nothing)
            (\err -> pure $ "sorry I got an error: " <> (CByteString.pack err))
            (\done -> pure $ maybe "i could not find an answer" Text.encodeUtf8 $ locateResponse done)

    locateResponse :: OpenAI.History -> Maybe Text
    locateResponse hist = do
        rsp <-
            Maybe.listToMaybe $
                Maybe.mapMaybe viewResponse $
                    toList $
                        Seq.reverse hist
        rsp.rspContent
      where
        viewResponse :: OpenAI.HistoryItem -> Maybe OpenAI.Response
        viewResponse (OpenAI.PromptAnswered _ rsp) = Just rsp
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
