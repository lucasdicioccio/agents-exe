{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Agents.Prompt where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Either as Either
import Data.Foldable (traverse_)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import GHC.IO.Handle (Handle)
import Prod.Tracer (Tracer (..), contramap, silent)
import System.IO (stderr, stdout)

import qualified System.Agents.Agent as Agent
import qualified System.Agents.FileLoader as FileLoader
import qualified System.Agents.LLMs.OpenAI as OpenAI
import qualified System.Agents.Tools as Tools
import qualified System.Agents.Tools.Bash as Tools
import qualified System.Agents.Tools.IO as Tools

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

mainPrintAgent :: Props -> IO ()
mainPrintAgent props = do
    withAgentRuntime props $ \x -> do
        case x of
            LoadingErrors errs -> traverse_ print errs
            OtherErrors errs -> traverse_ print errs
            Initialized _ -> pure ()

mainOneShotText :: Props -> Text -> IO ()
mainOneShotText props query = do
    withAgentRuntime props $ \x -> do
        case x of
            LoadingErrors errs -> traverse_ print errs
            OtherErrors errs -> traverse_ print errs
            Initialized ai -> runMainAgent ai.agentRuntime
  where
    agentFunctions =
        Agent.AgentFunctions
            (pure Nothing)
            (\err -> putStrLn $ unlines ["execution error", err])
            (\hist -> OpenAI.printLastAnswer hist)
    runMainAgent rt = do
        Agent.openAIAgent rt agentFunctions query

mainInteractiveAgent :: Props -> IO ()
mainInteractiveAgent props = do
    withAgentRuntime props $ \x -> do
        case x of
            LoadingErrors errs -> traverse_ print errs
            OtherErrors errs -> traverse_ print errs
            Initialized ai ->
                runMainAgent ai
  where
    agentFunctions ask =
        Agent.AgentFunctions
            (fmap queryOrNothing ask)
            (\err -> putStrLn $ unlines ["parse error", err])
            (\_ -> putStrLn "done")

    runMainAgent :: AgentInfo -> IO ()
    runMainAgent ai = do
        let nextQuery = askQuery ai
        query <- nextQuery
        Agent.openAIAgent ai.agentRuntime (agentFunctions nextQuery) query

    askQuery :: AgentInfo -> IO Text
    askQuery ai = do
        go
      where
        helpStr = do
            unlines
                [ "? or ?help -- show this help"
                , "?desc-main -- dump loaded main agent"
                , "?desc-agents -- dump loaded helper agents' descriptions"
                , "?desc-tools -- dump loaded tools"
                , "?reload-tools -- reload tools"
                ]
        go = do
            putStrLn "### Enter query:"
            query <- Text.pack <$> getLine
            case query of
                "?" -> do
                    putStrLn helpStr
                    go
                "?help" -> do
                    putStrLn helpStr
                    go
                "?desc-main" -> do
                    print ai.agentDescription
                    go
                "?desc-agents" -> do
                    traverse_ print ai.agentSibling.agents
                    go
                "?desc-tools" -> do
                    traverse_ printAgentTools (ai.agentRuntime : ai.agentSiblingRuntimes)
                    go
                "?reload-tools" -> do
                    traverse_ Agent.triggerRefreshTools (ai.agentRuntime : ai.agentSiblingRuntimes)

                    go
                txt -> pure txt

    printAgentTools :: Agent.Runtime -> IO ()
    printAgentTools rt = do
        registry <- rt.agentTools
        Text.putStrLn (renderToolRegistry registry)

    renderToolRegistry :: (Aeson.ToJSON a) => [Tools.Registration a b] -> Text
    renderToolRegistry registry =
        Text.unlines $
            fmap renderRegisteredTool registry

    renderRegisteredTool :: (Aeson.ToJSON a) => Tools.Registration a b -> Text
    renderRegisteredTool reg =
        case reg.innerTool.toolDef of
            Tools.BashTool bashScript ->
                Text.unwords ["command", Text.pack bashScript.scriptPath, Text.decodeUtf8 $ LByteString.toStrict $ Aeson.encode reg.declareTool]
            Tools.IOTool ioScript ->
                Text.unwords ["io", ioScript.ioSlug, ioScript.ioDescription]

    queryOrNothing :: Text -> Maybe Text
    queryOrNothing "" = Nothing
    queryOrNothing t = Just t

augmentMainAgentPromptWithSubAgents :: [Agent.Runtime] -> Text -> Text
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

traceSilent :: Tracer IO Trace
traceSilent = silent

traceWaitingOpenAIRateLimits :: OpenAI.ApiLimits -> (OpenAI.WaitAction -> IO ()) -> Tracer IO Trace
traceWaitingOpenAIRateLimits lims onWait = Tracer f
  where
    f (AgentTrace (Agent.AgentTrace _ _ (Agent.OpenAITrace tr))) =
        runTracer (OpenAI.waitRateLimit lims onWait) tr
    f _ = pure ()

tracePrintingTextResponses :: Tracer IO Trace
tracePrintingTextResponses = Tracer f
  where
    f (AgentTrace (Agent.AgentTrace slug _ trace)) =
        g [slug] trace
    f _ = pure ()

    g pfx (Agent.OpenAITrace (OpenAI.GotChatCompletion x)) =
        case Aeson.parseEither OpenAI.parseLLMResponse x of
            Left _ -> pure ()
            Right rsp ->
                Text.putStrLn $ Text.unwords [Text.intercalate "/" pfx, Maybe.fromMaybe "..." rsp.rspContent]
    g pfx (Agent.ChildrenTrace (Agent.AgentTrace childSlug _ sub)) =
        g (childSlug : pfx) sub
    g _ _ = pure ()

traceUsefulPromptStdout :: Tracer IO Trace
traceUsefulPromptStdout = traceUsefulPromptHandle stdout

traceUsefulPromptStderr :: Tracer IO Trace
traceUsefulPromptStderr = traceUsefulPromptHandle stderr

traceUsefulPromptHandle :: Handle -> Tracer IO Trace
traceUsefulPromptHandle h = Tracer f
  where
    f (AgentTrace (Agent.AgentTrace slug _ tr)) =
        Text.hPutStrLn h $
            Text.unlines
                [ mconcat ["@", slug, ":"]
                , renderBaseAgentTrace tr
                ]
    f (DataLoadingTrace x) = Text.hPutStrLn h (Text.pack $ show x)

renderBaseAgentTrace :: Agent.BaseTrace -> Text
renderBaseAgentTrace tr = case tr of
    Agent.ReloadToolsTrace _ -> "(reload-tools...)"
    Agent.BashToolsLoadingTrace _ -> "(reload-tools...)"
    Agent.RunToolTrace (Tools.BashToolsTrace (Tools.RunCommandStart p args)) ->
        Text.unwords ["bash-tool", "start", Text.pack p, Text.unwords $ map Text.pack args]
    Agent.RunToolTrace (Tools.BashToolsTrace (Tools.RunCommandStopped p args code _ _)) ->
        Text.unlines
            [ Text.unwords ["bash-tool", "stopped", Text.pack p, Text.unwords $ map Text.pack args]
            , Text.pack $ show code
            ]
    Agent.RunToolTrace (Tools.IOToolsTrace (Tools.IOScriptStarted desc _)) ->
        Text.unwords ["io-tool", desc.ioSlug, "start"]
    Agent.RunToolTrace (Tools.IOToolsTrace (Tools.IOScriptStopped desc _ _)) ->
        Text.unwords ["io-tool", desc.ioSlug, "stop"]
    Agent.OpenAITrace (OpenAI.HttpClientTrace _) -> "(http)"
    Agent.OpenAITrace (OpenAI.CallChatCompletion _) ->
        Text.unwords ["to: open-ai"]
    Agent.OpenAITrace (OpenAI.GotChatCompletion x) ->
        Text.unwords ["from: open-ai", jsonTxt x]
    Agent.StepTrace (Agent.GotResponse rsp) ->
        Text.unwords [Text.decodeUtf8 $ LByteString.toStrict $ Aeson.encode rsp.chosenMessage]
    Agent.ChildrenTrace (Agent.AgentTrace slug _ sub) ->
        Text.unwords ["(", slug, ")", renderBaseAgentTrace sub]
  where
    jsonTxt :: (Aeson.ToJSON a) => a -> Text
    jsonTxt = Text.decodeUtf8 . LByteString.toStrict . Aeson.encode

readApiKeys :: FilePath -> IO (Maybe ApiKeys)
readApiKeys path =
    Aeson.decode <$> LByteString.readFile path

flattenOpenAIKeys :: ApiKeys -> [(Text, OpenAI.ApiKey)]
flattenOpenAIKeys (ApiKeys keys) =
    [(k.apiKeyId, OpenAI.ApiKey $ Text.encodeUtf8 k.apiKeyValue) | k <- keys]

readOpenApiKeysFile :: FilePath -> IO [(Text, OpenAI.ApiKey)]
readOpenApiKeysFile path =
    maybe [] flattenOpenAIKeys <$> readApiKeys path
