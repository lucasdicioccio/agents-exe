{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Agents.CLI where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as LByteString
import Data.Foldable (traverse_)
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import GHC.IO.Handle (Handle)
import Prod.Tracer (Tracer (..), silent)
import System.IO (stderr, stdout)

import qualified System.Agents.Agent as Agent
import qualified System.Agents.FileLoader as FileLoader
import qualified System.Agents.LLMs.OpenAI as OpenAI
import qualified System.Agents.Tools as Tools
import qualified System.Agents.Tools.Bash as Tools
import qualified System.Agents.Tools.IO as Tools

import System.Agents.Conversation

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
        Agent.handleConversation rt agentFunctions query

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
        Agent.handleConversation ai.agentRuntime (agentFunctions nextQuery) query

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

    renderToolRegistry :: (Aeson.ToJSON b) => [Tools.Registration a b c] -> Text
    renderToolRegistry registry =
        Text.unlines $
            fmap renderRegisteredTool registry

    renderRegisteredTool :: (Aeson.ToJSON b) => Tools.Registration a b c -> Text
    renderRegisteredTool reg =
        case reg.innerTool.toolDef of
            Tools.BashTool bashScript ->
                Text.unwords ["command", Text.pack bashScript.scriptPath, Text.decodeUtf8 $ LByteString.toStrict $ Aeson.encode reg.declareTool]
            Tools.IOTool ioScript ->
                Text.unwords ["io", ioScript.ioSlug, ioScript.ioDescription]

    queryOrNothing :: Text -> Maybe Text
    queryOrNothing "" = Nothing
    queryOrNothing t = Just t

traceSilent :: Tracer IO Trace
traceSilent = silent

tracePrintingTextResponses :: Tracer IO Trace
tracePrintingTextResponses = Tracer f
  where
    f (AgentTrace (Agent.AgentTrace_Conversation slug _ _ trace)) =
        g [slug] trace
    f _ = pure ()

    g pfx (Agent.LLMTrace _ (OpenAI.GotChatCompletion x)) =
        case Aeson.parseEither OpenAI.parseLLMResponse x of
            Left _ -> pure ()
            Right rsp ->
                Text.putStrLn $ Text.unwords [Text.intercalate "/" pfx, Maybe.fromMaybe "..." rsp.rspContent]
    g pfx (Agent.ChildrenTrace (Agent.AgentTrace_Conversation childSlug _ _ sub)) =
        g (childSlug : pfx) sub
    g _ _ = pure ()

traceUsefulPromptStdout :: Tracer IO Trace
traceUsefulPromptStdout = traceUsefulPromptHandle stdout

traceUsefulPromptStderr :: Tracer IO Trace
traceUsefulPromptStderr = traceUsefulPromptHandle stderr

traceUsefulPromptHandle :: Handle -> Tracer IO Trace
traceUsefulPromptHandle h = Tracer f
  where
    f (AgentTrace tr) =
        Text.hPutStrLn h $ renderAgentTrace tr
    f (DataLoadingTrace x) = Text.hPutStrLn h (Text.pack $ show x)

renderAgentTrace :: Agent.Trace -> Text
renderAgentTrace (Agent.AgentTrace_Loading slug _ tr) =
    Text.unlines
        [ mconcat ["@", slug, ":"]
        , renderLoadingAgentTrace tr
        ]
renderAgentTrace (Agent.AgentTrace_Conversation slug _ _ tr) =
    Text.unlines
        [ mconcat ["@", slug, ":"]
        , renderConversationAgentTrace tr
        ]
renderAgentTrace (Agent.AgentTrace_Memorize slug _ _ tr) =
    Text.unlines
        [ mconcat ["@", slug, ":"]
        , renderMemorizeAgentTrace tr
        ]

renderLoadingAgentTrace :: Agent.LoadingTrace -> Text
renderLoadingAgentTrace tr = case tr of
    Agent.ReloadToolsTrace _ -> "(reload-tools...)"
    Agent.BashToolsLoadingTrace _ -> "(reload-tools...)"

renderMemorizeAgentTrace :: Agent.MemorizeTrace -> Text
renderMemorizeAgentTrace tr = case tr of
    Agent.Calling _ hist _ ->
        Text.unwords [Text.pack . show $ length hist, ">>>"]
    Agent.GotResponse _ hist _ rsp ->
        Text.unwords [Text.pack . show $ length hist, Text.decodeUtf8 $ LByteString.toStrict $ Aeson.encode rsp.chosenMessage]
    Agent.InteractionDone hist _ ->
        Text.unwords [Text.pack . show $ length hist, "<<<"]

renderConversationAgentTrace :: Agent.ConversationTrace -> Text
renderConversationAgentTrace tr = case tr of
    Agent.NewConversation -> ""
    Agent.RunToolTrace _ (Tools.BashToolsTrace (Tools.RunCommandStart p args)) ->
        Text.unwords ["bash-tool", "start", Text.pack p, Text.unwords $ map Text.pack args]
    Agent.RunToolTrace _ (Tools.BashToolsTrace (Tools.RunCommandStopped p args code _ _)) ->
        Text.unlines
            [ Text.unwords ["bash-tool", "stopped", Text.pack p, Text.unwords $ map Text.pack args]
            , Text.pack $ show code
            ]
    Agent.RunToolTrace _ (Tools.IOToolsTrace (Tools.IOScriptStarted desc _)) ->
        Text.unwords ["io-tool", desc.ioSlug, "start"]
    Agent.RunToolTrace _ (Tools.IOToolsTrace (Tools.IOScriptStopped desc _ _)) ->
        Text.unwords ["io-tool", desc.ioSlug, "stop"]
    Agent.LLMTrace _ (OpenAI.HttpClientTrace _) -> "(http)"
    Agent.LLMTrace _ (OpenAI.CallChatCompletion _) ->
        Text.unwords ["to: llm"]
    Agent.LLMTrace _ (OpenAI.GotChatCompletion x) ->
        Text.unwords ["from: llm", jsonTxt x]
    Agent.ChildrenTrace sub ->
        Text.unwords ["(", Agent.traceAgentSlug sub, ")", renderAgentTrace sub]
  where
    jsonTxt :: (Aeson.ToJSON a) => a -> Text
    jsonTxt = Text.decodeUtf8 . LByteString.toStrict . Aeson.encode
