{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_, (>=>))
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Functor.Contravariant.Divisible (choose)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Prod.Tracer as Prod
import qualified System.Agents.Agent as Agent
import qualified System.Agents.Agent as BaseAgent
import System.Agents.Base (AgentId, AgentSlug, ConversationId)
import qualified System.Agents.CLI as CLI
import System.Agents.CLI.Base (makeShowLogFileTracer)
import qualified System.Agents.CLI.InitProject as InitProject
import qualified System.Agents.Conversation as Conversation
import qualified System.Agents.FileLoader.Base as Agents
import qualified System.Agents.HttpClient as HttpClient
import qualified System.Agents.HttpLogger as HttpLogger
import qualified System.Agents.LLMs.OpenAI as LLMTrace
import qualified System.Agents.LLMs.OpenAI as OpenAI
import qualified System.Agents.MCP.Server as McpServer
import qualified System.Agents.Tools as ToolsTrace
import qualified System.Agents.Tools.Bash as Bash
import qualified System.Agents.Tools.Bash as ToolsTrace
import qualified System.Agents.Tools.IO as ToolsTrace
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)

import qualified System.Agents.Memory as Memory

import Options.Applicative

data Prog = Prog
    { apiKeysFile :: FilePath
    , logFile :: FilePath
    , logHttp :: Maybe String
    , memoryHttpStore :: Maybe String
    , agentFiles :: [FilePath]
    , agentsDir :: FilePath
    , mainCommand :: Command
    }

data Command
    = Check
    | InteractiveCommandLine
    | OneShot OneShotOptions
    | SelfDescribe
    | Initialize
    | McpServer

data OneShotOptions
    = OneShotOptions
    { fileOrPromptArg :: String
    }

data McpServerOptions
    = McpServerOptions
    { toolsDirectory :: FilePath
    }

parseCheckCommand :: Parser Command
parseCheckCommand =
    pure Check

parseCliCommand :: Parser Command
parseCliCommand =
    pure InteractiveCommandLine

parseOneShotTextualCommand :: Parser Command
parseOneShotTextualCommand =
    OneShot <$> parseOneShotOptions
  where
    parseOneShotOptions :: Parser OneShotOptions
    parseOneShotOptions =
        OneShotOptions
            <$> strOption
                ( long "prompt"
                    <> metavar "PROMPT"
                    <> help "prompt, magical values is @filepath"
                    <> showDefault
                    <> value "@/dev/stdin"
                )

parseMcpServer :: Parser Command
parseMcpServer =
    pure McpServer

{-
  where
    parseOptions :: Parser McpServerOptions
    parseOptions =
        McpServerOptions
            <$> strOption
                ( long "tooldir"
                    <> metavar "TOOLDIR"
                    <> help "tool directory"
                    <> showDefault
                    <> value "~/.agents-exe/tools/mcp"
                )
-}

parseInitializeCommand :: Parser Command
parseInitializeCommand =
    pure Initialize

parseSelfDescribeCommand :: Parser Command
parseSelfDescribeCommand =
    pure SelfDescribe

parseProgOptions :: Parser Prog
parseProgOptions =
    Prog
        <$> strOption
            ( long "api-keys"
                <> metavar "AGENTS-KEY"
                <> help "path to json-file containing API keys"
                <> showDefault
                <> value "agents-exe.keys"
            )
        <*> strOption
            ( long "log-file"
                <> metavar "LOGFILE"
                <> help "raw log file"
                <> showDefault
                <> value "agents-logfile"
            )
        <*> optional
            ( strOption
                ( long "log-http"
                    <> metavar "LOGHTTP"
                    <> help "http log sink"
                    <> showDefault
                )
            )
        <*> optional
            ( strOption
                ( long "memory-http-store"
                    <> metavar "MEMORYHTTPSTORE"
                    <> help "http memory sink"
                    <> showDefault
                )
            )
        <*> fmap
            addDefaultSomeAgentFile
            ( many
                ( strOption
                    ( long "agent-file"
                        <> metavar "AGENTFILE"
                        <> help (unlines ["root agent(s) description files", "defaults to 'agent.json'", "multi-support vary from command to command"])
                    )
                )
            )
        <*> strOption
            ( long "helper-agents-dir"
                <> metavar "AGENTSDIR"
                <> help "helper sub-agents directory"
                <> showDefault
                <> value "agents"
            )
        <*> hsubparser
            ( command "check" (info parseCheckCommand (idm))
                <> command "cli" (info parseCliCommand (idm))
                <> command "run" (info parseOneShotTextualCommand (idm))
                <> command "describe" (info parseSelfDescribeCommand (idm))
                <> command "init" (info parseInitializeCommand (idm))
                <> command "mcp-server" (info parseMcpServer (idm))
            )

-- | helper to make agent-file entirely optional whilst using the 'many' combinator
addDefaultSomeAgentFile :: [FilePath] -> [FilePath]
addDefaultSomeAgentFile [] = ["agent.json"]
addDefaultSomeAgentFile xs = xs

main :: IO ()
main = do
    hSetBuffering stderr LineBuffering
    hSetBuffering stdout LineBuffering
    prog =<< execParser infoProg
  where
    infoProg =
        info
            (parseProgOptions <**> helper)
            ( fullDesc
                <> progDesc "Your favorite agent framework."
                <> header "hi"
            )

    traceExtra :: Prod.Tracer IO a -> Maybe (Prod.Tracer IO a) -> Prod.Tracer IO a
    traceExtra t1 Nothing = t1
    traceExtra t1 (Just t2) = Prod.traceBoth t1 t2

    prog :: Prog -> IO ()
    prog args = do
        showFileTracer <- makeShowLogFileTracer args.logFile
        baseHttpTracer1 <- traverse (makeHttpJsonTrace Prod.silent . Text.pack) args.logHttp
        let logHttpTracer =
                case baseHttpTracer1 of
                    Nothing -> Nothing
                    Just t -> Just $ choose (maybeToEither . toJsonTrace) Prod.silent t
        baseHttpTracer2 <- traverse (makeHttpJsonTrace Prod.silent . Text.pack) args.memoryHttpStore
        let memoryHttpTracer =
                case baseHttpTracer2 of
                    Nothing -> Nothing
                    Just t -> Just $ choose extractMemories Prod.silent t

        let baseTracer = showFileTracer `traceExtra` logHttpTracer `traceExtra` memoryHttpTracer

        case args.mainCommand of
            Check -> do
                forM_ args.agentFiles $ \agentFile -> do
                    CLI.mainPrintAgent $
                        Conversation.Props
                            { Conversation.apiKeysFile = args.apiKeysFile
                            , Conversation.rawLogFile = args.logFile
                            , Conversation.mainAgentFile = agentFile
                            , Conversation.helperAgentsDir = args.agentsDir
                            , Conversation.interactiveTracer =
                                Prod.traceBoth baseTracer CLI.traceUsefulPromptStdout
                            }
            InteractiveCommandLine ->
                forM_ (take 1 args.agentFiles) $ \agentFile -> do
                    CLI.mainInteractiveAgent $
                        Conversation.Props
                            { Conversation.apiKeysFile = args.apiKeysFile
                            , Conversation.rawLogFile = args.logFile
                            , Conversation.mainAgentFile = agentFile
                            , Conversation.helperAgentsDir = args.agentsDir
                            , Conversation.interactiveTracer =
                                Prod.traceBoth
                                    baseTracer
                                    ( Prod.traceBoth
                                        CLI.traceUsefulPromptStdout
                                        ( Prod.traceBoth
                                            CLI.tracePrintingTextResponses
                                            (Conversation.traceWaitingOpenAIRateLimits (OpenAI.ApiLimits 100 10000) print)
                                        )
                                    )
                            }
            OneShot opts -> do
                forM_ (take 1 args.agentFiles) $ \agentFile -> do
                    prompt <-
                        if "@" `List.isPrefixOf` opts.fileOrPromptArg
                            then Text.readFile (drop 1 opts.fileOrPromptArg)
                            else pure $ Text.pack opts.fileOrPromptArg
                    let oneShot = flip CLI.mainOneShotText
                    oneShot prompt $
                        Conversation.Props
                            { Conversation.apiKeysFile = args.apiKeysFile
                            , Conversation.rawLogFile = args.logFile
                            , Conversation.mainAgentFile = agentFile
                            , Conversation.helperAgentsDir = args.agentsDir
                            , Conversation.interactiveTracer =
                                Prod.traceBoth
                                    baseTracer
                                    CLI.traceUsefulPromptStderr
                            }
            SelfDescribe ->
                Aeson.encodeFile "/dev/stdout" $
                    Bash.ScriptInfo
                        []
                        "self-introspect"
                        "introspect a version of yourself"
            McpServer -> do
                let oneAgent agentFile =
                        Conversation.Props
                            { Conversation.apiKeysFile = args.apiKeysFile
                            , Conversation.rawLogFile = args.logFile
                            , Conversation.mainAgentFile = agentFile
                            , Conversation.helperAgentsDir = args.agentsDir
                            , Conversation.interactiveTracer =
                                Prod.traceBoth
                                    baseTracer
                                    ( Prod.traceBoth
                                        CLI.traceUsefulPromptStderr
                                        (Conversation.traceWaitingOpenAIRateLimits (OpenAI.ApiLimits 100 10000) (\_ -> pure ()))
                                    )
                            }
                McpServer.multiAgentsServer (fmap oneAgent args.agentFiles)
            Initialize ->
                let o =
                        Agents.OpenAIAgent
                            { Agents.slug = "main-agent"
                            , Agents.apiKeyId = "main-key"
                            , Agents.flavor = "OpenAIv1"
                            , Agents.modelUrl = OpenAI.openAIv1Endpoint.getBaseUrl
                            , Agents.modelName = "gpt-4-turbo"
                            , Agents.announce = "a helpful pupper-master capable of orchestrating other agents ensuring"
                            , Agents.toolDirectory = "."
                            , Agents.systemPrompt =
                                [ "You are a helpful software agent trying to solve user requests"
                                , "Your preferred action mode is to act as a puppet master capable of driving other agents."
                                , "You can prompt other agents via tools by passing them a prompt using a JSON payload."
                                , "You efficiently single-shot prompt other agents to efficiently use your token buget."
                                , "You only provide prompt examples when you think other agents may benefit."
                                , "You notify users as you progress"
                                , "If an agent fails, do not retry and abdicate"
                                ]
                            }
                 in do
                        forM_ (take 1 args.agentFiles) $ \agentFile -> do
                            InitProject.initOpenAIAgent o agentFile
                            InitProject.initAgentsDir args.agentsDir
                            InitProject.initOpenAIKeys args.apiKeysFile

-------------------------------------------------------------------------------

maybeToEither :: Maybe a -> Either () a
maybeToEither Nothing = Left ()
maybeToEither (Just v) = Right v

extractMemories :: Conversation.Trace -> Either () [Memory.MemoryItem]
extractMemories x = case x of
    Conversation.AgentTrace tr -> go [] tr
    _ -> Left ()
  where
    parentInfo :: [BaseAgent.Trace] -> (Maybe AgentSlug, Maybe ConversationId, Maybe AgentId)
    parentInfo ((BaseAgent.AgentTrace_Memorize pSlug paId pcId _) : _) =
        (Just pSlug, Just pcId, Just paId)
    parentInfo ((BaseAgent.AgentTrace_Conversation pSlug paId pcId _) : _) =
        (Just pSlug, Just pcId, Just paId)
    parentInfo _ =
        (Nothing, Nothing, Nothing)

    rootConversationId :: [BaseAgent.Trace] -> Maybe ConversationId
    rootConversationId = rootTrace >=> BaseAgent.traceConversationId

    rootTrace :: [BaseAgent.Trace] -> Maybe BaseAgent.Trace
    rootTrace [] = Nothing
    rootTrace (t : []) = Just t
    rootTrace ts = rootTrace (drop 1 ts)

    go :: [BaseAgent.Trace] -> BaseAgent.Trace -> Either () [Memory.MemoryItem]
    go stack (BaseAgent.AgentTrace_Memorize aSlug aId cId (BaseAgent.Calling query hist stepId)) =
        let
            (pSlug, pcId, paId) = parentInfo stack
            rId = fromMaybe cId (rootConversationId stack)
         in
            Right
                [ Memory.MemoryItem
                    { Memory.rootConversationId = rId
                    , Memory.conversationId = cId
                    , Memory.agentSlug = aSlug
                    , Memory.agentId = aId
                    , Memory.stepId = stepId
                    , Memory.llmHistory = hist
                    , Memory.pendingQuery = query
                    , Memory.parentAgentSlug = pSlug
                    , Memory.parentConversationId = pcId
                    , Memory.parentAgentId = paId
                    }
                ]
    go stack (BaseAgent.AgentTrace_Memorize aSlug aId cId (BaseAgent.GotResponse query hist stepId rsp)) =
        let
            (pSlug, pcId, paId) = parentInfo stack
            rId = fromMaybe cId (rootConversationId stack)
         in
            Right
                [ Memory.MemoryItem
                    { Memory.rootConversationId = rId
                    , Memory.conversationId = cId
                    , Memory.agentSlug = aSlug
                    , Memory.agentId = aId
                    , Memory.stepId = stepId
                    , Memory.llmHistory = hist
                    , Memory.pendingQuery = query
                    , Memory.parentAgentSlug = pSlug
                    , Memory.parentConversationId = pcId
                    , Memory.parentAgentId = paId
                    }
                ]
    go stack (BaseAgent.AgentTrace_Memorize aSlug aId cId (BaseAgent.InteractionDone hist stepId)) =
        let
            (pSlug, pcId, paId) = parentInfo stack
            rId = fromMaybe cId (rootConversationId stack)
         in
            Right
                [ Memory.MemoryItem
                    { Memory.rootConversationId = rId
                    , Memory.conversationId = cId
                    , Memory.agentSlug = aSlug
                    , Memory.agentId = aId
                    , Memory.stepId = stepId
                    , Memory.llmHistory = hist
                    , Memory.pendingQuery = BaseAgent.Done
                    , Memory.parentAgentSlug = pSlug
                    , Memory.parentConversationId = pcId
                    , Memory.parentAgentId = paId
                    }
                ]
    go xs tr@(BaseAgent.AgentTrace_Conversation _ _ _ (BaseAgent.ChildrenTrace sub)) =
        go (tr : xs) sub
    go _ _ = Left ()

toJsonTrace :: Conversation.Trace -> Maybe Aeson.Value
toJsonTrace x = case x of
    Conversation.DataLoadingTrace _ -> Nothing
    Conversation.AgentTrace v -> encodeAgentTrace v
  where
    encodeAgentTrace :: BaseAgent.Trace -> Maybe Aeson.Value
    encodeAgentTrace tr = do
        baseVal <- encodeBaseTrace tr
        Just $
            Aeson.object
                [ "e"
                    .= Aeson.object
                        [ "slug" .= Agent.traceAgentSlug tr
                        , "agent-id" .= Agent.traceAgentId tr
                        , "val" .= baseVal
                        ]
                ]

    encodeBaseTrace :: BaseAgent.Trace -> Maybe Aeson.Value
    encodeBaseTrace (BaseAgent.AgentTrace_Loading _ _ tr) =
        encodeBaseTrace_Loading tr
    encodeBaseTrace (BaseAgent.AgentTrace_Memorize _ _ _ tr) =
        encodeBaseTrace_Memorize tr
    encodeBaseTrace (BaseAgent.AgentTrace_Conversation _ _ convId tr) = do
        baseVal <- encodeBaseTrace_Conversation tr
        Just $
            Aeson.object
                [ "conv-id" .= convId
                , "val" .= baseVal
                ]

    encodeBaseTrace_Loading :: BaseAgent.LoadingTrace -> Maybe Aeson.Value
    encodeBaseTrace_Loading bt =
        case bt of
            (BaseAgent.BashToolsLoadingTrace _) -> Nothing
            (BaseAgent.ReloadToolsTrace _) -> Nothing

    encodeBaseTrace_Memorize :: BaseAgent.MemorizeTrace -> Maybe Aeson.Value
    encodeBaseTrace_Memorize bt =
        case bt of
            (BaseAgent.Calling _ _ _) ->
                Nothing
            (BaseAgent.GotResponse _ _ _ _) ->
                Nothing
            (BaseAgent.InteractionDone _ _) ->
                Nothing

    encodeBaseTrace_Conversation :: BaseAgent.ConversationTrace -> Maybe Aeson.Value
    encodeBaseTrace_Conversation bt =
        case bt of
            (BaseAgent.NewConversation) ->
                Just $
                    Aeson.object
                        [ "x" .= ("new-conversation" :: Text.Text)
                        ]
            (BaseAgent.LLMTrace _ (LLMTrace.HttpClientTrace _)) ->
                Nothing
            (BaseAgent.LLMTrace uuid (LLMTrace.CallChatCompletion val)) ->
                Just $
                    Aeson.object
                        [ "x" .= ("llm" :: Text.Text)
                        , "action" .= ("call" :: Text.Text)
                        , "call-id" .= uuid
                        , "val" .= val
                        ]
            (BaseAgent.LLMTrace uuid (LLMTrace.GotChatCompletion val)) ->
                Just $
                    Aeson.object
                        [ "x" .= ("llm" :: Text.Text)
                        , "action" .= ("result" :: Text.Text)
                        , "call-id" .= uuid
                        , "val" .= val
                        ]
            (BaseAgent.RunToolTrace uuid (ToolsTrace.BashToolsTrace (ToolsTrace.RunCommandStart cmd args))) ->
                Just $
                    Aeson.object
                        [ "x" .= ("tool" :: Text.Text)
                        , "run-id" .= uuid
                        , "flavor" .= ("bash" :: Text.Text)
                        , "action" .= ("start" :: Text.Text)
                        , "cmd" .= cmd
                        , "args" .= args
                        ]
            (BaseAgent.RunToolTrace uuid (ToolsTrace.BashToolsTrace (ToolsTrace.RunCommandStopped cmd args code _ _))) ->
                Just $
                    Aeson.object
                        [ "x" .= ("tool" :: Text.Text)
                        , "run-id" .= uuid
                        , "flavor" .= ("bash" :: Text.Text)
                        , "action" .= ("stop" :: Text.Text)
                        , "code-str" .= show code -- todo: code
                        , "cmd" .= cmd
                        , "args" .= args
                        ]
            (BaseAgent.RunToolTrace uuid (ToolsTrace.IOToolsTrace (ToolsTrace.IOScriptStarted desc input))) ->
                Just $
                    Aeson.object
                        [ "x" .= ("tool" :: Text.Text)
                        , "run-id" .= uuid
                        , "flavor" .= ("io" :: Text.Text)
                        , "action" .= ("start" :: Text.Text)
                        , "tool" .= desc.ioSlug
                        , "input" .= input
                        ]
            (BaseAgent.RunToolTrace uuid (ToolsTrace.IOToolsTrace (ToolsTrace.IOScriptStopped desc input output))) ->
                Just $
                    Aeson.object
                        [ "x" .= ("tool" :: Text.Text)
                        , "run-id" .= uuid
                        , "flavor" .= ("io" :: Text.Text)
                        , "action" .= ("stop" :: Text.Text)
                        , "tool" .= desc.ioSlug
                        , "input" .= input
                        -- , "output" .= output
                        ]
            (BaseAgent.ChildrenTrace sub) -> do
                subVal <- encodeAgentTrace sub
                Just $ Aeson.object ["x" .= ("child" :: Text.Text), "sub" .= subVal]

makeHttpJsonTrace :: (Aeson.ToJSON a) => Prod.Tracer IO HttpClient.Trace -> Text.Text -> IO (Prod.Tracer IO a)
makeHttpJsonTrace baseTracer url = do
    rt <- HttpLogger.Runtime <$> HttpClient.newRuntime HttpClient.NoToken <*> pure url
    pure $ HttpLogger.httpTracer rt baseTracer
