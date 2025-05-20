{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Functor.Contravariant.Divisible (choose)
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Prod.Tracer as Prod
import qualified System.Agents.Agent as Agent
import qualified System.Agents.Agent as BaseAgent
import System.Agents.CLI.Base (makeShowLogFileTracer)
import qualified System.Agents.CLI.InitProject as InitProject
import qualified System.Agents.FileLoader.Base as Agents
import qualified System.Agents.HttpClient as HttpClient
import qualified System.Agents.HttpLogger as HttpLogger
import qualified System.Agents.LLMs.OpenAI as LLMTrace
import qualified System.Agents.LLMs.OpenAI as OpenAI
import qualified System.Agents.MCP.Server as McpServer
import qualified System.Agents.Prompt as Prompt
import qualified System.Agents.Tools as ToolsTrace
import qualified System.Agents.Tools.Bash as Bash
import qualified System.Agents.Tools.Bash as ToolsTrace
import qualified System.Agents.Tools.IO as ToolsTrace
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)

import Options.Applicative

data Prog = Prog
    { apiKeysFile :: FilePath
    , logFile :: FilePath
    , logHttp :: Maybe String
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

    prog :: Prog -> IO ()
    prog args = do
        showFileTracer <- makeShowLogFileTracer args.logFile
        baseHttpTracer1 <- traverse (makeHttpJsonTrace . Text.pack) args.logHttp
        let httpTracer =
                case baseHttpTracer1 of
                    Nothing -> Nothing
                    Just t -> Just $ choose (maybeToEither . toJsonTrace) Prod.silent t
        let baseTracer = maybe showFileTracer (Prod.traceBoth showFileTracer) httpTracer
        case args.mainCommand of
            Check -> do
                forM_ args.agentFiles $ \agentFile -> do
                    Prompt.mainPrintAgent $
                        Prompt.Props
                            { Prompt.apiKeysFile = args.apiKeysFile
                            , Prompt.rawLogFile = args.logFile
                            , Prompt.mainAgentFile = agentFile
                            , Prompt.helperAgentsDir = args.agentsDir
                            , Prompt.interactiveTracer =
                                Prod.traceBoth baseTracer Prompt.traceUsefulPromptStdout
                            }
            InteractiveCommandLine ->
                forM_ (take 1 args.agentFiles) $ \agentFile -> do
                    Prompt.mainInteractiveAgent $
                        Prompt.Props
                            { Prompt.apiKeysFile = args.apiKeysFile
                            , Prompt.rawLogFile = args.logFile
                            , Prompt.mainAgentFile = agentFile
                            , Prompt.helperAgentsDir = args.agentsDir
                            , Prompt.interactiveTracer =
                                Prod.traceBoth
                                    baseTracer
                                    ( Prod.traceBoth
                                        Prompt.traceUsefulPromptStdout
                                        ( Prod.traceBoth
                                            Prompt.tracePrintingTextResponses
                                            (Prompt.traceWaitingOpenAIRateLimits (OpenAI.ApiLimits 100 10000) print)
                                        )
                                    )
                            }
            OneShot opts -> do
                forM_ (take 1 args.agentFiles) $ \agentFile -> do
                    prompt <-
                        if "@" `List.isPrefixOf` opts.fileOrPromptArg
                            then Text.readFile (drop 1 opts.fileOrPromptArg)
                            else pure $ Text.pack opts.fileOrPromptArg
                    let oneShot = flip Prompt.mainOneShotText
                    oneShot prompt $
                        Prompt.Props
                            { Prompt.apiKeysFile = args.apiKeysFile
                            , Prompt.rawLogFile = args.logFile
                            , Prompt.mainAgentFile = agentFile
                            , Prompt.helperAgentsDir = args.agentsDir
                            , Prompt.interactiveTracer =
                                Prod.traceBoth
                                    baseTracer
                                    Prompt.traceUsefulPromptStderr
                            }
            SelfDescribe ->
                Aeson.encodeFile "/dev/stdout" $
                    Bash.ScriptInfo
                        []
                        "self-introspect"
                        "introspect a version of yourself"
            McpServer -> do
                let oneAgent agentFile =
                        Prompt.Props
                            { Prompt.apiKeysFile = args.apiKeysFile
                            , Prompt.rawLogFile = args.logFile
                            , Prompt.mainAgentFile = agentFile
                            , Prompt.helperAgentsDir = args.agentsDir
                            , Prompt.interactiveTracer =
                                Prod.traceBoth
                                    baseTracer
                                    ( Prod.traceBoth
                                        Prompt.traceUsefulPromptStderr
                                        (Prompt.traceWaitingOpenAIRateLimits (OpenAI.ApiLimits 100 10000) (\_ -> pure ()))
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

toJsonTrace :: Prompt.Trace -> Maybe Aeson.Value
toJsonTrace x = case x of
    Prompt.DataLoadingTrace _ -> Nothing
    Prompt.AgentTrace v -> encodeAgentTrace v
  where
    encodeAgentTrace :: BaseAgent.Trace -> Maybe Aeson.Value
    encodeAgentTrace tr = do
        baseVal <- encodeBaseTrace tr
        Just $
            Aeson.object
                [ "e"
                    .= Aeson.object
                        [ "s" .= Agent.traceAgentSlug tr
                        , "id" .= Agent.traceAgentId tr
                        , "x" .= baseVal
                        ]
                ]

    encodeBaseTrace :: BaseAgent.Trace -> Maybe Aeson.Value
    encodeBaseTrace (BaseAgent.AgentTrace_Loading _ _ tr) =
        encodeBaseTrace_Loading tr
    encodeBaseTrace (BaseAgent.AgentTrace_Info _ _ tr) =
        encodeBaseTrace_Info tr
    encodeBaseTrace (BaseAgent.AgentTrace_Conversation _ _ tr) =
        encodeBaseTrace_Conversation tr

    encodeBaseTrace_Loading :: BaseAgent.LoadingTrace -> Maybe Aeson.Value
    encodeBaseTrace_Loading bt =
        case bt of
            (BaseAgent.BashToolsLoadingTrace _) -> Nothing
            (BaseAgent.ReloadToolsTrace _) -> Nothing

    encodeBaseTrace_Info :: BaseAgent.InfoTrace -> Maybe Aeson.Value
    encodeBaseTrace_Info bt =
        case bt of
            (BaseAgent.GotResponse _) ->
                Nothing

    encodeBaseTrace_Conversation :: BaseAgent.ConversationTrace -> Maybe Aeson.Value
    encodeBaseTrace_Conversation bt =
        case bt of
            (BaseAgent.LLMTrace _ (LLMTrace.HttpClientTrace _)) ->
                Nothing
            (BaseAgent.LLMTrace uuid (LLMTrace.CallChatCompletion val)) ->
                Just $
                    Aeson.object
                        [ "x" .= ("llm" :: Text.Text)
                        , "a" .= ("call" :: Text.Text)
                        , "v" .= val
                        , "u" .= uuid
                        ]
            (BaseAgent.LLMTrace uuid (LLMTrace.GotChatCompletion val)) ->
                Just $
                    Aeson.object
                        [ "x" .= ("llm" :: Text.Text)
                        , "a" .= ("result" :: Text.Text)
                        , "v" .= val
                        , "u" .= uuid
                        ]
            (BaseAgent.RunToolTrace uuid (ToolsTrace.BashToolsTrace (ToolsTrace.RunCommandStart cmd args))) ->
                Just $
                    Aeson.object
                        [ "x" .= ("run-tool" :: Text.Text)
                        , "u" .= uuid
                        , "flavor" .= ("bash" :: Text.Text)
                        , "action" .= ("start" :: Text.Text)
                        , "cmd" .= cmd
                        , "args" .= args
                        ]
            (BaseAgent.RunToolTrace uuid (ToolsTrace.BashToolsTrace (ToolsTrace.RunCommandStopped cmd args code _ _))) ->
                Just $
                    Aeson.object
                        [ "x" .= ("run-tool" :: Text.Text)
                        , "u" .= uuid
                        , "flavor" .= ("bash" :: Text.Text)
                        , "action" .= ("stop" :: Text.Text)
                        , "code-str" .= show code -- todo: code
                        , "cmd" .= cmd
                        , "args" .= args
                        ]
            (BaseAgent.RunToolTrace uuid (ToolsTrace.IOToolsTrace (ToolsTrace.IOScriptStarted desc input))) ->
                Just $
                    Aeson.object
                        [ "x" .= ("run-tool" :: Text.Text)
                        , "u" .= uuid
                        , "flavor" .= ("io" :: Text.Text)
                        , "action" .= ("start" :: Text.Text)
                        , "tool" .= desc.ioSlug
                        , "input" .= input
                        ]
            (BaseAgent.RunToolTrace uuid (ToolsTrace.IOToolsTrace (ToolsTrace.IOScriptStopped desc input output))) ->
                Just $
                    Aeson.object
                        [ "x" .= ("run-tool" :: Text.Text)
                        , "u" .= uuid
                        , "flavor" .= ("io" :: Text.Text)
                        , "action" .= ("stop" :: Text.Text)
                        , "tool" .= desc.ioSlug
                        , "input" .= input
                        -- , "output" .= output
                        ]
            (BaseAgent.ChildrenTrace sub) ->
                case encodeAgentTrace sub of
                    Nothing ->
                        Just $ Aeson.object ["x" .= ("child" :: Text.Text)]
                    Just v ->
                        Just $ Aeson.object ["x" .= ("child" :: Text.Text), "sub" .= v]

makeHttpJsonTrace :: Text.Text -> IO (Prod.Tracer IO Aeson.Value)
makeHttpJsonTrace url = do
    rt <- HttpLogger.Runtime <$> HttpClient.newRuntime HttpClient.NoToken <*> pure url
    pure $ HttpLogger.httpTracer rt Prod.silent
