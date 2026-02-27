{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Functor.Contravariant.Divisible (choose)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import GHC.Generics (Generic)
import qualified Prod.Tracer as Prod
import qualified System.Agents.AgentTree as AgentTree
import qualified System.Agents.AgentTree.OneShotTool as OneShotTool
import System.Agents.Base (Agent (..), McpServerDescription (..), McpSimpleBinaryConfiguration (..))
import System.Agents.CLI.Base (makeShowLogFileTracer, makeFileJsonTracer)
import qualified System.Agents.CLI.InitProject as InitProject
import System.Agents.CLI.TraceUtils (traceUsefulPromptStderr, traceUsefulPromptStdout)
import qualified System.Agents.FileLoader as FileLoader
import qualified System.Agents.HttpClient as HttpClient
import qualified System.Agents.HttpLogger as HttpLogger
import qualified System.Agents.LLMs.OpenAI as LLMTrace
import qualified System.Agents.LLMs.OpenAI as OpenAI
import qualified System.Agents.MCP.Client as McpClient
import qualified System.Agents.MCP.Client.Runtime as McpClient
import qualified System.Agents.MCP.Server as McpServer
import qualified System.Agents.OneShot as OneShot
import qualified System.Agents.Runtime.Trace as Runtime
import qualified System.Agents.SessionPrint as SessionPrint
import qualified System.Agents.TUI2.Core as TUI2
import qualified System.Agents.Tools as ToolsTrace
import qualified System.Agents.Tools.Bash as Bash
import qualified System.Agents.Tools.Bash as ToolsTrace
import qualified System.Agents.Tools.BashToolbox as BashToolbox
import qualified System.Agents.Tools.IO as ToolsTrace
import qualified System.Agents.Tools.McpToolbox as McpTools
import System.Agents.TraceUtils (traceWaitingOpenAIRateLimits)
import System.Directory (doesFileExist, getCurrentDirectory, getHomeDirectory)
import System.FilePath (takeDirectory, (</>))
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)
import qualified System.Process as Process

import Options.Applicative

data ArgParserArgs
    = ArgParserArgs
    { configdir :: FilePath
    , defaultAgentFiles :: [FilePath]
    , defaultLogJsonHttpEndpoint :: Maybe String
    , defaultLogJsonFilepath :: Maybe FilePath
    , defaultLogRawFilepath :: Maybe FilePath
    , defaultLogSesionsJsonPrefix :: Maybe FilePath
    }

secretsKeyFile :: ArgParserArgs -> FilePath
secretsKeyFile pargs = pargs.configdir </> "secret-keys"

-- | helper to make agent-file entirely optional whilst using the 'many' combinator
addDefaultAgentFiles :: ArgParserArgs -> [FilePath] -> [FilePath]
addDefaultAgentFiles pargs [] = pargs.defaultAgentFiles
addDefaultAgentFiles _ xs = xs

data AgentsExeLogConfig = AgentsExeLogConfig
    { logJsonHttpEndpoint :: Maybe String
    , logJsonPath :: Maybe FilePath
    , logRawPath :: Maybe FilePath
    , logSessionsJsonPrefix :: Maybe FilePath
    }
    deriving (Show, Generic)
instance Aeson.FromJSON AgentsExeLogConfig

data AgentsExeConfig = AgentsExeConfig
    { agentsConfigDir :: Maybe FilePath
    , agentsDirectories :: [FilePath]
    , agentsFiles :: [FilePath]
    , agentsLogs :: Maybe AgentsExeLogConfig
    }
    deriving (Show, Generic)

instance Aeson.FromJSON AgentsExeConfig

locateAgentsExeConfig :: IO (Maybe FilePath)
locateAgentsExeConfig = do
    go =<< getCurrentDirectory
  where
    go :: FilePath -> IO (Maybe FilePath)
    go "" = pure Nothing
    go "/" = pure Nothing
    go path = do
        let temptative = path </> "agents-exe.cfg.json"
        exists <- doesFileExist temptative
        if exists
            then pure (Just temptative)
            else go (takeDirectory path)

initArgParserArgs :: IO ArgParserArgs
initArgParserArgs = do
    homedir <- getHomeDirectory
    agentsExecConfig <- locateAgentsExeConfig
    maybe (initWithoutAgentsExeConfig homedir) (initFromAgentsExeConfig homedir) agentsExecConfig
  where
    initFromAgentsExeConfig :: FilePath -> FilePath -> IO ArgParserArgs
    initFromAgentsExeConfig homedir agentsexecfgpath = do
        zeconfig <- Aeson.eitherDecodeFileStrict' agentsexecfgpath :: IO (Either String AgentsExeConfig)
        case zeconfig of
            Left err -> error ("failed to load agents-exe config at " <> agentsexecfgpath <> " " <> err)
            Right obj -> do
                let defaultconfigdir = homedir </> ".config/agents-exe"
                jsonPathss <- traverse FileLoader.listJsonDirectory obj.agentsDirectories
                pure $
                    ArgParserArgs
                        (fromMaybe defaultconfigdir obj.agentsConfigDir)
                        (obj.agentsFiles <> mconcat jsonPathss)
                        (logJsonHttpEndpoint =<< obj.agentsLogs)
                        (logJsonPath =<< obj.agentsLogs)
                        (logRawPath =<< obj.agentsLogs)
                        (logSessionsJsonPrefix =<< obj.agentsLogs)

    initWithoutAgentsExeConfig :: FilePath -> IO ArgParserArgs
    initWithoutAgentsExeConfig homedir = do
        let pconfigdir = homedir </> ".config/agents-exe"
        jsonPaths <- FileLoader.listJsonDirectory (pconfigdir </> "default")
        pure $
            ArgParserArgs
                pconfigdir
                jsonPaths
                Nothing
                Nothing
                Nothing
                Nothing

data Prog = Prog
    { apiKeysFile :: FilePath
    , logFile :: FilePath
    , logHttp :: Maybe String
    , logJsonFile :: Maybe FilePath
    , sessionsJsonPrefix :: Maybe FilePath
    , agentFiles :: [FilePath]
    , mainCommand :: Command
    }

data Command
    = Check
    | TerminalUI TuiOptions
    | OneShot OneShotOptions
    | EchoPrompt OneShotOptions
    | SelfDescribe
    | Initialize
    | McpServer
    | SessionPrint SessionPrint.SessionPrintOptions

data PromptScriptDirective
    = Str Text.Text
    | FileContents FilePath
    | Separator Int Text.Text
    | ShellOutput String
    deriving (Show)

type PromptScript =
    [PromptScriptDirective]

data TuiOptions
    = TuiOptions
    { sessionJsonPrefix :: Maybe FilePath
    }

data OneShotOptions
    = OneShotOptions
    { sessionFile :: Maybe FilePath
    , promptScript :: PromptScript
    }

data McpServerOptions
    = McpServerOptions
    { toolsDirectory :: FilePath
    }

parseCheckCommand :: Parser Command
parseCheckCommand =
    pure Check

parseTuiChatCommand :: Parser Command
parseTuiChatCommand =
    TerminalUI <$> parseTuiOptions

parseOneShotTextualCommand :: Parser Command
parseOneShotTextualCommand =
    OneShot <$> parseOneShotOptions

parseEchoPromptCommand :: Parser Command
parseEchoPromptCommand =
    EchoPrompt <$> parseOneShotOptions

parseTuiOptions :: Parser TuiOptions
parseTuiOptions =
    TuiOptions
        <$> optional
            ( strOption
                ( long "session-files-prefix"
                    <> metavar "SESSIONPREFIX"
                    <> help "file prefix to store per session json files"
                    <> showDefault
                )
            )

parseOneShotOptions :: Parser OneShotOptions
parseOneShotOptions =
    OneShotOptions
        <$> optional
            ( strOption
                ( long "session-file"
                    <> metavar "SESSIONFILE"
                    <> help "session file"
                    <> showDefault
                )
            )
        <*> ( many
                (promptOption <|> fileOption <|> shellOption <|> smallSeparatorFlag <|> largeSeparatorFlag)
            )
  where
    smallSeparatorFlag :: Parser PromptScriptDirective
    smallSeparatorFlag =
        Separator 4
            <$> strOption
                ( long "sep4"
                    <> short 's'
                    <> metavar "SEPARATOR"
                    <> help "a short separator"
                )

    largeSeparatorFlag :: Parser PromptScriptDirective
    largeSeparatorFlag =
        Separator 40
            <$> strOption
                ( long "sep40"
                    <> short 'S'
                    <> metavar "SEPARATOR"
                    <> help "a long separator"
                )

    promptOption :: Parser PromptScriptDirective
    promptOption =
        Str
            <$> strOption
                ( long "prompt"
                    <> short 'p'
                    <> metavar "PROMPT"
                    <> help "prompt text paragraph"
                )

    fileOption :: Parser PromptScriptDirective
    fileOption =
        FileContents
            <$> strOption
                ( long "file"
                    <> short 'f'
                    <> metavar "FILE"
                    <> help "prompt text file"
                )

    shellOption :: Parser PromptScriptDirective
    shellOption =
        ShellOutput
            <$> strOption
                ( long "shell"
                    <> metavar "SHELL"
                    <> help "prompt the stdout of a shell command"
                )

parseMcpServer :: Parser Command
parseMcpServer =
    pure McpServer

parseSessionPrintCommand :: Parser Command
parseSessionPrintCommand =
    SessionPrint <$> parseSessionPrintOptions

parseSessionPrintOptions :: Parser SessionPrint.SessionPrintOptions
parseSessionPrintOptions =
    SessionPrint.SessionPrintOptions
        <$> strArgument
            ( metavar "SESSIONFILE"
                <> help "Path to the session JSON file to print"
            )
        <*> switch
            ( long "show-tool-call-results"
                <> help "Show the results of tool calls in the output"
            )
        <*> optional
            ( option auto
                ( long "n-turns"
                    <> metavar "N"
                    <> help "Limit output to the first N turns (no limit if not specified)"
                    <> showDefault
                )
            )
        <*> switch
            ( long "repeat-system-prompt"
                <> help "Repeat the system prompt at each turn (default: False, always shown in first turn)"
            )
        <*> switch
            ( long "repeat-tools"
                <> help "Repeat the available tools at each turn (default: False, always shown in first turn)"
            )
        <*> flag
            SessionPrint.Chronological
            SessionPrint.Antichronological
            ( long "antichronological"
                <> help "Display session steps in antichronological order (newest first). Default is chronological (oldest first)."
            )

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

parseProgOptions :: ArgParserArgs -> Parser Prog
parseProgOptions argparserargs =
    Prog
        <$> strOption
            ( long "api-keys"
                <> metavar "AGENTS-KEY"
                <> help "path to json-file containing API keys"
                <> showDefault
                <> value (secretsKeyFile argparserargs)
            )
        <*> strOption
            ( long "log-file"
                <> metavar "LOGFILE"
                <> help "raw log file"
                <> showDefault
                <> value "agents-logfile"
                <> (maybe mempty value argparserargs.defaultLogRawFilepath)
            )
        <*> optional
            ( strOption
                ( long "log-http"
                    <> metavar "LOGHTTP"
                    <> help "http log sink"
                    <> showDefault
                    <> (maybe mempty value argparserargs.defaultLogJsonHttpEndpoint)
                )
            )
        <*> optional
            ( strOption
                ( long "log-json-file"
                    <> metavar "JSONFILE"
                    <> help "local JSON file log sink"
                    <> (maybe mempty value argparserargs.defaultLogJsonFilepath)
                )
            )
        <*> optional
            ( strOption
                ( long "session-json-file-prefix"
                    <> metavar "SESSIONSJSONPREFIX"
                    <> help "local JSON sessions file prefix"
                    <> (maybe mempty value argparserargs.defaultLogSesionsJsonPrefix)
                )
            )
        <*> fmap
            (addDefaultAgentFiles argparserargs)
            ( many
                ( strOption
                    ( long "agent-file"
                        <> metavar "AGENTFILE"
                        <> help (unlines ["root agent(s) description files", "defaults to 'agent.json'", "multi-support vary from command to command"])
                    )
                )
            )
        <*> hsubparser
            ( command "check" (info parseCheckCommand (idm))
                <> command "tui" (info parseTuiChatCommand (idm))
                <> command "run" (info parseOneShotTextualCommand (idm))
                <> command "echo-prompt" (info parseEchoPromptCommand (idm))
                <> command "describe" (info parseSelfDescribeCommand (idm))
                <> command "init" (info parseInitializeCommand (idm))
                <> command "mcp-server" (info parseMcpServer (idm))
                <> command "session-print" (info parseSessionPrintCommand (progDesc "Print a session file in markdown format"))
            )

main :: IO ()
main = do
    hSetBuffering stderr LineBuffering
    hSetBuffering stdout LineBuffering
    argzparam <- initArgParserArgs
    prog =<< execParser (infoProg argzparam)
  where
    infoProg argzparam =
        info
            (parseProgOptions argzparam <**> helper)
            ( fullDesc
                <> progDesc "Your favorite agent framework."
                <> header "hi"
            )

    traceExtra :: Prod.Tracer IO a -> Maybe (Prod.Tracer IO a) -> Prod.Tracer IO a
    traceExtra t1 Nothing = t1
    traceExtra t1 (Just t2) = Prod.traceBoth t1 t2

    prog :: Prog -> IO ()
    prog pargs = do
        showFileTracer <- makeShowLogFileTracer pargs.logFile
        baseJsonFileTracer1 <- traverse makeFileJsonTracer pargs.logJsonFile
        let logFileJsonTracer =
                case baseJsonFileTracer1 of
                    Nothing -> Nothing
                    Just t -> Just $ choose (maybeToEither . toJsonTrace) Prod.silent t
        baseHttpTracer1 <- traverse (makeHttpJsonTrace Prod.silent . Text.pack) pargs.logHttp
        let logHttpTracer =
                case baseHttpTracer1 of
                    Nothing -> Nothing
                    Just t -> Just $ choose (maybeToEither . toJsonTrace) Prod.silent t

        let baseTracer = showFileTracer `traceExtra` logHttpTracer `traceExtra` logFileJsonTracer

        case pargs.mainCommand of
            Check -> do
                apiKeys <- AgentTree.readOpenApiKeysFile pargs.apiKeysFile
                forM_ pargs.agentFiles $ \agentFile -> do
                    OneShot.mainPrintAgent $
                        AgentTree.Props
                            { AgentTree.apiKeys = apiKeys
                            , AgentTree.rootAgentFile = agentFile
                            , AgentTree.interactiveTracer =
                                Prod.traceBoth baseTracer traceUsefulPromptStdout
                            , AgentTree.agentToTool = OneShotTool.turnAgentRuntimeIntoIOTool Nothing
                            }
            TerminalUI opts -> do
                apiKeys <- AgentTree.readOpenApiKeysFile pargs.apiKeysFile
                let sessionMpath = opts.sessionJsonPrefix <|> pargs.sessionsJsonPrefix
                let oneAgent agentFile =
                        AgentTree.Props
                            { AgentTree.apiKeys = apiKeys
                            , AgentTree.rootAgentFile = agentFile
                            , AgentTree.interactiveTracer =
                                Prod.traceBoth
                                    baseTracer
                                    (traceWaitingOpenAIRateLimits (OpenAI.ApiLimits 100 10000) print)
                            , AgentTree.agentToTool = OneShotTool.turnAgentRuntimeIntoIOTool sessionMpath
                            }
                TUI2.runTUI sessionMpath (fmap oneAgent pargs.agentFiles)
            EchoPrompt opts -> do
                promptContents <- interpretPromptScript opts.promptScript
                Text.putStr promptContents
            OneShot opts -> do
                let sessionMpath = pargs.sessionsJsonPrefix
                apiKeys <- AgentTree.readOpenApiKeysFile pargs.apiKeysFile
                forM_ (take 1 pargs.agentFiles) $ \agentFile -> do
                    promptContents <- interpretPromptScript opts.promptScript
                    let oneShot = flip (OneShot.mainOneShotText opts.sessionFile)
                    oneShot promptContents $
                        AgentTree.Props
                            { AgentTree.apiKeys = apiKeys
                            , AgentTree.rootAgentFile = agentFile
                            , AgentTree.interactiveTracer =
                                baseTracer
                            , AgentTree.agentToTool = OneShotTool.turnAgentRuntimeIntoIOTool sessionMpath
                            }
            SelfDescribe -> do
                -- verify the api-key file exists at least
                _ <- AgentTree.readOpenApiKeysFile pargs.apiKeysFile
                Aeson.encodeFile "/dev/stdout" $
                    Bash.ScriptInfo
                        [ Bash.ScriptArg
                            "prompt"
                            "the prompt to call the agent with"
                            "string"
                            "string"
                            Bash.Single
                            Bash.DashDashSpace
                        ]
                        "self_call"
                        "calls oneself with a prompt"
                        (Just $ Bash.AddMessage "--no output--")
            McpServer -> do
                apiKeys <- AgentTree.readOpenApiKeysFile pargs.apiKeysFile
                let sessionMpath = pargs.sessionsJsonPrefix
                let oneAgent agentFile =
                        AgentTree.Props
                            { AgentTree.apiKeys = apiKeys
                            , AgentTree.rootAgentFile = agentFile
                            , AgentTree.interactiveTracer =
                                Prod.traceBoth
                                    baseTracer
                                    ( Prod.traceBoth
                                        traceUsefulPromptStderr
                                        (traceWaitingOpenAIRateLimits (OpenAI.ApiLimits 100 10000) (\_ -> pure ()))
                                    )
                            , AgentTree.agentToTool = OneShotTool.turnAgentRuntimeIntoIOTool sessionMpath
                            }
                McpServer.multiAgentsServer McpServer.defaultMcpServerConfig (fmap oneAgent pargs.agentFiles)
            Initialize ->
                let o =
                        Agent
                            { slug = "main-agent"
                            , apiKeyId = "main-key"
                            , flavor = "OpenAIv1"
                            , modelUrl = "https://api.openai.com/v1"
                            , modelName = "gpt-4-turbo"
                            , announce = "a helpful pupper-master capable of orchestrating other agents ensuring"
                            , toolDirectory = "tools"
                            , systemPrompt =
                                [ "You are a helpful software agent trying to solve user requests"
                                , "Your preferred action mode is to act as a puppet master capable of driving other agents."
                                , "You can prompt other agents via tools by passing them a prompt using a JSON payload."
                                , "You efficiently single-shot prompt other agents to efficiently use your token buget."
                                , "You only provide prompt examples when you think other agents may benefit."
                                , "You notify users as you progress"
                                , "If an agent fails, do not retry and abdicate"
                                ]
                            , mcpServers = Just []
                            }
                 in do
                        forM_ (take 1 pargs.agentFiles) $ \agentFile -> do
                            InitProject.initAgentFile o agentFile
                            InitProject.initAgentTooldir o agentFile
                            InitProject.initKeyFile pargs.apiKeysFile
            SessionPrint opts -> do
                SessionPrint.handleSessionPrint opts

-------------------------------------------------------------------------------
-- Utility Functions
-------------------------------------------------------------------------------

maybeToEither :: Maybe a -> Either () a
maybeToEither Nothing = Left ()
maybeToEither (Just v) = Right v

toJsonTrace :: AgentTree.Trace -> Maybe Aeson.Value
toJsonTrace x = case x of
    AgentTree.AgentTrace v -> encodeAgentTrace v
    AgentTree.McpTrace cfg v -> encodeMcpTrace cfg v
    AgentTree.DataLoadingTrace _ -> Nothing
    AgentTree.ConfigLoadedTrace _ -> Nothing
  where
    encodeAgentTrace :: Runtime.Trace -> Maybe Aeson.Value
    encodeAgentTrace tr = do
        baseVal <- encodeBaseAgentTrace tr
        Just $
            Aeson.object
                [ "e"
                    .= Aeson.object
                        [ "slug" .= Runtime.traceAgentSlug tr
                        , "agent-id" .= Runtime.traceAgentId tr
                        , "val" .= baseVal
                        ]
                ]

    encodeBaseAgentTrace :: Runtime.Trace -> Maybe Aeson.Value
    encodeBaseAgentTrace (Runtime.AgentTrace_Loading _ _ tr) =
        encodeBaseTrace_Loading tr
    encodeBaseAgentTrace (Runtime.AgentTrace_Conversation _ _ convId tr) = do
        baseVal <- encodeBaseTrace_Conversation tr
        Just $
            Aeson.object
                [ "conv-id" .= convId
                , "val" .= baseVal
                ]

    encodeBaseTrace_Loading :: BashToolbox.Trace -> Maybe Aeson.Value
    encodeBaseTrace_Loading bt =
        case bt of
            (BashToolbox.BashToolsLoadingTrace _) -> Nothing
            (BashToolbox.ReloadToolsTrace _) -> Nothing

    encodeBaseTrace_Conversation :: Runtime.ConversationTrace -> Maybe Aeson.Value
    encodeBaseTrace_Conversation bt =
        case bt of
            (Runtime.NewConversation) ->
                Just $
                    Aeson.object
                        [ "x" .= ("new-conversation" :: Text.Text)
                        ]
            (Runtime.WaitingForPrompt) ->
                Nothing
            (Runtime.LLMTrace _ (LLMTrace.HttpClientTrace _)) ->
                Nothing
            (Runtime.LLMTrace uuid (LLMTrace.CallChatCompletion val)) ->
                Just $
                    Aeson.object
                        [ "x" .= ("llm" :: Text.Text)
                        , "action" .= ("call" :: Text.Text)
                        , "call-id" .= uuid
                        , "val" .= val
                        ]
            (Runtime.LLMTrace uuid (LLMTrace.GotChatCompletion val)) ->
                Just $
                    Aeson.object
                        [ "x" .= ("llm" :: Text.Text)
                        , "action" .= ("result" :: Text.Text)
                        , "call-id" .= uuid
                        , "val" .= val
                        ]
            (Runtime.RunToolTrace uuid (ToolsTrace.BashToolsTrace (ToolsTrace.RunCommandStart cmd targs))) ->
                Just $
                    Aeson.object
                        [ "x" .= ("tool" :: Text.Text)
                        , "run-id" .= uuid
                        , "flavor" .= ("bash" :: Text.Text)
                        , "action" .= ("start" :: Text.Text)
                        , "cmd" .= cmd
                        , "args" .= targs
                        ]
            (Runtime.RunToolTrace uuid (ToolsTrace.BashToolsTrace (ToolsTrace.RunCommandStopped cmd targs code _ _))) ->
                Just $
                    Aeson.object
                        [ "x" .= ("tool" :: Text.Text)
                        , "run-id" .= uuid
                        , "flavor" .= ("bash" :: Text.Text)
                        , "action" .= ("stop" :: Text.Text)
                        , "code-str" .= show code -- todo: code
                        , "cmd" .= cmd
                        , "args" .= targs
                        ]
            (Runtime.RunToolTrace uuid (ToolsTrace.IOToolsTrace (ToolsTrace.IOScriptStarted desc input))) ->
                Just $
                    Aeson.object
                        [ "x" .= ("tool" :: Text.Text)
                        , "run-id" .= uuid
                        , "flavor" .= ("io" :: Text.Text)
                        , "action" .= ("start" :: Text.Text)
                        , "tool" .= desc.ioSlug
                        , "input" .= input
                        ]
            (Runtime.RunToolTrace uuid (ToolsTrace.IOToolsTrace (ToolsTrace.IOScriptStopped desc input _))) ->
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
            (Runtime.ChildrenTrace sub) -> do
                subVal <- encodeAgentTrace sub
                Just $ Aeson.object ["x" .= ("child" :: Text.Text), "sub" .= subVal]

    encodeMcpTrace :: McpServerDescription -> McpTools.Trace -> Maybe Aeson.Value
    encodeMcpTrace (McpSimpleBinary cfg) tr = do
        baseVal <- encodeBaseMcpTrace tr
        Just $
            Aeson.object
                [ "e"
                    .= Aeson.object
                        [ "server" .= cfg.name
                        , "val" .= baseVal
                        ]
                ]

    encodeBaseMcpTrace :: McpTools.Trace -> Maybe Aeson.Value
    encodeBaseMcpTrace (McpTools.McpClientClientTrace _) = Nothing
    encodeBaseMcpTrace
        (McpTools.McpClientRunTrace (McpClient.RunBufferMoved _ _)) =
            Nothing
    encodeBaseMcpTrace
        (McpTools.McpClientRunTrace (McpClient.RunCommandStart _)) =
            Just $
                Aeson.object
                    [ "x" .= ("program-start" :: Text.Text)
                    ]
    encodeBaseMcpTrace
        (McpTools.McpClientRunTrace (McpClient.RunCommandStopped _ code)) =
            Just $
                Aeson.object
                    [ "x" .= ("program-end" :: Text.Text)
                    , "code-str" .= show code -- todo: code
                    ]
    encodeBaseMcpTrace
        (McpTools.McpClientLoopTrace McpClient.ExitingToolCallLoop) =
            Just $
                Aeson.object
                    [ "x" .= ("loop-end" :: Text.Text)
                    ]
    encodeBaseMcpTrace
        (McpTools.McpClientLoopTrace (McpClient.ToolsRefreshed _)) =
            Just $
                Aeson.object
                    [ "x" .= ("tools-reloaded" :: Text.Text)
                    ]
    encodeBaseMcpTrace
        (McpTools.McpClientLoopTrace (McpClient.StartToolCall n _)) =
            Just $
                Aeson.object
                    [ "x" .= ("tool-call-start" :: Text.Text)
                    , "name" .= n
                    ]
    encodeBaseMcpTrace
        (McpTools.McpClientLoopTrace (McpClient.EndToolCall n _ _)) =
            Just $
                Aeson.object
                    [ "x" .= ("tool-call-end" :: Text.Text)
                    , "name" .= n
                    ]

makeHttpJsonTrace :: (Aeson.ToJSON a) => Prod.Tracer IO HttpClient.Trace -> Text.Text -> IO (Prod.Tracer IO a)
makeHttpJsonTrace baseTracer url = do
    rt <- HttpLogger.Runtime <$> HttpClient.newRuntime HttpClient.NoToken <*> pure url
    pure $ HttpLogger.httpTracer rt baseTracer

interpretPromptScript :: PromptScript -> IO Text.Text
interpretPromptScript [] =
    Text.unlines <$> traverse interpretPromptScriptDirective [FileContents "/dev/stdin"]
interpretPromptScript directives =
    Text.unlines <$> traverse interpretPromptScriptDirective directives

interpretPromptScriptDirective :: PromptScriptDirective -> IO Text.Text
interpretPromptScriptDirective x =
    case x of
        Str s -> pure s
        FileContents p -> Text.readFile p
        Separator n s -> pure $ Text.replicate n s
        ShellOutput cmd -> Text.pack <$> Process.readCreateProcess (Process.shell cmd) ("" :: String)

