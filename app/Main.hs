{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_, join, when)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Functor.Contravariant.Divisible (choose)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import GHC.Generics (Generic)
import qualified Prod.Tracer as Prod
import qualified System.Agents.AgentTree as AgentTree
import qualified System.Agents.AgentTree.OneShotTool as OneShotTool
import System.Agents.Base (Agent (..), McpServerDescription (..), McpSimpleBinaryConfiguration (..))
import System.Agents.CLI.Base (makeShowLogFileTracer, makeFileJsonTracer)
import qualified System.Agents.CLI.InitProject as InitProject
import System.Agents.CLI.TraceUtils (traceUsefulPromptStderr)
import System.Agents.ExportImport.Types
    ( ArchiveFormat(..)
    , InstallOptions(..)
    , ToolPackage(..)
    , StandaloneToolExport(..)
    , defaultInstallOptions
    )
import qualified System.Agents.ExportImport.Archive as ExportArchive
import qualified System.Agents.ExportImport.Git as ExportGit
import qualified System.Agents.ExportImport.ToolInstall as ToolInstall
import qualified System.Agents.FileLoader as FileLoader
import qualified System.Agents.HttpClient as HttpClient
import qualified System.Agents.HttpLogger as HttpLogger
import qualified System.Agents.LLMs.OpenAI as LLMTrace
import qualified System.Agents.LLMs.OpenAI as OpenAI
import qualified System.Agents.MCP.Client as McpClient
import qualified System.Agents.MCP.Client.Runtime as McpClient
import qualified System.Agents.MCP.Server as McpServer
import qualified System.Agents.OneShot as OneShot
import qualified System.Agents.Runtime as Runtime
import qualified System.Agents.Runtime.Trace as RuntimeTrace
import qualified System.Agents.SessionPrint as SessionPrint
import qualified System.Agents.SessionStore as SessionStore
import qualified System.Agents.TUI.Core as TUI
import qualified System.Agents.Tools as ToolsTrace
import qualified System.Agents.Tools.Bash as Bash
import qualified System.Agents.Tools.Bash as ToolsTrace
import qualified System.Agents.Tools.BashToolbox as BashToolbox
import qualified System.Agents.Tools.IO as ToolsTrace
import qualified System.Agents.Tools.McpToolbox as McpTools
import System.Agents.ToolRegistration (ToolRegistration(..))
import System.Agents.TraceUtils (traceWaitingOpenAIRateLimits)
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, getHomeDirectory)
import System.FilePath (takeDirectory, takeFileName, (</>))
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
    | Export ExportOptions
    | Import ImportOptions
    | ToolList ToolListOptions

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
    {
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

-- Dummy tool registration for config loading
dummyToolRegistration :: ToolRegistration
dummyToolRegistration = ToolRegistration
    (error "dummy tool") 
    (error "dummy tool") 
    (const Nothing)

-------------------------------------------------------------------------------
-- Export Types and Parsers
-------------------------------------------------------------------------------

data ExportSource
    = ExportCurrentAgent
    | ExportAllAgents
    | ExportAgentBySlug Text.Text
    | ExportCurrentTools
    | ExportToolByName Text.Text
    deriving (Show)

data ExportOptions = ExportOptions
    { exportSource :: ExportSource
    , exportOutput :: FilePath
    , exportFormat :: ArchiveFormat
    , exportToolsOnly :: Bool
    }
    deriving (Show)

parseExportCommand :: Parser Command
parseExportCommand =
    Export <$> parseExportOptions

parseExportOptions :: Parser ExportOptions
parseExportOptions =
    ExportOptions
        <$> parseExportSource
        <*> strOption
            ( long "output"
                <> short 'o'
                <> metavar "OUTPUT"
                <> help "Output file path for export"
            )
        <*> flag TarGz TarGz
            ( long "tar-gz"
                <> help "Export as tar.gz (default)"
            )
        <*> switch
            ( long "tools-only"
                <> short 't'
                <> help "Export only tools, not agent configuration"
            )

parseExportSource :: Parser ExportSource
parseExportSource =
    asum
        [ flag' ExportCurrentTools
            ( long "tools-only"
                <> short 't'
                <> help "Export only tools from current agent"
            )
        , ExportToolByName <$> strOption
            ( long "tool"
                <> metavar "TOOLNAME"
                <> help "Export a specific tool by name"
            )
        , pure ExportCurrentAgent
        ]

-------------------------------------------------------------------------------
-- Import Types and Parsers
-------------------------------------------------------------------------------

data ImportDestination
    = ImportToCurrentDir
    | ImportToPath FilePath
    | ImportToConfigDir
    | ImportToAgent FilePath
    | ImportToToolDir FilePath
    deriving (Show)

data ImportOptions = ImportOptions
    { importSource :: ImportSource
    , importDestination :: ImportDestination
    , importToolsOnly :: Bool
    , importForce :: Bool
    , importLink :: Bool
    }
    deriving (Show)

data ImportSource
    = ImportFromFile FilePath
    | ImportFromGit String (Maybe Text.Text)
    | ImportFromAgent FilePath
    deriving (Show)

parseImportCommand :: Parser Command
parseImportCommand =
    Import <$> parseImportOptions

parseImportOptions :: Parser ImportOptions
parseImportOptions =
    ImportOptions
        <$> parseImportSource
        <*> parseImportDestination
        <*> switch
            ( long "tools-only"
                <> short 't'
                <> help "Import only tools, not agent configuration"
            )
        <*> switch
            ( long "force"
                <> short 'f'
                <> help "Overwrite existing tools"
            )
        <*> switch
            ( long "link"
                <> short 'l'
                <> help "Use symlinks instead of copy (for git-based tools)"
            )

parseImportSource :: Parser ImportSource
parseImportSource =
    asum
        [ ImportFromGit
            <$> strOption
                ( long "git-url"
                    <> metavar "URL"
                    <> help "Import from a git repository"
                )
            <*> optional (strOption
                ( long "git-ref"
                    <> metavar "REF"
                    <> help "Git reference (branch/tag/commit)"
                ))
        , ImportFromAgent <$> strOption
            ( long "from-agent"
                <> metavar "AGENTFILE"
                <> help "Copy tools from another agent"
            )
        , ImportFromFile <$> strOption
            ( long "from-file"
                <> metavar "FILE"
                <> help "Import from an archive file"
            )
        ]

parseImportDestination :: Parser ImportDestination
parseImportDestination =
    asum
        [ ImportToAgent <$> strOption
            ( long "install-to-agent"
                <> metavar "AGENTFILE"
                <> help "Install tools to this agent's tool directory"
            )
        , ImportToToolDir <$> strOption
            ( long "to"
                <> metavar "TOOLDIR"
                <> help "Install tools to a specific directory"
            )
        , pure ImportToCurrentDir
        ]

-------------------------------------------------------------------------------
-- Tool List Types and Parsers
-------------------------------------------------------------------------------

data ToolListOptions = ToolListOptions
    { toolListSource :: ToolListSource
    , toolListFormat :: ToolListFormat
    }
    deriving (Show)

data ToolListSource
    = ToolListFromAgent FilePath
    | ToolListFromGit String
    | ToolListFromDir FilePath
    deriving (Show)

data ToolListFormat
    = ToolListSimple
    | ToolListDetailed
    | ToolListJson
    deriving (Show)

parseToolListCommand :: Parser Command
parseToolListCommand =
    ToolList <$> parseToolListOptions

parseToolListOptions :: Parser ToolListOptions
parseToolListOptions =
    ToolListOptions
        <$> parseToolListSource
        <*> parseToolListFormat

parseToolListSource :: Parser ToolListSource
parseToolListSource =
    asum
        [ ToolListFromGit <$> strOption
            ( long "git-url"
                <> metavar "URL"
                <> help "List tools from a git repository"
            )
        , ToolListFromAgent <$> strOption
            ( long "agent-file"
                <> metavar "AGENTFILE"
                <> help "List tools from an agent"
            )
        , ToolListFromDir <$> strArgument
            ( metavar "DIRECTORY"
                <> help "Directory to list tools from"
                <> value "."
            )
        ]

parseToolListFormat :: Parser ToolListFormat
parseToolListFormat =
    flag ToolListSimple ToolListDetailed
        ( long "detailed"
            <> short 'd'
            <> help "Show detailed tool information"
        )
    <|> flag' ToolListJson
        ( long "json"
            <> help "Output as JSON"
        )
    <|> pure ToolListSimple

-------------------------------------------------------------------------------
-- Original Parsers
-------------------------------------------------------------------------------

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
    pure TuiOptions

parseOneShotOptions :: Parser OneShotOptions
parseOneShotOptions =
    OneShotOptions
        <$> optional
            ( strOption
                ( long "session-file"
                    <> metavar "SESSIONFILE"
                    <> help "extra session-file to resume/store"
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
                <> command "export" (info parseExportCommand (progDesc "Export agent or tools"))
                <> command "import" (info parseImportCommand (progDesc "Import agent or tools"))
                <> command "list-tools" (info parseToolListCommand (progDesc "List available tools"))
            )

-------------------------------------------------------------------------------
-- Main Entry Point
-------------------------------------------------------------------------------

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

        -- Initialize SessionStore from the session prefix argument
        let sessionStore = maybe SessionStore.defaultSessionStore SessionStore.mkSessionStore pargs.sessionsJsonPrefix

        case pargs.mainCommand of
            Check -> do
                apiKeys <- AgentTree.readOpenApiKeysFile pargs.apiKeysFile
                forM_ pargs.agentFiles $ \agentFile -> do
                    -- Use silent tracer to suppress diagnostic output during agent loading
                    AgentTree.withAgentTreeRuntime
                        AgentTree.Props
                            { AgentTree.apiKeys = apiKeys
                            , AgentTree.rootAgentFile = agentFile
                            , AgentTree.interactiveTracer = Prod.silent
                            , AgentTree.agentToTool = OneShotTool.turnAgentRuntimeIntoIOTool sessionStore
                            }
                        $ \result -> case result of
                            AgentTree.Errors errs -> mapM_ print errs
                            AgentTree.Initialized tree -> printAgentCheck tree
            TerminalUI _ -> do
                apiKeys <- AgentTree.readOpenApiKeysFile pargs.apiKeysFile
                let sessionStore = maybe SessionStore.defaultSessionStore SessionStore.mkSessionStore pargs.sessionsJsonPrefix
                let oneAgent agentFile =
                        AgentTree.Props
                            { AgentTree.apiKeys = apiKeys
                            , AgentTree.rootAgentFile = agentFile
                            , AgentTree.interactiveTracer =
                                Prod.traceBoth
                                    baseTracer
                                    (traceWaitingOpenAIRateLimits (OpenAI.ApiLimits 100 10000) print)
                            , AgentTree.agentToTool = OneShotTool.turnAgentRuntimeIntoIOTool sessionStore
                            }
                TUI.runTUI sessionStore (fmap oneAgent pargs.agentFiles)
            EchoPrompt opts -> do
                promptContents <- interpretPromptScript opts.promptScript
                Text.putStr promptContents
            OneShot opts -> do
                let sessionStore = maybe SessionStore.defaultSessionStore SessionStore.mkSessionStore pargs.sessionsJsonPrefix
                apiKeys <- AgentTree.readOpenApiKeysFile pargs.apiKeysFile
                forM_ (take 1 pargs.agentFiles) $ \agentFile -> do
                    promptContents <- interpretPromptScript opts.promptScript
                    mSession <- join <$> traverse SessionStore.readSessionFromFile opts.sessionFile
                    let oneShot text props = OneShot.mainOneShotText sessionStore opts.sessionFile mSession props text
                    oneShot promptContents $
                        AgentTree.Props
                            { AgentTree.apiKeys = apiKeys
                            , AgentTree.rootAgentFile = agentFile
                            , AgentTree.interactiveTracer = baseTracer
                            , AgentTree.agentToTool = OneShotTool.turnAgentRuntimeIntoIOTool sessionStore
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
                let sessionStore = maybe SessionStore.defaultSessionStore SessionStore.mkSessionStore pargs.sessionsJsonPrefix
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
                            , AgentTree.agentToTool = OneShotTool.turnAgentRuntimeIntoIOTool sessionStore
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
            
            -- NEW: Export command handler
            Export opts -> handleExportCommand pargs opts
            
            -- NEW: Import command handler  
            Import opts -> handleImportCommand pargs opts
            
            -- NEW: List tools command handler
            ToolList opts -> handleToolListCommand opts

-------------------------------------------------------------------------------
-- Export Command Handler
-------------------------------------------------------------------------------

handleExportCommand :: Prog -> ExportOptions -> IO ()
handleExportCommand pargs opts = do
    case exportSource opts of
        ExportCurrentTools -> do
            -- Export tools from current agent's tool directory
            forM_ (take 1 pargs.agentFiles) $ \agentFile -> do
                eTree <- AgentTree.loadAgentTreeConfig $
                    AgentTree.Props
                        { AgentTree.apiKeys = []  -- Not needed for config loading
                        , AgentTree.rootAgentFile = agentFile
                        , AgentTree.interactiveTracer = Prod.silent
                        , AgentTree.agentToTool = \_ _ _ -> dummyToolRegistration
                        }
                case eTree of
                    Left errs -> do
                        Text.putStrLn $ "Error loading agent config: " <> Text.pack (show errs)
                    Right cfg -> do
                        let toolDir = AgentTree.agentRootDir cfg </> (AgentTree.agentConfig cfg).toolDirectory
                        exportToolsFromDirectory toolDir opts
                        
        ExportToolByName toolName -> do
            -- Export a specific tool by name
            forM_ (take 1 pargs.agentFiles) $ \agentFile -> do
                eTree <- AgentTree.loadAgentTreeConfig $
                    AgentTree.Props
                        { AgentTree.apiKeys = []
                        , AgentTree.rootAgentFile = agentFile
                        , AgentTree.interactiveTracer = Prod.silent
                        , AgentTree.agentToTool = \_ _ _ -> dummyToolRegistration
                        }
                case eTree of
                    Left errs -> do
                        Text.putStrLn $ "Error loading agent config: " <> Text.pack (show errs)
                    Right cfg -> do
                        let toolDir = AgentTree.agentRootDir cfg </> (AgentTree.agentConfig cfg).toolDirectory
                        exportSingleTool toolDir toolName opts
                        
        _ -> do
            Text.putStrLn "Export of full agents not yet implemented. Use --tools-only for now."

exportToolsFromDirectory :: FilePath -> ExportOptions -> IO ()
exportToolsFromDirectory toolDir opts = do
    exists <- doesDirectoryExist toolDir
    if not exists
        then Text.putStrLn $ "Tool directory not found: " <> Text.pack toolDir
        else do
            -- Discover tools in the directory
            toolDescs <- ExportArchive.discoverTools toolDir
            
            if null toolDescs
                then Text.putStrLn "No tools found in directory"
                else do
                    -- Convert to StandaloneToolExport
                    exports <- mapM ExportArchive.exportToolFromDescription toolDescs
                    
                    -- Export to archive
                    ExportArchive.exportToolsToArchive exports (exportFormat opts) (exportOutput opts)
                    Text.putStrLn $ "Exported " <> Text.pack (show (length exports)) <> " tools to " <> Text.pack (exportOutput opts)

exportSingleTool :: FilePath -> Text.Text -> ExportOptions -> IO ()
exportSingleTool toolDir toolName opts = do
    exists <- doesDirectoryExist toolDir
    if not exists
        then Text.putStrLn $ "Tool directory not found: " <> Text.pack toolDir
        else do
            -- Discover tools in the directory
            toolDescs <- ExportArchive.discoverTools toolDir
            
            -- Find the specific tool - FIXED: Use proper field accessor
            case filter (\d -> Bash.scriptSlug (Bash.scriptInfo d) == toolName) toolDescs of
                [] -> Text.putStrLn $ "Tool not found: " <> toolName
                (desc:_) -> do
                    export <- ExportArchive.exportToolFromDescription desc
                    ExportArchive.exportToolsToArchive [export] (exportFormat opts) (exportOutput opts)
                    Text.putStrLn $ "Exported tool '" <> toolName <> "' to " <> Text.pack (exportOutput opts)

-------------------------------------------------------------------------------
-- Import Command Handler
-------------------------------------------------------------------------------

handleImportCommand :: Prog -> ImportOptions -> IO ()
handleImportCommand pargs opts = do
    let installOpts = defaultInstallOptions
            { installForce = importForce opts
            , installLink = importLink opts
            }
    
    case importSource opts of
        ImportFromFile filePath -> do
            -- Import from archive file
            result <- ExportArchive.importToolsFromArchive filePath
            case result of
                Left err -> Text.putStrLn $ "Import failed: " <> Text.pack (show err)
                Right pkg -> installToolPackage pkg (importDestination opts) pargs installOpts
                
        ImportFromGit url _mRef -> do
            -- Import from git
            let gitUrl = ExportGit.GitUrl $ Text.pack url
            result <- ExportGit.importToolsFromGit gitUrl Nothing
            case result of
                Left err -> Text.putStrLn $ "Git import failed: " <> Text.pack (show err)
                Right pkg -> installToolPackage pkg (importDestination opts) pargs installOpts
                
        ImportFromAgent agentFile -> do
            -- Copy tools from another agent
            eSourceTree <- AgentTree.loadAgentTreeConfig $
                AgentTree.Props
                    { AgentTree.apiKeys = []
                    , AgentTree.rootAgentFile = agentFile
                    , AgentTree.interactiveTracer = Prod.silent
                    , AgentTree.agentToTool = \_ _ _ -> dummyToolRegistration
                    }
            case eSourceTree of
                Left errs -> Text.putStrLn $ "Error loading source agent: " <> Text.pack (show errs)
                Right sourceCfg -> do
                    case importDestination opts of
                        ImportToAgent targetFile -> do
                            eTargetTree <- AgentTree.loadAgentTreeConfig $
                                AgentTree.Props
                                    { AgentTree.apiKeys = []
                                    , AgentTree.rootAgentFile = targetFile
                                    , AgentTree.interactiveTracer = Prod.silent
                                    , AgentTree.agentToTool = \_ _ _ -> dummyToolRegistration
                                    }
                            case eTargetTree of
                                Left errs -> Text.putStrLn $ "Error loading target agent: " <> Text.pack (show errs)
                                Right targetCfg -> do
                                    result <- ToolInstall.copyToolsBetweenAgents sourceCfg targetCfg installOpts
                                    case result of
                                        Left err -> Text.putStrLn $ "Copy failed: " <> Text.pack (show err)
                                        Right () -> Text.putStrLn "Tools copied successfully"
                        _ -> Text.putStrLn "Must specify --install-to-agent when copying from another agent"

installToolPackage :: ToolPackage -> ImportDestination -> Prog -> InstallOptions -> IO ()
installToolPackage pkg dest pargs opts = do
    case dest of
        ImportToAgent agentFile -> do
            eTree <- AgentTree.loadAgentTreeConfig $
                AgentTree.Props
                    { AgentTree.apiKeys = []
                    , AgentTree.rootAgentFile = agentFile
                    , AgentTree.interactiveTracer = Prod.silent
                    , AgentTree.agentToTool = \_ _ _ -> dummyToolRegistration
                    }
            case eTree of
                Left errs -> Text.putStrLn $ "Error loading agent: " <> Text.pack (show errs)
                Right cfg -> do
                    let agent = AgentTree.agentConfig cfg
                    result <- ToolInstall.installToolsToAgent (toolPackageTools pkg) agent agentFile opts
                    case result of
                        Left err -> Text.putStrLn $ "Installation failed: " <> Text.pack (show err)
                        Right () -> Text.putStrLn $ "Installed " <> Text.pack (show (length (toolPackageTools pkg))) <> " tools to agent"
                        
        ImportToToolDir toolDir -> do
            result <- ToolInstall.installToolsGlobally (toolPackageTools pkg) toolDir opts
            case result of
                Left err -> Text.putStrLn $ "Installation failed: " <> Text.pack (show err)
                Right () -> Text.putStrLn $ "Installed " <> Text.pack (show (length (toolPackageTools pkg))) <> " tools to " <> Text.pack toolDir
                
        ImportToCurrentDir -> do
            -- Install to current directory as "tools/"
            result <- ToolInstall.installToolsGlobally (toolPackageTools pkg) "tools" opts
            case result of
                Left err -> Text.putStrLn $ "Installation failed: " <> Text.pack (show err)
                Right () -> Text.putStrLn $ "Installed " <> Text.pack (show (length (toolPackageTools pkg))) <> " tools to ./tools"
                
        ImportToPath path -> do
            result <- ToolInstall.installToolsGlobally (toolPackageTools pkg) path opts
            case result of
                Left err -> Text.putStrLn $ "Installation failed: " <> Text.pack (show err)
                Right () -> Text.putStrLn $ "Installed " <> Text.pack (show (length (toolPackageTools pkg))) <> " tools to " <> Text.pack path
                
        ImportToConfigDir -> do
            Text.putStrLn "Import to config directory not yet implemented"

-------------------------------------------------------------------------------
-- Tool List Command Handler
-------------------------------------------------------------------------------

handleToolListCommand :: ToolListOptions -> IO ()
handleToolListCommand opts = do
    tools <- case toolListSource opts of
        ToolListFromDir dir -> do
            exists <- doesDirectoryExist dir
            if not exists
                then do
                    Text.putStrLn $ "Directory not found: " <> Text.pack dir
                    pure []
                else do
                    descs <- ExportArchive.discoverTools dir
                    pure $ map Bash.scriptInfo descs
                    
        ToolListFromAgent agentFile -> do
            eTree <- AgentTree.loadAgentTreeConfig $
                AgentTree.Props
                    { AgentTree.apiKeys = []
                    , AgentTree.rootAgentFile = agentFile
                    , AgentTree.interactiveTracer = Prod.silent
                    , AgentTree.agentToTool = \_ _ _ -> dummyToolRegistration
                    }
            case eTree of
                Left errs -> do
                    Text.putStrLn $ "Error loading agent: " <> Text.pack (show errs)
                    pure []
                Right cfg -> do
                    let toolDir = AgentTree.agentRootDir cfg </> (AgentTree.agentConfig cfg).toolDirectory
                    exists <- doesDirectoryExist toolDir
                    if not exists
                        then pure []
                        else do
                            descs <- ExportArchive.discoverTools toolDir
                            pure $ map Bash.scriptInfo descs
                            
        ToolListFromGit url -> do
            let gitUrl = ExportGit.GitUrl $ Text.pack url
            result <- ExportGit.listGitTools gitUrl
            case result of
                Left err -> do
                    Text.putStrLn $ "Git list failed: " <> Text.pack (show err)
                    pure []
                Right listings -> do
                    pure $ map snd listings
    
    -- Output based on format
    case toolListFormat opts of
        ToolListJson -> do
            Aeson.encodeFile "/dev/stdout" tools
        ToolListDetailed -> do
            forM_ tools $ \tool -> do
                Text.putStrLn $ ""
                Text.putStrLn $ "Name: " <> Bash.scriptSlug tool
                Text.putStrLn $ "Description: " <> Bash.scriptDescription tool
                Text.putStrLn $ "Arguments: " <> Text.pack (show (length (Bash.scriptArgs tool)))
        ToolListSimple -> do
            forM_ tools $ \tool -> do
                Text.putStrLn $ Bash.scriptSlug tool <> ": " <> Bash.scriptDescription tool

-------------------------------------------------------------------------------
-- Check Command Helper
-------------------------------------------------------------------------------

-- | Print agent check information in the format: slug: announce (n-tools)
printAgentCheck :: AgentTree.AgentTree -> IO ()
printAgentCheck tree = do
    tools <- Runtime.agentTools tree.agentRuntime
    let toolCount = length tools
    Text.putStrLn $
        Runtime.agentSlug tree.agentRuntime
        <> ": "
        <> Runtime.agentAnnounce tree.agentRuntime
        <> " ("
        <> Text.pack (show toolCount)
        <> " tools)"

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
    encodeAgentTrace :: RuntimeTrace.Trace -> Maybe Aeson.Value
    encodeAgentTrace tr = do
        baseVal <- encodeBaseAgentTrace tr
        Just $
            Aeson.object
                [ "e"
                    .= Aeson.object
                        [ "slug" .= RuntimeTrace.traceAgentSlug tr
                        , "agent-id" .= RuntimeTrace.traceAgentId tr
                        , "val" .= baseVal
                        ]
                ]

    encodeBaseAgentTrace :: RuntimeTrace.Trace -> Maybe Aeson.Value
    encodeBaseAgentTrace (RuntimeTrace.AgentTrace_Loading _ _ tr) =
        encodeBaseTrace_Loading tr
    encodeBaseAgentTrace (RuntimeTrace.AgentTrace_Conversation _ _ convId tr) = do
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

    encodeBaseTrace_Conversation :: RuntimeTrace.ConversationTrace -> Maybe Aeson.Value
    encodeBaseTrace_Conversation bt =
        case bt of
            (RuntimeTrace.NewConversation) ->
                Just $
                    Aeson.object
                        [ "x" .= ("new-conversation" :: Text.Text)
                        ]
            (RuntimeTrace.WaitingForPrompt) ->
                Nothing
            (RuntimeTrace.LLMTrace _ (LLMTrace.HttpClientTrace _)) ->
                Nothing
            (RuntimeTrace.LLMTrace uuid (LLMTrace.CallChatCompletion val)) ->
                Just $
                    Aeson.object
                        [ "x" .= ("llm" :: Text.Text)
                        , "action" .= ("call" :: Text.Text)
                        , "call-id" .= uuid
                        , "val" .= val
                        ]
            (RuntimeTrace.LLMTrace uuid (LLMTrace.GotChatCompletion val)) ->
                Just $
                    Aeson.object
                        [ "x" .= ("llm" :: Text.Text)
                        , "action" .= ("result" :: Text.Text)
                        , "call-id" .= uuid
                        , "val" .= val
                        ]
            (RuntimeTrace.RunToolTrace uuid (ToolsTrace.BashToolsTrace (ToolsTrace.RunCommandStart cmd targs))) ->
                Just $
                    Aeson.object
                        [ "x" .= ("tool" :: Text.Text)
                        , "run-id" .= uuid
                        , "flavor" .= ("bash" :: Text.Text)
                        , "action" .= ("start" :: Text.Text)
                        , "cmd" .= cmd
                        , "args" .= targs
                        ]
            (RuntimeTrace.RunToolTrace uuid (ToolsTrace.BashToolsTrace (ToolsTrace.RunCommandStopped cmd targs code _ _))) ->
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
            (RuntimeTrace.RunToolTrace uuid (ToolsTrace.IOToolsTrace (ToolsTrace.IOScriptStarted desc input))) ->
                Just $
                    Aeson.object
                        [ "x" .= ("tool" :: Text.Text)
                        , "run-id" .= uuid
                        , "flavor" .= ("io" :: Text.Text)
                        , "action" .= ("start" :: Text.Text)
                        , "tool" .= desc.ioSlug
                        , "input" .= input
                        ]
            (RuntimeTrace.RunToolTrace uuid (ToolsTrace.IOToolsTrace (ToolsTrace.IOScriptStopped desc input _))) ->
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
            (RuntimeTrace.ChildrenTrace sub) -> do
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

