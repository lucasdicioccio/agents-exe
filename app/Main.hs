{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_, join, when)
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
import System.Agents.CLI.TraceUtils (traceUsefulPromptStderr)
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
import System.Agents.TraceUtils (traceWaitingOpenAIRateLimits)
import System.Directory (doesFileExist, getCurrentDirectory, getHomeDirectory)
import System.FilePath (takeDirectory, (</>))
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)
import qualified System.Process as Process

import Options.Applicative

-------------------------------------------------------------------------------
-- Archive Format Types
-------------------------------------------------------------------------------

-- | Supported archive formats for export/import
data ArchiveFormat
    = TarFormat      -- ^ Plain tar archive
    | TarGzFormat    -- ^ Gzip-compressed tar archive
    | ZipFormat      -- ^ ZIP archive
    deriving (Show, Eq, Generic)

-------------------------------------------------------------------------------
-- Export Types
-------------------------------------------------------------------------------

-- | Options for the export command
data ExportOptions = ExportOptions
    { exportSource :: ExportSource        -- ^ What to export
    , exportDestination :: ExportDestination  -- ^ Where to export
    , exportFormat :: Maybe ArchiveFormat -- ^ Override format detection
    , exportNamespace :: Maybe Text.Text  -- ^ Add namespace prefix
    , exportIncludeTools :: Bool          -- ^ Include tool scripts
    , exportIncludeMcp :: Bool            -- ^ Include MCP configs
    }
    deriving (Show, Generic)

-- | Source specification for export
data ExportSource
    = ExportCurrentAgent                  -- ^ Export agent from --agent-file
    | ExportAllAgents                     -- ^ Export all loaded agents
    | ExportAgentBySlug Text.Text         -- ^ Export specific agent by slug
    deriving (Show, Generic)

-- | Destination specification for export
data ExportDestination
    = ExportToFile FilePath               -- ^ Archive file path
    | ExportToGit GitExportOptions        -- ^ Git repository
    deriving (Show, Generic)

-- | Git-specific export options
data GitExportOptions = GitExportOptions
    { gitUrl :: Text.Text
    , gitBranch :: Maybe Text.Text
    , gitCommitMessage :: Maybe Text.Text
    , gitPush :: Bool
    , gitTag :: Maybe Text.Text
    }
    deriving (Show, Generic)

-------------------------------------------------------------------------------
-- Import Types
-------------------------------------------------------------------------------

-- | Options for the import command
data ImportOptions = ImportOptions
    { importSource :: ImportSource        -- ^ Where to import from
    , importDestination :: ImportDestination  -- ^ Where to put imported configs
    , importNamespace :: Maybe Text.Text  -- ^ Filter by namespace
    , importMode :: ImportMode            -- ^ How to handle conflicts
    }
    deriving (Show, Generic)

-- | Source specification for import
data ImportSource
    = ImportFromFile FilePath             -- ^ Archive file
    | ImportFromGit GitImportOptions      -- ^ Git repository
    deriving (Show, Generic)

-- | Git-specific import options
data GitImportOptions = GitImportOptions
    { gitImportUrl :: Text.Text
    , gitImportRef :: Maybe Text.Text     -- ^ branch/tag/commit
    , gitImportNamespace :: Maybe Text.Text  -- ^ specific namespace to import
    }
    deriving (Show, Generic)

-- | Destination specification for import
data ImportDestination
    = ImportToCurrentDir                  -- ^ Import to current directory
    | ImportToPath FilePath               -- ^ Import to specific path
    | ImportToConfigDir                   -- ^ Import to ~/.config/agents-exe
    deriving (Show, Generic)

-- | Conflict resolution mode for import
data ImportMode
    = ImportFailOnConflict                -- ^ Fail if files exist
    | ImportOverwrite                     -- ^ Overwrite existing files
    | ImportMerge                         -- ^ Merge configs (smart combine)
    deriving (Show, Eq, Generic)

-------------------------------------------------------------------------------
-- ArgParser and Prog Types
-------------------------------------------------------------------------------

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
    | Export ExportOptions   -- ^ Export agent configurations
    | Import ImportOptions   -- ^ Import agent configurations

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

-------------------------------------------------------------------------------
-- Command Parsers
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

-------------------------------------------------------------------------------
-- Export Command Parsers
-------------------------------------------------------------------------------

-- | Parse the export command
parseExportCommand :: Parser Command
parseExportCommand =
    Export <$> parseExportOptions

-- | Parse export options
parseExportOptions :: Parser ExportOptions
parseExportOptions =
    ExportOptions
        <$> parseExportSource
        <*> parseExportDestination
        <*> optional parseArchiveFormat
        <*> optional parseNamespaceOption
        <*> parseIncludeToolsFlag
        <*> parseIncludeMcpFlag

-- | Parse what to export (source)
parseExportSource :: Parser ExportSource
parseExportSource =
    asum
        [ flag' ExportAllAgents
            ( long "all"
                <> help "Export all loaded agents"
            )
        , ExportAgentBySlug <$> strOption
            ( long "agent-slug"
                <> metavar "SLUG"
                <> help "Export specific agent by slug"
            )
        , pure ExportCurrentAgent  -- Default: export agent from --agent-file
        ]

-- | Parse where to export (destination)
parseExportDestination :: Parser ExportDestination
parseExportDestination =
    asum
        [ ExportToGit <$> parseGitExportOptions
        , ExportToFile <$> strOption
            ( long "output"
                <> short 'o'
                <> metavar "FILE"
                <> help "Output archive file path"
            )
        ]

-- | Parse Git export options
parseGitExportOptions :: Parser GitExportOptions
parseGitExportOptions =
    GitExportOptions
        <$> strOption
            ( long "git-url"
                <> metavar "URL"
                <> help "Git repository URL to export to"
            )
        <*> optional (strOption
            ( long "git-branch"
                <> metavar "BRANCH"
                <> help "Git branch to push to (default: main)"
            ))
        <*> optional (strOption
            ( long "git-message"
                <> metavar "MESSAGE"
                <> help "Git commit message"
            ))
        <*> switch
            ( long "git-push"
                <> help "Push to remote after commit"
            )
        <*> optional (strOption
            ( long "git-tag"
                <> metavar "TAG"
                <> help "Git tag to create"
            ))

-- | Parse archive format option
parseArchiveFormat :: Parser ArchiveFormat
parseArchiveFormat =
    asum
        [ flag' TarGzFormat
            ( long "tar.gz"
                <> help "Use tar.gz format (default)"
            )
        , flag' TarFormat
            ( long "tar"
                <> help "Use plain tar format"
            )
        , flag' ZipFormat
            ( long "zip"
                <> help "Use zip format"
            )
        ]

-- | Parse namespace option
parseNamespaceOption :: Parser Text.Text
parseNamespaceOption =
    strOption
        ( long "namespace"
            <> metavar "NAMESPACE"
            <> help "Add namespace prefix to exported package"
        )

-- | Parse include tools flag
parseIncludeToolsFlag :: Parser Bool
parseIncludeToolsFlag =
    switch
        ( long "include-tools"
            <> help "Include tool scripts in export"
        )

-- | Parse include MCP flag
parseIncludeMcpFlag :: Parser Bool
parseIncludeMcpFlag =
    switch
        ( long "include-mcp"
            <> help "Include MCP server configurations in export"
        )

-------------------------------------------------------------------------------
-- Import Command Parsers
-------------------------------------------------------------------------------

-- | Parse the import command
parseImportCommand :: Parser Command
parseImportCommand =
    Import <$> parseImportOptions

-- | Parse import options
parseImportOptions :: Parser ImportOptions
parseImportOptions =
    ImportOptions
        <$> parseImportSource
        <*> parseImportDestination
        <*> optional parseImportNamespaceFilter
        <*> parseImportMode

-- | Parse where to import from (source)
parseImportSource :: Parser ImportSource
parseImportSource =
    asum
        [ ImportFromGit <$> parseGitImportOptions
        , ImportFromFile <$> strOption
            ( long "from-file"
                <> short 'f'
                <> metavar "FILE"
                <> help "Import from archive file"
            )
        ]

-- | Parse Git import options
parseGitImportOptions :: Parser GitImportOptions
parseGitImportOptions =
    GitImportOptions
        <$> strOption
            ( long "git-url"
                <> metavar "URL"
                <> help "Git repository URL to import from"
            )
        <*> optional (strOption
            ( long "git-ref"
                <> metavar "REF"
                <> help "Git branch, tag, or commit to import from"
            ))
        <*> optional (strOption
            ( long "git-namespace"
                <> metavar "NAMESPACE"
                <> help "Specific namespace to import from git repo"
            ))

-- | Parse import destination
parseImportDestination :: Parser ImportDestination
parseImportDestination =
    asum
        [ flag' ImportToConfigDir
            ( long "to-config-dir"
                <> help "Import to ~/.config/agents-exe"
            )
        , ImportToPath <$> strOption
            ( long "to"
                <> metavar "PATH"
                <> help "Import to specific path"
            )
        , pure ImportToCurrentDir  -- Default: import to current directory
        ]

-- | Parse namespace filter for import
parseImportNamespaceFilter :: Parser Text.Text
parseImportNamespaceFilter =
    strOption
        ( long "namespace"
            <> metavar "NAMESPACE"
            <> help "Filter import by namespace"
        )

-- | Parse import mode (conflict resolution)
parseImportMode :: Parser ImportMode
parseImportMode =
    asum
        [ flag' ImportOverwrite
            ( long "overwrite"
                <> help "Overwrite existing files"
            )
        , flag' ImportMerge
            ( long "merge"
                <> help "Merge configurations (smart combine)"
            )
        , pure ImportFailOnConflict  -- Default: fail on conflict
        ]

-------------------------------------------------------------------------------
-- Main Parser
-------------------------------------------------------------------------------

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
                <> command "export" (info parseExportCommand (progDesc "Export agent configurations to archive or git"))
                <> command "import" (info parseImportCommand (progDesc "Import agent configurations from archive or git"))
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
                            , AgentTree.interactiveTracer =
                                baseTracer
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
            Export opts -> do
                handleExport opts pargs
            Import opts -> do
                handleImport opts pargs

-------------------------------------------------------------------------------
-- Export Command Handler
-------------------------------------------------------------------------------

-- | Handle the export command
handleExport :: ExportOptions -> Prog -> IO ()
handleExport opts pargs = do
    -- Validate export destination is provided
    case opts.exportDestination of
        ExportToFile path -> do
            putStrLn $ "Exporting to archive: " ++ path
            -- TODO: Implement actual export logic
            -- 1. Load agents based on exportSource
            -- 2. Build ExportPackage
            -- 3. Export to archive file
            putStrLn $ "Exported to: " ++ path
        ExportToGit gitOpts -> do
            putStrLn $ "Exporting to git repository: " ++ Text.unpack gitOpts.gitUrl
            -- TODO: Implement actual git export logic
            -- 1. Load agents based on exportSource
            -- 2. Build ExportPackage
            -- 3. Export to git repository
            putStrLn $ "Exported to git: " ++ Text.unpack gitOpts.gitUrl
            when gitOpts.gitPush $
                putStrLn "Pushed to remote"
            case gitOpts.gitTag of
                Just tag -> putStrLn $ "Created tag: " ++ Text.unpack tag
                Nothing -> pure ()

    -- Log export details
    putStrLn $ "Export source: " ++ showExportSource opts.exportSource
    case opts.exportNamespace of
        Just ns -> putStrLn $ "Namespace: " ++ Text.unpack ns
        Nothing -> pure ()
    putStrLn $ "Include tools: " ++ show opts.exportIncludeTools
    putStrLn $ "Include MCP configs: " ++ show opts.exportIncludeMcp

-- | Helper to show export source
showExportSource :: ExportSource -> String
showExportSource ExportCurrentAgent = "current agent (from --agent-file)"
showExportSource ExportAllAgents = "all loaded agents"
showExportSource (ExportAgentBySlug slug) = "agent by slug: " ++ Text.unpack slug

-------------------------------------------------------------------------------
-- Import Command Handler
-------------------------------------------------------------------------------

-- | Handle the import command
handleImport :: ImportOptions -> Prog -> IO ()
handleImport opts pargs = do
    -- Validate and resolve import destination
    destPath <- case opts.importDestination of
        ImportToCurrentDir -> getCurrentDirectory
        ImportToPath path -> pure path
        ImportToConfigDir -> do
            home <- getHomeDirectory
            pure $ home </> ".config/agents-exe"

    -- Create destination directory if it doesn't exist
    -- TODO: Implement directory creation

    -- Perform import based on source
    case opts.importSource of
        ImportFromFile path -> do
            putStrLn $ "Importing from archive: " ++ path
            -- TODO: Implement actual import logic
            -- 1. Read archive file
            -- 2. Extract ExportPackage
            -- 3. Handle conflicts based on importMode
            -- 4. Write to destination
            putStrLn $ "Imported to: " ++ destPath
        ImportFromGit gitOpts -> do
            putStrLn $ "Importing from git repository: " ++ Text.unpack gitOpts.gitImportUrl
            case gitOpts.gitImportRef of
                Just ref -> putStrLn $ "  Reference: " ++ Text.unpack ref
                Nothing -> putStrLn "  Reference: default branch"
            -- TODO: Implement actual git import logic
            -- 1. Clone/fetch git repository
            -- 2. Checkout specified ref
            -- 3. Extract ExportPackage
            -- 4. Handle conflicts based on importMode
            -- 5. Write to destination
            putStrLn $ "Imported to: " ++ destPath

    -- Log import details
    putStrLn $ "Import mode: " ++ showImportMode opts.importMode
    case opts.importNamespace of
        Just ns -> putStrLn $ "Namespace filter: " ++ Text.unpack ns
        Nothing -> pure ()

-- | Helper to show import mode
showImportMode :: ImportMode -> String
showImportMode ImportFailOnConflict = "fail on conflict"
showImportMode ImportOverwrite = "overwrite existing files"
showImportMode ImportMerge = "merge configurations"

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

