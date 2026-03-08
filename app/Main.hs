{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_, when, unless)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LByteString
import Data.Functor.Contravariant.Divisible (choose)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.IO as Text
import GHC.Generics (Generic)
import qualified Prod.Tracer as Prod
import qualified System.Agents.AgentTree as AgentTree
import qualified System.Agents.AgentTree.OneShotTool as OneShotTool
import System.Agents.Base (Agent (..), AgentDescription (..), McpServerDescription (..), McpSimpleBinaryConfiguration (..), ExtraAgentRef (..))
import System.Agents.CLI.Base (makeShowLogFileTracer, makeFileJsonTracer)
import qualified System.Agents.CLI.InitProject as InitProject
import System.Agents.CLI.TraceUtils (traceUsefulPromptStderr)
import qualified System.Agents.ExportImport.Archive as ExportImport
import qualified System.Agents.ExportImport.Git as ExportImport
import qualified System.Agents.ExportImport.ToolInstall as ExportInstall
import qualified System.Agents.ExportImport.Types as ExportImport
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
import qualified System.Agents.Tools.List as ToolsList
import qualified System.Agents.Tools.McpToolbox as McpTools
import qualified System.Agents.Tools.Nest as Nest
import System.Agents.TraceUtils (traceWaitingOpenAIRateLimits)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getCurrentDirectory, getHomeDirectory)
import System.Exit (ExitCode(..), exitFailure, exitSuccess)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)
import qualified System.Process as Process

import Options.Applicative
import Data.Time (getCurrentTime)

-------------------------------------------------------------------------------
-- Default Configuration and Example Agents
-------------------------------------------------------------------------------

-- | Default API keys configuration template
defaultApiKeysContent :: LByteString.ByteString
defaultApiKeysContent = Aeson.encodePretty $ Aeson.object
    [ "keys" .=
        [ Aeson.object
            [ "id" .= ("main-key" :: Text)
            , "value" .= ("<insert-your-openai-api-key-here>" :: Text)
            ]
        , Aeson.object
            [ "id" .= ("mistral-key" :: Text)
            , "value" .= ("<insert-your-mistral-api-key-here>" :: Text)
            ]
        , Aeson.object
            [ "id" .= ("ollama-key" :: Text)
            , "value" .= ("ollama" :: Text)
            ]
        ]
    ]

-- | Default OpenAI agent configuration
defaultOpenAIAgent :: Agent
defaultOpenAIAgent = Agent
    { slug = "openai-assistant"
    , apiKeyId = "main-key"
    , flavor = "OpenAIv1"
    , modelUrl = "https://api.openai.com/v1"
    , modelName = "gpt-4-turbo-preview"
    , announce = "a helpful assistant powered by OpenAI GPT-4"
    , systemPrompt =
        [ "You are a helpful assistant."
        , "You provide clear, accurate, and concise responses."
        , "When using tools, you explain your actions to the user."
        ]
    , toolDirectory = "tools"
    , mcpServers = Just []
    , openApiToolboxes = Nothing
    , postgrestToolboxes = Nothing
    , extraAgents = Nothing
    }

-- | Mistral AI agent configuration
mistralAgent :: Agent
mistralAgent = Agent
    { slug = "mistral-assistant"
    , apiKeyId = "mistral-key"
    , flavor = "OpenAIv1"
    , modelUrl = "https://api.mistral.ai/v1"
    , modelName = "mistral-large-latest"
    , announce = "a helpful assistant powered by Mistral AI"
    , systemPrompt =
        [ "You are a helpful assistant powered by Mistral AI."
        , "You provide clear, accurate, and concise responses."
        , "You excel at reasoning and following instructions precisely."
        ]
    , toolDirectory = "tools"
    , mcpServers = Just []
    , openApiToolboxes = Nothing
    , postgrestToolboxes = Nothing
    , extraAgents = Nothing
    }

-- | Ollama local LLM agent configuration
ollamaAgent :: Agent
ollamaAgent = Agent
    { slug = "ollama-assistant"
    , apiKeyId = "ollama-key"
    , flavor = "OpenAIv1"
    , modelUrl = "http://localhost:11434/v1"
    , modelName = "llama3.2"
    , announce = "a helpful assistant running locally via Ollama"
    , systemPrompt =
        [ "You are a helpful assistant running locally on the user's machine."
        , "You provide clear and accurate responses while respecting privacy."
        , "Note: You are running on local hardware, which may limit capabilities."
        ]
    , toolDirectory = "tools"
    , mcpServers = Just []
    , openApiToolboxes = Nothing
    , postgrestToolboxes = Nothing
    , extraAgents = Nothing
    }

-- | Orchestrator agent that can delegate to other agents
orchestratorAgent :: Agent
orchestratorAgent = Agent
    { slug = "orchestrator"
    , apiKeyId = "main-key"
    , flavor = "OpenAIv1"
    , modelUrl = "https://api.openai.com/v1"
    , modelName = "gpt-4-turbo-preview"
    , announce = "a puppet-master capable of orchestrating other agents"
    , systemPrompt =
        [ "You are a helpful software agent trying to solve user requests."
        , "Your preferred action mode is to act as a puppet master capable of driving other agents."
        , "You can prompt other agents via tools by passing them a prompt using a JSON payload."
        , "You efficiently single-shot prompt other agents to efficiently use your token budget."
        , "You only provide prompt examples when you think other agents may benefit."
        , "You notify users as you progress."
        , "If an agent fails, do not retry and abdicate."
        ]
    , toolDirectory = "tools"
    , mcpServers = Just []
    , openApiToolboxes = Nothing
    , postgrestToolboxes = Nothing
    , extraAgents = Just
        [ ExtraAgentRef "openai-assistant" "openai-assistant.json"
        , ExtraAgentRef "mistral-assistant" "mistral-assistant.json"
        , ExtraAgentRef "ollama-assistant" "ollama-assistant.json"
        ]
    }

-- | Ensure the config directory structure exists with default files
ensureConfigStructure :: FilePath -> FilePath -> IO ()
ensureConfigStructure configDir secretKeysPath = do
    -- Create config directory
    createDirectoryIfMissing True configDir

    -- Create default agents directory
    let defaultAgentsDir = configDir </> "default"
    createDirectoryIfMissing True defaultAgentsDir

    -- Create API keys file if it doesn't exist
    keysExist <- doesFileExist secretKeysPath
    unless keysExist $ do
        LByteString.writeFile secretKeysPath defaultApiKeysContent
        putStrLn $ "Created API keys template at: " ++ secretKeysPath
        putStrLn "Please edit this file and add your actual API keys."

    -- Create example agent files if the default directory is empty
    defaultDirContents <- FileLoader.listJsonDirectory defaultAgentsDir
    when (null defaultDirContents) $ do
        createExampleAgents defaultAgentsDir
        putStrLn $ "Created example agent configurations in: " ++ defaultAgentsDir

-- | Create example agent configuration files
createExampleAgents :: FilePath -> IO ()
createExampleAgents agentsDir = do
    -- Write OpenAI agent
    LByteString.writeFile (agentsDir </> "openai-assistant.json") $
        Aeson.encodePretty (AgentDescription defaultOpenAIAgent)

    -- Write Mistral agent
    LByteString.writeFile (agentsDir </> "mistral-assistant.json") $
        Aeson.encodePretty (AgentDescription mistralAgent)

    -- Write Ollama agent
    LByteString.writeFile (agentsDir </> "ollama-assistant.json") $
        Aeson.encodePretty (AgentDescription ollamaAgent)

    -- Write orchestrator agent
    LByteString.writeFile (agentsDir </> "orchestrator.json") $
        Aeson.encodePretty (AgentDescription orchestratorAgent)

    -- Create tools directory for orchestrator
    createDirectoryIfMissing True (agentsDir </> "tools")

-------------------------------------------------------------------------------
-- CLI Argument Parsing
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

    let defaultConfigDir = homedir </> ".config/agents-exe"
    let secretKeysPath = defaultConfigDir </> "secret-keys"

    -- Ensure config structure exists before trying to load from it
    ensureConfigStructure defaultConfigDir secretKeysPath

    maybe (initWithoutAgentsExeConfig defaultConfigDir) (initFromAgentsExeConfig homedir) agentsExecConfig
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
    initWithoutAgentsExeConfig pconfigdir = do
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
    | ToolboxNest NestOptions
    | ListTools ToolsList.ListToolsOptions

instance Show Command where
    show Check = "Check"
    show (TerminalUI _) = "TerminalUI"
    show (OneShot _) = "OneShot"
    show (EchoPrompt _) = "EchoPrompt"
    show SelfDescribe = "SelfDescribe"
    show Initialize = "Initialize"
    show McpServer = "McpServer"
    show (SessionPrint _) = "SessionPrint"
    show (Export _) = "Export"
    show (Import _) = "Import"
    show (ToolboxNest _) = "ToolboxNest"
    show (ListTools _) = "ListTools"

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
    deriving (Show)

data OneShotOptions
    = OneShotOptions
    { sessionFile :: Maybe FilePath
    , promptScript :: PromptScript
    }
    deriving (Show)

-- | Export options for the export command
data ExportOptions = ExportOptions
    { exportSource :: ExportSource
    , exportDestination :: ExportDestination
    , exportFormat :: Maybe ExportImport.ArchiveFormat
    , exportNamespace :: Maybe Text
    , exportIncludeTools :: Bool
    , exportIncludeMcp :: Bool
    , exportGitOptions :: Maybe GitExportOptions
    }
    deriving (Show)

data ExportSource
    = ExportCurrentAgent
    | ExportAllAgents
    | ExportAgentBySlug Text
    | ExportCurrentTools         -- Tools only
    | ExportToolByName Text      -- Specific tool
    deriving (Show)

data ExportDestination
    = ExportToFile FilePath
    | ExportToGit GitExportDest
    deriving (Show)

data GitExportDest = GitExportDest
    { gitUrl :: Text
    , gitBranch :: Maybe Text
    , gitCommitMessage :: Maybe Text
    , gitPush :: Bool
    , gitTag :: Maybe Text
    }
    deriving (Show)

data GitExportOptions = GitExportOptions
    { gitDestUrl :: Text
    , gitDestBranch :: Maybe Text
    , gitDestCommitMessage :: Text
    , gitDestPush :: Bool
    , gitDestTag :: Maybe Text
    }
    deriving (Show)

-- | Import options for the import command
data ImportOptions = ImportOptions
    { importSource :: ImportSource
    , importDestination :: ExportInstall.ImportDestination
    , importNamespace :: Maybe Text
    , importMode :: ImportMode
    , importToolsOnly :: Bool
    , importListNamespaces :: Bool
    , importListTools :: Bool
    }
    deriving (Show)

data ImportSource
    = ImportFromFile FilePath
    | ImportFromGit GitImportSource
    deriving (Show)

data GitImportSource = GitImportSource
    { gitImportUrl :: Text
    , gitImportRef :: Maybe Text
    , gitImportNamespace :: Maybe Text
    }
    deriving (Show)

data ImportMode
    = ImportFailOnConflict
    | ImportOverwrite
    | ImportMerge
    deriving (Show, Eq)

-- | Options for the toolbox nest command
data NestOptions = NestOptions
    { nestAgentFile :: FilePath
    , nestOutputPath :: FilePath
    , nestToolName :: Maybe Text
    , nestIncludeTools :: [Text]
    , nestExcludeTools :: [Text]
    }
    deriving (Show)

-------------------------------------------------------------------------------
-- Parsers
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
        <*>
 flag
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

-- | Parse the export command
parseExportCommand :: Parser Command
parseExportCommand =
    Export <$> parseExportOptions

parseExportOptions :: Parser ExportOptions
parseExportOptions =
    ExportOptions
        <$> parseExportSource
        <*> parseExportDestination
        <*> optional parseArchiveFormat
        <*> optional parseNamespace
        <*> parseIncludeTools
        <*> parseIncludeMcp
        <*> optional parseGitExportOptions

parseExportSource :: Parser ExportSource
parseExportSource =
    asum
        [ flag' ExportCurrentTools
            ( long "tools-only"
                <> help "Export only tools, not the agent configuration"
            )
        , ExportToolByName <$> strOption
            ( long "tool"
                <> metavar "TOOLNAME"
                <> help "Export a specific tool by name"
            )
        , flag' ExportAllAgents
            ( long "all"
                <> help "Export all loaded agents"
            )
        , ExportAgentBySlug <$> strOption
            ( long "agent-slug"
                <> metavar "SLUG"
                <> help "Export agent by slug"
            )
        , pure ExportCurrentAgent
        ]

parseExportDestination :: Parser ExportDestination
parseExportDestination =
    asum
        [ ExportToGit <$> parseGitExportDest
        , ExportToFile <$> strOption
            ( long "output"
                <> short 'o'
                <> metavar "OUTPUT"
                <> help "Output file path"
            )
        ]

parseGitExportDest :: Parser GitExportDest
parseGitExportDest =
    GitExportDest
        <$> strOption
            ( long "git-url"
                <> metavar "URL"
                <> help "Git remote URL"
            )
        <*> optional (strOption
            ( long "git-branch"
                <> metavar "BRANCH"
                <> help "Git branch"
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

parseGitExportOptions :: Parser GitExportOptions
parseGitExportOptions =
    GitExportOptions
        <$> strOption
            ( long "git-url"
                <> metavar "URL"
                <> help "Git remote URL"
            )
        <*> optional (strOption
            ( long "git-branch"
                <> metavar "BRANCH"
                <> help "Git branch"
            ))
        <*> (fromMaybe "Update agent configurations" <$> optional (strOption
            ( long "git-message"
                <> metavar "MESSAGE"
                <> help "Git commit message"
                <> value "Update agent configurations"
            )))
        <*> switch
            ( long "git-push"
                <> help "Push to remote after commit"
            )
        <*> optional (strOption
            ( long "git-tag"
                <> metavar "TAG"
                <> help "Git tag to create"
            ))

parseArchiveFormat :: Parser ExportImport.ArchiveFormat
parseArchiveFormat =
    option (maybeReader parseFormat)
        ( long "format"
            <> metavar "FORMAT"
            <> help "Archive format: tar, tar.gz, zip (auto-detected from output if not specified)"
        )
  where
    parseFormat :: String -> Maybe ExportImport.ArchiveFormat
    parseFormat "tar" = Just ExportImport.TarFormat
    parseFormat "tar.gz" = Just ExportImport.TarGzFormat
    parseFormat "zip" = Just ExportImport.ZipFormat
    parseFormat _ = Nothing

parseNamespace :: Parser Text
parseNamespace =
    strOption
        ( long "namespace"
            <> metavar "NAMESPACE"
            <> help "Namespace for export (e.g., 'team-a.project')"
        )

parseIncludeTools :: Parser Bool
parseIncludeTools =
    not <$> switch
        ( long "no-tools"
            <> help "Exclude tools from export"
        )

parseIncludeMcp :: Parser Bool
parseIncludeMcp =
    not <$> switch
        ( long "no-mcp"
            <> help "Exclude MCP servers from export"
        )

-- | Parse the import command
parseImportCommand :: Parser Command
parseImportCommand =
    Import <$> parseImportOptions

parseImportOptions :: Parser ImportOptions
parseImportOptions =
    ImportOptions
        <$> parseImportSource
        <*> parseImportDestination
        <*> optional parseNamespace
        <*> parseImportMode
        <*> parseImportToolsOnly
        <*> parseListNamespaces
        <*> parseListTools

parseImportSource :: Parser ImportSource
parseImportSource =
    asum
        [ ImportFromGit <$> parseGitImportSource
        , ImportFromFile <$> strOption
            ( long "from-file"
                <> short 'f'
                <> metavar "FILE"
                <> help "Import from archive file"
            )
        ]

parseGitImportSource :: Parser GitImportSource
parseGitImportSource =
    GitImportSource
        <$> strOption
            ( long "git-url"
                <> metavar "URL"
                <> help "Git remote URL"
            )
        <*> optional (strOption
            ( long "git-ref"
                <> metavar "REF"
                <> help "Git ref (branch, tag, or commit)"
            ))
        <*> optional (strOption
            ( long "namespace"
                <> metavar "NAMESPACE"
                <> help "Namespace to import from"
            ))

parseImportDestination :: Parser ExportInstall.ImportDestination
parseImportDestination =
    asum
        [ flag' ExportInstall.ImportToCurrentDir
            ( long "to-current"
                <> help "Import to current directory"
            )
        , ExportInstall.ImportToPath <$> strOption
            ( long "to"
                <> metavar "PATH"
                <> help "Import to specific path"
            )
        , flag' ExportInstall.ImportToConfigDir
            ( long "to-config-dir"
                <> help "Import to config directory"
            )
        , ExportInstall.ImportToAgent <$> strOption
            ( long "install-to-agent"
                <> metavar "AGENTFILE"
                <> help "Install tools to agent's tool directory"
            )
        , ExportInstall.ImportToToolDir <$> strOption
            ( long "install-to-tooldir"
                <> metavar "TOOLDIR"
                <> help "Install tools to specific tool directory"
            )
        , pure ExportInstall.ImportToCurrentDir
        ]

parseImportMode :: Parser ImportMode
parseImportMode =
    asum
        [ flag' ImportOverwrite
            ( long "overwrite"
                <> help "Overwrite existing files on conflict"
            )
        , flag' ImportMerge
            ( long "merge"
                <> help "Merge with existing files"
            )
        , pure ImportFailOnConflict
        ]

parseImportToolsOnly :: Parser Bool
parseImportToolsOnly =
    switch
        ( long "tools-only"
            <> help "Import only tools"
        )

parseListNamespaces :: Parser Bool
parseListNamespaces =
    switch
        ( long "list-namespaces"
                <> help "List available namespaces in git repo"
        )

parseListTools :: Parser Bool
parseListTools =
    switch
        ( long "list-tools"
                <> help "List available tools in git repo"
        )

-- | Parse the toolbox nest command
parseToolboxNestCommand :: Parser Command
parseToolboxNestCommand =
    ToolboxNest <$> parseNestOptions

parseNestOptions :: Parser NestOptions
parseNestOptions =
    NestOptions
        <$> strOption
            ( long "agent-file"
                <> metavar "AGENTFILE"
                <> help "Path to the agent JSON file"
            )
        <*> strOption
            ( long "output"
                <> short 'o'
                <> metavar "OUTPUT"
                <> help "Output path for the nested tool script"
            )
        <*> optional (strOption
            ( long "name"
                <> short 'n'
                <> metavar "NAME"
                <> help "Name for the nested tool (defaults to agent slug)"
            ))
        <*> many (strOption
            ( long "include-tool"
                <> metavar "TOOL"
                <> help "Include only specific tool (can be specified multiple times)"
            ))
        <*> many (strOption
            ( long "exclude-tool"
                <> metavar "TOOL"
                <> help "Exclude specific tool (can be specified multiple times)"
            ))

-- | Parse toolbox subcommands
parseToolboxCommand :: Parser Command
parseToolboxCommand =
    hsubparser
        ( command "nest" (info parseToolboxNestCommand
            (progDesc "Nest multiple tools into a single bash-compatible tool"))
        <> command "list" (info parseListToolsCommand
            (progDesc "List available tools"))
        )

-- | Parse the list tools command
parseListToolsCommand :: Parser Command
parseListToolsCommand =
    ListTools <$> parseListToolsOptions

-- | Parse options for the list tools command
parseListToolsOptions :: Parser ToolsList.ListToolsOptions
parseListToolsOptions =
    ToolsList.ListToolsOptions
        <$> parseToolSource
        <*> parseListFormat

-- | Parse the tool source (agent file, directory, or MCP config)
parseToolSource :: Parser ToolsList.ToolSource
parseToolSource =
    asum
        [ ToolsList.ToolSourceAgent <$> strOption
            ( long "agent-file"
                <> metavar "AGENTFILE"
                <> help "Load tools from an agent's tool directory"
            )
        , ToolsList.ToolSourceDirectory <$> strOption
            ( long "tool-dir"
                <> metavar "TOOLDIR"
                <> help "Load tools directly from a directory"
            )
        , ToolsList.ToolSourceMcp <$> strOption
            ( long "mcp-config"
                <> metavar "MCPCONFIG"
                <> help "Load tools from an MCP server configuration"
            )
        ]

-- | Parse the output format
parseListFormat :: Parser ToolsList.ListFormat
parseListFormat =
    option (maybeReader parseFormat)
        ( long "format"
            <> metavar "FORMAT"
            <> help "Output format: table, json, names (default: table)"
            <> value ToolsList.FormatTable
        )
  where
    parseFormat :: String -> Maybe ToolsList.ListFormat
    parseFormat "table" = Just ToolsList.FormatTable
    parseFormat "json" = Just ToolsList.FormatJson
    parseFormat "names" = Just ToolsList.FormatNames
    parseFormat _ = Nothing

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
                <> command "export" (info parseExportCommand 
                    (progDesc "Export agent/tool configurations"))
                <> command "import" (info parseImportCommand
                    (progDesc "Import agent/tool configurations"))
                <> command "toolbox" (info parseToolboxCommand
                    (progDesc "Toolbox manipulation commands"))
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
                    -- Create a new runtime registry for this agent tree
                    registry <- AgentTree.newRuntimeRegistry
                    -- Use silent tracer to suppress diagnostic output during agent loading
                    AgentTree.withAgentTreeRuntime
                        AgentTree.Props
                            { AgentTree.apiKeys = apiKeys
                            , AgentTree.rootAgentFile = agentFile
                            , AgentTree.interactiveTracer = Prod.silent
                            , AgentTree.agentToTool = OneShotTool.turnAgentRuntimeIntoIOTool sessionStore
                            , AgentTree.runtimeRegistry = registry
                            }
                        $ \result -> case result of
                            AgentTree.Errors errs -> mapM_ print errs
                            AgentTree.Initialized tree -> printAgentCheck tree
            TerminalUI _ -> do
                apiKeys <- AgentTree.readOpenApiKeysFile pargs.apiKeysFile
                let oneAgent agentFile = do
                        registry <- AgentTree.newRuntimeRegistry
                        pure $ AgentTree.Props
                            { AgentTree.apiKeys = apiKeys
                            , AgentTree.rootAgentFile = agentFile
                            , AgentTree.interactiveTracer =
                                Prod.traceBoth
                                    baseTracer
                                    (traceWaitingOpenAIRateLimits (OpenAI.ApiLimits 100 10000) print)
                            , AgentTree.agentToTool = OneShotTool.turnAgentRuntimeIntoIOTool sessionStore
                            , AgentTree.runtimeRegistry = registry
                            }
                -- Use traverse to sequence the IO actions for creating Props
                agentPropsList <- traverse oneAgent pargs.agentFiles
                TUI.runTUI sessionStore agentPropsList
            EchoPrompt opts -> do
                promptContents <- interpretPromptScript opts.promptScript
                Text.putStr promptContents
            OneShot opts -> do
                apiKeys <- AgentTree.readOpenApiKeysFile pargs.apiKeysFile
                forM_ (take 1 pargs.agentFiles) $ \agentFile -> do
                    promptContents <- interpretPromptScript opts.promptScript
                    mSession <- maybe (pure Nothing) SessionStore.readSessionFromFile opts.sessionFile
                    registry <- AgentTree.newRuntimeRegistry
                    let oneShot text props = OneShot.mainOneShotText sessionStore opts.sessionFile mSession props text
                    oneShot promptContents $
                        AgentTree.Props
                            { AgentTree.apiKeys = apiKeys
                            , AgentTree.rootAgentFile = agentFile
                            , AgentTree.interactiveTracer = baseTracer
                            , AgentTree.agentToTool = OneShotTool.turnAgentRuntimeIntoIOTool sessionStore
                            , AgentTree.runtimeRegistry = registry
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
                let oneAgent agentFile = do
                        registry <- AgentTree.newRuntimeRegistry
                        pure $ AgentTree.Props
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
                            , AgentTree.runtimeRegistry = registry
                            }
                -- Use traverse to sequence the IO actions for creating Props
                agentPropsList <- traverse oneAgent pargs.agentFiles
                McpServer.multiAgentsServer McpServer.defaultMcpServerConfig agentPropsList
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
                            , openApiToolboxes = Nothing
                            , postgrestToolboxes = Nothing
                            , extraAgents = Nothing
                            }
                 in do
                        forM_ (take 1 pargs.agentFiles) $ \agentFile -> do
                            InitProject.initAgentFile o agentFile
                            InitProject.initAgentTooldir o agentFile
                            InitProject.initKeyFile pargs.apiKeysFile
            SessionPrint opts -> do
                SessionPrint.handleSessionPrint opts
            Export opts -> handleExport opts pargs
            Import opts -> handleImport opts
            ToolboxNest opts -> handleToolboxNest opts
            ListTools opts -> ToolsList.runListCommand opts

-------------------------------------------------------------------------------
-- Toolbox List Handler (moved to module)
-------------------------------------------------------------------------------
-- The list command is now handled in System.Agents.Tools.List

-------------------------------------------------------------------------------
-- Toolbox Nest Handler
-------------------------------------------------------------------------------

handleToolboxNest :: NestOptions -> IO ()
handleToolboxNest opts = do
    -- Convert our NestOptions to the library's NestOptions
    let nestOpts = Nest.NestOptions
            { Nest.nestAgentFile = nestAgentFile opts
            , Nest.nestOutputPath = nestOutputPath opts
            , Nest.nestToolName = nestToolName opts
            , Nest.nestIncludeTools = nestIncludeTools opts
            , Nest.nestExcludeTools = nestExcludeTools opts
            }
    Nest.runNestCommand nestOpts

-------------------------------------------------------------------------------
-- Export Handler
-------------------------------------------------------------------------------

handleExport :: ExportOptions -> Prog -> IO ()
handleExport opts pargs = do
    -- Determine archive format
    let archiveFormat = case (opts.exportDestination, opts.exportFormat) of
            (ExportToFile path, Nothing) -> fromMaybe ExportImport.TarGzFormat (ExportImport.detectArchiveFormat path)
            (_, Just fmt) -> fmt
            (_, Nothing) -> ExportImport.TarGzFormat
    
    -- Build export package
    ePackage <- buildExportPackage opts pargs
    
    case ePackage of
        Left err -> do
            Text.hPutStrLn stderr $ "Export error: " <> err
            exitFailure
        Right pkg -> do
            case opts.exportDestination of
                ExportToFile path -> do
                    result <- ExportImport.exportToArchive pkg archiveFormat path
                    case result of
                        Left err -> do
                            Text.hPutStrLn stderr $ "Export failed: " <> Text.pack (show err)
                            exitFailure
                        Right () -> do
                            Text.putStrLn $ "Exported to " <> Text.pack path
                            exitSuccess
                
                ExportToGit gitDest -> do
                    -- Convert to GitUrl and GitExportOptions
                    let gitUrl0 = ExportImport.GitUrl
                            { ExportImport.gitRemote = gitDest.gitUrl
                            , ExportImport.gitBranch = gitDest.gitBranch
                            , ExportImport.gitPath = parseNamespaceToMaybeNs opts.exportNamespace
                            }
                    let gitOpts = ExportImport.GitExportOptions
                            { ExportImport.gitCommitMessage = fromMaybe "Update agent configurations" gitDest.gitCommitMessage
                            , ExportImport.gitTag = gitDest.gitTag
                            , ExportImport.gitPush = gitDest.gitPush
                            }
                    
                    result <- ExportImport.exportToGit pkg gitUrl0 gitOpts
                    case result of
                        Left err -> do
                            Text.hPutStrLn stderr $ "Git export failed: " <> Text.pack (show err)
                            exitFailure
                        Right () -> do
                            Text.putStrLn "Exported to git repository"
                            when gitDest.gitPush $ Text.putStrLn "Changes pushed to remote"
                            exitSuccess

-- Helper to parse namespace text to Maybe Namespace
parseNamespaceToMaybeNs :: Maybe Text -> Maybe ExportImport.Namespace
parseNamespaceToMaybeNs Nothing = Nothing
parseNamespaceToMaybeNs (Just txt) = 
    case ExportImport.parseNamespace txt of
        Left _ -> Nothing
        Right ns -> Just ns

buildExportPackage :: ExportOptions -> Prog -> IO (Either Text ExportImport.ExportPackage)
buildExportPackage opts pargs = do
    now <- getCurrentTime
    let metadata = ExportImport.PackageMetadata
            { ExportImport.packageVersion = ExportImport.exportSchemaVersion
            , ExportImport.packageCreatedAt = now
            , ExportImport.packageDescription = Nothing
            , ExportImport.packageSource = Nothing
            }
    
    case opts.exportSource of
        ExportCurrentAgent -> do
            case pargs.agentFiles of
                [] -> pure $ Left "No agent file specified"
                (agentFile:_) -> do
                    -- Load agent
                    eAgent <- loadAgentFromFile agentFile
                    case eAgent of
                        Left err -> pure $ Left err
                        Right agent -> do
                            -- Load agent tools
                            tools <- if opts.exportIncludeTools
                                then loadToolsForAgent agentFile agent
                                else pure []
                            
                            let agentExport = ExportImport.AgentExport
                                    { ExportImport.agentConfig = agent
                                    , ExportImport.agentNamespace = opts.exportNamespace
                                    , ExportImport.agentTools = tools
                                    }
                            
                            pure $ Right $ ExportImport.ExportPackage
                                { ExportImport.packageMetadata = metadata
                                , ExportImport.packageAgents = [agentExport]
                                , ExportImport.packageTools = []
                                , ExportImport.packageMcpServers = if opts.exportIncludeMcp
                                    then maybe [] (map (\m -> ExportImport.McpServerExport m opts.exportNamespace)) (mcpServers agent)
                                    else []
                                }
        
        ExportAllAgents -> do
            -- Export all loaded agents
            agentExports <- mapM (\agentFile -> do
                eAgent <- loadAgentFromFile agentFile
                case eAgent of
                    Left _ -> pure Nothing
                    Right agent -> do
                        tools <- if opts.exportIncludeTools
                            then loadToolsForAgent agentFile agent
                            else pure []
                        pure $ Just $ ExportImport.AgentExport
                            { ExportImport.agentConfig = agent
                            , ExportImport.agentNamespace = opts.exportNamespace
                            , ExportImport.agentTools = tools
                            }) pargs.agentFiles
            
            pure $ Right $ ExportImport.ExportPackage
                { ExportImport.packageMetadata = metadata
                , ExportImport.packageAgents = [a | Just a <- agentExports]
                , ExportImport.packageTools = []
                , ExportImport.packageMcpServers = []
                }
        
        ExportAgentBySlug targetSlug -> do
            -- Find agent with matching slug
            agents <- mapM loadAgentFromFile pargs.agentFiles
            case [a | Right a <- agents, targetSlug == slug a] of
                [] -> pure $ Left $ "Agent with slug '" <> targetSlug <> "' not found"
                (agent:_) -> do
                    let agentFile = case [f | (f, Right a) <- zip pargs.agentFiles agents, slug a == targetSlug] of
                            (f:_) -> f
                            [] -> case pargs.agentFiles of
                                (first:_) -> first
                                [] -> error "No agent files available"
                    tools <- if opts.exportIncludeTools
                        then loadToolsForAgent agentFile agent
                        else pure []
                    
                    pure $ Right $ ExportImport.ExportPackage
                        { ExportImport.packageMetadata = metadata
                        , ExportImport.packageAgents = 
                            [ ExportImport.AgentExport
                                { ExportImport.agentConfig = agent
                                , ExportImport.agentNamespace = opts.exportNamespace
                                , ExportImport.agentTools = tools
                                }
                            ]
                        , ExportImport.packageTools = []
                        , ExportImport.packageMcpServers = if opts.exportIncludeMcp
                            then maybe [] (map (\m -> ExportImport.McpServerExport m opts.exportNamespace)) (mcpServers agent)
                            else []
                        }
        
        ExportCurrentTools -> do
            -- Export tools from current agent
            case pargs.agentFiles of
                [] -> pure $ Left "No agent file specified"
                (agentFile:_) -> do
                    eAgent <- loadAgentFromFile agentFile
                    case eAgent of
                        Left err -> pure $ Left err
                        Right agent -> do
                            standaloneTools <- loadStandaloneToolsForAgent agentFile agent
                            pure $ Right $ ExportImport.ExportPackage
                                { ExportImport.packageMetadata = metadata
                                , ExportImport.packageAgents = []
                                , ExportImport.packageTools = standaloneTools
                                , ExportImport.packageMcpServers = []
                                }
        
        ExportToolByName toolName -> do
            -- Find and export specific tool
            case pargs.agentFiles of
                [] -> pure $ Left "No agent file specified"
                (agentFile:_) -> do
                    eAgent <- loadAgentFromFile agentFile
                    case eAgent of
                        Left err -> pure $ Left err
                        Right agent -> do
                            standaloneTools <- loadStandaloneToolsForAgent agentFile agent
                            case [t | t <- standaloneTools, (Bash.scriptSlug . ExportImport.standaloneToolInfo) t == toolName] of
                                [] -> pure $ Left $ "Tool '" <> toolName <> "' not found"
                                (tool:_) -> pure $ Right $ ExportImport.ExportPackage
                                    { ExportImport.packageMetadata = metadata
                                    , ExportImport.packageAgents = []
                                    , ExportImport.packageTools = [tool]
                                    , ExportImport.packageMcpServers = []
                                    }

-------------------------------------------------------------------------------
-- Import Handler
-------------------------------------------------------------------------------

handleImport :: ImportOptions -> IO ()
handleImport opts
    | importListNamespaces opts = handleListNamespaces opts
    | importListTools opts = handleListTools opts
    | otherwise = do
        -- Perform import
        eResult <- case importSource opts of
            ImportFromFile path -> do
                if importToolsOnly opts
                    then do
                        ePkg <- ExportImport.importToolsFromArchive path
                        case ePkg of
                            Left err -> pure $ Left $ Text.pack $ show err
                            Right pkg -> do
                                let installOpts = ExportInstall.InstallOptions
                                        { ExportInstall.installForce = importMode opts == ImportOverwrite || importMode opts == ImportMerge
                                        , ExportInstall.installLink = False
                                        , ExportInstall.installPrefix = importNamespace opts
                                        }
                                result <- ExportInstall.importToolPackage pkg (importDestination opts) installOpts
                                case result of
                                    Left err -> pure $ Left $ Text.pack $ show err
                                    Right () -> pure $ Right ()
                    else do
                        ePkg <- ExportImport.importFromArchive path
                        case ePkg of
                            Left err -> pure $ Left $ Text.pack $ show err
                            Right _pkg -> do
                                -- TODO: Handle full package import
                                pure $ Right ()
            
            ImportFromGit gitSource -> do
                let gitUrl0 = ExportImport.GitUrl
                        { ExportImport.gitRemote = gitImportUrl gitSource
                        , ExportImport.gitBranch = gitImportRef gitSource
                        , ExportImport.gitPath = parseNamespaceToMaybeNs (importNamespace opts)
                        }
                let gitOpts = ExportImport.GitImportOptions
                        { ExportImport.gitRef = gitImportRef gitSource
                        , ExportImport.gitSparsePaths = []
                        }
                
                if importToolsOnly opts
                    then do
                        ePkg <- ExportImport.importToolsFromGit gitUrl0 (parseNamespaceToMaybeNs (importNamespace opts))
                        case ePkg of
                            Left err -> pure $ Left $ Text.pack $ show err
                            Right pkg -> do
                                let installOpts = ExportInstall.InstallOptions
                                        { ExportInstall.installForce = importMode opts == ImportOverwrite || importMode opts == ImportMerge
                                        , ExportInstall.installLink = False
                                        , ExportInstall.installPrefix = Nothing
                                        }
                                result <- ExportInstall.importToolPackage pkg (importDestination opts) installOpts
                                case result of
                                    Left err -> pure $ Left $ Text.pack $ show err
                                    Right () -> pure $ Right ()
                    else do
                        ePkg <- ExportImport.importFromGit gitUrl0 gitOpts
                        case ePkg of
                            Left err -> pure $ Left $ Text.pack $ show err
                            Right _pkg -> do
                                -- TODO: Handle full package import
                                pure $ Right ()
        
        case eResult of
            Left err -> do
                Text.hPutStrLn stderr $ "Import failed: " <> err
                exitFailure
            Right () -> do
                Text.putStrLn "Import successful"
                exitSuccess

handleListNamespaces :: ImportOptions -> IO ()
handleListNamespaces opts = do
    case importSource opts of
        ImportFromGit gitSource -> do
            let gitUrl0 = ExportImport.GitUrl
                    { ExportImport.gitRemote = gitImportUrl gitSource
                    , ExportImport.gitBranch = gitImportRef gitSource
                    , ExportImport.gitPath = parseNamespaceToMaybeNs (importNamespace opts)
                    }
            result <- ExportImport.listGitNamespaces gitUrl0
            case result of
                Left err -> do
                    Text.hPutStrLn stderr $ "Failed to list namespaces: " <> Text.pack (show err)
                    exitFailure
                Right namespaces -> do
                    Text.putStrLn "Available namespaces:"
                    mapM_ (Text.putStrLn . ("  " <>) . Text.intercalate "." . ExportImport.unNamespace) namespaces
                    exitSuccess
        _ -> do
            Text.hPutStrLn stderr "List namespaces only supported for git sources"
            exitFailure

handleListTools :: ImportOptions -> IO ()
handleListTools opts = do
    case importSource opts of
        ImportFromGit gitSource -> do
            let gitUrl0 = ExportImport.GitUrl
                    { ExportImport.gitRemote = gitImportUrl gitSource
                    , ExportImport.gitBranch = gitImportRef gitSource
                        , ExportImport.gitPath = parseNamespaceToMaybeNs (importNamespace opts)
                    }
            result <- ExportImport.listGitTools gitUrl0
            case result of
                Left err -> do
                    Text.hPutStrLn stderr $ "Failed to list tools: " <> Text.pack (show err)
                    exitFailure
                Right tools -> do
                    Text.putStrLn "Available tools:"
                    mapM_ (\(ns, info0) -> do
                        Text.putStrLn $ "  " <> Text.intercalate "." (ExportImport.unNamespace ns) <> ":"
                        Text.putStrLn $ "    " <> Bash.scriptSlug info0 <> " - " <> Bash.scriptDescription info0
                        ) tools
                    exitSuccess
        _ -> do
            Text.hPutStrLn stderr "List tools only supported for git sources"
            exitFailure

-------------------------------------------------------------------------------
-- Helper Functions
-------------------------------------------------------------------------------

loadAgentFromFile :: FilePath -> IO (Either Text Agent)
loadAgentFromFile path = do
    result <- Aeson.eitherDecodeFileStrict' path
    case result of
        Left err -> pure $ Left $ Text.pack $ "Failed to parse agent file: " ++ err
        Right (AgentDescription agent) -> pure $ Right agent

loadToolsForAgent :: FilePath -> Agent -> IO [ExportImport.ToolExport]
loadToolsForAgent agentFile agent = do
    let toolDir = takeDirectory agentFile </> toolDirectory agent
    exists <- doesDirectoryExist toolDir
    if not exists
        then pure []
        else do
            toolFiles <- ExportInstall.discoverTools toolDir
            mapM (loadToolExport toolDir) toolFiles
  where
    loadToolExport :: FilePath -> FilePath -> IO ExportImport.ToolExport
    loadToolExport _baseDir toolPath = do
        content <- ByteString.readFile toolPath
        perms <- ExportImport.getFileMode toolPath
        let tName = Text.pack $ takeFileName toolPath
        -- Run describe to get metadata
        _ <- ExportInstall.validateTool toolPath
        pure $ ExportImport.ToolExport
            { ExportImport.toolName = tName
            , ExportImport.toolContent = content
            , ExportImport.toolPermissions = perms
            , ExportImport.toolMetadata = Nothing  -- Could be populated by running describe
            , ExportImport.toolNamespace = Nothing
            }

loadStandaloneToolsForAgent :: FilePath -> Agent -> IO [ExportImport.StandaloneToolExport]
loadStandaloneToolsForAgent agentFile agent = do
    let toolDir = takeDirectory agentFile </> toolDirectory agent
    exists <- doesDirectoryExist toolDir
    if not exists
        then pure []
        else do
            toolFiles <- ExportInstall.discoverTools toolDir
            mapM (loadStandaloneTool toolDir) toolFiles
  where
    loadStandaloneTool :: FilePath -> FilePath -> IO ExportImport.StandaloneToolExport
    loadStandaloneTool _baseDir toolPath = do
        content <- ByteString.readFile toolPath
        perms <- ExportImport.getFileMode toolPath
        -- Run describe to get metadata
        (code, out, _) <- Process.readProcessWithExitCode toolPath ["describe"] ""
        let metadata = case code of
                ExitSuccess ->
                    case Aeson.eitherDecode (LByteString.fromStrict $ TextEncoding.encodeUtf8 $ Text.pack out) of
                        Left _ -> defaultMetadata toolPath
                        Right info0 -> info0
                _ -> defaultMetadata toolPath
        pure $ ExportImport.StandaloneToolExport
            { ExportImport.standaloneToolInfo = metadata
            , ExportImport.standaloneToolScript = content
            , ExportImport.standaloneToolAuxFiles = []
            , ExportImport.standaloneToolPermissions = perms
            }
    
    defaultMetadata :: FilePath -> Bash.ScriptInfo
    defaultMetadata path = Bash.ScriptInfo
        { Bash.scriptArgs = []
        , Bash.scriptSlug = Text.pack $ takeFileName path
        , Bash.scriptDescription = "Imported tool"
        , Bash.scriptEmptyResultBehavior = Nothing
        }

-------------------------------------------------------------------------------
-- Check Command Helper
-------------------------------------------------------------------------------

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
    AgentTree.OpenAPITrace _desc _v -> Nothing  -- OpenAPI traces not serialized to JSON for now
    AgentTree.PostgRESTTrace _desc _v -> Nothing  -- PostgREST traces not serialized to JSON for now
    AgentTree.DataLoadingTrace _ -> Nothing
    AgentTree.ConfigLoadedTrace _ -> Nothing
    AgentTree.CyclicReferencesWarning _ -> Nothing
    AgentTree.ReferenceValidationTrace _ -> Nothing
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
                        , "code-str" .= show code
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
                        , "code-str" .= show code
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

