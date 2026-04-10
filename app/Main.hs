{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Main entry point for the agents-exe executable.

This module handles command-line argument parsing and dispatches to
the appropriate command handlers. The actual command logic is implemented
in separate modules under 'System.Agents.CLI'.
-}
module Main where

import Control.Monad (unless, when)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as LByteString
import Data.Functor.Contravariant.Divisible (choose)
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import GHC.Generics (Generic)
import Options.Applicative
import qualified Prod.Tracer as Prod
import System.Directory (createDirectoryIfMissing, doesFileExist, getCurrentDirectory, getHomeDirectory)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, (</>))
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)

import System.Agents.Base (Agent (..), AgentDescription (..), ExtraAgentRef (..))
import System.Agents.CLI.Aliases (
    AliasDefinition,
    defaultAliases,
    resolveAliases,
 )
import System.Agents.CLI.Base (makeFileJsonTracer, makeShowLogFileTracer)
import qualified System.Agents.CLI.Check as CheckCmd
import qualified System.Agents.CLI.CheckToolCall as CheckToolCallCmd
import qualified System.Agents.CLI.Cowsay as CowsayCmd
import qualified System.Agents.CLI.DescribeTool as DescribeToolCmd
import qualified System.Agents.CLI.EchoPrompt as EchoPromptCmd
import qualified System.Agents.CLI.Initialize as InitializeCmd
import qualified System.Agents.CLI.McpServer as McpServerCmd
import qualified System.Agents.CLI.New as NewCmd
import qualified System.Agents.CLI.OneShot as OneShotCmd
import qualified System.Agents.CLI.Paths as PathsCmd
import System.Agents.CLI.PromptScript (PromptScript, PromptScriptDirective (..))
import qualified System.Agents.CLI.ReplayToolCall as ReplayToolCallCmd
import qualified System.Agents.CLI.SelfDescribe as SelfDescribeCmd
import qualified System.Agents.CLI.SessionEdit as SessionEditCmd
import qualified System.Agents.CLI.SessionIndex as SessionIndexCmd
import qualified System.Agents.CLI.SessionSearch as SessionSearchCmd
import qualified System.Agents.CLI.Spec as SpecCmd
import qualified System.Agents.CLI.TUI as TUICmd
import qualified System.Agents.CLI.ToolCall as ToolCallCmd
import qualified System.Agents.FileLoader as FileLoader
import qualified System.Agents.HttpClient as HttpClient
import qualified System.Agents.HttpLogger as HttpLogger
import qualified System.Agents.OneShot as OneShot
import System.Agents.SessionPrint (PrintAmount (..), PrintVisibility (..))
import qualified System.Agents.SessionPrint as SessionPrint
import qualified System.Agents.SessionPrint.Inject as SessionInject
import qualified System.Agents.SessionStore as SessionStore
import System.Agents.Session.Search.Types (IndexOperation (..), DateFilter (..))

import System.Agents.CLI (Trace (..), toJsonTrace)

-------------------------------------------------------------------------------
-- Default Configuration
-------------------------------------------------------------------------------

-- | Default API keys configuration template
defaultApiKeysContent :: LByteString.ByteString
defaultApiKeysContent =
    Aeson.encodePretty $
        Aeson.object
            [ "keys"
                .= [ Aeson.object
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
defaultOpenAIAgent =
    Agent
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
        , toolDirectory = Just "tools"
        , bashToolboxes = Nothing
        , mcpServers = Just []
        , openApiToolboxes = Nothing
        , postgrestToolboxes = Nothing
        , builtinToolboxes = Just []
        , extraAgents = Nothing
        , skillSources = Nothing
        , autoEnableSkills = Nothing
        }

-- | Mistral AI agent configuration
mistralAgent :: Agent
mistralAgent =
    Agent
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
        , toolDirectory = Just "tools"
        , bashToolboxes = Nothing
        , mcpServers = Just []
        , openApiToolboxes = Nothing
        , postgrestToolboxes = Nothing
        , builtinToolboxes = Just []
        , extraAgents = Nothing
        , skillSources = Nothing
        , autoEnableSkills = Nothing
        }

-- | Ollama local LLM agent configuration
ollamaAgent :: Agent
ollamaAgent =
    Agent
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
        , toolDirectory = Just "tools"
        , bashToolboxes = Nothing
        , mcpServers = Just []
        , openApiToolboxes = Nothing
        , postgrestToolboxes = Nothing
        , builtinToolboxes = Just []
        , extraAgents = Nothing
        , skillSources = Nothing
        , autoEnableSkills = Nothing
        }

-- | Orchestrator agent that can delegate to other agents
orchestratorAgent :: Agent
orchestratorAgent =
    Agent
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
        , toolDirectory = Just "tools"
        , bashToolboxes = Nothing
        , mcpServers = Just []
        , openApiToolboxes = Nothing
        , postgrestToolboxes = Nothing
        , builtinToolboxes = Just []
        , extraAgents =
            Just
                [ ExtraAgentRef "openai-assistant" "openai-assistant.json"
                , ExtraAgentRef "mistral-assistant" "mistral-assistant.json"
                , ExtraAgentRef "ollama-assistant" "ollama-assistant.json"
                ]
        , skillSources = Nothing
        , autoEnableSkills = Nothing
        }

-- | Ensure the config directory structure exists with default files
ensureConfigStructure :: FilePath -> FilePath -> IO ()
ensureConfigStructure cfgDir secretKeysPath = do
    -- Create config directory
    createDirectoryIfMissing True cfgDir

    -- Create default agents directory
    let defaultAgentsDir = cfgDir </> "default"
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

-- | Arguments needed to initialize the argument parser
data ArgParserArgs = ArgParserArgs
    { configdir :: FilePath
    , defaultAgentFiles :: [FilePath]
    , defaultLogJsonHttpEndpoint :: Maybe String
    , defaultLogJsonFilepath :: Maybe FilePath
    , defaultLogRawFilepath :: Maybe FilePath
    , defaultLogSesionsJsonPrefix :: Maybe FilePath
    , argPromptAliases :: Map Text AliasDefinition
    , defaultSelfDescribeSlug :: Maybe String
    , defaultSelfDescribeDescription :: Maybe String
    }

-- | Get the path to the secrets key file
secretsKeyFile :: ArgParserArgs -> FilePath
secretsKeyFile pargs = pargs.configdir </> "secret-keys"

-- | Helper to make agent-file entirely optional whilst using the 'many' combinator
addDefaultAgentFiles :: ArgParserArgs -> [FilePath] -> [FilePath]
addDefaultAgentFiles pargs [] = pargs.defaultAgentFiles
addDefaultAgentFiles _ xs = xs

-- | Logging configuration from config file
data AgentsExeLogConfig = AgentsExeLogConfig
    { logJsonHttpEndpoint :: Maybe String
    , logJsonPath :: Maybe FilePath
    , logRawPath :: Maybe FilePath
    , logSessionsJsonPrefix :: Maybe FilePath
    }
    deriving (Show, Generic)

instance Aeson.FromJSON AgentsExeLogConfig

-- | Main configuration file format
data AgentsExeConfig = AgentsExeConfig
    { agentsConfigDir :: Maybe FilePath
    , agentsDirectories :: [FilePath]
    , agentsFiles :: [FilePath]
    , agentsLogs :: Maybe AgentsExeLogConfig
    , cfgPromptAliases :: Maybe (Map Text AliasDefinition)
    , cfgSelfDescribeSlug :: Maybe String
    , cfgSelfDescribeDescription :: Maybe String
    }
    deriving (Show, Generic)

instance Aeson.FromJSON AgentsExeConfig where
    parseJSON = Aeson.withObject "AgentsExeConfig" $ \v ->
        AgentsExeConfig
            <$> v Aeson..:? "agentsConfigDir"
            <*> v Aeson..:? "agentsDirectories" Aeson..!= []
            <*> v Aeson..:? "agentsFiles" Aeson..!= []
            <*> v Aeson..:? "agentsLogs"
            <*> v Aeson..:? "promptAliases"
            <*> v Aeson..:? "selfDescribeSlug"
            <*> v Aeson..:? "selfDescribeDescription"

-- | Locate the agents-exe.cfg.json by traversing up the directory tree
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

-- | Initialize the argument parser with configuration
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
                        (resolveAliases obj.cfgPromptAliases)
                        obj.cfgSelfDescribeSlug
                        obj.cfgSelfDescribeDescription

    initWithoutAgentsExeConfig :: FilePath -> IO ArgParserArgs
    initWithoutAgentsExeConfig pconfigdir = do
        jsonPaths <- FileLoader.listJsonDirectory (pconfigdir </> "default")

        -- Create sessions directory for storing conversations when outside projects
        -- This ensures session files are properly persisted instead of being written
        -- to the current working directory with an empty prefix
        let sessionsDir = pconfigdir </> "sessions"
        createDirectoryIfMissing True sessionsDir

        pure $
            ArgParserArgs
                pconfigdir
                jsonPaths
                Nothing
                Nothing
                Nothing
                (Just sessionsDir)
                defaultAliases
                Nothing
                Nothing

-- | Main program configuration
data Prog = Prog
    { configDir :: FilePath
    , apiKeysFile :: FilePath
    , logFile :: FilePath
    , logHttp :: Maybe String
    , logJsonFile :: Maybe FilePath
    , sessionsJsonPrefix :: Maybe FilePath
    , agentFiles :: [FilePath]
    , selectedAgentSlug :: Maybe Text
    , progPromptAliases :: Map Text AliasDefinition
    , mainCommand :: Command
    }

-- | Available commands
data Command
    = Check CheckCmd.CheckOptions
    | CheckToolCall CheckToolCallCmd.CheckToolCallOptions
    | ListToolCalls ReplayToolCallCmd.ListToolCallsOptions
    | ReplayToolCall ReplayToolCallCmd.ReplayToolCallOptions
    | TerminalUI TUICmd.TuiOptions
    | OneShot OneShotCmd.OneShotOptions
    | EchoPrompt EchoPromptCmd.EchoPromptOptions
    | SelfDescribe SelfDescribeCmd.SelfDescribeOptions
    | DescribeTool DescribeToolCmd.DescribeToolOptions
    | Initialize
    | McpServer
    | SessionPrint SessionPrint.SessionPrintOptions
    | SessionEdit SessionEditCmd.SessionEditOptions
    | SessionIndex SessionIndexCmd.SessionIndexOptions
    | SessionSearch SessionSearchCmd.SessionSearchOptions
    | Paths PathsCmd.PathsOptions
    | Cowsay CowsayCmd.CowsayOptions
    | Spec SpecCmd.SpecOptions
    | New NewCmd.NewOptions
    | ToolCall ToolCallCmd.ToolCallOptions

instance Show Command where
    show (Check _) = "Check"
    show (CheckToolCall _) = "CheckToolCall"
    show (ListToolCalls _) = "ListToolCalls"
    show (ReplayToolCall _) = "ReplayToolCall"
    show (TerminalUI _) = "TerminalUI"
    show (OneShot _) = "OneShot"
    show (EchoPrompt _) = "EchoPrompt"
    show (SelfDescribe _) = "SelfDescribe"
    show (DescribeTool _) = "DescribeTool"
    show Initialize = "Initialize"
    show McpServer = "McpServer"
    show (SessionPrint _) = "SessionPrint"
    show (SessionEdit _) = "SessionEdit"
    show (SessionIndex _) = "SessionIndex"
    show (SessionSearch _) = "SessionSearch"
    show (Paths _) = "Paths"
    show (Cowsay _) = "Cowsay"
    show (Spec _) = "Spec"
    show (New _) = "New"
    show (ToolCall _) = "ToolCall"

-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------

-- | Parse the --tools option for the check command
parseToolsOption :: Parser CheckCmd.ToolsOutputMode
parseToolsOption =
    option
        (maybeReader parseMode)
        ( long "tools"
            <> metavar "MODE"
            <> help "Tool display mode: none, list, agents-exe, openai (default: none)"
            <> value CheckCmd.ToolsNone
            <> showDefaultWith showMode
        )
  where
    parseMode :: String -> Maybe CheckCmd.ToolsOutputMode
    parseMode "none" = Just CheckCmd.ToolsNone
    parseMode "list" = Just CheckCmd.ToolsList
    parseMode "agents-exe" = Just CheckCmd.ToolsAgentsExe
    parseMode "openai" = Just CheckCmd.ToolsOpenAI
    parseMode _ = Nothing

    showMode :: CheckCmd.ToolsOutputMode -> String
    showMode CheckCmd.ToolsNone = "none"
    showMode CheckCmd.ToolsList = "list"
    showMode CheckCmd.ToolsAgentsExe = "agents-exe"
    showMode CheckCmd.ToolsOpenAI = "openai"

parseCheckCommand :: Parser Command
parseCheckCommand = Check <$> parseCheckOptions

parseCheckOptions :: Parser CheckCmd.CheckOptions
parseCheckOptions =
    CheckCmd.CheckOptions
        <$> parseToolsOption

-- | Parse the check-tool-call command
parseCheckToolCallCommand :: Parser Command
parseCheckToolCallCommand = CheckToolCall <$> parseCheckToolCallOptions

parseCheckToolCallOptions :: Parser CheckToolCallCmd.CheckToolCallOptions
parseCheckToolCallOptions =
    CheckToolCallCmd.CheckToolCallOptions
        <$> strOption
            ( long "tool"
                <> short 't'
                <> metavar "TOOLPATH"
                <> help "Path to the tool script to validate against"
            )

-- | Parse the list-tool-calls command
parseListToolCallsCommand :: Parser Command
parseListToolCallsCommand = ListToolCalls <$> parseListToolCallsOptions

parseListToolCallsOptions :: Parser ReplayToolCallCmd.ListToolCallsOptions
parseListToolCallsOptions =
    ReplayToolCallCmd.ListToolCallsOptions
        <$> strArgument
            ( metavar "SESSIONFILE"
                <> help "Path to the session JSON file to extract tool calls from"
            )
        <*> parseListFormatOption

parseListFormatOption :: Parser ReplayToolCallCmd.ListFormat
parseListFormatOption =
    option
        (maybeReader parseFormat)
        ( long "format"
            <> short 'f'
            <> metavar "FORMAT"
            <> help "Output format: human, json, or brief (default: human)"
            <> value ReplayToolCallCmd.ListFormatHuman
            <> showDefaultWith showFormat
        )
  where
    parseFormat :: String -> Maybe ReplayToolCallCmd.ListFormat
    parseFormat "human" = Just ReplayToolCallCmd.ListFormatHuman
    parseFormat "json" = Just ReplayToolCallCmd.ListFormatJson
    parseFormat "brief" = Just ReplayToolCallCmd.ListFormatBrief
    parseFormat _ = Nothing

    showFormat :: ReplayToolCallCmd.ListFormat -> String
    showFormat ReplayToolCallCmd.ListFormatHuman = "human"
    showFormat ReplayToolCallCmd.ListFormatJson = "json"
    showFormat ReplayToolCallCmd.ListFormatBrief = "brief"

-- | Parse the replay-tool-call command
parseReplayToolCallCommand :: Parser Command
parseReplayToolCallCommand = ReplayToolCall <$> parseReplayToolCallOptions

parseReplayToolCallOptions :: Parser ReplayToolCallCmd.ReplayToolCallOptions
parseReplayToolCallOptions =
    ReplayToolCallCmd.ReplayToolCallOptions
        <$> strOption
            ( long "session"
                <> short 's'
                <> metavar "SESSIONFILE"
                <> help "Path to the session file containing the tool call"
            )
        <*> option
            auto
            ( long "tool-call"
                <> short 'i'
                <> metavar "INDEX"
                <> help "Index of the tool call to replay (0-based)"
            )
        <*> strOption
            ( long "tool"
                <> short 't'
                <> metavar "TOOLPATH"
                <> help "Path to the tool script to execute"
            )
        <*> switch
            ( long "validate-only"
                <> help "Only validate the tool call arguments, don't execute"
            )
        <*> switch
            ( long "raw"
                <> help "Show raw output instead of formatted"
            )

parseTuiChatCommand :: Parser Command
parseTuiChatCommand = TerminalUI <$> parseTuiOptions

parseOneShotTextualCommand :: Parser Command
parseOneShotTextualCommand = OneShot <$> parseOneShotOptions

parseEchoPromptCommand :: Parser Command
parseEchoPromptCommand = EchoPrompt <$> parseEchoPromptOptions

parseTuiOptions :: Parser TUICmd.TuiOptions
parseTuiOptions = pure TUICmd.TuiOptions

-- | Parse the --thinking option with choices: none, stdout, stderr
parseThinkingOption :: Parser OneShot.ThinkingOutput
parseThinkingOption =
    option
        (maybeReader parseThinking)
        ( long "thinking"
            <> metavar "TARGET"
            <> help "Where to output thinking content: none, stdout, or stderr (default: none)"
            <> value OneShot.ThinkingNone
            <> showDefaultWith showThinking
        )
  where
    parseThinking :: String -> Maybe OneShot.ThinkingOutput
    parseThinking "none" = Just OneShot.ThinkingNone
    parseThinking "stdout" = Just OneShot.ThinkingStdout
    parseThinking "stderr" = Just OneShot.ThinkingStderr
    parseThinking _ = Nothing

    showThinking :: OneShot.ThinkingOutput -> String
    showThinking OneShot.ThinkingNone = "none"
    showThinking OneShot.ThinkingStdout = "stdout"
    showThinking OneShot.ThinkingStderr = "stderr"

parseOneShotOptions :: Parser OneShotCmd.OneShotOptions
parseOneShotOptions =
    OneShotCmd.OneShotOptions
        <$> optional
            ( strOption
                ( long "session-file"
                    <> metavar "SESSIONFILE"
                    <> help "extra session-file to resume/store"
                )
            )
        <*> parseThinkingOption
        <*> parsePromptScriptInput

parseEchoPromptOptions :: Parser EchoPromptCmd.EchoPromptOptions
parseEchoPromptOptions =
    EchoPromptCmd.EchoPromptOptions
        <$> optional
            ( strOption
                ( long "session-file"
                    <> metavar "SESSIONFILE"
                    <> help "extra session-file for alias input mode"
                )
            )
        <*> parsePromptScriptInput

-- | Parse prompt script input
parsePromptScriptInput :: Parser PromptScript
parsePromptScriptInput =
    let pair = (,) <$> optional parseAliasPrompt <*> many parseRegularDirectives
     in fmap (\(md0, ds) -> maybe ds (: ds) md0) pair
  where
    parseRegularDirectives :: Parser PromptScriptDirective
    parseRegularDirectives =
        asum
            [ promptOption
            , fileOption
            , shellOption
            , smallSeparatorFlag
            , largeSeparatorFlag
            , sessionXSOption
            , sessionSOption
            , sessionMOption
            , sessionLOption
            , sessionXLOption
            ]

parseAliasPrompt :: Parser PromptScriptDirective
parseAliasPrompt =
    AliasPrompt
        <$> strOption
            ( long "alias"
                <> metavar "NAME"
                <> help "Use a predefined prompt alias (e.g., translate, summarize, code-review, explain)"
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

sessionXSOption :: Parser PromptScriptDirective
sessionXSOption =
    SessionContents
        <$> strOption
            ( long "session-xs"
                <> metavar "SESSIONFILE"
                <> help "inject session file content at minimal verbosity (queries/responses only, skips tool-only turns)"
            )
        <*> pure SessionInject.SessionXS

sessionSOption :: Parser PromptScriptDirective
sessionSOption =
    SessionContents
        <$> strOption
            ( long "session-s"
                <> metavar "SESSIONFILE"
                <> help "inject session file content at low verbosity (+thinking, +tool names)"
            )
        <*> pure SessionInject.SessionS

sessionMOption :: Parser PromptScriptDirective
sessionMOption =
    SessionContents
        <$> strOption
            ( long "session-m"
                <> metavar "SESSIONFILE"
                <> help "inject session file content at medium verbosity (+statistics)"
            )
        <*> pure SessionInject.SessionM

sessionLOption :: Parser PromptScriptDirective
sessionLOption =
    SessionContents
        <$> strOption
            ( long "session-l"
                <> metavar "SESSIONFILE"
                <> help "inject session file content at high verbosity (+tool call results)"
            )
        <*> pure SessionInject.SessionL

sessionXLOption :: Parser PromptScriptDirective
sessionXLOption =
    SessionContents
        <$> strOption
            ( long "session-xl"
                <> metavar "SESSIONFILE"
                <> help "inject session file content at maximum verbosity (complete)"
            )
        <*> pure SessionInject.SessionXL

parseMcpServer :: Parser Command
parseMcpServer = pure McpServer

parseVisibilityOption :: String -> String -> Parser PrintVisibility
parseVisibilityOption optName helpText =
    option
        (maybeReader parseVisibility)
        ( long optName
            <> metavar "MODE"
            <> help (helpText <> ": hidden, shown, or elided (default: hidden)")
            <> value Hidden
            <> showDefaultWith showVisibility
        )
  where
    parseVisibility :: String -> Maybe PrintVisibility
    parseVisibility "hidden" = Just Hidden
    parseVisibility "shown" = Just ShownFull
    parseVisibility "elided" = Just (Elided (Lines 10) (Lines 10))
    parseVisibility _ = Nothing

    showVisibility :: PrintVisibility -> String
    showVisibility Hidden = "hidden"
    showVisibility ShownFull = "shown"
    showVisibility (Elided _ _) = "elided"

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
        <*> parseVisibilityOption "show-tool-call-results" "How to display tool call results"
        <*> parseVisibilityOption "show-tool-call-arguments" "How to display tool call arguments"
        <*> optional
            ( option
                auto
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
        <*> switch
            ( long "no-funny-stamp"
                <> help "Skip the ASCII art logo stamp in the header (default: show logo)"
            )

parseInitializeCommand :: Parser Command
parseInitializeCommand = pure Initialize

parseSelfDescribeCommand :: ArgParserArgs -> Parser Command
parseSelfDescribeCommand argArgs = SelfDescribe <$> parseSelfDescribeOptions argArgs

parseSelfDescribeOptions :: ArgParserArgs -> Parser SelfDescribeCmd.SelfDescribeOptions
parseSelfDescribeOptions argArgs =
    SelfDescribeCmd.SelfDescribeOptions
        <$> optional
            ( strOption
                ( long "self-describe-slug"
                    <> metavar "NAME"
                    <> help "The name/slug field for self-describe output"
                    <> maybe mempty value argArgs.defaultSelfDescribeSlug
                )
            )
        <*> optional
            ( strOption
                ( long "self-describe-description"
                    <> metavar "DESC"
                    <> help "The description/announce for self-describe output"
                    <> maybe mempty value argArgs.defaultSelfDescribeDescription
                )
            )

-- | Parse the describe-tool command
parseDescribeToolCommand :: Parser Command
parseDescribeToolCommand = DescribeTool <$> parseDescribeToolOptions

parseDescribeToolOptions :: Parser DescribeToolCmd.DescribeToolOptions
parseDescribeToolOptions =
    DescribeToolCmd.DescribeToolOptions
        <$> strArgument
            ( metavar "TOOLPATH"
                <> help "Path to the tool script to describe"
            )
        <*> ( option
                (maybeReader parseFormat)
                ( long "format"
                    <> short 'f'
                    <> metavar "FORMAT"
                    <> help "Output format: json or pretty (default: json)"
                    <> value DescribeToolCmd.FormatJson
                    <> showDefaultWith showFormat
                )
            )
        <*> switch
            ( long "check-only"
                <> help "Only check validity, don't output full description"
            )
  where
    parseFormat "json" = Just DescribeToolCmd.FormatJson
    parseFormat "pretty" = Just DescribeToolCmd.FormatPretty
    parseFormat _ = Nothing
    showFormat DescribeToolCmd.FormatJson = "json"
    showFormat DescribeToolCmd.FormatPretty = "pretty"

parsePathsCommand :: Parser Command
parsePathsCommand = Paths <$> parsePathsOptions

parsePathsOptions :: Parser PathsCmd.PathsOptions
parsePathsOptions =
    PathsCmd.PathsOptions
        <$> ( flag
                PathsCmd.PathsOutputHuman
                PathsCmd.PathsOutputJson
                ( long "json"
                    <> help "Output in JSON format"
                )
            )

parseSessionEditCommand :: Parser Command
parseSessionEditCommand = SessionEdit <$> parseSessionEditOptions

parseSessionEditOptions :: Parser SessionEditCmd.SessionEditOptions
parseSessionEditOptions = SessionEditCmd.SessionEditOptions <$> many parseSessionEditOp

parseSessionEditOp :: Parser SessionEditCmd.SessionEditOp
parseSessionEditOp = asum [parseTake, parseTakeTail, parseDrop, parseDropTail, parseCensorToolCalls, parseCensorThinking]
  where
    parseTake = flag' () (long "take" <> help "Take first N turns (use --count N)") *> (SessionEditCmd.SessionEditTake <$> parseCountOption)
    parseTakeTail = flag' () (long "take-tail" <> help "Take last N turns (use --count N)") *> (SessionEditCmd.SessionEditTakeTail <$> parseCountOption)
    parseDrop = flag' () (long "drop" <> help "Drop first N turns (use --count N)") *> (SessionEditCmd.SessionEditDrop <$> parseCountOption)
    parseDropTail = flag' () (long "drop-tail" <> help "Drop last N turns (use --count N)") *> (SessionEditCmd.SessionEditDropTail <$> parseCountOption)
    parseCensorToolCalls = flag' SessionEditCmd.SessionEditCensorToolCalls (long "censor-tool-calls" <> help "Remove all tool calls from the session")
    parseCensorThinking = flag' SessionEditCmd.SessionEditCensorThinking (long "censor-thinking" <> help "Remove all thinking content from the session")
    parseCountOption = option auto (long "count" <> short 'n' <> metavar "N" <> help "Number of turns" <> value 1 <> showDefault)

-- | Parse the session-index command
parseSessionIndexCommand :: SessionStore.SessionStore -> Parser Command
parseSessionIndexCommand sessionStore = SessionIndex <$> parseSessionIndexOptions sessionStore

parseSessionIndexOptions :: SessionStore.SessionStore -> Parser SessionIndexCmd.SessionIndexOptions
parseSessionIndexOptions sessionStore =
    SessionIndexCmd.SessionIndexOptions
        <$> parseIndexOperation
        <*> optional (strOption (long "db-path" <> metavar "PATH" <> help "Path to search index database"))
        <*> pure sessionStore
        <*> switch (long "include-tool-outputs" <> help "Include tool outputs in the index")

parseIndexOperation :: Parser IndexOperation
parseIndexOperation =
    asum
        [ flag' IndexBuild (long "build" <> help "Build the search index from scratch")
        , flag' IndexUpdate (long "update" <> help "Incrementally update the search index")
        , flag' IndexStatus (long "status" <> help "Show index status")
        , flag' IndexClean (long "clean" <> help "Remove the search index")
        , pure IndexStatus -- Default to status
        ]

-- | Parse the session-search command
parseSessionSearchCommand :: SessionStore.SessionStore -> Parser Command
parseSessionSearchCommand sessionStore = SessionSearch <$> parseSessionSearchOptions sessionStore

parseSessionSearchOptions :: SessionStore.SessionStore -> Parser SessionSearchCmd.SessionSearchOptions
parseSessionSearchOptions sessionStore =
    SessionSearchCmd.SessionSearchOptions
        <$> strArgument (metavar "QUERY" <> help "Search query text")
        <*> optional (strOption (long "db-path" <> metavar "PATH" <> help "Path to search index database"))
        <*> pure sessionStore
        <*> optional parseDateFilter
        <*> many (strOption (long "tool" <> metavar "TOOL" <> help "Filter by tool name (can be specified multiple times)"))
        <*> optional (strOption (long "agent" <> metavar "AGENT" <> help "Filter by agent slug"))
        <*> switch (long "include-tool-outputs" <> help "Include tool outputs in search")
        <*> switch (long "json" <> help "Output results as JSON")
        <*> option auto (long "preview" <> metavar "N" <> help "Show N lines of context" <> value 0 <> showDefault)
        <*> optional (option auto (long "limit" <> metavar "N" <> help "Limit to N results"))
        <*> switch (long "auto" <> help "Auto-update index if stale before searching")

parseDateFilter :: Parser DateFilter
parseDateFilter =
    asum
        [ AfterDate <$> option (maybeReader parseDate) (long "after" <> metavar "DATE" <> help "Filter sessions after date (YYYY-MM-DD)")
        , BeforeDate <$> option (maybeReader parseDate) (long "before" <> metavar "DATE" <> help "Filter sessions before date (YYYY-MM-DD)")
        ]

parseCowsayCommand :: Parser Command
parseCowsayCommand = Cowsay <$> parseCowsayOptions

parseCowsayOptions :: Parser CowsayCmd.CowsayOptions
parseCowsayOptions =
    CowsayCmd.CowsayOptions
        <$> optional
            ( strArgument
                ( metavar "MESSAGE"
                    <> help "Message to display (reads from stdin if not provided)"
                )
            )
        <*> option
            auto
            ( long "width"
                <> short 'W'
                <> metavar "WIDTH"
                <> help "Maximum width of the speech bubble"
                <> value 40
                <> showDefault
            )

-- | Parse the spec command
parseSpecCommand :: Parser Command
parseSpecCommand = Spec <$> parseSpecOptions

parseSpecOptions :: Parser SpecCmd.SpecOptions
parseSpecOptions =
    SpecCmd.SpecOptions
        <$> subparser
            ( command
                "bash-tools"
                ( info
                    (pure SpecCmd.BashToolsSpec)
                    (progDesc "Display the bash-tools specification documentation")
                )
            )

-- | Parse the new command
parseNewCommand :: Parser Command
parseNewCommand = New <$> parseNewOptions

-- | Help footer for the new command listing available presets
newCommandFooter :: String
newCommandFooter =
    "Available presets for 'new agent': "
        ++ NewCmd.formatPresetListHelp
        ++ ". The preset configures the provider URL, default model, and API key reference."

parseNewOptions :: Parser NewCmd.NewOptions
parseNewOptions =
    NewCmd.NewOptions
        <$> subparser
            ( command
                "agent"
                (info parseNewAgentCommand (progDesc "Create a new agent configuration"))
                <> command
                    "tool"
                    (info parseNewToolCommand (progDesc "Create a new tool script"))
            )
        <*> switch
            ( long "force"
                <> short 'f'
                <> help "Overwrite existing files"
            )

parseNewAgentCommand :: Parser NewCmd.NewCommand
parseNewAgentCommand =
    NewCmd.NewAgent <$> parseNewAgentOptions

parseNewAgentOptions :: Parser NewCmd.NewAgentOptions
parseNewAgentOptions =
    NewCmd.NewAgentOptions
        <$> strArgument
            ( metavar "SLUG"
                <> help "Unique identifier for the agent"
            )
        <*> strArgument
            ( metavar "FILE"
                <> help "Path where agent configuration will be written"
            )
        <*> optional
            ( strArgument
                ( metavar "MODEL"
                    <> help "Model name (e.g., gpt-4o, mistral-large)"
                )
            )
        <*> strOption
            ( long "preset"
                <> short 'p'
                <> metavar "PRESET"
                <> help ("Provider preset (" ++ NewCmd.formatPresetListHelp ++ ")")
                <> value "openai"
                <> showDefault
            )

parseNewToolCommand :: Parser NewCmd.NewCommand
parseNewToolCommand =
    NewCmd.NewTool <$> parseNewToolOptions

{- | Parse the new tool command options
Format: agents-exe new tool <slug> <language> <file>
-}
parseNewToolOptions :: Parser NewCmd.NewToolOptions
parseNewToolOptions =
    NewCmd.NewToolOptions
        <$> strArgument
            ( metavar "SLUG"
                <> help "Unique identifier for the tool (used in describe output)"
            )
        <*> argument
            parseLanguage
            ( metavar "LANGUAGE"
                <> help "Programming language (bash, python, haskell, node)"
            )
        <*> strArgument
            ( metavar "FILE"
                <> help "Path where tool script will be written (without extension)"
            )
  where
    parseLanguage :: ReadM NewCmd.ToolLanguage
    parseLanguage = eitherReader $ \lang ->
        case Map.lookup (Text.toLower $ Text.pack lang) NewCmd.supportedLanguages of
            Just l -> Right l
            Nothing -> Left $ "Unknown language: " <> lang <> ". Supported: bash, python, haskell, node"

-- | Parse the tool-call command
parseToolCallCommand :: Parser Command
parseToolCallCommand = ToolCall <$> parseToolCallOptions

parseToolCallOptions :: Parser ToolCallCmd.ToolCallOptions
parseToolCallOptions =
    ToolCallCmd.ToolCallOptions
        <$> strArgument
            ( metavar "TOOLNAME"
                <> help "Name of the tool to call"
            )
        <*> optional
            ( strOption
                ( long "log-file"
                    <> short 'l'
                    <> metavar "LOGFILE"
                    <> help "Optional log file for tracing tool execution"
                )
            )

parseProgOptions :: ArgParserArgs -> Parser Prog
parseProgOptions argparserargs =
    Prog
        <$> pure argparserargs.configdir
        <*> strOption
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
        <*> optional
            ( strOption
                ( long "agent"
                    <> short 'a'
                    <> metavar "SLUG"
                    <> help "Select agent by slug instead of file path"
                )
            )
        <*> pure argparserargs.argPromptAliases
        <*> hsubparser
            ( command "check" (info parseCheckCommand (progDesc "Validate agent configurations and optionally dump tool schemas"))
                <> command "check-tool-call" (info parseCheckToolCallCommand (progDesc "Validate a tool call payload against a tool schema (reads JSON from stdin)"))
                <> command "list-tool-calls" (info parseListToolCallsCommand (progDesc "List all tool calls from a session file"))
                <> command "replay-tool-call" (info parseReplayToolCallCommand (progDesc "Replay a tool call from a session file, validating and optionally executing"))
                <> command "tui" (info parseTuiChatCommand (idm))
                <> command "run" (info parseOneShotTextualCommand (idm))
                <> command "echo-prompt" (info parseEchoPromptCommand (idm))
                <> command "describe" (info (parseSelfDescribeCommand argparserargs) (idm))
                <> command "describe-tool" (info parseDescribeToolCommand (progDesc "Validate and describe a tool script without agent context"))
                <> command "init" (info parseInitializeCommand (idm))
                <> command "mcp-server" (info parseMcpServer (idm))
                <> command "session-print" (info parseSessionPrintCommand (progDesc "Print a session file in markdown format"))
                <> command "session-edit" (info parseSessionEditCommand (progDesc "Edit a session file (reads JSON from STDIN, writes JSON to STDOUT)"))
                <> command "session-index" (info (parseSessionIndexCommand sessionStore) (progDesc "Manage the session search index"))
                <> command "session-search" (info (parseSessionSearchCommand sessionStore) (progDesc "Search session files with fuzzy matching"))
                <> command
                    "paths"
                    ( info
                        parsePathsCommand
                        (progDesc "Show important configuration paths")
                    )
                <> command
                    "cowsay"
                    ( info
                        parseCowsayCommand
                        (progDesc "Display a message with the agents-exe mascot")
                    )
                <> command
                    "spec"
                    ( info
                        parseSpecCommand
                        (progDesc "Display specification documentation (e.g., bash-tools)")
                    )
                <> command
                    "new"
                    ( info
                        parseNewCommand
                        ( progDesc "Create new agent or tool scaffolding"
                            <> footer newCommandFooter
                        )
                    )
                <> command
                    "tool-call"
                    ( info
                        parseToolCallCommand
                        (progDesc "Call a tool from the first loaded agent with JSON payload from stdin")
                    )
            )
  where
    -- Session store for session-index and session-search commands
    sessionStore = maybe SessionStore.defaultSessionStore SessionStore.mkSessionStore argparserargs.defaultLogSesionsJsonPrefix

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

        -- Resolve agent files based on selected slug
        resolvedAgentFiles <- resolveAgentFiles pargs.agentFiles pargs.selectedAgentSlug

        case resolvedAgentFiles of
            Left err -> do
                Text.hPutStrLn stderr err
                exitFailure
            Right agentFiles' ->
                runCommand pargs baseTracer sessionStore agentFiles'

-- | Resolve agent files based on optional slug selection
resolveAgentFiles :: [FilePath] -> Maybe Text -> IO (Either Text [FilePath])
resolveAgentFiles files Nothing = pure $ Right files
resolveAgentFiles files (Just agentSlug) = do
    -- Load all agents to find matching slug
    agentsWithFiles <- mapM loadAgentWithFile files
    case find (\(_, agent) -> agentSlug == agent.slug) agentsWithFiles of
        Just (file, _) -> pure $ Right [file]
        Nothing -> do
            -- Build error message with available slugs
            let availableSlugs = map (\(f, a) -> (a.slug, f)) agentsWithFiles
            pure $ Left $ formatSlugNotFoundError agentSlug availableSlugs
  where
    loadAgentWithFile :: FilePath -> IO (FilePath, Agent)
    loadAgentWithFile file = do
        result <- Aeson.eitherDecodeFileStrict' file
        case result of
            Left err -> error $ "Failed to parse agent file " ++ file ++ ": " ++ err
            Right (AgentDescription agent) -> pure (file, agent)

    formatSlugNotFoundError :: Text -> [(Text, FilePath)] -> Text
    formatSlugNotFoundError targetSlug available =
        Text.unlines $
            [ "Error: Agent '" <> targetSlug <> "' not found."
            , ""
            , "Available agents:"
            ]
                ++ map (\(s, f) -> "  - " <> s <> " (" <> Text.pack f <> ")") available

-- | Run the selected command
runCommand :: Prog -> Prod.Tracer IO Trace -> SessionStore.SessionStore -> [FilePath] -> IO ()
runCommand pargs baseTracer sessionStore files =
    case pargs.mainCommand of
        Check checkOpts ->
            CheckCmd.handleCheck (Prod.contramap CheckCmdTrace baseTracer) checkOpts pargs.apiKeysFile files
        CheckToolCall opts ->
            CheckToolCallCmd.handleCheckToolCall Prod.silent opts
        ListToolCalls opts ->
            ReplayToolCallCmd.handleListToolCalls opts
        ReplayToolCall opts ->
            ReplayToolCallCmd.handleReplayToolCall Prod.silent opts
        TerminalUI _ ->
            TUICmd.handleTUI (Prod.contramap TUICmdTrace baseTracer) sessionStore pargs.apiKeysFile files
        EchoPrompt opts ->
            EchoPromptCmd.handleEchoPrompt pargs.progPromptAliases opts
        OneShot opts ->
            OneShotCmd.handleOneShot (Prod.contramap OneShotCmdTrace baseTracer) sessionStore pargs.apiKeysFile files pargs.progPromptAliases opts
        SelfDescribe opts ->
            SelfDescribeCmd.handleSelfDescribe opts pargs.apiKeysFile
        DescribeTool opts ->
            DescribeToolCmd.handleDescribeTool Prod.silent opts
        Initialize ->
            InitializeCmd.handleInitialize pargs.apiKeysFile files
        McpServer ->
            McpServerCmd.handleMcpServer (Prod.contramap McpServerCmdTrace baseTracer) sessionStore pargs.apiKeysFile files
        SessionPrint opts ->
            SessionPrint.handleSessionPrint opts
        SessionEdit opts ->
            SessionEditCmd.handleSessionEdit opts
        SessionIndex opts ->
            SessionIndexCmd.handleSessionIndex opts
        SessionSearch opts ->
            SessionSearchCmd.handleSessionSearch opts
        Paths opts ->
            PathsCmd.handlePaths opts pargs.configDir files pargs.apiKeysFile pargs.sessionsJsonPrefix
        Cowsay opts ->
            CowsayCmd.handleCowsay opts
        Spec opts ->
            SpecCmd.handleSpec opts
        New opts ->
            NewCmd.handleNew opts
        ToolCall opts ->
            ToolCallCmd.handleToolCall (Prod.contramap ToolCallTrace baseTracer) opts pargs.apiKeysFile files

-- | Create HTTP JSON tracer
makeHttpJsonTrace :: (Aeson.ToJSON a) => Prod.Tracer IO HttpClient.Trace -> Text -> IO (Prod.Tracer IO a)
makeHttpJsonTrace baseTracer url = do
    rt <- HttpLogger.Runtime <$> HttpClient.newRuntime HttpClient.NoToken <*> pure url
    pure $ HttpLogger.httpTracer rt baseTracer

-------------------------------------------------------------------------------
-- Utility Functions
-------------------------------------------------------------------------------

maybeToEither :: Maybe a -> Either () a
maybeToEither Nothing = Left ()
maybeToEither (Just v) = Right v

-- | Parse a date string in YYYY-MM-DD format
parseDate :: String -> Maybe UTCTime
parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%d"

