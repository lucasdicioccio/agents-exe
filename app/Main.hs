{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Aeson as Aeson
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Prod.Tracer as Prod
import System.Agents.CLI.Base (makeShowLogFileTracer)
import qualified System.Agents.CLI.InitProject as InitProject
import qualified System.Agents.FileLoader.Base as Agents
import qualified System.Agents.LLMs.OpenAI as OpenAI
import qualified System.Agents.MCP.Server as McpServer
import qualified System.Agents.Prompt as Prompt
import qualified System.Agents.Tools.Bash as Bash
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)

import Options.Applicative

data Prog = Prog
    { apiKeysFile :: FilePath
    , logFile :: FilePath
    , agentFile :: FilePath
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
        <*> strOption
            ( long "agent-file"
                <> metavar "AGENTFILE"
                <> help "main agent description file"
                <> showDefault
                <> value "agent.json"
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
        baseTracer <- makeShowLogFileTracer args.logFile
        case args.mainCommand of
            Check ->
                Prompt.mainPrintAgent $
                    Prompt.Props
                        { Prompt.apiKeysFile = args.apiKeysFile
                        , Prompt.rawLogFile = args.logFile
                        , Prompt.mainAgentFile = args.agentFile
                        , Prompt.helperAgentsDir = args.agentsDir
                        , Prompt.interactiveTracer =
                            Prod.traceBoth baseTracer Prompt.traceUsefulPromptStdout
                        }
            InteractiveCommandLine ->
                Prompt.mainInteractiveAgent $
                    Prompt.Props
                        { Prompt.apiKeysFile = args.apiKeysFile
                        , Prompt.rawLogFile = args.logFile
                        , Prompt.mainAgentFile = args.agentFile
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
                prompt <-
                    if "@" `List.isPrefixOf` opts.fileOrPromptArg
                        then Text.readFile (drop 1 opts.fileOrPromptArg)
                        else pure $ Text.pack opts.fileOrPromptArg
                let oneShot = flip Prompt.mainOneShotText
                oneShot prompt $
                    Prompt.Props
                        { Prompt.apiKeysFile = args.apiKeysFile
                        , Prompt.rawLogFile = args.logFile
                        , Prompt.mainAgentFile = args.agentFile
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
            McpServer ->
                McpServer.mainAgentServer $
                    Prompt.Props
                        { Prompt.apiKeysFile = args.apiKeysFile
                        , Prompt.rawLogFile = args.logFile
                        , Prompt.mainAgentFile = args.agentFile
                        , Prompt.helperAgentsDir = args.agentsDir
                        , Prompt.interactiveTracer =
                            Prod.traceBoth
                                baseTracer
                                ( Prod.traceBoth
                                    Prompt.traceUsefulPromptStderr
                                    (Prompt.traceWaitingOpenAIRateLimits (OpenAI.ApiLimits 100 10000) (\_ -> pure ()))
                                )
                        }
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
                        InitProject.initOpenAIAgent o args.agentFile
                        InitProject.initAgentsDir args.agentsDir
                        InitProject.initOpenAIKeys args.apiKeysFile
