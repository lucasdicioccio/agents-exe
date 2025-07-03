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
import System.Agents.Base (Agent (..), AgentId, AgentSlug, ConversationId, PingPongQuery (..))
import qualified System.Agents.CLI as CLI
import System.Agents.CLI.Base (makeShowLogFileTracer)
import qualified System.Agents.CLI.InitProject as InitProject
import System.Agents.CLI.TraceUtils (tracePrintingTextResponses, traceUsefulPromptStderr, traceUsefulPromptStdout)
import qualified System.Agents.HttpClient as HttpClient
import qualified System.Agents.HttpLogger as HttpLogger
import qualified System.Agents.LLMs.OpenAI as LLMTrace
import qualified System.Agents.LLMs.OpenAI as OpenAI
import qualified System.Agents.MCP.Server as McpServer
import qualified System.Agents.OneShot as OneShot
import qualified System.Agents.Runtime.Trace as Runtime
import qualified System.Agents.TUI as TUI
import qualified System.Agents.Tools as ToolsTrace
import qualified System.Agents.Tools.Bash as Bash
import qualified System.Agents.Tools.Bash as ToolsTrace
import qualified System.Agents.Tools.BashToolbox as BashToolbox
import qualified System.Agents.Tools.IO as ToolsTrace
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)

import qualified System.Agents.Memory as Memory

import Options.Applicative

data ArgParserArgs
    = ArgParserArgs
    { configdir :: FilePath
    }

initArgParserArgs :: IO ArgParserArgs
initArgParserArgs = do
    homedir <- getHomeDirectory
    pure $ ArgParserArgs (homedir </> ".config/agents-exe")

data Prog = Prog
    { apiKeysFile :: FilePath
    , logFile :: FilePath
    , logHttp :: Maybe String
    , memoryHttpStore :: Maybe String
    , agentFiles :: [FilePath]
    , mainCommand :: Command
    }

data Command
    = Check
    | InteractiveCommandLine
    | TerminalUI
    | OneShot OneShotOptions
    | SelfDescribe
    | Initialize
    | McpServer

data OneShotOptions
    = OneShotOptions
    { fileOrPromptArgs :: [String]
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

parseTuiChatCommand :: Parser Command
parseTuiChatCommand =
    pure TerminalUI

parseOneShotTextualCommand :: Parser Command
parseOneShotTextualCommand =
    OneShot <$> parseOneShotOptions
  where
    parseOneShotOptions :: Parser OneShotOptions
    parseOneShotOptions =
        OneShotOptions
            <$> fmap
                addDefaultStdinFile
                ( many
                    ( strOption
                        ( long "prompt"
                            <> metavar "PROMPT"
                            <> help "prompt, magical values is @filepath"
                        )
                    )
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

parseProgOptions :: ArgParserArgs -> Parser Prog
parseProgOptions argparserargs =
    Prog
        <$> strOption
            ( long "api-keys"
                <> metavar "AGENTS-KEY"
                <> help "path to json-file containing API keys"
                <> showDefault
                <> value (argparserargs.configdir </> "secret-keys")
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
            (addDefaultSomeAgentFile argparserargs.configdir)
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
                <> command "cli" (info parseCliCommand (idm))
                <> command "tui" (info parseTuiChatCommand (idm))
                <> command "run" (info parseOneShotTextualCommand (idm))
                <> command "describe" (info parseSelfDescribeCommand (idm))
                <> command "init" (info parseInitializeCommand (idm))
                <> command "mcp-server" (info parseMcpServer (idm))
            )

-- | helper to make agent-file entirely optional whilst using the 'many' combinator
addDefaultSomeAgentFile :: FilePath -> [FilePath] -> [FilePath]
addDefaultSomeAgentFile configdir [] = [configdir </> "default" </> "agent.json"]
addDefaultSomeAgentFile _ xs = xs

addDefaultStdinFile :: [FilePath] -> [FilePath]
addDefaultStdinFile [] = ["@/dev/stdin"]
addDefaultStdinFile xs = xs

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
                apiKeys <- Agent.readOpenApiKeysFile args.apiKeysFile
                forM_ args.agentFiles $ \agentFile -> do
                    OneShot.mainPrintAgent $
                        Agent.Props
                            { Agent.apiKeys = apiKeys
                            , Agent.rootAgentFile = agentFile
                            , Agent.interactiveTracer =
                                Prod.traceBoth baseTracer traceUsefulPromptStdout
                            }
            InteractiveCommandLine -> do
                apiKeys <- Agent.readOpenApiKeysFile args.apiKeysFile
                let oneProp agentFile =
                        Agent.Props
                            { Agent.apiKeys = apiKeys
                            , Agent.rootAgentFile = agentFile
                            , Agent.interactiveTracer =
                                Prod.traceBoth
                                    baseTracer
                                    ( Prod.traceBoth
                                        tracePrintingTextResponses
                                        (Agent.traceWaitingOpenAIRateLimits (OpenAI.ApiLimits 100 10000) print)
                                    )
                            }
                CLI.mainInteractiveAgent $ map oneProp args.agentFiles
            TerminalUI -> do
                apiKeys <- Agent.readOpenApiKeysFile args.apiKeysFile
                let oneAgent agentFile =
                        Agent.Props
                            { Agent.apiKeys = apiKeys
                            , Agent.rootAgentFile = agentFile
                            , Agent.interactiveTracer =
                                Prod.traceBoth
                                    baseTracer
                                    (Agent.traceWaitingOpenAIRateLimits (OpenAI.ApiLimits 100 10000) print)
                            }
                TUI.mainMultiAgents (fmap oneAgent args.agentFiles)
            OneShot opts -> do
                apiKeys <- Agent.readOpenApiKeysFile args.apiKeysFile
                forM_ (take 1 args.agentFiles) $ \agentFile -> do
                    promptLines <- traverse interpretPromptArg opts.fileOrPromptArgs
                    let oneShot = flip OneShot.mainOneShotText
                    oneShot (Text.unlines promptLines) $
                        Agent.Props
                            { Agent.apiKeys = apiKeys
                            , Agent.rootAgentFile = agentFile
                            , Agent.interactiveTracer =
                                Prod.traceBoth
                                    baseTracer
                                    traceUsefulPromptStderr
                            }
            SelfDescribe -> do
                apiKeys <- Agent.readOpenApiKeysFile args.apiKeysFile
                Aeson.encodeFile "/dev/stdout" $
                    Bash.ScriptInfo
                        []
                        "self-introspect"
                        "introspect a version of yourself"
            McpServer -> do
                apiKeys <- Agent.readOpenApiKeysFile args.apiKeysFile
                let oneAgent agentFile =
                        Agent.Props
                            { Agent.apiKeys = apiKeys
                            , Agent.rootAgentFile = agentFile
                            , Agent.interactiveTracer =
                                Prod.traceBoth
                                    baseTracer
                                    ( Prod.traceBoth
                                        traceUsefulPromptStderr
                                        (Agent.traceWaitingOpenAIRateLimits (OpenAI.ApiLimits 100 10000) (\_ -> pure ()))
                                    )
                            }
                McpServer.multiAgentsServer (fmap oneAgent args.agentFiles)
            Initialize ->
                let o =
                        Agent
                            { slug = "main-agent"
                            , apiKeyId = "main-key"
                            , flavor = "OpenAIv1"
                            , modelUrl = OpenAI.openAIv1Endpoint.getBaseUrl
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
                            }
                 in do
                        forM_ (take 1 args.agentFiles) $ \agentFile -> do
                            InitProject.initAgentFile o agentFile
                            InitProject.initAgentTooldir o agentFile
                            InitProject.initKeyFile args.apiKeysFile

-------------------------------------------------------------------------------

maybeToEither :: Maybe a -> Either () a
maybeToEither Nothing = Left ()
maybeToEither (Just v) = Right v

extractMemories :: Agent.Trace -> Either () [Memory.MemoryItem]
extractMemories x = case x of
    Agent.AgentTrace tr -> go [] tr
    _ -> Left ()
  where
    parentInfo :: [Runtime.Trace] -> (Maybe AgentSlug, Maybe ConversationId, Maybe AgentId)
    parentInfo ((Runtime.AgentTrace_Memorize pSlug paId pcId _) : _) =
        (Just pSlug, Just pcId, Just paId)
    parentInfo ((Runtime.AgentTrace_Conversation pSlug paId pcId _) : _) =
        (Just pSlug, Just pcId, Just paId)
    parentInfo _ =
        (Nothing, Nothing, Nothing)

    rootConversationId :: [Runtime.Trace] -> Maybe ConversationId
    rootConversationId = rootTrace >=> Runtime.traceConversationId

    rootTrace :: [Runtime.Trace] -> Maybe Runtime.Trace
    rootTrace [] = Nothing
    rootTrace (t : []) = Just t
    rootTrace ts = rootTrace (drop 1 ts)

    go :: [Runtime.Trace] -> Runtime.Trace -> Either () [Memory.MemoryItem]
    go stack (Runtime.AgentTrace_Memorize aSlug aId cId (Runtime.Calling query hist stepId)) =
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
    go stack (Runtime.AgentTrace_Memorize aSlug aId cId (Runtime.GotResponse query hist stepId rsp)) =
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
    go stack (Runtime.AgentTrace_Memorize aSlug aId cId (Runtime.InteractionDone hist stepId)) =
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
                    , Memory.pendingQuery = NoQuery
                    , Memory.parentAgentSlug = pSlug
                    , Memory.parentConversationId = pcId
                    , Memory.parentAgentId = paId
                    }
                ]
    go xs tr@(Runtime.AgentTrace_Conversation _ _ _ (Runtime.ChildrenTrace sub)) =
        go (tr : xs) sub
    go _ _ = Left ()

toJsonTrace :: Agent.Trace -> Maybe Aeson.Value
toJsonTrace x = case x of
    Agent.DataLoadingTrace _ -> Nothing
    Agent.AgentTrace v -> encodeAgentTrace v
    Agent.ConfigLoadedTrace _ -> Nothing
  where
    encodeAgentTrace :: Runtime.Trace -> Maybe Aeson.Value
    encodeAgentTrace tr = do
        baseVal <- encodeBaseTrace tr
        Just $
            Aeson.object
                [ "e"
                    .= Aeson.object
                        [ "slug" .= Runtime.traceAgentSlug tr
                        , "agent-id" .= Runtime.traceAgentId tr
                        , "val" .= baseVal
                        ]
                ]

    encodeBaseTrace :: Runtime.Trace -> Maybe Aeson.Value
    encodeBaseTrace (Runtime.AgentTrace_Loading _ _ tr) =
        encodeBaseTrace_Loading tr
    encodeBaseTrace (Runtime.AgentTrace_Memorize _ _ _ tr) =
        encodeBaseTrace_Memorize tr
    encodeBaseTrace (Runtime.AgentTrace_Conversation _ _ convId tr) = do
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

    encodeBaseTrace_Memorize :: Runtime.MemorizeTrace -> Maybe Aeson.Value
    encodeBaseTrace_Memorize bt =
        case bt of
            (Runtime.Calling _ _ _) ->
                Nothing
            (Runtime.GotResponse _ _ _ _) ->
                Nothing
            (Runtime.InteractionDone _ _) ->
                Nothing

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
            (Runtime.RunToolTrace uuid (ToolsTrace.BashToolsTrace (ToolsTrace.RunCommandStart cmd args))) ->
                Just $
                    Aeson.object
                        [ "x" .= ("tool" :: Text.Text)
                        , "run-id" .= uuid
                        , "flavor" .= ("bash" :: Text.Text)
                        , "action" .= ("start" :: Text.Text)
                        , "cmd" .= cmd
                        , "args" .= args
                        ]
            (Runtime.RunToolTrace uuid (ToolsTrace.BashToolsTrace (ToolsTrace.RunCommandStopped cmd args code _ _))) ->
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
            (Runtime.RunToolTrace uuid (ToolsTrace.IOToolsTrace (ToolsTrace.IOScriptStopped desc input output))) ->
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

makeHttpJsonTrace :: (Aeson.ToJSON a) => Prod.Tracer IO HttpClient.Trace -> Text.Text -> IO (Prod.Tracer IO a)
makeHttpJsonTrace baseTracer url = do
    rt <- HttpLogger.Runtime <$> HttpClient.newRuntime HttpClient.NoToken <*> pure url
    pure $ HttpLogger.httpTracer rt baseTracer

-- | interprets a `--prompt` arg as either a magic file or a chunk of text
interpretPromptArg :: FilePath -> IO Text.Text
interpretPromptArg path =
    if "@" `List.isPrefixOf` path
        then Text.readFile (drop 1 path)
        else pure $ Text.pack path
