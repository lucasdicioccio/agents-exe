{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Agents.CLI where

import qualified Control.Concurrent.STM as STM
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LByteString
import Data.Foldable (traverse_)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import System.Console.Haskeline

import System.Agents.Agent
import System.Agents.Base (newConversationId)
import qualified System.Agents.Conversation as Conversation
import qualified System.Agents.FileLoader as FileLoader
import qualified System.Agents.Runtime as Runtime
import qualified System.Agents.Tools as Tools
import qualified System.Agents.Tools.Bash as Tools
import qualified System.Agents.Tools.IO as Tools

mainInteractiveAgent :: Props -> IO ()
mainInteractiveAgent props = do
    withAgentRuntime props $ \x -> do
        case x of
            LoadingErrors errs -> traverse_ print errs
            OtherErrors errs -> traverse_ print errs
            Initialized ai ->
                runMainAgent ai
  where
    runMainAgent :: AgentInfo -> IO ()
    runMainAgent ai = do
        runInputT (settings ai) (oneAgentLoop ai)

    settings :: AgentInfo -> Settings IO
    settings ai =
        (defaultSettings :: Settings IO)
            { historyFile = Just "agents-exe.hist"
            , complete = handleComplete ai
            }

    handleComplete :: AgentInfo -> CompletionFunc IO
    handleComplete ai (chunk1r, chunk2) =
        pure (chunk1r, [Completion (drop len w) h True | (w, h) <- commands ai, chunk1 `List.isPrefixOf` w])
      where
        chunk1 = reverse chunk1r
        len = length chunk1

    commands :: AgentInfo -> [(String, String)]
    commands ai =
        Maybe.catMaybes
            [ Just ("quit", "quit (quit this program)")
            , Just ("help", "help (display some help)")
            , Just ("info", "show current agent infos")
            , Just ("list agents", "agents")
            , Just ("list tools", "tools")
            , Just ("reload", "reload (refresh tools)")
            , let z = atSlug ai.agentDescription in (,) <$> z <*> z
            ]
      where
        atSlug :: FileLoader.AgentDescription -> Maybe String
        atSlug (FileLoader.OpenAIAgentDescription desc) =
            Just $ Text.unpack ("@" <> FileLoader.slug desc)
        atSlug (FileLoader.Unspecified _) =
            Nothing

    oneAgentLoop :: AgentInfo -> InputT IO ()
    oneAgentLoop ai = do
        minput <- getInputLine "% "
        case minput of
            Nothing -> return ()
            Just "quit" -> return ()
            Just "reload" -> do
                liftIO $ traverse_ Runtime.triggerRefreshTools (ai.agentRuntime : ai.agentSiblingRuntimes)
                oneAgentLoop ai
            Just "info" -> do
                outputStrLn $ show ai.agentDescription
                oneAgentLoop ai
            Just "list agents" -> do
                traverse_ (outputStrLn . show) ai.agentSibling.agents
                oneAgentLoop ai
            Just "list tools" -> do
                traverse_ printAgentTools (ai.agentRuntime : ai.agentSiblingRuntimes)
                oneAgentLoop ai
            Just ('@' : input) -> do
                let (atSlug, prompt) = List.break (== ' ') input
                conv <- liftIO $ Conversation.converse ai.agentRuntime (Text.pack $ drop 1 prompt)
                liftIO $ STM.atomically $ conv.prompt Nothing
                liftIO $ Conversation.wait conv >>= print
                oneAgentLoop ai
            Just (input) -> do
                outputStrLn $
                    unlines
                        ( "Unrecognized input: "
                            : map fst (commands ai)
                        )
                oneAgentLoop ai

    printAgentTools :: Runtime.Runtime -> InputT IO ()
    printAgentTools rt = do
        registry <- liftIO rt.agentTools
        outputStrLn $ Text.unpack (renderToolRegistry registry)

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

{-
    agentFunctions ask =
        Runtime.AgentFunctions
            (fmap queryOrNothing ask)
            (\_hist -> pure ())
            (\err -> putStrLn $ unlines ["parse error", err])
            (\_ -> putStrLn "done")

    queryOrNothing :: Text -> Maybe Text
    queryOrNothing "" = Nothing
    queryOrNothing t = Just t
-}
