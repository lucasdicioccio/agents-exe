{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Agents.CLI where

import qualified Control.Concurrent.STM as STM
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import qualified Data.List as List
import qualified Data.List.Extra as ListExtra
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import System.Console.Haskeline

import System.Agents.AgentTree
import System.Agents.CLI.State
import qualified System.Agents.Conversation as Conversation
import System.Agents.Dialogues
import qualified System.Agents.FileLoader as FileLoader
import qualified System.Agents.Runtime as Runtime

mainInteractiveAgent :: [Props] -> IO ()
mainInteractiveAgent xs =
    mainInteractiveAgent2 [] xs

mainInteractiveAgent2 :: [LoadedAgent] -> [Props] -> IO ()
mainInteractiveAgent2 agents (props : rest) = do
    withAgentTreeRuntime props $ \x -> do
        case x of
            Errors errs -> traverse_ print errs
            Initialized ai -> do
                let oai = ai.agentBase
                -- registry <- liftIO ai.agentRuntime.agentTools
                let loadedAgent = LoadedAgent ai.agentRuntime oai
                mainInteractiveAgent2 (loadedAgent : agents) rest
mainInteractiveAgent2 xs [] =
    runMainCLI $
        CliState
            (NonEmpty.fromList xs)
            []
  where
    runMainCLI :: CliState -> IO ()
    runMainCLI state = do
        runInputT (settings state) (oneAgentLoop state)

    settings :: CliState -> Settings IO
    settings state =
        (defaultSettings :: Settings IO)
            { historyFile = Just "agents-exe.hist"
            , complete = handleComplete state
            }

    handleComplete :: CliState -> CompletionFunc IO
    handleComplete state (chunk1r, _) =
        pure (chunk1r, [Completion (drop len w) h True | (w, h) <- commands state, chunk1 `List.isPrefixOf` w])
      where
        chunk1 = reverse chunk1r
        len = length chunk1

    commands :: CliState -> [(String, String)]
    commands state =
        Maybe.catMaybes
            ( [ Just ("quit", "quit (quit this program)")
              , Just ("help", "help (display some help)")
              , Just ("info", "show current agent infos")
              , Just ("reload", "reload (refresh tools)")
              ]
                <> atCommands
            )
      where
        atCommands :: [Maybe (String, String)]
        atCommands =
            let
                f :: LoadedAgent -> Maybe (String, String)
                f ai = let z = atSlug ai.loadedAgentInfo in Just (z, z)
             in
                NonEmpty.toList (fmap f state.loadedAgents)

    atSlug :: FileLoader.Agent -> String
    atSlug desc =
        Text.unpack ("@" <> FileLoader.slug desc)

    oneAgentLoop :: CliState -> InputT IO ()
    oneAgentLoop state = do
        minput <- getInputLine "% "
        case fmap ListExtra.trim minput of
            Nothing -> return ()
            Just "" -> do
                oneAgentLoop state
            Just "quit" -> return ()
            Just "reload" -> do
                liftIO $ traverse_ (Runtime.triggerRefreshTools . loadedAgentRuntime) state.loadedAgents
                oneAgentLoop state
            Just "info" -> do
                outputStrLn $
                    unlines $
                        NonEmpty.toList $
                            fmap (show . loadedAgentInfo) state.loadedAgents
                oneAgentLoop state
            Just ('@' : input) -> do
                let (searchedSlug, promptChunk) = List.break (== ' ') input
                let promptMessage = Text.pack (drop 1 promptChunk)
                let foundAgent = List.find (\ai -> Text.unpack ai.loadedAgentInfo.slug == searchedSlug) state.loadedAgents
                let foundConversation = foundAgent >>= \ai -> List.find (\conv -> conv.conversingAgent == ai.loadedAgentInfo) state.ongoingConversations
                case (foundConversation, foundAgent) of
                    (Just conv, _) -> do
                        if promptMessage /= ""
                            then do
                                _ <- liftIO $ STM.atomically $ conv.prompt (Just promptMessage)
                                oneAgentLoop state
                            else do
                                _ <- liftIO $ STM.atomically $ conv.prompt Nothing
                                -- liftIO $ Conversation.wait conv >>= print
                                oneAgentLoop (removeConversation state conv)
                    (Nothing, Just ai) -> do
                        conv <- liftIO $ Conversation.converse ai.loadedAgentRuntime promptMessage
                        let ongoingConv = OngoingConversation conv.conversationId ai.loadedAgentInfo Conversation.Executing [] conv.prompt promptMessage
                        oneAgentLoop (insertConversation state ongoingConv)
                    (Nothing, Nothing) -> do
                        outputStrLn $
                            unlines
                                ["No such agent: ", searchedSlug]
                        oneAgentLoop state
            Just _ -> do
                outputStrLn $
                    unlines
                        ( "Unrecognized input. Valid commands: "
                            : map fst (commands state)
                        )
                oneAgentLoop state

{-
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
-}
