{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Agents.TUI where

import Control.Monad (void)
import Data.Text (Text)
import Prod.Tracer (Tracer (..))

import qualified System.Agents.Agent as Agent
import System.Agents.Conversation
import qualified System.Agents.FileLoader as FileLoader

import Brick
import Brick.BChan (BChan, newBChan, writeBChan)

import System.Agents.TUI.Handler (tui_appHandleEvent, tui_appStartEvent)
import System.Agents.TUI.Render (tui_appAttrMap, tui_appChooseCursor, tui_appDraw)
import System.Agents.TUI.State

runMultiAgents :: BChan Agent.Trace -> [LoadedAgent] -> IO ()
runMultiAgents _ [] = putStrLn "no agents loaded"
runMultiAgents bChan agents = do
    st0 <- newCliState agents
    let app =
            App
                { appDraw = tui_appDraw
                , appChooseCursor = tui_appChooseCursor
                , appHandleEvent = tui_appHandleEvent
                , appStartEvent = tui_appStartEvent
                , appAttrMap = tui_appAttrMap
                }
    void $ customMainWithDefaultVty (Just bChan) app st0

-------------------------------------------------------------------------------

mainMultiAgents2 :: BChan Agent.Trace -> Int -> [Props] -> [LoadedAgent] -> IO ()
mainMultiAgents2 bChan idx (props : xs) agents = do
    withAgentRuntime props go
  where
    go (Initialized ai) = do
        case ai.agentDescription of
            (FileLoader.Unspecified _) -> do
                print ("cannot load an agent with unspecified description" :: Text)
            (FileLoader.OpenAIAgentDescription oai) -> do
                let rt = Agent.addTracer ai.agentRuntime (traceInChan bChan)
                tools <- Agent.agentTools rt
                mainMultiAgents2 bChan (succ idx) xs ((rt, tools, oai) : agents)
    go _ = do
        print ("failed to initialize" :: Text)
mainMultiAgents2 bChan _ [] agents = do
    runMultiAgents bChan agents

mainMultiAgents :: [Props] -> IO ()
mainMultiAgents xs = do
    bChan <- newBChan 10
    mainMultiAgents2 bChan 0 xs []

traceInChan :: BChan Agent.Trace -> Tracer IO Agent.Trace
traceInChan bChan = Tracer (writeBChan bChan)
