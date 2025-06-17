{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Agents.TUI where

import Brick
import Brick.BChan (BChan, newBChan, writeBChan)
import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Monad (forever, void)
import Data.Text (Text)
import Prod.Tracer (Tracer (..))

import System.Agents.Agent
import System.Agents.Dialogues (LoadedAgent (..))
import qualified System.Agents.FileLoader as FileLoader
import qualified System.Agents.Runtime as Runtime
import System.Agents.TUI.Event
import System.Agents.TUI.Handler (tui_appHandleEvent, tui_appStartEvent)
import System.Agents.TUI.Render (tui_appAttrMap, tui_appChooseCursor, tui_appDraw)
import System.Agents.TUI.State

runMultiAgents :: BChan AppEvent -> [LoadedAgent] -> IO ()
runMultiAgents _ [] = putStrLn "no agents loaded"
runMultiAgents bChan agents = do
    st0 <- newTuiState agents
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

mainMultiAgents2 :: BChan AppEvent -> Int -> [Props] -> [LoadedAgent] -> IO ()
mainMultiAgents2 bChan idx (props : xs) agents = do
    withAgentRuntime props go
  where
    go (Initialized ai) = do
        case ai.agentDescription of
            (FileLoader.Unspecified _) -> do
                print ("cannot load an agent with unspecified description" :: Text)
            (FileLoader.OpenAIAgentDescription oai) -> do
                let rt = Runtime.addTracer ai.agentRuntime (traceInChan bChan)
                tools <- Runtime.agentTools rt
                let la = LoadedAgent rt tools oai
                mainMultiAgents2 bChan (succ idx) xs (la : agents)
    go _ = do
        print ("failed to initialize" :: Text)
mainMultiAgents2 bChan _ [] agents = do
    runMultiAgents bChan agents

mainMultiAgents :: [Props] -> IO ()
mainMultiAgents xs = do
    bChan <- newBChan 10
    _ <- publishHeartBeats bChan
    mainMultiAgents2 bChan 0 xs []

traceInChan :: BChan AppEvent -> Tracer IO Runtime.Trace
traceInChan bChan = Tracer (writeBChan bChan . AppEvent_AgentTrace)

publishHeartBeats :: BChan AppEvent -> IO ThreadId
publishHeartBeats bChan = do
    forkIO $ forever $ do
        let oneSec = 1000000
        threadDelay oneSec
        writeBChan bChan AppEvent_Heartbeat
