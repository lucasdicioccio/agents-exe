{- | Provides a runtime value capable to load and reload a mcp list of tools.
Note that reloads are asynchronous.
-}
module System.Agents.Tools.McpToolbox where

import Control.Concurrent.Async (Async, async)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TBMChan (TBMChan, newTBMChanIO, readTBMChan, writeTBMChan)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, writeTVar)
import qualified Data.Maybe as Maybe
import Prod.Tracer (Tracer (..), silent)
import System.Process (CreateProcess)

import System.Agents.MCP.Client (LoopProps (..), LoopTrace (..))
import qualified System.Agents.MCP.Client as McpClient
import qualified System.Agents.MCP.Client.Runtime as McpClient

-------------------------------------------------------------------------------
data Trace
    = McpClientLoopTrace !McpClient.LoopTrace
    deriving (Show)

data BackgroundMcpTools = BackgroundMcpTools
    { job :: Async ()
    , enqueueCall :: McpClient.ToolCall -> STM ()
    , toolsList :: TVar [McpClient.ListToolsResultRsp]
    }

initializeMcpToolbox ::
    Tracer IO Trace ->
    CreateProcess ->
    IO BackgroundMcpTools
initializeMcpToolbox tracer proc = do
    -- tool calls
    chan <- newTBMChanIO 30
    let nextToolCall = atomically $ readTBMChan chan
    let enqueueCall = writeTBMChan chan

    -- tool discovery
    discoveredTools <- newTVarIO []
    let toolsListTracer = storeToolsInDiscoveredValues discoveredTools
    -- tracers
    let rtTracer = silent
    let clientTracer = silent
    let loopTracer = toolsListTracer

    -- wire things together
    mcpRt <- McpClient.initRuntime rtTracer proc
    let props = LoopProps loopTracer nextToolCall
    job <- async (McpClient.runClient clientTracer mcpRt (McpClient.defaultLoop props))
    pure $ BackgroundMcpTools job enqueueCall discoveredTools

storeToolsInDiscoveredValues :: TVar [McpClient.ListToolsResultRsp] -> Tracer IO LoopTrace
storeToolsInDiscoveredValues list = Tracer f
  where
    f :: LoopTrace -> IO ()
    f (StartToolCall _ _) = pure ()
    f (EndToolCall _ _ _) = pure ()
    f (ExitingToolCallLoop) =
        atomically (writeTVar list [])
    f (ToolsRefreshed mitems) =
        let
            f (Just (Right rsp)) = Just rsp
            f _ = Nothing
            items = Maybe.mapMaybe f mitems
         in
            atomically (writeTVar list items)
