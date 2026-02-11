{- | Provides a runtime value capable to load and reload a mcp list of tools.
Note that reloads are asynchronous.
-}
module System.Agents.Tools.McpToolbox where

import Control.Concurrent.Async (Async, async)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBMChan (newTBMChanIO, readTBMChan, writeTBMChan)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, writeTVar)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Network.JSONRPC as Rpc
import Prod.Tracer (Tracer (..), contramap, traceBoth)
import System.Process (CreateProcess)

import qualified System.Agents.MCP.Base as Mcp
import System.Agents.MCP.Client (LoopProps (..), LoopTrace (..))
import qualified System.Agents.MCP.Client as McpClient
import qualified System.Agents.MCP.Client.Runtime as McpClient

-------------------------------------------------------------------------------
newtype ToolDescription = ToolDescription {getToolDescription :: Mcp.Tool}
    deriving (Show)

data Trace
    = McpClientRunTrace !McpClient.RunTrace
    | McpClientClientTrace !McpClient.ClientTrace
    | McpClientLoopTrace !McpClient.LoopTrace
    deriving (Show)

data Toolbox = Toolbox
    { name :: Text
    , job :: Async ()
    , toolsList :: TVar [ToolDescription]
    , callTool :: ToolDescription -> Maybe Aeson.Object -> IO (McpClient.ToolCallResponse)
    }

initializeMcpToolbox ::
    Tracer IO Trace ->
    Text ->
    CreateProcess ->
    IO Toolbox
initializeMcpToolbox ttracer tname proc = do
    -- tool calls
    chan <- newTBMChanIO 30
    let nextToolCall = atomically $ readTBMChan chan
    let doCallTool :: ToolDescription -> Maybe Aeson.Object -> IO McpClient.ToolCallResponse
        doCallTool td param = do
            let tc = McpClient.ToolCall td.getToolDescription.name param
            mbox <- newEmptyMVar
            let done res = do
                    putMVar mbox res
            let fullcall = McpClient.FullToolCall tc done
            atomically $ writeTBMChan chan fullcall
            takeMVar mbox

    -- tool discovery
    discoveredTools <- newTVarIO []
    let toolsListTracer = storeToolsInDiscoveredValues discoveredTools
    -- tracers
    let rtTracer = contramap McpClientRunTrace ttracer
    let clientTracer = contramap McpClientClientTrace ttracer
    let loopTracer = traceBoth toolsListTracer (contramap McpClientLoopTrace ttracer)

    -- wire things together
    mcpRt <- McpClient.initRuntime rtTracer proc
    let props = LoopProps loopTracer nextToolCall
    ajob <- async (McpClient.runClient clientTracer mcpRt (McpClient.defaultLoop props))
    pure $ Toolbox tname ajob discoveredTools doCallTool

storeToolsInDiscoveredValues :: TVar [ToolDescription] -> Tracer IO LoopTrace
storeToolsInDiscoveredValues list = Tracer f
  where
    f :: LoopTrace -> IO ()
    f (StartToolCall _ _) = pure ()
    f (EndToolCall _ _ _) = pure ()
    f (ExitingToolCallLoop) =
        atomically (writeTVar list [])
    f (ToolsRefreshed mitems) =
        let
            g :: Maybe (Either Rpc.ErrorObj McpClient.ListToolsResultRsp) -> [ToolDescription]
            g (Just (Right rsp)) = fmap ToolDescription rsp.getListToolsResult.tools
            g _ = []

            items = mconcat $ map g mitems
         in
            atomically (writeTVar list items)
