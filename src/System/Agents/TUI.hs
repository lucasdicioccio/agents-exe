{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Agents.TUI where

import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM (atomically)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as LByteString
import Data.Foldable (traverse_)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.Vector as Vector
import GHC.IO.Handle (Handle)
import Prod.Tracer (Tracer (..), silent)
import System.IO (stderr, stdout)

import qualified System.Agents.Agent as Agent
import System.Agents.Base (newConversationId)
import System.Agents.Conversation
import qualified System.Agents.FileLoader as FileLoader
import qualified System.Agents.LLMs.OpenAI as OpenAI
import qualified System.Agents.Party as Party
import qualified System.Agents.Tools as Tools
import qualified System.Agents.Tools.Bash as Tools
import qualified System.Agents.Tools.IO as Tools

import Brick
import Brick.Focus (FocusRing, focusGetCurrent, focusNext, focusPrev, focusRing)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Edit
import Brick.Widgets.List
import Control.Lens hiding (zoom) -- (makeLenses, to, use, (%=))
import qualified Graphics.Vty as Vty

import System.Agents.TUI.Handler (tui_appHandleEvent, tui_appStartEvent)
import System.Agents.TUI.Render (tui_appAttrMap, tui_appChooseCursor, tui_appDraw)
import System.Agents.TUI.State

runMultiAgents :: [LoadedAgent] -> IO ()
runMultiAgents [] = print "no agents loaded"
runMultiAgents agents = do
    st0 <- newCliState agents
    let app =
            App
                { appDraw = tui_appDraw
                , appChooseCursor = tui_appChooseCursor
                , appHandleEvent = tui_appHandleEvent
                , appStartEvent = tui_appStartEvent
                , appAttrMap = tui_appAttrMap
                }
    void $ defaultMain app st0

-------------------------------------------------------------------------------

mainMultiAgents2 :: Int -> [Props] -> [LoadedAgent] -> IO ()
mainMultiAgents2 idx (props : xs) agents = do
    withAgentRuntime props go
  where
    go (Initialized ai) = do
        case ai.agentDescription of
            (FileLoader.Unspecified _) -> do
                print ("cannot load an agent with unspecified description" :: Text)
            (FileLoader.OpenAIAgentDescription oai) -> do
                tools <- Agent.agentTools ai.agentRuntime
                mainMultiAgents2 (succ idx) xs ((ai.agentRuntime, tools, oai) : agents)
    go _ = do
        print ("failed to initialize" :: Text)
mainMultiAgents2 _ [] agents = do
    runMultiAgents agents

mainMultiAgents :: [Props] -> IO ()
mainMultiAgents xs = mainMultiAgents2 0 xs []
