module System.Agents.TUI.Event where

import qualified System.Agents.Runtime.Trace as Runtime

data AppEvent
    = AppEvent_Heartbeat
    | AppEvent_AgentTrace !Runtime.Trace
