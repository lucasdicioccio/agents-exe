module System.Agents.TUI.Event where

import qualified System.Agents.Agent as Agent

data AppEvent
    = AppEvent_Heartbeat
    | AppEvent_AgentTrace !Agent.Trace
