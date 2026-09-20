{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Core types and data structures for the TUI application.

This module re-exports all TUI types from sub-modules for backward compatibility.
For new code, consider importing directly from the sub-modules:

* "System.Agents.TUI.Types.Core" - Core types (WidgetName, Tab, AppEvent, TuiAgent, etc.)
* "System.Agents.TUI.Types.Conversation" - Conversation types
* "System.Agents.TUI.Types.State" - State types (Core, UIState, TuiState)
-}
module System.Agents.TUI.Types (
    -- * Re-exports from Core
    module System.Agents.TUI.Types.Core,

    -- * Re-exports from Conversation
    module System.Agents.TUI.Types.Conversation,

    -- * Re-exports from State
    module System.Agents.TUI.Types.State,
) where

import System.Agents.TUI.Types.Conversation
import System.Agents.TUI.Types.Core
import System.Agents.TUI.Types.State
