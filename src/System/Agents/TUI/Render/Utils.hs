{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Utility functions for TUI rendering.
module System.Agents.TUI.Render.Utils where

import Brick
import Brick.Focus (focusGetCurrent)
import Brick.Widgets.Border (borderWithLabel)
import Control.Lens ((^.))
import Data.Text (Text)

import System.Agents.TUI.Render.Attributes (focusedAttr)
import System.Agents.TUI.Types (TuiState, tuiUI, uiFocusRing, WidgetName)

-- | Create a border that shows focus.
borderWithFocus :: TuiState -> WidgetName -> Text -> Widget n -> Widget n
borderWithFocus st widgetName labelText content =
    let labelWidget =
            if focusGetCurrent (st ^. tuiUI . uiFocusRing) == Just widgetName
                then withAttr focusedAttr (txt labelText)
                else txt labelText
     in borderWithLabel labelWidget content

