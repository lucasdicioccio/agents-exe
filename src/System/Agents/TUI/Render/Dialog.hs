{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Dialog rendering functions for file path and file browser dialogs.
module System.Agents.TUI.Render.Dialog where

import Brick
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (center)
import Brick.Widgets.Edit (renderEditor)
import Brick.Widgets.FileBrowser (renderFileBrowser)
import Control.Lens ((^.))
import qualified Data.Text as Text

import System.Agents.TUI.Render.Attributes
import System.Agents.TUI.Types

-- | Render the file path input dialog overlay.
renderFilePathDialog :: TuiState -> Widget N
renderFilePathDialog st =
    center $
        withAttr dialogAttr $
            borderWithLabel (txt " Attach File (Ctrl+F) ") $
                vBox
                    [ txt "Enter file path (or mime/type;path for explicit type):"
                    , txt ""
                    , hLimit 60 $
                        renderEditor
                            (txt . Text.unlines)
                            True
                            (st ^. tuiUI . filePathInput)
                    , txt ""
                    , txt "Enter: confirm  Esc: cancel"
                    ]

-- | Render the file browser dialog overlay.
renderFileBrowserDialog :: TuiState -> Widget N
renderFileBrowserDialog st =
    case st ^. tuiUI . fileBrowser of
        Nothing -> renderFilePathDialog st
        Just fb ->
            center $
                withAttr dialogAttr $
                    borderWithLabel (txt " Attach File (Ctrl+F) ") $
                        vBox
                            [ hLimit 80 $ vLimit 20 $ renderFileBrowser True fb
                            , txt ""
                            , txt "Enter: select file | Space: toggle | /: search | Esc: cancel"
                            ]

