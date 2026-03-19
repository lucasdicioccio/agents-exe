{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- | LuaToolbox - A Lua scripting environment for agent tools

This module provides a sandboxed Lua environment for executing
user scripts with access to JSON encoding/decoding capabilities.
-}
module System.Agents.Tools.LuaToolbox (
    LuaToolbox,
    newToolbox,
    closeToolbox,
    executeScript,
    withToolbox,
    ExecutionResult (..),
) where

import Control.Exception (bracket)
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

-- | A sandboxed Lua execution environment
data LuaToolbox = LuaToolbox
    { _toolboxState :: () -- Placeholder for actual Lua state
    }

-- | Result of executing a Lua script
data ExecutionResult = ExecutionResult
    { resultValues :: [Value] -- Values returned by the script
    , resultOutput :: Maybe Text.Text -- Captured stdout output
    }
    deriving (Show, Eq)

-- | Create a new Lua toolbox with all modules loaded
newToolbox :: IO LuaToolbox
newToolbox = do
    -- Initialize Lua state and register modules
    -- In a real implementation, this would use hslua or similar
    pure $ LuaToolbox ()

-- | Close a Lua toolbox and free resources
closeToolbox :: LuaToolbox -> IO ()
closeToolbox _ = do
    -- Clean up Lua state
    pure ()

{- | Execute a Lua script in the toolbox

The script has access to:
  - json.encode(value) - Encode a Lua table to JSON string
  - json.decode(str)   - Decode a JSON string to Lua table
-}
executeScript :: LuaToolbox -> String -> IO (Either String ExecutionResult)
executeScript _ script = do
    -- In a real implementation, this would:
    -- 1. Load the script into the Lua state
    -- 2. Execute it
    -- 3. Convert return values to Aeson Values
    -- 4. Capture any output

    -- For now, return a placeholder that indicates the script was received
    pure $
        Right $
            ExecutionResult
                { resultValues = [Aeson.String $ Text.pack $ "Script executed: " ++ take 50 script ++ "..."]
                , resultOutput = Nothing
                }

-- | Helper to run with a toolbox, ensuring cleanup
withToolbox :: (LuaToolbox -> IO a) -> IO a
withToolbox = bracket newToolbox closeToolbox
