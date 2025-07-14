module System.Agents.Tools.Trace where

import qualified Data.Aeson.Types as Aeson
import Data.ByteString.Char8 (ByteString)

-------------------------------------------------------------------------------
import qualified System.Agents.Tools.Bash as BashTools
import qualified System.Agents.Tools.IO as IOTools

-------------------------------------------------------------------------------
data ToolTrace
    = BashToolsTrace !BashTools.RunTrace
    | IOToolsTrace (IOTools.Trace Aeson.Value ByteString)
    deriving (Show)

data CallResult call
    = ToolSuccess call ByteString
    | ToolNotFound call
    | BashToolError call BashTools.RunScriptError
    | IOToolError call IOTools.RunError
    | McpToolError call String -- TODO: better error
    deriving (Show)
