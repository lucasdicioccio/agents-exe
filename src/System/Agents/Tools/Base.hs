module System.Agents.Tools.Base where

-------------------------------------------------------------------------------
import Data.ByteString.Char8 (ByteString)
import qualified System.Agents.MCP.Base as Mcp
import qualified System.Agents.Tools.Bash as BashTools
import qualified System.Agents.Tools.IO as IOTools

-------------------------------------------------------------------------------
data CallResult call
    = BlobToolSuccess call ByteString
    | ToolNotFound call
    | BashToolError call BashTools.RunScriptError
    | IOToolError call IOTools.RunError
    | McpToolResult call Mcp.CallToolResult
    | McpToolError call String -- TODO: better error
    deriving (Show)
