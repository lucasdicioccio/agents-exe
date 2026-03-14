module System.Agents.Tools.Trace where

import qualified Data.Aeson.Types as Aeson
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)

-------------------------------------------------------------------------------
import qualified System.Agents.Tools.Bash as BashTools
import qualified System.Agents.Tools.IO as IOTools
import qualified System.Agents.Tools.SqliteToolbox as SqliteTools
import qualified System.Agents.Tools.SystemToolbox as SystemTools

-------------------------------------------------------------------------------

{- | Tool trace events for debugging and auditing.

This type aggregates trace events from all tool types in the system,
allowing unified tracing and logging of tool invocations.

The trace events can be used for:
* Debugging tool execution
* Performance analysis
* Security auditing
* Call tree reconstruction for nested tool calls

When adding new tool types, extend this type with a new constructor
and implement the appropriate conversion from the tool's internal
trace type.
-}
data ToolTrace
    = -- | Trace from bash tool execution
      BashToolsTrace !BashTools.RunTrace
    | -- | Trace from IO tool execution
      IOToolsTrace (IOTools.Trace Aeson.Value ByteString)
    | -- | Trace from SQLite toolbox operations
      SqliteToolsTrace !SqliteTools.Trace
    | -- | Trace from system toolbox operations
      SystemToolsTrace !SystemTools.Trace
    | {- | Generic trace message or event type from Lua execution
      The Text contains a message or event type identifier
      -}
      LuaToolsTrace !Text
    | {- | Trace of a tool call from within Lua
      Arguments: tool name, JSON arguments
      -}
      LuaToolCallTrace !Text !Aeson.Value
    deriving (Show)
