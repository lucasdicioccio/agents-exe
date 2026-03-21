module System.Agents.Tools.Trace where

import qualified Data.Aeson.Types as Aeson
import Data.ByteString.Char8 (ByteString)

-------------------------------------------------------------------------------
import qualified System.Agents.Tools.Bash as BashTools
import qualified System.Agents.Tools.DeveloperToolbox as DeveloperTools
import qualified System.Agents.Tools.IO as IOTools
import qualified System.Agents.Tools.LuaToolbox as LuaTools
import qualified System.Agents.Tools.SqliteToolbox as SqliteTools
import qualified System.Agents.Tools.SystemToolbox as SystemTools

-------------------------------------------------------------------------------
data ToolTrace
    = BashToolsTrace !BashTools.RunTrace
    | IOToolsTrace (IOTools.Trace Aeson.Value ByteString)
    | SqliteToolsTrace !SqliteTools.Trace
    | SystemToolsTrace !SystemTools.Trace
    | DeveloperToolsTrace !DeveloperTools.Trace
    | LuaToolsTrace !LuaTools.Trace
    deriving (Show)
