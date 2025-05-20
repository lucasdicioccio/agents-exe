{-# LANGUAGE OverloadedRecordDot #-}

module System.Agents.Tools where

import qualified Data.Aeson.Types as Aeson
import Data.ByteString.Char8 as CByteString
import Prod.Tracer (Tracer, contramap)

-------------------------------------------------------------------------------
import qualified System.Agents.Tools.Bash as BashTools
import qualified System.Agents.Tools.IO as IOTools

-------------------------------------------------------------------------------

data ToolTrace
    = BashToolsTrace !BashTools.RunTrace
    | IOToolsTrace (IOTools.Trace Aeson.Value ByteString)
    deriving (Show)

data CallResult call
    = ToolNotFound call
    | BashToolError call BashTools.RunScriptError
    | IOToolError call IOTools.RunError
    | ToolSuccess call ByteString
    deriving (Show)

extractCall :: CallResult call -> call
extractCall (ToolNotFound c) = c
extractCall (BashToolError c _) = c
extractCall (IOToolError c _) = c
extractCall (ToolSuccess c _) = c

mapCallResult :: (a -> b) -> CallResult a -> CallResult b
mapCallResult f c =
    case c of
        (ToolNotFound v) -> ToolNotFound (f v)
        (BashToolError v e) -> BashToolError (f v) e
        (IOToolError v e) -> IOToolError (f v) e
        (ToolSuccess v b) -> ToolSuccess (f v) b

data ToolDef
    = BashTool !BashTools.ScriptDescription
    | IOTool !IOTools.IOScriptDescription
    deriving (Show)

data Tool rtVal call
    = Tool
    { toolDef :: ToolDef
    , toolRun :: Tracer IO ToolTrace -> rtVal -> Aeson.Value -> IO (CallResult call)
    }

mapToolCall :: (a -> b) -> Tool x a -> Tool x b
mapToolCall f (Tool d run) =
    Tool d (\tracer rtval v -> fmap (mapCallResult f) (run tracer rtval v))

data Registration rtVal tool call
    = Registration
    { innerTool :: Tool rtVal ()
    , declareTool :: tool
    , findTool :: call -> Maybe (Tool rtVal call)
    }
instance Show (Registration x t c) where
    show (Registration d _ _) = Prelude.unwords ["Registration(", show d.toolDef, ")"]

-------------------------------------------------------------------------------
bashTool ::
    BashTools.ScriptDescription ->
    Tool a ()
bashTool script =
    Tool
        { toolDef = BashTool script
        , toolRun = run
        }
  where
    call = ()
    run tracer _ v = do
        ret <- BashTools.runValue (contramap BashToolsTrace tracer) script v
        case ret of
            Left err -> pure $ BashToolError call err
            Right rsp -> pure $ ToolSuccess call rsp

-------------------------------------------------------------------------------
ioTool ::
    (Aeson.FromJSON llmArg) =>
    IOTools.IOScript rtVal llmArg ByteString ->
    Tool rtVal ()
ioTool script =
    Tool
        { toolDef = IOTool script.description
        , toolRun = run
        }
  where
    call = ()
    run tracer runtimeValue v = do
        -- we trace the original input object
        let adaptTrace = IOToolsTrace . IOTools.adaptTraceInput (const v)
        ret <- IOTools.runValue (contramap adaptTrace tracer) script runtimeValue v
        case ret of
            Left err -> pure $ IOToolError call err
            Right rsp -> pure $ ToolSuccess call rsp
