{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Defines an LLM tool registration.
module System.Agents.ToolRegistration where

import qualified Data.Aeson.Types as Aeson
import Data.ByteString (ByteString)

import System.Agents.Base
import qualified System.Agents.LLMs.OpenAI as LLM
import System.Agents.Tools
import qualified System.Agents.Tools.Bash as BashTools
import qualified System.Agents.Tools.IO as IOTools

-------------------------------------------------------------------------------

-- | We register tool that will take as extra inner information a ConversationId
type ToolRuntimeArg = ConversationId

data ToolRegistration
    = ToolRegistration
    { innerTool :: Tool ToolRuntimeArg ()
    , declareTool :: LLM.Tool
    , findTool :: LLM.ToolCall -> Maybe (Tool ConversationId LLM.ToolCall)
    }
instance Show ToolRegistration where
    show (ToolRegistration d _ _) = Prelude.unwords ["ToolRegistration(", show d.toolDef, ")"]

-------------------------------------------------------------------------------

-- naming policy for IO tools
io2LLMName :: forall a b. IOTools.IOScript ToolRuntimeArg a b -> LLM.ToolName
io2LLMName io = LLM.ToolName (mconcat ["io_", io.description.ioSlug])

-- naming policy for Bash tools
bash2LLMName :: BashTools.ScriptDescription -> LLM.ToolName
bash2LLMName bash = LLM.ToolName (mconcat ["bash_", bash.scriptInfo.scriptSlug])

-------------------------------------------------------------------------------

registerBashToolInLLM ::
    BashTools.ScriptDescription ->
    ToolRegistration
registerBashToolInLLM script =
    let
        matchName :: BashTools.ScriptDescription -> LLM.ToolCall -> Bool
        matchName bash call = bash2LLMName bash == call.toolCallFunction.toolCallFunctionName

        mapToolDescriptionBash2LLM :: BashTools.ScriptDescription -> LLM.Tool
        mapToolDescriptionBash2LLM bash =
            LLM.Tool
                { LLM.toolName = bash2LLMName bash
                , LLM.toolDescription = bash.scriptInfo.scriptDescription
                , LLM.toolParamProperties = fmap mapArg bash.scriptInfo.scriptArgs
                }

        mapArg :: BashTools.ScriptArg -> LLM.ParamProperty
        mapArg arg =
            LLM.ParamProperty
                { LLM.propertyKey = arg.argName
                , LLM.propertyType = arg.argBackingTypeString
                , LLM.propertyDescription = arg.argDescription
                }

        tool :: Tool ToolRuntimeArg ()
        tool = bashTool script

        find :: LLM.ToolCall -> Maybe (Tool ToolRuntimeArg LLM.ToolCall)
        find call = if matchName script call then Just (mapToolResult (const call) tool) else Nothing
     in
        ToolRegistration tool (mapToolDescriptionBash2LLM script) find

{- | registers an IO Script, since we have not yet decided on a way to capture the
shape of the tool for IOScript (ideally some generics or something like Data.Aeson.Encoding) we take the whole Tool definition
-}
registerIOScriptInLLM ::
    (Aeson.FromJSON a) =>
    IOTools.IOScript ToolRuntimeArg a ByteString ->
    [LLM.ParamProperty] ->
    ToolRegistration
registerIOScriptInLLM script llmProps =
    let
        matchName :: IOTools.IOScript ToolRuntimeArg a b -> LLM.ToolCall -> Bool
        matchName io call = io2LLMName io == call.toolCallFunction.toolCallFunctionName

        tool :: Tool ToolRuntimeArg ()
        tool = ioTool script

        find :: LLM.ToolCall -> Maybe (Tool ToolRuntimeArg LLM.ToolCall)
        find call = if matchName script call then Just (mapToolResult (const call) tool) else Nothing

        llmTool :: LLM.Tool
        llmTool =
            LLM.Tool
                { LLM.toolName = io2LLMName script
                , LLM.toolDescription = script.description.ioDescription
                , LLM.toolParamProperties = llmProps
                }
     in
        ToolRegistration tool llmTool find
