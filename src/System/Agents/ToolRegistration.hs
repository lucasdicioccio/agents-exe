{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Defines an LLM tool registration.
module System.Agents.ToolRegistration where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.Types as Aeson
import Data.ByteString (ByteString)
import Data.Foldable.WithIndex (ifoldl')
import qualified Data.Maybe as Maybe
import Data.Text (Text)

import System.Agents.Base
import qualified System.Agents.LLMs.OpenAI as LLM
import qualified System.Agents.MCP.Base as Mcp
import System.Agents.Tools
import qualified System.Agents.Tools.Bash as BashTools
import qualified System.Agents.Tools.IO as IOTools
import qualified System.Agents.Tools.McpToolbox as McpTools

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

-- naming policy for MCP tools
mcp2LLMName :: McpTools.Toolbox -> McpTools.ToolDescription -> LLM.ToolName
mcp2LLMName box mcp =
    LLM.ToolName (mconcat ["mcp_", box.name, "_", mcp.getToolDescription.name])

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

registerMcpToolInLLM ::
    McpTools.Toolbox ->
    McpTools.ToolDescription ->
    Either String ToolRegistration
registerMcpToolInLLM box mcp =
    let
        matchName :: McpTools.ToolDescription -> LLM.ToolCall -> Bool
        matchName mcp call = mcp2LLMName box mcp == call.toolCallFunction.toolCallFunctionName

        llmBasedSchema :: Either String [LLM.ParamProperty]
        llmBasedSchema = adaptSchema mcp.getToolDescription.inputSchema

        llmName :: LLM.ToolName
        llmName = mcp2LLMName box mcp

        llmDescription :: Text
        llmDescription = Maybe.fromMaybe "" mcp.getToolDescription.description

        mapToolDescriptionMcp2LLM :: [LLM.ParamProperty] -> LLM.Tool
        mapToolDescriptionMcp2LLM schema =
            LLM.Tool
                { LLM.toolName = llmName
                , LLM.toolDescription = llmDescription
                , LLM.toolParamProperties = schema
                }

        tool :: Tool ToolRuntimeArg ()
        tool = mcpTool box mcp

        find :: LLM.ToolCall -> Maybe (Tool ToolRuntimeArg LLM.ToolCall)
        find call = if matchName mcp call then Just (mapToolResult (const call) tool) else Nothing
     in
        case llmBasedSchema of
            Right schema ->
                Right $ ToolRegistration tool (mapToolDescriptionMcp2LLM schema) find
            Left err ->
                Left err

adaptSchema :: Mcp.InputSchema -> Either String [LLM.ParamProperty]
adaptSchema schema =
    case schema.properties of
        Nothing -> Right []
        Just obj -> ifoldl' f (Right []) obj
  where
    f :: Aeson.Key -> Either String [LLM.ParamProperty] -> Aeson.Value -> Either String [LLM.ParamProperty]
    f _ err@(Left _) _ = err
    f k (Right xs) v =
        case adaptProperty k v of
            Left err -> Left err
            Right x -> Right (x : xs)

adaptProperty :: Aeson.Key -> Aeson.Value -> Either String LLM.ParamProperty
adaptProperty k val =
    case propMappingResult of
        Aeson.Success prop ->
            Right $
                LLM.ParamProperty
                    (AesonKey.toText k)
                    prop._type
                    prop._description
        Aeson.Error err -> Left err
  where
    propMappingResult :: Aeson.Result PropertyHelper
    propMappingResult = Aeson.fromJSON val

data PropertyHelper
    = PropertyHelper {_type :: Text, _description :: Text}

instance Aeson.FromJSON PropertyHelper where
    parseJSON = Aeson.withObject "PropertyHelper" $ \o ->
        PropertyHelper <$> o Aeson..: "type" <*> o Aeson..: "title"
