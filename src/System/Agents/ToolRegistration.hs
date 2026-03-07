{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Defines an LLM tool registration.
module System.Agents.ToolRegistration where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import Data.ByteString (ByteString)
import Data.Foldable.WithIndex (ifoldl')
import qualified Data.Maybe as Maybe
import Data.Text (Text)

import qualified System.Agents.LLMs.OpenAI as OpenAI
import qualified System.Agents.MCP.Base as Mcp
import System.Agents.Tools
import System.Agents.ToolSchema
import qualified System.Agents.Tools.Bash as BashTools
import qualified System.Agents.Tools.IO as IOTools
import qualified System.Agents.Tools.McpToolbox as McpTools

-------------------------------------------------------------------------------

-- | We register tools that will take a ToolExecutionContext for execution.
--
-- The 'innerTool' field uses 'Tool ()' since the tool execution context
-- is passed at runtime via 'toolRun', not stored in the tool itself.
data ToolRegistration
    = ToolRegistration
    { innerTool :: Tool ()
    , declareTool :: OpenAI.Tool
    , findTool :: OpenAI.ToolCall -> Maybe (Tool OpenAI.ToolCall)
    }
instance Show ToolRegistration where
    show (ToolRegistration d _ _) = Prelude.unwords ["ToolRegistration(", show d.toolDef, ")"]

-------------------------------------------------------------------------------

-- naming policy for IO tools
io2LLMName :: forall a b. IOTools.IOScript a b -> OpenAI.ToolName
io2LLMName io = OpenAI.ToolName (mconcat ["io_", io.description.ioSlug])

-- naming policy for Bash tools
bash2LLMName :: BashTools.ScriptDescription -> OpenAI.ToolName
bash2LLMName bash = OpenAI.ToolName (mconcat ["bash_", bash.scriptInfo.scriptSlug])

-- naming policy for MCP tools
mcp2LLMName :: McpTools.Toolbox -> McpTools.ToolDescription -> OpenAI.ToolName
mcp2LLMName box mcp =
    OpenAI.ToolName (mconcat ["mcp_", box.name, "_", mcp.getToolDescription.name])

-------------------------------------------------------------------------------

registerBashToolInLLM ::
    BashTools.ScriptDescription ->
    ToolRegistration
registerBashToolInLLM script =
    let
        matchName :: BashTools.ScriptDescription -> OpenAI.ToolCall -> Bool
        matchName bash call = bash2LLMName bash == call.toolCallFunction.toolCallFunctionName

        mapToolDescriptionBash2LLM :: BashTools.ScriptDescription -> OpenAI.Tool
        mapToolDescriptionBash2LLM bash =
            OpenAI.Tool
                { OpenAI.toolName = bash2LLMName bash
                , OpenAI.toolDescription = bash.scriptInfo.scriptDescription
                , OpenAI.toolParamProperties = fmap mapArg bash.scriptInfo.scriptArgs
                }

        mapArg :: BashTools.ScriptArg -> ParamProperty
        mapArg arg =
            ParamProperty
                { propertyKey = arg.argName
                , propertyType = OpaqueParamType arg.argBackingTypeString
                , propertyDescription = arg.argDescription
                }

        tool :: Tool ()
        tool = bashTool script

        find :: OpenAI.ToolCall -> Maybe (Tool OpenAI.ToolCall)
        find call = if matchName script call then Just (mapToolResult (const call) tool) else Nothing
     in
        ToolRegistration tool (mapToolDescriptionBash2LLM script) find

{- | registers an IO Script, since we have not yet decided on a way to capture the
shape of the tool for IOScript (ideally some generics or something like Data.Aeson.Encoding) we take the whole Tool definition
-}
registerIOScriptInLLM ::
    (Aeson.FromJSON a) =>
    IOTools.IOScript a ByteString ->
    [ParamProperty] ->
    ToolRegistration
registerIOScriptInLLM script llmProps =
    let
        matchName :: IOTools.IOScript a b -> OpenAI.ToolCall -> Bool
        matchName io call = io2LLMName io == call.toolCallFunction.toolCallFunctionName

        tool :: Tool ()
        tool = ioTool script

        find :: OpenAI.ToolCall -> Maybe (Tool OpenAI.ToolCall)
        find call = if matchName script call then Just (mapToolResult (const call) tool) else Nothing

        llmTool :: OpenAI.Tool
        llmTool =
            OpenAI.Tool
                { OpenAI.toolName = io2LLMName script
                , OpenAI.toolDescription = script.description.ioDescription
                , OpenAI.toolParamProperties = llmProps
                }
     in
        ToolRegistration tool llmTool find

registerMcpToolInLLM ::
    McpTools.Toolbox ->
    McpTools.ToolDescription ->
    Either String ToolRegistration
registerMcpToolInLLM box mcp =
    let
        matchName :: McpTools.ToolDescription -> OpenAI.ToolCall -> Bool
        matchName td call = mcp2LLMName box td == call.toolCallFunction.toolCallFunctionName

        llmBasedSchema :: Either String [ParamProperty]
        llmBasedSchema = adaptSchema mcp.getToolDescription.inputSchema

        llmName :: OpenAI.ToolName
        llmName = mcp2LLMName box mcp

        llmDescription :: Text
        llmDescription = Maybe.fromMaybe "" mcp.getToolDescription.description

        mapToolDescriptionMcp2LLM :: [ParamProperty] -> OpenAI.Tool
        mapToolDescriptionMcp2LLM schema =
            OpenAI.Tool
                { OpenAI.toolName = llmName
                , OpenAI.toolDescription = llmDescription
                , OpenAI.toolParamProperties = schema
                }

        tool :: Tool ()
        tool = mcpTool box mcp

        find :: OpenAI.ToolCall -> Maybe (Tool OpenAI.ToolCall)
        find call = if matchName mcp call then Just (mapToolResult (const call) tool) else Nothing
     in
        case llmBasedSchema of
            Right schema ->
                Right $ ToolRegistration tool (mapToolDescriptionMcp2LLM schema) find
            Left err ->
                Left err

adaptSchema :: Mcp.InputSchema -> Either String [ParamProperty]
adaptSchema schema =
    case schema.properties of
        Nothing -> Right []
        Just obj -> ifoldl' f (Right []) obj
  where
    f :: Aeson.Key -> Either String [ParamProperty] -> Aeson.Value -> Either String [ParamProperty]
    f _ err@(Left _) _ = err
    f k (Right xs) v =
        case adaptProperty k v of
            Left err -> Left err
            Right x -> Right (x : xs)

adaptProperty :: Aeson.Key -> Aeson.Value -> Either String ParamProperty
adaptProperty k val =
    case propMappingResult of
        Aeson.Success prop ->
            Right $
                ParamProperty
                    (AesonKey.toText k)
                    (OpaqueParamType prop._type)
                    prop._description
        Aeson.Error err -> Left err
  where
    propMappingResult :: Aeson.Result PropertyHelper
    propMappingResult = Aeson.fromJSON val

data PropertyHelper
    = PropertyHelper {_type :: Text, _description :: Text}

instance Aeson.FromJSON PropertyHelper where
    parseJSON = Aeson.withObject "PropertyHelper" $ \o ->
        PropertyHelper <$> o Aeson..: "type" <*> o Aeson..: "description"

