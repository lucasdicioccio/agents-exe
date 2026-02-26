{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Defines an LLM tool registration.
module System.Agents.ToolRegistration where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as AesonKeyMap
import qualified Data.Aeson.Key as AesonKey
import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Data.Foldable.WithIndex (ifoldl')
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text

import System.Agents.Base
import qualified System.Agents.LLMs.OpenAI as OpenAI
import qualified System.Agents.MCP.Base as Mcp
import System.Agents.Tools
import System.Agents.ToolSchema
import qualified System.Agents.Tools.Bash as BashTools
import qualified System.Agents.Tools.IO as IOTools
import qualified System.Agents.Tools.McpToolbox as McpTools
import qualified System.Agents.Tools.OpenApi as OpenApiTools
import qualified System.Agents.Tools.OpenApiToolbox as OpenApiTools

-------------------------------------------------------------------------------

-- | We register tool that will take as extra inner information a ConversationId
type ToolRuntimeArg = ConversationId

data ToolRegistration
    = ToolRegistration
    { innerTool :: Tool ToolRuntimeArg ()
    , declareTool :: OpenAI.Tool
    , findTool :: OpenAI.ToolCall -> Maybe (Tool ConversationId OpenAI.ToolCall)
    }
instance Show ToolRegistration where
    show (ToolRegistration d _ _) = Prelude.unwords ["ToolRegistration(", show d.toolDef, ")"]

-------------------------------------------------------------------------------

-- naming policy for IO tools
io2LLMName :: forall a b. IOTools.IOScript ToolRuntimeArg a b -> OpenAI.ToolName
io2LLMName io = OpenAI.ToolName (mconcat ["io_", io.description.ioSlug])

-- naming policy for Bash tools
bash2LLMName :: BashTools.ScriptDescription -> OpenAI.ToolName
bash2LLMName bash = OpenAI.ToolName (mconcat ["bash_", bash.scriptInfo.scriptSlug])

-- naming policy for MCP tools
mcp2LLMName :: McpTools.Toolbox -> McpTools.ToolDescription -> OpenAI.ToolName
mcp2LLMName box mcp =
    OpenAI.ToolName (mconcat ["mcp_", box.name, "_", mcp.getToolDescription.name])

-- naming policy for OpenAPI tools
openapi2LLMName :: OpenApiTools.Toolbox -> OpenApiTools.ToolDescription -> OpenAI.ToolName
openapi2LLMName box desc =
    OpenAI.ToolName (mconcat ["openapi_", box.name, "_", desc.operationId])

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

        tool :: Tool ToolRuntimeArg ()
        tool = bashTool script

        find :: OpenAI.ToolCall -> Maybe (Tool ToolRuntimeArg OpenAI.ToolCall)
        find call = if matchName script call then Just (mapToolResult (const call) tool) else Nothing
     in
        ToolRegistration tool (mapToolDescriptionBash2LLM script) find

{- | registers an IO Script, since we have not yet decided on a way to capture the
shape of the tool for IOScript (ideally some generics or something like Data.Aeson.Encoding) we take the whole Tool definition
-}
registerIOScriptInLLM ::
    (Aeson.FromJSON a) =>
    IOTools.IOScript ToolRuntimeArg a ByteString ->
    [ParamProperty] ->
    ToolRegistration
registerIOScriptInLLM script llmProps =
    let
        matchName :: IOTools.IOScript ToolRuntimeArg a b -> OpenAI.ToolCall -> Bool
        matchName io call = io2LLMName io == call.toolCallFunction.toolCallFunctionName

        tool :: Tool ToolRuntimeArg ()
        tool = ioTool script

        find :: OpenAI.ToolCall -> Maybe (Tool ToolRuntimeArg OpenAI.ToolCall)
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

        tool :: Tool ToolRuntimeArg ()
        tool = mcpTool box mcp

        find :: OpenAI.ToolCall -> Maybe (Tool ToolRuntimeArg OpenAI.ToolCall)
        find call = if matchName mcp call then Just (mapToolResult (const call) tool) else Nothing
     in
        case llmBasedSchema of
            Right schema ->
                Right $ ToolRegistration tool (mapToolDescriptionMcp2LLM schema) find
            Left err ->
                Left err

registerOpenApiToolInLLM ::
    OpenApiTools.Toolbox ->
    OpenApiTools.ToolDescription ->
    Either String ToolRegistration
registerOpenApiToolInLLM box desc =
    let
        matchName :: OpenApiTools.ToolDescription -> OpenAI.ToolCall -> Bool
        matchName d call = openapi2LLMName box d == call.toolCallFunction.toolCallFunctionName

        llmName :: OpenAI.ToolName
        llmName = openapi2LLMName box desc

        llmDescription :: Text
        llmDescription = desc.summary <> ":\n" <> desc.description

        -- Convert parameters to ParamProperty list
        paramProps :: [ParamProperty]
        paramProps = concatMap paramToProperty desc.parameters

        -- Add body parameter if present
        allProps :: [ParamProperty]
        allProps = paramProps <> bodyParam desc.requestBody desc.hasRequiredBody

        llmTool :: OpenAI.Tool
        llmTool =
            OpenAI.Tool
                { OpenAI.toolName = llmName
                , OpenAI.toolDescription = llmDescription
                , OpenAI.toolParamProperties = allProps
                }

        tool :: Tool ToolRuntimeArg ()
        tool = openapiTool box desc

        find :: OpenAI.ToolCall -> Maybe (Tool ToolRuntimeArg OpenAI.ToolCall)
        find call = if matchName desc call then Just (mapToolResult (const call) tool) else Nothing
     in
        Right $ ToolRegistration tool llmTool find

-- | Convert an OpenAPI parameter to ParamProperty
paramToProperty :: OpenApiTools.Parameter -> [ParamProperty]
paramToProperty param =
    let key = "p_" <> OpenApiTools.paramName param
        propType = schemaToParamType (OpenApiTools.paramSchema param)
        -- Try to extract description from schema
        desc = case OpenApiTools.paramSchema param of
            Aeson.Object obj -> 
                case AesonKeyMap.lookup "description" obj of
                    Just (Aeson.String s) -> s
                    _ -> ""
            _ -> ""
    in [ParamProperty key propType desc]

-- | Convert JSON schema to ParamType
schemaToParamType :: Aeson.Value -> ParamType
schemaToParamType schema =
    case schema of
        Aeson.Object obj ->
            -- Check for enum
            case AesonKeyMap.lookup "enum" obj of
                Just (Aeson.Array arr) ->
                    let vals = [s | Aeson.String s <- toList arr]
                    in EnumParamType vals
                _ ->
                    -- Check for type
                    case AesonKeyMap.lookup "type" obj of
                        Just (Aeson.String "string") -> StringParamType
                        Just (Aeson.String "integer") -> NumberParamType
                        Just (Aeson.String "number") -> NumberParamType
                        Just (Aeson.String "boolean") -> BoolParamType
                        Just (Aeson.String "array") ->
                            case AesonKeyMap.lookup "items" obj of
                                Just items ->
                                    let itemType = schemaToParamType items
                                    in OpaqueParamType ("array of " <> showItemType itemType)
                                Nothing -> OpaqueParamType "array"
                        Just (Aeson.String "object") ->
                            case AesonKeyMap.lookup "properties" obj of
                                Just (Aeson.Object props) ->
                                    let subProps = concatMap (\(k, v) -> 
                                            [ParamProperty (AesonKey.toText k) (schemaToParamType v) ""]) 
                                            (AesonKeyMap.toList props)
                                    in ObjectParamType subProps
                                _ -> OpaqueParamType "object"
                        _ -> OpaqueParamType "any"
        _ -> OpaqueParamType "any"

showItemType :: ParamType -> Text
showItemType StringParamType = "string"
showItemType NumberParamType = "number"
showItemType BoolParamType = "boolean"
showItemType NullParamType = "null"
showItemType (EnumParamType vals) = "enum(" <> Text.intercalate "," vals <> ")"
showItemType (OpaqueParamType t) = t
showItemType (MultipleParamType t) = t
showItemType (ObjectParamType _) = "object"

-- | Create body parameter if request body exists
bodyParam :: Maybe OpenApiTools.RequestBodySchema -> Bool -> [ParamProperty]
bodyParam Nothing _ = []
bodyParam (Just reqBody) required =
    let schema = OpenApiTools.bodySchema reqBody
        propType = schemaToParamType schema
    in [ParamProperty "b" propType "Request body"]

-------------------------------------------------------------------------------

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

