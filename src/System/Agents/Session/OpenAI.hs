{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Agents.Session.OpenAI where

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text

import Prod.Tracer (Tracer)
import qualified System.Agents.HttpClient as HttpClient
import qualified System.Agents.LLMs.OpenAI as OpenAI

import System.Agents.ToolSchema
import System.Agents.Session.Base

-------------------------------------------------------------------------------

-- | Configuration for OpenAI-based completion
data OpenAICompletionConfig = OpenAICompletionConfig
    { cfgTracer :: Tracer IO OpenAI.Trace
    , cfgRuntime :: HttpClient.Runtime
    , cfgBaseUrl :: OpenAI.ApiBaseUrl
    , cfgModelName :: Text
    , cfgModelFlavor :: OpenAI.ModelFlavor
    }

{- | Creates a completion function that uses OpenAI's API via callLLMPayload.
This function converts the LlmCompletion into an OpenAI-compatible payload,
calls the LLM, and parses the response into LlmResponse and [LlmToolCall].
-}
mkOpenAICompletion :: OpenAICompletionConfig -> (LlmCompletion -> IO (LlmResponse, [LlmToolCall]))
mkOpenAICompletion config completion = do
    let payload = buildPayload completion

    result <- OpenAI.callLLMPayload config.cfgTracer config.cfgRuntime config.cfgBaseUrl payload

    case result of
        Left err -> ioError $ userError $ "LLM API call failed: " ++ err
        Right responseValue ->
            case Aeson.parse OpenAI.parseLLMResponse responseValue of
                Aeson.Error parseErr -> ioError $ userError $ "Failed to parse LLM response: " ++ parseErr
                Aeson.Success response -> pure $ extractResponse responseValue response
  where
    -- Build the OpenAI-compatible payload from LlmCompletion
    buildPayload :: LlmCompletion -> Aeson.Value
    buildPayload comp =
        let 
            tools = map systemToolToOpenAI comp.completeTools
            messages = buildMessages comp
         in Aeson.object $
                [ "model" .= config.cfgModelName
                , "messages" .= messages
                ]
                    ++ ["tools" .= tools | not (null tools)]
                    ++ flavorSpecificFields config.cfgModelFlavor

    -- Convert SystemTool (which contains JSON text) to OpenAI Tool.
    -- Since OpenAI.Tool doesn't have a FromJSON instance, we parse the JSON
    -- manually to extract name, description, and parameters.
    systemToolToOpenAI :: SystemTool -> OpenAI.Tool
    systemToolToOpenAI (SystemTool (V1 tool)) =
        OpenAI.Tool
            { OpenAI.toolName = OpenAI.ToolName $ tool.llmName
            , OpenAI.toolDescription = tool.description
            , OpenAI.toolParamProperties = tool.properties
            }
    systemToolToOpenAI (SystemTool (V0 (Aeson.Object v0obj))) =
        OpenAI.Tool
            { OpenAI.toolName = OpenAI.ToolName $ extractName v0obj
            , OpenAI.toolDescription = extractDescription v0obj
            , OpenAI.toolParamProperties = extractParams v0obj
            }
      where
        extractName :: KeyMap.KeyMap Aeson.Value -> Text
        extractName obj =
            case KeyMap.lookup "name" obj of
                Just (Aeson.String n) -> n
                _ -> case KeyMap.lookup "function" obj of
                    Just (Aeson.Object func) ->
                        case KeyMap.lookup "name" func of
                            Just (Aeson.String n) -> n
                            _ -> "no_name"
                    _ -> "no_name"

        extractDescription :: KeyMap.KeyMap Aeson.Value -> Text
        extractDescription obj =
            case KeyMap.lookup "description" obj of
                Just (Aeson.String d) -> d
                _ -> case KeyMap.lookup "function" obj of
                    Just (Aeson.Object func) ->
                        case KeyMap.lookup "description" func of
                            Just (Aeson.String d) -> d
                            _ -> ""
                    _ -> ""

        extractParams :: KeyMap.KeyMap Aeson.Value -> [ParamProperty]
        extractParams obj =
            case KeyMap.lookup "parameters" obj of
                Just params -> parseParamsObject params
                _ -> case KeyMap.lookup "function" obj of
                    Just (Aeson.Object func) ->
                        case KeyMap.lookup "parameters" func of
                            Just params -> parseParamsObject params
                            _ -> []
                    _ -> []

        parseParamsObject :: Aeson.Value -> [ParamProperty]
        parseParamsObject (Aeson.Object params) =
            case KeyMap.lookup "properties" params of
                Just (Aeson.Object props) ->
                    KeyMap.toList props >>= \(key, val) -> parseProperty key val
                _ -> []
        parseParamsObject _ = []

        parseProperty :: Aeson.Key -> Aeson.Value -> [ParamProperty]
        parseProperty key (Aeson.Object propObj) =
            let propType = case KeyMap.lookup "type" propObj of
                    Just (Aeson.String t) -> parseParamType t propObj
                    _ -> StringParamType
                propDesc = case KeyMap.lookup "description" propObj of
                    Just (Aeson.String d) -> d
                    _ -> ""
             in [ParamProperty (Key.toText key) propType propDesc]
        parseProperty _ _ = []

        parseParamType :: Text -> KeyMap.KeyMap Aeson.Value -> ParamType
        parseParamType "string" _ = StringParamType
        parseParamType "integer" _ = NumberParamType
        parseParamType "number" _ = NumberParamType
        parseParamType "boolean" _ = BoolParamType
        parseParamType "null" _ = NullParamType
        parseParamType "object" obj =
            case KeyMap.lookup "properties" obj of
                Just (Aeson.Object props) ->
                    ObjectParamType $ KeyMap.toList props >>= \(k, v) -> parseProperty k v
                _ -> ObjectParamType []
        parseParamType "array" _ = OpaqueParamType "array"
        parseParamType other _ = OpaqueParamType other

    -- Build the messages array for the OpenAI API
    buildMessages :: LlmCompletion -> [Aeson.Value]
    buildMessages comp =
        let (SystemPrompt sysPromptTxt) = comp.completePrompt
            systemMsg =
                Aeson.object
                    [ "role" .= ("system" :: Text)
                    , "content" .= sysPromptTxt
                    ]
            mQuery = comp.completeQuery
            userMsg = userQueryToMessages mQuery
            toolResponses = comp.completeToolResponses
            toolMsgs = concatMap toolResponseToMessages toolResponses
            histMsgs = historyToMessages comp.completeConversationHistory
         in systemMsg : histMsgs ++ userMsg ++ toolMsgs

    historyToMessages :: [Turn] -> [Aeson.Value]
    historyToMessages ts = concatMap turnToMessages (reverse ts)

    turnToMessages :: Turn -> [Aeson.Value]
    turnToMessages (LlmTurn turn) =
        case Aeson.parse OpenAI.parseLLMResponse turn.llmResponse.rawResponse of
          Aeson.Error _ -> []
          Aeson.Success rsp -> [Aeson.Object rsp.chosenMessage]
    turnToMessages (UserTurn turn) =
        let 
            mQuery = turn.userQuery
            userMsg = userQueryToMessages mQuery
            toolResponses = turn.userToolResponses
            toolMsgs = concatMap toolResponseToMessages toolResponses
         in userMsg ++ toolMsgs

    userQueryToMessages :: Maybe UserQuery -> [Aeson.Value]
    userQueryToMessages mQuery = case mQuery of
      Nothing -> []
      Just (UserQuery q) ->
        [ Aeson.object
          [ "role" .= ("user" :: Text)
          , "content" .= q
          ]
        ]

    -- Convert a tool call/response pair to OpenAI message format
    toolResponseToMessages :: (LlmToolCall, UserToolResponse) -> [Aeson.Value]
    toolResponseToMessages (LlmToolCall callVal, UserToolResponse rspVal) =
        -- Extract tool_call_id from the call if available
        let toolCallId = case callVal of
                Aeson.Object obj -> case KeyMap.lookup "id" obj of
                    Just (Aeson.String tid) -> tid
                    _ -> ""
                _ -> ""
         in [ Aeson.object
                [ "role" .= ("tool" :: Text)
                , "tool_call_id" .= toolCallId
                , "content" .= case rspVal of
                    Aeson.String s -> s
                    other -> Text.decodeUtf8 $ BSL.toStrict $ Aeson.encode other
                ]
            ]

    -- Add flavor-specific fields to the payload
    flavorSpecificFields :: OpenAI.ModelFlavor -> [(Aeson.Key, Aeson.Value)]
    flavorSpecificFields OpenAI.MistralV1 =
        [ "tool_choice" .= ("auto" :: Text)
        , "parallel_tool_calls" .= True
        ]
    flavorSpecificFields OpenAI.KimiV1 =
        [] -- Kimi uses default settings
    flavorSpecificFields OpenAI.ClaudeV1 =
        [ "thinking"
            .= Aeson.object
                [ "type" .= ("enabled" :: Text)
                , "budget_tokens" .= (10000 :: Int)
                ]
        ]
    flavorSpecificFields OpenAI.OpenAIv1 =
        [] -- OpenAI uses default settings

    -- Extract LlmResponse and [LlmToolCall] from parsed Response
    extractResponse :: Aeson.Value -> OpenAI.Response -> (LlmResponse, [LlmToolCall])
    extractResponse rawValue rsp =
        let llmRsp = LlmResponse
                { responseText = rsp.rspContent
                , responseThinking = rsp.rspReasoningContent  -- Extract thinking/reasoning content
                , rawResponse = rawValue
                }
            toolCalls = case rsp.rspToolCalls of
                Nothing -> []
                Just calls -> map toolCallToLlmToolCall calls
         in (llmRsp, toolCalls)

    -- Convert OpenAI ToolCall to LlmToolCall (wrapping the raw object)
    toolCallToLlmToolCall :: OpenAI.ToolCall -> LlmToolCall
    toolCallToLlmToolCall tc = LlmToolCall $ Aeson.Object tc.rawToolCall

newConfig :: Tracer IO OpenAI.Trace -> IO OpenAICompletionConfig
newConfig tracer =
    OpenAICompletionConfig
        <$> pure tracer
        <*> (HttpClient.newRuntime =<< (HttpClient.BearerToken . Text.strip <$> Text.readFile "token.txt"))
        <*> pure (OpenAI.ApiBaseUrl "https://api.openai.com/v1")
        <*> pure "gpt-4.1-mini"
        <*> pure OpenAI.OpenAIv1

