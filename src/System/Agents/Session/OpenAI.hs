{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Agents.Session.OpenAI where

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.UUID as UUID

import Prod.Tracer (Tracer)
import qualified System.Agents.HttpClient as HttpClient
import qualified System.Agents.LLMs.OpenAI as OpenAI

import System.Agents.Media.Types (ContentPart (..), MediaAttachment (..))
import System.Agents.Session.Base
import System.Agents.ToolSchema

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
            -- Convert session ID to text for prompt_cache_key
            promptCacheKey = case comp.completeSessionId of
                Just (SessionId uuid) -> Just $ Text.pack $ UUID.toString uuid
                Nothing -> Nothing
         in
            Aeson.object $
                [ "model" .= config.cfgModelName
                , "messages" .= messages
                ]
                    ++ ["tools" .= tools | not (null tools)]
                    ++ ["prompt_cache_key" .= key | Just key <- [promptCacheKey]]
                    ++ flavorSpecificFields config.cfgModelFlavor

    -- Convert SystemTool (which contains JSON text) to OpenAI Tool.
    -- Since OpenAI.Tool doesn't have a FromJSON instance, we parse the JSON
    -- manually to extract name, description, and parameters.
    systemToolToOpenAI :: SystemTool -> OpenAI.Tool
    systemToolToOpenAI (SystemTool (V1 tool)) =
        OpenAI.Tool $
            ToolDescription
                { toolDescriptionName = OpenAI.ToolName $ tool.llmName
                , toolDescriptionText = tool.description
                , toolDescriptionParamProperties = tool.properties
                }
    systemToolToOpenAI (SystemTool (V0 (Aeson.Object v0obj))) =
        OpenAI.Tool $
            ToolDescription
                { toolDescriptionName = ToolName $ extractName v0obj
                , toolDescriptionText = extractDescription v0obj
                , toolDescriptionParamProperties = extractParams v0obj
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
            let requiredSet = case KeyMap.lookup "required" params of
                    Just (Aeson.Array arr) ->
                        foldr (\v acc -> case v of Aeson.String s -> Set.insert s acc; _ -> acc) Set.empty arr
                    _ -> Set.empty
             in case KeyMap.lookup "properties" params of
                    Just (Aeson.Object props) ->
                        KeyMap.toList props >>= \(key, val) -> parseProperty requiredSet key val
                    _ -> []
        parseParamsObject _ = []

        parseProperty :: Set Text -> Key.Key -> Aeson.Value -> [ParamProperty]
        parseProperty requiredSet key (Aeson.Object propObj) =
            let propType = case KeyMap.lookup "type" propObj of
                    Just (Aeson.String t) -> parseParamType requiredSet t propObj
                    _ -> StringParamType
                propDesc = case KeyMap.lookup "description" propObj of
                    Just (Aeson.String d) -> d
                    _ -> ""
                propRequired = Set.member (Key.toText key) requiredSet
             in [ParamProperty (Key.toText key) propType propDesc propRequired]
        parseProperty _ _ _ = []

        parseParamType :: Set Text -> Text -> KeyMap.KeyMap Aeson.Value -> ParamType
        parseParamType _ "string" _ = StringParamType
        parseParamType _ "integer" _ = NumberParamType
        parseParamType _ "number" _ = NumberParamType
        parseParamType _ "boolean" _ = BoolParamType
        parseParamType _ "null" _ = NullParamType
        parseParamType _requiredSet "object" obj =
            case KeyMap.lookup "properties" obj of
                Just (Aeson.Object props) ->
                    let nestedRequiredSet = case KeyMap.lookup "required" obj of
                            Just (Aeson.Array arr) ->
                                foldr (\v acc -> case v of Aeson.String s -> Set.insert s acc; _ -> acc) Set.empty arr
                            _ -> Set.empty
                     in ObjectParamType $ KeyMap.toList props >>= \(k, v) -> parseProperty nestedRequiredSet k v
                _ -> ObjectParamType []
        parseParamType _ "array" _ = OpaqueParamType "array"
        parseParamType _ other _ = OpaqueParamType other
    systemToolToOpenAI (SystemTool (V0 other)) =
        -- Handle non-object V0 values by creating a minimal tool
        OpenAI.Tool $
            ToolDescription
                { toolDescriptionName = OpenAI.ToolName "unknown_tool"
                , toolDescriptionText = "Unknown tool format: " <> Text.pack (show other)
                , toolDescriptionParamProperties = []
                }

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
            -- Include media from both the query and the completion attachments
            mediaAttachments =
                case mQuery of
                    Nothing -> comp.completeMedia
                    Just (UserQuery _ userMedia) -> userMedia ++ comp.completeMedia
            userMsg = userQueryToMessages mQuery mediaAttachments
            toolResponses = comp.completeToolResponses
            toolMsgs = concatMap toolResponseToMessages toolResponses
            histMsgs = historyToMessages comp.completeConversationHistory
         in systemMsg : histMsgs ++ userMsg ++ toolMsgs

    historyToMessages :: [Turn] -> [Aeson.Value]
    historyToMessages ts = concatMap turnToMessages (reverse ts)

    turnToMessages :: Turn -> [Aeson.Value]
    turnToMessages (LlmTurn turn _mUsage) =
        case Aeson.parse OpenAI.parseLLMResponse turn.llmResponse.rawResponse of
            Aeson.Error _ -> []
            Aeson.Success rsp -> [Aeson.Object rsp.chosenMessage]
    turnToMessages (UserTurn turn _mUsage) =
        let
            mQuery = turn.userQuery
            -- Extract media from the query
            mediaAttachments = case mQuery of
                Nothing -> []
                Just (UserQuery _ media) -> media
            userMsg = userQueryToMessages mQuery mediaAttachments
            toolResponses = turn.userToolResponses
            toolMsgs = concatMap toolResponseToMessages toolResponses
         in
            userMsg ++ toolMsgs

    -- Convert a UserQuery and media attachments to OpenAI message format
    -- Supports multi-modal content with text and images
    userQueryToMessages :: Maybe UserQuery -> [MediaAttachment] -> [Aeson.Value]
    userQueryToMessages mQuery mediaAttachments = case mQuery of
        Nothing -> []
        Just (UserQuery text [])
            | null mediaAttachments ->
                -- Text-only query with no media
                [ Aeson.object
                    [ "role" .= ("user" :: Text)
                    , "content" .= text
                    ]
                ]
        Just (UserQuery text userMedia) ->
            -- Multi-modal query with text and media
            let allMedia = userMedia ++ mediaAttachments
                contentParts = buildContentParts text allMedia
             in [ Aeson.object
                    [ "role" .= ("user" :: Text)
                    , "content" .= contentParts
                    ]
                ]

    -- Build content parts array for multi-modal messages
    -- The text part comes first, followed by all media attachments
    buildContentParts :: Text -> [MediaAttachment] -> [Aeson.Value]
    buildContentParts text media =
        [ Aeson.object
            [ "type" .= ("text" :: Text)
            , "text" .= text
            ]
        ]
            ++ map mediaAttachmentToContentPart media

    -- Convert a MediaAttachment to OpenAI content part format
    mediaAttachmentToContentPart :: MediaAttachment -> Aeson.Value
    mediaAttachmentToContentPart media =
        let isPdf = "application/pdf" `Text.isPrefixOf` media.mediaMimeType
         in if isPdf
                then -- PDF files use file_data format
                    Aeson.object
                        [ "type" .= ("file" :: Text)
                        , "file"
                            .= Aeson.object
                                [ "filename" .= maybe "document.pdf" id media.mediaFilename
                                , "file_data" .= ("data:" <> media.mediaMimeType <> ";base64," <> media.mediaBase64Data)
                                ]
                        ]
                else -- Images and other media use image_url format
                    Aeson.object
                        [ "type" .= ("image_url" :: Text)
                        , "image_url"
                            .= Aeson.object
                                [ "url" .= ("data:" <> media.mediaMimeType <> ";base64," <> media.mediaBase64Data)
                                ]
                        ]

    -- Convert a tool call/response pair to OpenAI message format
    toolResponseToMessages :: (LlmToolCall, UserToolResponse) -> [Aeson.Value]
    toolResponseToMessages (LlmToolCall callVal, response) =
        -- Extract tool_call_id from the call if available
        let toolCallId = case callVal of
                Aeson.Object obj -> case KeyMap.lookup "id" obj of
                    Just (Aeson.String tid) -> tid
                    _ -> ""
                _ -> ""
         in case response of
                TextResponse txt ->
                    [ Aeson.object
                        [ "role" .= ("tool" :: Text)
                        , "tool_call_id" .= toolCallId
                        , "content" .= txt
                        ]
                    ]
                JsonResponse val ->
                    [ Aeson.object
                        [ "role" .= ("tool" :: Text)
                        , "tool_call_id" .= toolCallId
                        , "content" .= Text.decodeUtf8 (BSL.toStrict $ Aeson.encode val)
                        ]
                    ]
                MediaResponse media ->
                    [ Aeson.object
                        [ "role" .= ("tool" :: Text)
                        , "tool_call_id" .= toolCallId
                        , "content"
                            .= [ Aeson.object
                                    [ "type" .= ("image_url" :: Text)
                                    , "image_url"
                                        .= Aeson.object
                                            [ "url" .= ("data:" <> media.mediaMimeType <> ";base64," <> media.mediaBase64Data)
                                            ]
                                    ]
                               ]
                        ]
                    ]
                MixedResponse parts ->
                    [ Aeson.object
                        [ "role" .= ("tool" :: Text)
                        , "tool_call_id" .= toolCallId
                        , "content" .= map contentPartToOpenAI parts
                        ]
                    ]

    -- Convert a ContentPart to OpenAI format
    contentPartToOpenAI :: ContentPart -> Aeson.Value
    contentPartToOpenAI (TextPart txt) =
        Aeson.object
            [ "type" .= ("text" :: Text)
            , "text" .= txt
            ]
    contentPartToOpenAI (MediaPart media) =
        let isPdf = "application/pdf" `Text.isPrefixOf` media.mediaMimeType
         in if isPdf
                then -- PDF files use file_data format
                    Aeson.object
                        [ "type" .= ("file" :: Text)
                        , "file"
                            .= Aeson.object
                                [ "filename" .= maybe "document.pdf" id media.mediaFilename
                                , "file_data" .= ("data:" <> media.mediaMimeType <> ";base64," <> media.mediaBase64Data)
                                ]
                        ]
                else -- Images and other media use image_url format
                    Aeson.object
                        [ "type" .= ("image_url" :: Text)
                        , "image_url"
                            .= Aeson.object
                                [ "url" .= ("data:" <> media.mediaMimeType <> ";base64," <> media.mediaBase64Data)
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
        let llmRsp =
                LlmResponse
                    { responseText = rsp.rspContent
                    , responseThinking = rsp.rspReasoningContent
                    , rawResponse = rawValue
                    , responseTokenUsage = rsp.rspTokenUsage
                    }
            toolCalls = case rsp.rspToolCalls of
                Nothing -> []
                Just calls -> map toolCallToLlmToolCall calls
         in (llmRsp, toolCalls)

    -- Convert OpenAI OpenAIToolCall to LlmToolCall (wrapping the raw object)
    toolCallToLlmToolCall :: OpenAI.OpenAIToolCall -> LlmToolCall
    toolCallToLlmToolCall tc = LlmToolCall $ Aeson.Object tc.rawToolCall

newConfig :: Tracer IO OpenAI.Trace -> IO OpenAICompletionConfig
newConfig tracer =
    OpenAICompletionConfig
        <$> pure tracer
        <*> (HttpClient.newRuntime =<< (HttpClient.BearerToken . Text.strip <$> Text.readFile "token.txt"))
        <*> pure (OpenAI.ApiBaseUrl "https://api.openai.com/v1")
        <*> pure "gpt-4.1-mini"
        <*> pure OpenAI.OpenAIv1

parseToolCall_openAI :: Aeson.Value -> Maybe OpenAI.OpenAIToolCall
parseToolCall_openAI val =
    case Aeson.parseMaybe Aeson.parseJSON val of
        Just tc -> Just tc
        Nothing ->
            -- Try to extract from our LlmToolCall format
            case val of
                Aeson.Object obj ->
                    case (KeyMap.lookup "id" obj, KeyMap.lookup "function" obj) of
                        (Just (Aeson.String tid), Just funcVal) ->
                            Just $
                                OpenAI.OpenAIToolCall
                                    { OpenAI.rawToolCall = obj
                                    , OpenAI.toolCallId = tid
                                    , OpenAI.toolCallType = KeyMap.lookup "type" obj >>= \v -> case v of Aeson.String t -> Just t; _ -> Nothing
                                    , OpenAI.toolCallFunction = case Aeson.parseMaybe Aeson.parseJSON funcVal of
                                        Just f -> f
                                        Nothing -> OpenAI.ToolCallFunction (OpenAI.ToolName "") "" Nothing
                                    }
                        _ -> Nothing
                _ -> Nothing

