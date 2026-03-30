{-# LANGUAGE OverloadedRecordDot #-}

{- | Compatibility layer for session and tool call conversion.

This module provides functions for converting between CallResult types
and UserToolResponse, maintaining backwards compatibility while
supporting the new media attachment types.
-}
module System.Agents.Session.Compat (
    -- * Tool call parsing
    parseToolCallFromLlmToolCall,

    -- * Response conversion
    callResultToUserToolResponse,
) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as AesonTypes
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text

import qualified System.Agents.LLMs.OpenAI as OpenAI
import System.Agents.Media.Detection (detectFromMagicBytes, isValidUtf8)
import System.Agents.Media.Types
import System.Agents.Session.Types (LlmToolCall (..), UserToolResponse (..))
import System.Agents.Tools.Base (CallResult (..))
import qualified System.Agents.Tools.Bash as BashTools
import qualified System.Agents.Tools.DeveloperToolbox as DeveloperTools
import qualified System.Agents.Tools.IO as IOTools
import qualified System.Agents.Tools.LuaToolbox as LuaTools
import qualified System.Agents.Tools.McpToolbox as McpTools
import qualified System.Agents.Tools.OpenAPI.Types as OpenAPITypes
import qualified System.Agents.Tools.PostgREST.Types as PostgRESTypes
import qualified System.Agents.Tools.SqliteToolbox as SqliteTools
import qualified System.Agents.Tools.SystemToolbox as SystemTools

-------------------------------------------------------------------------------
-- Tool Call Parsing
-------------------------------------------------------------------------------

{- | Parse an LLM tool call from the session representation.

Extracts the tool call information from an LlmToolCall, which wraps
the raw JSON value from the LLM response.
-}
parseToolCallFromLlmToolCall :: LlmToolCall -> Maybe OpenAI.OpenAIToolCall
parseToolCallFromLlmToolCall (LlmToolCall val) =
    case AesonTypes.parseMaybe Aeson.parseJSON val of
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
                                    , OpenAI.toolCallFunction = case AesonTypes.parseMaybe Aeson.parseJSON funcVal of
                                        Just f -> f
                                        Nothing -> OpenAI.ToolCallFunction (OpenAI.ToolName "") "" Nothing
                                    }
                        _ -> Nothing
                _ -> Nothing

-------------------------------------------------------------------------------
-- Response Conversion
-------------------------------------------------------------------------------

{- | Convert a CallResult to a UserToolResponse.

This is the core conversion function that maps tool execution results
to LLM-agnostic user tool responses. The conversion handles:

1. Binary tool outputs with media type hints -> MediaResponse
2. Binary tool outputs without hints -> UTF-8 check, then magic detection
3. Structured results (JSON) -> JsonResponse
4. Error cases -> TextResponse

The lenient UTF-8 decoding ensures that binary data never causes crashes.
-}
callResultToUserToolResponse :: OpenAI.OpenAIToolCall -> CallResult OpenAI.OpenAIToolCall -> UserToolResponse
callResultToUserToolResponse _ result =
    case result of
        BlobToolSuccess _ bytes mHint ->
            case mHint of
                Just (MediaTypeHint mediaType) ->
                    -- Binary media detected - encode as base64
                    MediaResponse $
                        MediaAttachment
                            { mediaMimeType = mediaTypeToMime mediaType
                            , mediaBase64Data = Text.decodeUtf8 (B64.encode bytes)
                            , mediaFilename = Nothing
                            }
                Just BinaryUnknown ->
                    -- Known binary but unknown type - fall back to text with lenient decode
                    TextResponse $ Text.decodeUtf8With Text.lenientDecode bytes
                Nothing ->
                    -- Try text first, fall back to media detection
                    if isValidUtf8 bytes
                        then TextResponse $ Text.decodeUtf8 bytes
                        else
                            -- Try magic byte detection
                            case detectFromMagicBytes bytes of
                                Just mediaType ->
                                    MediaResponse $
                                        MediaAttachment
                                            { mediaMimeType = mediaTypeToMime mediaType
                                            , mediaBase64Data = Text.decodeUtf8 (B64.encode bytes)
                                            , mediaFilename = Nothing
                                            }
                                Nothing -> TextResponse $ Text.decodeUtf8With Text.lenientDecode bytes
        -- Structured results become JsonResponse
        OpenAPIToolResult _ toolResult -> JsonResponse $ Aeson.toJSON toolResult
        PostgRESToolResult _ toolResult -> JsonResponse $ Aeson.toJSON toolResult
        McpToolResult _ res -> JsonResponse $ Aeson.toJSON res
        SqliteToolResult _ toolResult -> JsonResponse $ Aeson.toJSON toolResult
        SystemToolResult _ toolResult -> JsonResponse $ Aeson.toJSON toolResult
        DeveloperToolResult _ valResult -> JsonResponse $ Aeson.toJSON valResult
        DeveloperToolScaffoldResult _ scaffoldResult -> JsonResponse $ Aeson.toJSON scaffoldResult
        DeveloperToolSpecResult _ content -> JsonResponse $ Aeson.String content
        DeveloperToolAgentValidationResult _ result' -> JsonResponse $ Aeson.toJSON result'
        DeveloperToolCreateResult _ result' -> JsonResponse $ Aeson.toJSON result'
        LuaToolResult _ toolResult -> JsonResponse toolResult
        -- Error cases become TextResponse
        ToolNotFound _ -> TextResponse "Tool not found"
        BashToolError _ err -> TextResponse $ Text.pack $ show err
        IOToolError _ err -> TextResponse $ Text.pack $ show err
        McpToolError _ err -> TextResponse $ Text.pack err
        OpenAPIToolError _ err -> TextResponse $ Text.pack err
        PostgRESToolError _ err -> TextResponse $ Text.pack err
        SqliteToolError _ err -> TextResponse $ Text.pack $ show err
        SystemToolError _ err -> TextResponse $ Text.pack $ show err
        DeveloperToolError _ err -> TextResponse $ Text.pack $ show err
        LuaToolError _ err -> err
