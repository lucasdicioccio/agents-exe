{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}

{- |
Compatibility and migration utilities for session handling.

This module provides functions for:
* Converting between different tool call formats (legacy and modern)
* Converting tool results to user responses
* Session version migration helpers

LlmToolCall is still an opaque Aeson.Value wrapper and historically an
LlmToolCall may have contained OpenAI function calls stored as JSON while
the internal need was unclear. Thus we need this compat layer while we
transition to a new world where LlmToolCall has a better-understood structure
with enough info to map back and forth.
-}
module System.Agents.Session.Compat (
    -- * Tool Call Parsing
    CompatToolCall (..),
    parseToolCallFromLlmToolCall,
    parseToolCall,
    parseLlmToolCall_Compat,

    -- * Result Conversion
    callResultToUserToolResponse,

    -- * Session Migration
    migrateSessionToV2,
    migrateTurnToV2,
    ensureSessionVersion,
) where

import Control.Applicative ((<|>))
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import qualified System.Agents.LLMs.OpenAI as OpenAI
import System.Agents.Media.Types (MediaAttachment (..), mediaTypeToMime)
import System.Agents.Session.Types (
    LlmToolCall (..),
    Session (..),
    Turn (..),
    UserToolResponse (..),
    migrateSessionV1ToV2,
 )
import System.Agents.ToolSchema (ToolName (..))
import System.Agents.Tools.Base (CallResult (..))
import System.Agents.Tools.Context (ToolCall (..))

data CompatToolCall
    = ToolCall_Native ToolCall
    | ToolCall_OpenApi OpenAI.OpenAIToolCall

parseToolCallFromLlmToolCall :: LlmToolCall -> Maybe ToolCall
parseToolCallFromLlmToolCall (LlmToolCall val) = parseToolCall val

parseToolCall :: Aeson.Value -> Maybe ToolCall
parseToolCall val = f =<< parseLlmToolCall_Compat val
  where
    f :: CompatToolCall -> Maybe ToolCall
    f (ToolCall_Native tc) = Just tc
    f (ToolCall_OpenApi tc) =
        case tc.toolCallFunction.toolCallFunctionArgs of
            Nothing -> Nothing
            Just args -> Just $ ToolCall tc.toolCallFunction.toolCallFunctionName.getToolName args

parseLlmToolCall_Compat :: Aeson.Value -> Maybe CompatToolCall
parseLlmToolCall_Compat val =
    native <|> openai
  where
    native :: Maybe CompatToolCall
    native = ToolCall_Native <$> Aeson.parseMaybe Aeson.parseJSON val

    openai :: Maybe CompatToolCall
    openai = ToolCall_OpenApi <$> parseToolCall_openAI val

{- | Parse an OpenAI-style tool call from a JSON value.
This is a local implementation since OpenAI.parseToolCall_openAI is not exported.
-}
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

{- | Convert a CallResult to a UserToolResponse.

This function handles the conversion from internal tool results to the
session's user-facing response type. Key behaviors:

* BlobToolSuccess with Just MediaType -> MediaResponse (base64-encoded)
* BlobToolSuccess with Nothing and valid UTF-8 -> TextResponse
* BlobToolSuccess with Nothing and binary -> MediaResponse (octet-stream)
* Structured results -> JsonResponse
* Errors -> TextResponse with error message
-}
callResultToUserToolResponse ::
    forall toolcall.
    toolcall ->
    CallResult toolcall ->
    UserToolResponse
callResultToUserToolResponse _ result =
    case result of
        -- Blob with explicit media type -> base64 MediaResponse
        BlobToolSuccess _ bytes (Just mediaType) ->
            MediaResponse $
                MediaAttachment
                    { mediaMimeType = mediaTypeToMime mediaType
                    , mediaBase64Data = base64Encode bytes
                    , mediaFilename = Nothing
                    }
        -- Blob without media type -> try UTF-8 first, then fall back to binary
        BlobToolSuccess _ bytes Nothing ->
            case Text.decodeUtf8' bytes of
                Right txt -> TextResponse txt
                Left _ ->
                    -- Binary data without media type -> encode as generic binary
                    MediaResponse $
                        MediaAttachment
                            { mediaMimeType = "application/octet-stream"
                            , mediaBase64Data = base64Encode bytes
                            , mediaFilename = Nothing
                            }
        -- Structured results -> JsonResponse
        McpToolResult _ res -> JsonResponse $ Aeson.toJSON res
        OpenAPIToolResult _ toolResult -> JsonResponse $ Aeson.toJSON toolResult
        OpenAPIToolError _ err ->
            JsonResponse $ Aeson.object ["error" .= ("OpenAPI tool error: " <> err)]
        PostgRESToolResult _ toolResult -> JsonResponse $ Aeson.toJSON toolResult
        PostgRESToolError _ err ->
            JsonResponse $ Aeson.object ["error" .= ("PostgREST tool error: " <> err)]
        SqliteToolResult _ toolResult -> JsonResponse $ Aeson.toJSON toolResult
        SqliteToolError _ err ->
            JsonResponse $ Aeson.object ["error" .= ("SQLite tool error: " <> show err)]
        SystemToolResult _ toolResult -> JsonResponse $ Aeson.toJSON toolResult
        SystemToolError _ err ->
            JsonResponse $ Aeson.object ["error" .= ("System tool error: " <> show err)]
        DeveloperToolResult _ valResult -> JsonResponse $ Aeson.toJSON valResult
        DeveloperToolScaffoldResult _ scaffoldResult -> JsonResponse $ Aeson.toJSON scaffoldResult
        DeveloperToolSpecResult _ content -> TextResponse content
        DeveloperToolAgentValidationResult _ validationResult -> JsonResponse $ Aeson.toJSON validationResult
        DeveloperToolCreateResult _ createResult -> JsonResponse $ Aeson.toJSON createResult
        DeveloperToolReadFileRangeResult _ readResult -> JsonResponse $ Aeson.toJSON readResult
        DeveloperToolWriteFileRangeResult _ writeResult -> JsonResponse $ Aeson.toJSON writeResult
        DeveloperToolPatchResult _ patchResult -> JsonResponse $ Aeson.toJSON patchResult
        DeveloperToolError _ err ->
            JsonResponse $ Aeson.object ["error" .= ("Developer tool error: " <> show err)]
        LuaToolResult _ toolResult -> JsonResponse toolResult
        LuaToolError _ err ->
            JsonResponse $ Aeson.object ["error" .= ("Lua tool error: " <> err)]
        -- Error cases
        ToolNotFound _ ->
            JsonResponse $ Aeson.object ["error" .= ("Tool not found" :: Text)]
        BashToolError _ err ->
            JsonResponse $ Aeson.object ["error" .= ("Bash tool error: " <> Text.pack (show err))]
        IOToolError _ err ->
            JsonResponse $ Aeson.object ["error" .= ("IO tool error: " <> Text.pack (show err))]
        McpToolError _ err ->
            JsonResponse $ Aeson.object ["error" .= ("MCP tool error: " <> Text.pack (show err))]

-- | Base64 encode ByteString to Text.
base64Encode :: BS.ByteString -> Text
base64Encode = Text.decodeUtf8 . B64.encode

-------------------------------------------------------------------------------
-- Session Migration
-------------------------------------------------------------------------------

{- | Migrate any session to version 2 (async/resumable support).

This function ensures a session has the latest structure by:
1. Migrating from legacy (Nothing) or V1 to V2
2. Setting default execution mode to Synchronous for existing sessions
3. Converting any legacy turn formats

This is idempotent - calling it on an already V2 session is a no-op.
-}
migrateSessionToV2 :: Session -> Session
migrateSessionToV2 session =
    case session.sessionVersion of
        Nothing -> migrateSessionV1ToV2 (migrateV0ToV1 session)
        Just 1 -> migrateSessionV1ToV2 session
        Just 2 -> session
        Just _ -> migrateSessionV1ToV2 session -- Future versions: handle appropriately
  where
    migrateV0ToV1 :: Session -> Session
    migrateV0ToV1 s = s{sessionVersion = Just 1}

{- | Migrate a single turn to V2 format.

Handles any necessary conversions for individual turns.
Currently this is a no-op since turns are backward-compatible,
but provides a hook for future migrations.
-}
migrateTurnToV2 :: Turn -> Turn
migrateTurnToV2 turn = turn

{- | Ensure a session has the proper version and execution mode.

This is a convenience function for use when loading sessions from storage.
It ensures backward compatibility with older session formats.

Example:

@
rawSession <- loadSessionFromDisk sessionId
let session = ensureSessionVersion rawSession
-- Now safe to use with modern agent code
@
-}
ensureSessionVersion :: Session -> Session
ensureSessionVersion = migrateSessionToV2

