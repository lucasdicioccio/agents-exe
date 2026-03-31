{-# LANGUAGE RankNTypes #-}

-- LlmToolCall still is an opaque Aeson.Value wrapper and
-- historycally an LlmToolCall may have contained OpenAI function calls stored as JSON while the internal need was unclear.
-- Thus we need this compat layer while we transition to a new world where LlmToolCall has a better-understood structure with enough info to map back and forth.
module System.Agents.Session.Compat where

import Control.Applicative ((<|>))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding.Error (lenientDecode)

import qualified System.Agents.LLMs.OpenAI as OpenAI
import qualified System.Agents.Session.OpenAI as OpenAI
import System.Agents.Session.Types (LlmToolCall (..), UserToolResponse (..))
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
    openai = ToolCall_OpenApi <$> OpenAI.parseToolCall_openAI val

callResultToUserToolResponse ::
    forall toolcall.
    toolcall ->
    CallResult toolcall ->
    UserToolResponse
callResultToUserToolResponse _ result =
    case result of
        ToolNotFound _ ->
            UserToolResponse $ Aeson.String "Tool not found"
        BashToolError _ err ->
            UserToolResponse $ Aeson.String $ Text.pack $ show err
        IOToolError _ err ->
            UserToolResponse $ Aeson.String $ Text.pack $ show err
        McpToolError _ err ->
            UserToolResponse $ Aeson.String $ Text.unlines ["tool-error", Text.pack $ show err]
        McpToolResult _ res ->
            UserToolResponse $ Aeson.toJSON res
        BlobToolSuccess _ v ->
            -- Use lenient UTF-8 decoding to handle binary data safely.
            -- Invalid bytes are replaced with U+FFFD (replacement character).
            UserToolResponse $ Aeson.String $ Text.decodeUtf8With lenientDecode v
        OpenAPIToolResult _ toolResult ->
            UserToolResponse $ Aeson.toJSON toolResult
        OpenAPIToolError _ err ->
            UserToolResponse $ Aeson.String $ Text.pack $ "OpenAPI tool error: " <> err
        PostgRESToolResult _ toolResult ->
            UserToolResponse $ Aeson.toJSON toolResult
        PostgRESToolError _ err ->
            UserToolResponse $ Aeson.String $ Text.pack $ "PostgREST tool error: " <> err
        SqliteToolResult _ toolResult ->
            UserToolResponse $ Aeson.toJSON toolResult
        SqliteToolError _ err ->
            UserToolResponse $ Aeson.String $ Text.pack $ "SQLite tool error: " <> show err
        SystemToolResult _ toolResult ->
            UserToolResponse $ Aeson.toJSON toolResult
        SystemToolError _ err ->
            UserToolResponse $ Aeson.String $ Text.pack $ "System tool error: " <> show err
        DeveloperToolResult _ valResult ->
            UserToolResponse $ Aeson.toJSON valResult
        DeveloperToolScaffoldResult _ scaffoldResult ->
            UserToolResponse $ Aeson.toJSON scaffoldResult
        DeveloperToolSpecResult _ content ->
            UserToolResponse $ Aeson.String content
        DeveloperToolAgentValidationResult _ validationResult ->
            UserToolResponse $ Aeson.toJSON validationResult
        DeveloperToolCreateResult _ createResult ->
            UserToolResponse $ Aeson.toJSON createResult
        DeveloperToolError _ err ->
            UserToolResponse $ Aeson.String $ Text.pack $ "Developer tool error: " <> show err
        LuaToolResult _ toolResult ->
            UserToolResponse $ Aeson.toJSON toolResult
        LuaToolError _ err ->
            UserToolResponse $ Aeson.String $ "Lua tool error: " <> err
