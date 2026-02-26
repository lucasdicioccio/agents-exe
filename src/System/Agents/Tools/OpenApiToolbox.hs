{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

{- | Provides a runtime value capable to load and use OpenAPI-based tools.
Fetches an OpenAPI specification from a URL and converts endpoints to callable tools.
-}
module System.Agents.Tools.OpenApiToolbox where

import Control.Concurrent.Async (Async, async)
import Control.Concurrent.STM (STM, TVar, atomically, newTVarIO, writeTVar)
import Control.Exception (catch)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text.Encoding.Error as TextEncErr
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import GHC.Generics (Generic)
import Network.HTTP.Simple
import Prod.Tracer (Tracer (..), contramap)

import qualified System.Agents.Tools.IO as IOTools
import System.Agents.Tools.Base ()
import System.Agents.Tools.OpenApi (CallResult(..), makeCallTool, ToolDescription(..), RequestBodySchema(..), Parameter(..), ParamLocation(..))
import qualified System.Agents.Tools.OpenApi as OpenApi

-------------------------------------------------------------------------------
-- Types

data Trace
    = FetchingOpenApiSpec !Text
    | FetchedOpenApiSpec !Int !Int  -- status code, num tools
    | ParsingOpenApiSpecError !String
    | EndpointTrace !OpenApi.Trace
    deriving (Show)

data Toolbox = Toolbox
    { name :: Text
    , baseUrl :: Text
    , toolsList :: TVar [ToolDescription]
    , callTool :: ToolDescription -> Aeson.Object -> IO CallResult
    }

data OpenApiSpec = OpenApiSpec
    { specPaths :: HashMap Text (HashMap Text Operation)
    , specComponents :: Aeson.Value
    }
    deriving (Show)

data Operation = Operation
    { opOperationId :: Maybe Text
    , opSummary :: Maybe Text
    , opDescription :: Maybe Text
    , opParameters :: [Aeson.Value]
    , opRequestBody :: Maybe Aeson.Value
    }
    deriving (Show, Generic)

instance Aeson.FromJSON Operation where
    parseJSON = Aeson.withObject "Operation" $ \o -> Operation
        <$> o Aeson..:? "operationId"
        <*> o Aeson..:? "summary"
        <*> o Aeson..:? "description"
        <*> o Aeson..:? "parameters" Aeson..!= []
        <*> o Aeson..:? "requestBody"

-------------------------------------------------------------------------------
-- Initialization

initializeToolbox ::
    Tracer IO Trace ->
    Text ->           -- toolbox name
    Text ->           -- base URL
    Text ->           -- OpenAPI spec URL
    Maybe Text ->     -- optional auth token
    IO Toolbox
initializeToolbox tracer tname baseUrl specUrl mToken = do
    trace tracer (FetchingOpenApiSpec specUrl)
    
    -- Fetch the OpenAPI spec
    req <- parseRequest (Text.unpack specUrl)
    let reqWithAuth = case mToken of
            Nothing -> req
            Just tok -> setRequestHeader "Authorization" ["Bearer " <> TextEnc.encodeUtf8 tok] req
    
    response <- httpLBS reqWithAuth
    let status = getResponseStatusCode response
    
    if status /= 200
        then do
            trace tracer (ParsingOpenApiSpecError $ "HTTP " ++ show status)
            emptyTools <- newTVarIO []
            pure $ Toolbox tname baseUrl emptyTools (\_ _ -> pure $ CallNetworkError "Failed to fetch spec")
        else do
            let body = getResponseBody response
            case Aeson.decode body of
                Nothing -> do
                    trace tracer (ParsingOpenApiSpecError "Invalid JSON")
                    emptyTools <- newTVarIO []
                    pure $ Toolbox tname baseUrl emptyTools (\_ _ -> pure $ CallNetworkError "Invalid spec JSON")
                Just spec -> do
                    let tools = convertSpecToTools spec
                    trace tracer (FetchedOpenApiSpec status (length tools))
                    toolsVar <- newTVarIO tools
                    pure $ Toolbox
                        { name = tname
                        , baseUrl = baseUrl
                        , toolsList = toolsVar
                        , callTool = makeCallTool (contramap EndpointTrace tracer) baseUrl mToken
                        }

-------------------------------------------------------------------------------
-- Schema Resolution

resolveSchema :: Aeson.Value -> Aeson.Value -> Aeson.Value
resolveSchema components schema = case schema of
    Aeson.Object obj -> 
        case KeyMap.lookup "$ref" obj of
            Just refVal -> 
                let refPath = parseRef refVal
                    resolved = lookupRef components refPath
                in resolveSchema components resolved
            Nothing -> Aeson.Object $ KeyMap.map (resolveSchema components) obj
    Aeson.Array arr -> Aeson.Array $ fmap (resolveSchema components) arr
    other -> other

parseRef :: Aeson.Value -> [Text]
parseRef (Aeson.String s) = 
    let stripped = Text.dropWhile (== '#') $ Text.dropWhile (== '/') s
    in Text.split (== '/') stripped
parseRef _ = []

lookupRef :: Aeson.Value -> [Text] -> Aeson.Value
lookupRef _ [] = Aeson.Object mempty
lookupRef (Aeson.Object obj) (key:rest) = 
    case KeyMap.lookup (AesonKey.fromText key) obj of
        Just val -> lookupRef val rest
        Nothing -> Aeson.Object mempty
lookupRef val (_:rest) = lookupRef val rest
lookupRef val [] = val

-------------------------------------------------------------------------------
-- Tool Conversion

convertSpecToTools :: Aeson.Value -> [ToolDescription]
convertSpecToTools spec = 
    let components = case spec of
            Aeson.Object obj -> fromMaybe (Aeson.Object mempty) (KeyMap.lookup "components" obj)
            _ -> Aeson.Object mempty
        paths = case spec of
            Aeson.Object obj -> fromMaybe Aeson.Null (KeyMap.lookup "paths" obj)
            _ -> Aeson.Object mempty
    in parsePaths components paths

parsePaths :: Aeson.Value -> Aeson.Value -> [ToolDescription]
parsePaths components (Aeson.Object paths) = 
    concatMap (\(pathKey, methods) -> parseMethods components (AesonKey.toText pathKey) methods) (KeyMap.toList paths)
parsePaths _ _ = []

parseMethods :: Aeson.Value -> Text -> Aeson.Value -> [ToolDescription]
parseMethods components path (Aeson.Object methods) =
    concatMap (\(methodKey, operation) -> 
        case parseOperation components path (AesonKey.toText methodKey) operation of
            Just desc -> [desc]
            Nothing -> []) (KeyMap.toList methods)
parseMethods _ _ _ = []

parseOperation :: Aeson.Value -> Text -> Text -> Aeson.Value -> Maybe ToolDescription
parseOperation components path method opVal = do
    op <- Aeson.decode (Aeson.encode opVal)
    opId <- opOperationId op
    
    let params = parseParameters components (opParameters op)
        mBody = parseRequestBody components (opRequestBody op)
        hasRequired = case opRequestBody op of
            Just (Aeson.Object obj) -> fromMaybe False (KeyMap.lookup "required" obj >>= \case Aeson.Bool b -> Just b; _ -> Nothing)
            _ -> False
    
    pure $ ToolDescription
        { operationId = opId
        , summary = fromMaybe "" (opSummary op)
        , description = fromMaybe "" (opDescription op)
        , method = Text.toUpper method
        , path = path
        , parameters = params
        , requestBody = mBody
        , hasRequiredBody = hasRequired
        }

parseParameters :: Aeson.Value -> [Aeson.Value] -> [Parameter]
parseParameters components = concatMap parseParam
  where
    parseParam val = case val of
        Aeson.Object obj -> 
            let name = fromMaybe "" (KeyMap.lookup "name" obj >>= \case Aeson.String s -> Just s; _ -> Nothing)
                in_ = case KeyMap.lookup "in" obj of
                    Just (Aeson.String "path") -> PathParam
                    Just (Aeson.String "query") -> QueryParam
                    Just (Aeson.String "header") -> HeaderParam
                    _ -> QueryParam
                required = fromMaybe False (KeyMap.lookup "required" obj >>= \case Aeson.Bool b -> Just b; _ -> Nothing)
                schema = case KeyMap.lookup "schema" obj of
                    Just s -> resolveSchema components s
                    Nothing -> Aeson.Object mempty
            in [Parameter name in_ required schema]
        _ -> []

parseRequestBody :: Aeson.Value -> Maybe Aeson.Value -> Maybe RequestBodySchema
parseRequestBody components mBody = do
    body <- mBody
    Aeson.Object bodyObj <- pure body
    content <- KeyMap.lookup "content" bodyObj
    Aeson.Object contentObj <- pure content
    jsonContent <- KeyMap.lookup "application/json" contentObj
    Aeson.Object jsonObj <- pure jsonContent
    schema <- KeyMap.lookup "schema" jsonObj
    let resolved = resolveSchema components schema
    pure $ RequestBodySchema resolved

-------------------------------------------------------------------------------
-- Helper

trace :: Tracer IO Trace -> Trace -> IO ()
trace (Tracer f) t = f t

