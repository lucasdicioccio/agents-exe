{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Provides a runtime value capable to load and use OpenAPI-based tools.
Fetches an OpenAPI specification from a URL and converts endpoints to callable tools.
-}
module System.Agents.Tools.OpenApi where

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

-------------------------------------------------------------------------------
-- Types

data Trace
    = CallingEndpoint !Text !Text  -- method, path
    | EndpointResponse !Int !Text  -- status, content type
    deriving (Show)

data ToolDescription = ToolDescription
    { operationId :: Text
    , summary :: Text
    , description :: Text
    , method :: Text
    , path :: Text
    , parameters :: [Parameter]
    , requestBody :: Maybe RequestBodySchema
    , hasRequiredBody :: Bool
    }
    deriving (Show)

data Parameter = Parameter
    { paramName :: Text
    , paramIn :: ParamLocation
    , paramRequired :: Bool
    , paramSchema :: Aeson.Value
    }
    deriving (Show)

data ParamLocation = PathParam | QueryParam | HeaderParam
    deriving (Show, Eq)

data RequestBodySchema = RequestBodySchema
    { bodySchema :: Aeson.Value
    }
    deriving (Show)

data CallResult
    = CallSuccess Aeson.Value  -- JSON response
    | CallError Int Text       -- status code, error text
    | CallNetworkError Text    -- network error message
    deriving (Show)

-------------------------------------------------------------------------------
-- Tool Execution

makeCallTool :: Tracer IO Trace -> Text -> Maybe Text -> ToolDescription -> Aeson.Object -> IO CallResult
makeCallTool tracer baseUrl mToken desc args = do
    let path = substitutePathParams desc.path args
        queryParams = extractQueryParams desc.parameters args
    
    trace tracer (CallingEndpoint desc.method path)
    
    let url = Text.unpack $ baseUrl <> path
    req <- parseRequest url
    
    let reqWithMethod = case Text.toUpper desc.method of
            "GET" -> setRequestMethod "GET" req
            "POST" -> setRequestMethod "POST" req
            "PUT" -> setRequestMethod "PUT" req
            "PATCH" -> setRequestMethod "PATCH" req
            "DELETE" -> setRequestMethod "DELETE" req
            m -> setRequestMethod (TextEnc.encodeUtf8 m) req
    
    let reqWithQuery = foldr (\(k, v) r -> setRequestQueryString [(TextEnc.encodeUtf8 k, Just (TextEnc.encodeUtf8 v))] r) reqWithMethod queryParams
    
    let reqWithHeaders = setRequestHeader "Content-Type" ["application/json"] reqWithQuery
    
    let reqWithAuth = case mToken of
            Nothing -> reqWithHeaders
            Just tok -> setRequestHeader "Authorization" ["Bearer " <> TextEnc.encodeUtf8 tok] reqWithHeaders
    
    -- Add body if present
    let reqFinal = case (desc.requestBody, KeyMap.lookup "b" args) of
            (Just _, Just bodyVal) -> setRequestBodyLBS (Aeson.encode bodyVal) reqWithAuth
            _ -> reqWithAuth
    
    response <- tryHttp $ httpLBS reqFinal
    
    case response of
        Left err -> do
            pure $ CallNetworkError (Text.pack $ show err)
        Right rsp -> do
            let status = getResponseStatusCode rsp
                contentType = getResponseHeader "Content-Type" rsp
            trace tracer (EndpointResponse status (Text.pack $ show contentType))
            
            let body = getResponseBody rsp
            if status >= 200 && status < 300
                then case Aeson.decode body of
                    Just val -> pure $ CallSuccess val
                    Nothing -> pure $ CallSuccess (Aeson.String $ TextEnc.decodeUtf8With TextEncErr.lenientDecode $ LBS.toStrict body)
                else pure $ CallError status (Text.pack $ LBS.unpack body)

tryHttp :: IO a -> IO (Either HttpException a)
tryHttp action = catch (Right <$> action) (\e -> pure $ Left (e :: HttpException))

substitutePathParams :: Text -> Aeson.Object -> Text
substitutePathParams path args = 
    let parts = Text.split (== '{') path
        replacePart part = case Text.break (== '}') part of
            (before, after) | Text.null after -> part
            (paramName, after) -> 
                let key = AesonKey.fromText ("p_" <> paramName)
                in case KeyMap.lookup key args of
                    Just (Aeson.String s) -> s <> Text.drop 1 after
                    Just other -> Text.pack (show other) <> Text.drop 1 after
                    Nothing -> part
    in Text.concat $ map replacePart parts

extractQueryParams :: [Parameter] -> Aeson.Object -> [(Text, Text)]
extractQueryParams params args = 
    concatMap extract params
  where
    extract p | paramIn p == QueryParam = 
        let key = AesonKey.fromText ("p_" <> paramName p)
        in case KeyMap.lookup key args of
            Just (Aeson.String s) -> [(paramName p, s)]
            Just other -> [(paramName p, Text.pack $ show other)]
            Nothing -> []
    extract _ = []

-------------------------------------------------------------------------------
-- Helper

trace :: Tracer IO Trace -> Trace -> IO ()
trace (Tracer f) t = f t


