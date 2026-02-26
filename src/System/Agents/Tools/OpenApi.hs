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
import Data.Aeson ((.=),(.:))
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
    | EndpointResponse !Int !Text  -- status code, content type
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
-- Predicate Types for Filtering OpenAPI Endpoints

-- | A predicate for filtering OpenAPI endpoints.
-- Used to subset which endpoints are exposed as tools.
-- Multiple predicates can be combined with 'And', 'Or', and 'Not'.
data ToolPredicate
    = MatchMethod !Text              -- ^ Match by HTTP method (e.g., "GET", "POST")
    | MatchPathPrefix !Text          -- ^ Match by path prefix (e.g., "/api/v1")
    | MatchPathRegex !Text           -- ^ Match by path regex pattern
    | And ![ToolPredicate]           -- ^ Conjunction: all predicates must match
    | Or ![ToolPredicate]            -- ^ Disjunction: at least one predicate must match
    | Not !ToolPredicate             -- ^ Negation: predicate must not match
    deriving (Show, Eq, Ord, Generic)

instance Aeson.FromJSON ToolPredicate where
    parseJSON = Aeson.withObject "ToolPredicate" $ \o -> do
        tag <- o .: "tag"
        case (tag :: Text) of
            "MatchMethod" -> MatchMethod <$> o .: "method"
            "MatchPathPrefix" -> MatchPathPrefix <$> o .: "prefix"
            "MatchPathRegex" -> MatchPathRegex <$> o .: "pattern"
            "And" -> And <$> o .: "predicates"
            "Or" -> Or <$> o .: "predicates"
            "Not" -> Not <$> o .: "predicate"
            _ -> fail $ "Unknown ToolPredicate tag: " <> Text.unpack tag

instance Aeson.ToJSON ToolPredicate where
    toJSON pred = case pred of
        MatchMethod m -> Aeson.object ["tag" .= ("MatchMethod" :: Text), "method" .= m]
        MatchPathPrefix p -> Aeson.object ["tag" .= ("MatchPathPrefix" :: Text), "prefix" .= p]
        MatchPathRegex r -> Aeson.object ["tag" .= ("MatchPathRegex" :: Text), "pattern" .= r]
        And ps -> Aeson.object ["tag" .= ("And" :: Text), "predicates" .= ps]
        Or ps -> Aeson.object ["tag" .= ("Or" :: Text), "predicates" .= ps]
        Not p -> Aeson.object ["tag" .= ("Not" :: Text), "predicate" .= p]

-- | Type alias for a conjunction of predicates.
-- All predicates in the list must match for a tool to be included.
-- An empty list means "match all".
type ConjunctionPredicates = [ToolPredicate]

-- | Evaluate a predicate against a tool description.
evalPredicate :: ToolPredicate -> ToolDescription -> Bool
evalPredicate pred desc = case pred of
    MatchMethod m -> Text.toUpper desc.method == Text.toUpper m
    MatchPathPrefix p -> p `Text.isPrefixOf` desc.path
    MatchPathRegex r -> desc.path =~ r
    And ps -> all (\p -> evalPredicate p desc) ps
    Or ps -> any (\p -> evalPredicate p desc) ps
    Not p -> not (evalPredicate p desc)

-- | Evaluate a conjunction of predicates against a tool description.
-- All predicates must match (returns True for empty list).
evalConjunction :: ConjunctionPredicates -> ToolDescription -> Bool
evalConjunction preds desc = all (\p -> evalPredicate p desc) preds

-- | Match a Text against a regex pattern (using simple glob-like matching for now).
-- Supports:
-- - "*" matches any sequence of characters
-- - "?" matches any single character
-- - "**" matches any sequence including "/"
(=~) :: Text -> Text -> Bool
txt =~ pat = matchGlob (Text.unpack txt) (Text.unpack pat)
  where
    matchGlob :: String -> String -> Bool
    matchGlob [] [] = True
    matchGlob _ [] = False
    matchGlob [] ('*':ps) = matchGlob [] ps
    matchGlob [] ('*':'*':ps) = matchGlob [] ps
    matchGlob [] (_:_) = False
    matchGlob (t:ts) ('*':'*':ps) = 
        matchGlob ts ('*':'*':ps) || matchGlob ts ps || matchGlob (t:ts) ps
    matchGlob (t:ts) ('*':ps) = 
        matchGlob ts ps || matchGlob (t:ts) ps
    matchGlob (t:ts) ('?':ps) = matchGlob ts ps
    matchGlob (t:ts) (p:ps) = t == p && matchGlob ts ps

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

