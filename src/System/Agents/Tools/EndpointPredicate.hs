{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Endpoint predicate filtering for OpenAPI and PostgREST toolboxes.

This module provides a predicate calculus for filtering API endpoints
based on their paths, methods, and operation IDs. The predicates are
designed to be serializable from JSON for user configuration.

The primary use case is to reduce the number of tools exposed to agents
when working with large APIs like OpenAPI or PostgREST specs that may
generate many spurious tools. Users can define filters in their agent
configuration to include only the endpoints they need.

Example JSON configuration:

@
{
  "filter": {
    "tag": "And",
    "contents": [
      {"tag": "PathPrefix", "contents": "/api/v1"},
      {"tag": "Not", "contents": {"tag": "PathSuffix", "contents": "/admin"}}
    ]
  }
}
@
-}
module System.Agents.Tools.EndpointPredicate (
    -- * Predicate type
    EndpointPredicate (..),

    -- * Endpoint info type
    EndpointInfo (..),

    -- * Matching functions
    matchesEndpoint,
    matchesTool,
    matchesOpenAPITool,
    matchesPostgRESTool,

    -- * Path utilities
    splitPath,
    normalizePath,
    isPathPrefixOf,
    isPathSuffixOf,

    -- * Re-exports for convenience
    Text,
) where

import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.List (isPrefixOf, isSuffixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)

import qualified System.Agents.Tools.OpenAPI.Converter as OpenAPI
import qualified System.Agents.Tools.OpenAPI.Types as OpenAPITypes
import qualified System.Agents.Tools.PostgREST.Converter as PostgREST

-- -------------------------------------------------------------------------
-- Endpoint Predicate Type
-- -------------------------------------------------------------------------

{- | A predicate for filtering API endpoints.

Predicates can be combined using boolean logic (AND, OR, NOT) and
can match on various endpoint properties like path prefixes, suffixes,
or exact matches.

The predicates are designed to be JSON-serializable for configuration
in agent definitions.
-}
data EndpointPredicate
    = -- | Logical AND of two predicates
      And EndpointPredicate EndpointPredicate
    | -- | Logical OR of two predicates
      Or EndpointPredicate EndpointPredicate
    | -- | Logical NOT of a predicate
      Not EndpointPredicate
    | -- | Match paths starting with the given prefix
      PathPrefix Text
    | -- | Match paths ending with the given suffix
      PathSuffix Text
    | -- | Match paths exactly equal to the given path
      PathEqual Text
    | -- | Match paths containing the given substring
      PathContains Text
    | -- | Match HTTP methods (case-insensitive)
      MethodEquals Text
    | -- | Match OpenAPI operation IDs exactly
      OperationIdEquals Text
    | -- | Match PostgREST table names
      TableNameEquals Text
    | -- | Always returns True
      AlwaysTrue
    | -- | Always returns False
      AlwaysFalse
    deriving (Show, Eq, Ord, Generic)

-- -------------------------------------------------------------------------
-- JSON Serialization
-- -------------------------------------------------------------------------

{- | Custom JSON instances for tagged union format.

Example JSON:

@
{"tag": "PathPrefix", "contents": "/api/v1"}
{"tag": "And", "contents": [{"tag": "PathPrefix", "contents": "/api"}, {"tag": "Not", "contents": {"tag": "PathSuffix", "contents": "/admin"}}]}
@
-}
instance ToJSON EndpointPredicate where
    toJSON (And p1 p2) =
        Aeson.object
            [ "tag" .= ("And" :: Text)
            , "contents" .= [p1, p2]
            ]
    toJSON (Or p1 p2) =
        Aeson.object
            [ "tag" .= ("Or" :: Text)
            , "contents" .= [p1, p2]
            ]
    toJSON (Not p) =
        Aeson.object
            [ "tag" .= ("Not" :: Text)
            , "contents" .= p
            ]
    toJSON (PathPrefix path) =
        Aeson.object
            [ "tag" .= ("PathPrefix" :: Text)
            , "contents" .= path
            ]
    toJSON (PathSuffix suffix) =
        Aeson.object
            [ "tag" .= ("PathSuffix" :: Text)
            , "contents" .= suffix
            ]
    toJSON (PathEqual path) =
        Aeson.object
            [ "tag" .= ("PathEqual" :: Text)
            , "contents" .= path
            ]
    toJSON (PathContains substr) =
        Aeson.object
            [ "tag" .= ("PathContains" :: Text)
            , "contents" .= substr
            ]
    toJSON (MethodEquals method) =
        Aeson.object
            [ "tag" .= ("MethodEquals" :: Text)
            , "contents" .= method
            ]
    toJSON (OperationIdEquals opId) =
        Aeson.object
            [ "tag" .= ("OperationIdEquals" :: Text)
            , "contents" .= opId
            ]
    toJSON (TableNameEquals table) =
        Aeson.object
            [ "tag" .= ("TableNameEquals" :: Text)
            , "contents" .= table
            ]
    toJSON AlwaysTrue =
        Aeson.object
            [ "tag" .= ("AlwaysTrue" :: Text)
            ]
    toJSON AlwaysFalse =
        Aeson.object
            [ "tag" .= ("AlwaysFalse" :: Text)
            ]

-- | Parse a predicate from JSON using tagged union format.
instance FromJSON EndpointPredicate where
    parseJSON = Aeson.withObject "EndpointPredicate" $ \o -> do
        tag <- o .: "tag"
        case (tag :: Text) of
            "And" -> do
                contents <- o .: "contents"
                case contents of
                    [p1, p2] -> pure $ And p1 p2
                    _ -> fail "And expects exactly two predicates in contents array"
            "Or" -> do
                contents <- o .: "contents"
                case contents of
                    [p1, p2] -> pure $ Or p1 p2
                    _ -> fail "Or expects exactly two predicates in contents array"
            "Not" -> Not <$> (o .: "contents")
            "PathPrefix" -> PathPrefix <$> (o .: "contents")
            "PathSuffix" -> PathSuffix <$> (o .: "contents")
            "PathEqual" -> PathEqual <$> (o .: "contents")
            "PathContains" -> PathContains <$> (o .: "contents")
            "MethodEquals" -> MethodEquals <$> (o .: "contents")
            "OperationIdEquals" -> OperationIdEquals <$> (o .: "contents")
            "TableNameEquals" -> TableNameEquals <$> (o .: "contents")
            "AlwaysTrue" -> pure AlwaysTrue
            "AlwaysFalse" -> pure AlwaysFalse
            other -> fail $ "Unknown predicate tag: " ++ Text.unpack other

-- -------------------------------------------------------------------------
-- Path Utilities
-- -------------------------------------------------------------------------

{- | Split a path into its components.

Examples:

>>> splitPath "/api/v1/users"
["api","v1","users"]

>>> splitPath "/"
[]

>>> splitPath ""
[]
-}
splitPath :: Text -> [Text]
splitPath = filter (not . Text.null) . Text.splitOn "/" . normalizePath

{- | Normalize a path by:
* Removing trailing slashes
* Ensuring it starts with /
* Collapsing multiple consecutive slashes

Examples:

>>> normalizePath "api/v1/users"
"/api/v1/users"

>>> normalizePath "/api/v1/users/"
"/api/v1/users"

>>> normalizePath "//api///v1//users//"
"/api/v1/users"
-}
normalizePath :: Text -> Text
normalizePath path
    | Text.null path = "/"
    | otherwise =
        let
            -- Ensure leading slash
            withLeading = if Text.isPrefixOf "/" path then path else "/" <> path
            -- Remove trailing slash (but keep root /)
            withoutTrailing =
                if Text.length withLeading > 1 && Text.isSuffixOf "/" withLeading
                    then Text.init withLeading
                    else withLeading
            -- Collapse multiple slashes
            collapsed = Text.intercalate "/" $ filter (not . Text.null) $ Text.splitOn "/" withoutTrailing
         in
            if Text.null collapsed then "/" else "/" <> collapsed

-- -------------------------------------------------------------------------
-- Matching Functions
-- -------------------------------------------------------------------------

{- | Endpoint information used for matching.

This type captures the key information about an endpoint that
can be matched against predicates.
-}
data EndpointInfo = EndpointInfo
    { endpointPath :: Text
    , endpointMethod :: Text
    , endpointOperationId :: Maybe Text
    , endpointTableName :: Maybe Text
    }
    deriving (Show, Eq)

{- | Check if an endpoint matches a predicate.

This is the core evaluation function that recursively evaluates
the predicate tree against endpoint information.

Examples:

>>> let ep = EndpointInfo "/api/v1/users" "GET" (Just "listUsers") Nothing
>>> matchesEndpoint (PathPrefix "/api") ep
True

>>> matchesEndpoint (And (PathPrefix "/api") (MethodEquals "GET")) ep
True

>>> matchesEndpoint (PathSuffix "/admin") ep
False
-}
matchesEndpoint :: EndpointPredicate -> EndpointInfo -> Bool
matchesEndpoint predicate info =
    let normPath = normalizePath (endpointPath info)
        normMethod = Text.toUpper (endpointMethod info)
     in case predicate of
            AlwaysTrue -> True
            AlwaysFalse -> False
            And p1 p2 -> matchesEndpoint p1 info && matchesEndpoint p2 info
            Or p1 p2 -> matchesEndpoint p1 info || matchesEndpoint p2 info
            Not p -> not (matchesEndpoint p info)
            PathPrefix prefix -> prefix `isPathPrefixOf` normPath
            PathSuffix suffix -> suffix `isPathSuffixOf` normPath
            PathEqual path -> normalizePath path == normPath
            PathContains substr -> substr `Text.isInfixOf` normPath
            MethodEquals method -> Text.toUpper method == normMethod
            OperationIdEquals opId -> endpointOperationId info == Just opId
            TableNameEquals table -> endpointTableName info == Just table

{- | Check if a path is a prefix of another path.

Handles path components correctly (e.g., /api is a prefix of /api/v1/users
but not of /apikey).

Examples:

>>> "/api" `isPathPrefixOf` "/api/v1/users"
True

>>> "/api" `isPathPrefixOf` "/apikey"
False

>>> "/" `isPathPrefixOf` "/anything"
True
-}
isPathPrefixOf :: Text -> Text -> Bool
isPathPrefixOf prefix path =
    let prefixParts = splitPath prefix
        pathParts = splitPath path
     in prefixParts `isPrefixOf` pathParts

{- | Check if a path is a suffix of another path.

Examples:

>>> "/users" `isPathSuffixOf` "/api/v1/users"
True

>>> "users" `isPathSuffixOf` "/api/v1/users"
True

>>> "/admin" `isPathSuffixOf` "/api/v1/users"
False
-}
isPathSuffixOf :: Text -> Text -> Bool
isPathSuffixOf suffix path =
    let suffixParts = splitPath suffix
        pathParts = splitPath path
     in suffixParts `isSuffixOf` pathParts

-- -------------------------------------------------------------------------
-- Tool Matching
-- -------------------------------------------------------------------------

{- | Check if a generic tool matches a predicate.

This is a placeholder for generic Tool matching that always returns True.
Specific tool types (OpenAPI, PostgREST) have their own specialized matching.
-}
matchesTool :: EndpointPredicate -> tool -> Bool
matchesTool _ _ = True

{- | Check if an OpenAPI tool matches a predicate.

Extracts the relevant endpoint information from the OpenAPI tool
and evaluates the predicate.
-}
matchesOpenAPITool :: EndpointPredicate -> OpenAPI.OpenAPITool -> Bool
matchesOpenAPITool predicate tool =
    let op = OpenAPI.toolOperation tool
        info =
            EndpointInfo
                { endpointPath = OpenAPI.toolPath tool
                , endpointMethod = OpenAPI.toolMethod tool
                , endpointOperationId = OpenAPITypes.opOperationId op
                , endpointTableName = Nothing
                }
     in matchesEndpoint predicate info

{- | Check if a PostgREST tool matches a predicate.

Extracts the relevant endpoint information from the PostgREST tool
and evaluates the predicate.
-}
matchesPostgRESTool :: EndpointPredicate -> PostgREST.PostgRESTool -> Bool
matchesPostgRESTool predicate tool =
    let info =
            EndpointInfo
                { endpointPath = PostgREST.prtPath tool
                , endpointMethod = PostgREST.methodToText (PostgREST.prtMethod tool)
                , endpointOperationId = Nothing
                , endpointTableName = Just $ Text.dropWhile (== '/') (PostgREST.prtPath tool)
                }
     in matchesEndpoint predicate info
