{-# LANGUAGE OverloadedStrings #-}

-- | Tests for EndpointPredicate filtering functionality.
--
-- This module tests the predicate calculus for filtering API endpoints
-- including path matching, boolean logic, and JSON serialization.
module EndpointPredicateTests where

import Data.Aeson (decode, encode)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Tasty
import Test.Tasty.HUnit

import System.Agents.Tools.EndpointPredicate

-- -------------------------------------------------------------------------
-- Test Entry Point
-- -------------------------------------------------------------------------

-- | All EndpointPredicate tests.
tests :: TestTree
tests =
    testGroup
        "EndpointPredicate Tests"
        [ predicateTypeTests
        , pathMatchingTests
        , booleanLogicTests
        , methodMatchingTests
        , jsonSerializationTests
        , openAPIToolMatchingTests
        , postgRESToolMatchingTests
        ]

-- -------------------------------------------------------------------------
-- 1. Predicate Type Tests
-- -------------------------------------------------------------------------

predicateTypeTests :: TestTree
predicateTypeTests =
    testGroup
        "Predicate Types"
        [ testCase "AlwaysTrue always matches" $ do
            let info = EndpointInfo "/api/v1/users" "GET" (Just "listUsers") Nothing
            matchesEndpoint AlwaysTrue info @?= True
        , testCase "AlwaysFalse never matches" $ do
            let info = EndpointInfo "/api/v1/users" "GET" (Just "listUsers") Nothing
            matchesEndpoint AlwaysFalse info @?= False
        , testCase "PathPrefix with matching path" $ do
            let info = EndpointInfo "/api/v1/users" "GET" Nothing Nothing
            matchesEndpoint (PathPrefix "/api") info @?= True
        , testCase "PathPrefix with non-matching path" $ do
            let info = EndpointInfo "/other/path" "GET" Nothing Nothing
            matchesEndpoint (PathPrefix "/api") info @?= False
        , testCase "PathSuffix with matching path" $ do
            let info = EndpointInfo "/api/v1/users" "GET" Nothing Nothing
            matchesEndpoint (PathSuffix "/users") info @?= True
        , testCase "PathEqual with exact match" $ do
            let info = EndpointInfo "/api/v1/users" "GET" Nothing Nothing
            matchesEndpoint (PathEqual "/api/v1/users") info @?= True
        , testCase "PathEqual with non-matching path" $ do
            let info = EndpointInfo "/api/v1/users" "GET" Nothing Nothing
            matchesEndpoint (PathEqual "/api/v1/other") info @?= False
        , testCase "PathContains with substring match" $ do
            let info = EndpointInfo "/api/v1/users" "GET" Nothing Nothing
            matchesEndpoint (PathContains "v1") info @?= True
        , testCase "OperationIdEquals with matching ID" $ do
            let info = EndpointInfo "/api/v1/users" "GET" (Just "listUsers") Nothing
            matchesEndpoint (OperationIdEquals "listUsers") info @?= True
        , testCase "OperationIdEquals with non-matching ID" $ do
            let info = EndpointInfo "/api/v1/users" "GET" (Just "listUsers") Nothing
            matchesEndpoint (OperationIdEquals "otherOp") info @?= False
        , testCase "TableNameEquals with matching table" $ do
            let info = EndpointInfo "/public/users" "GET" Nothing (Just "public.users")
            matchesEndpoint (TableNameEquals "public.users") info @?= True
        ]

-- -------------------------------------------------------------------------
-- 2. Path Matching Tests
-- -------------------------------------------------------------------------

pathMatchingTests :: TestTree
pathMatchingTests =
    testGroup
        "Path Matching"
        [ testCase "splitPath splits correctly" $ do
            splitPath "/api/v1/users" @?= ["api", "v1", "users"]
        , testCase "splitPath handles root path" $ do
            splitPath "/" @?= ([] :: [Text])
        , testCase "normalizePath adds leading slash" $ do
            normalizePath "api/v1/users" @?= "/api/v1/users"
        , testCase "normalizePath removes trailing slash" $ do
            normalizePath "/api/v1/users/" @?= "/api/v1/users"
        , testCase "normalizePath collapses multiple slashes" $ do
            normalizePath "//api///v1//users//" @?= "/api/v1/users"
        , testCase "isPathPrefixOf matches prefix components" $ do
            "/api" `isPathPrefixOf` "/api/v1/users" @?= True
        , testCase "isPathPrefixOf does not match partial component" $ do
            "/api" `isPathPrefixOf` "/apikey" @?= False
        , testCase "isPathPrefixOf root matches everything" $ do
            "/" `isPathPrefixOf` "/anything" @?= True
        , testCase "isPathSuffixOf matches suffix components" $ do
            "/users" `isPathSuffixOf` "/api/v1/users" @?= True
        , testCase "isPathSuffixOf matches simple suffix" $ do
            "users" `isPathSuffixOf` "/api/v1/users" @?= True
        , testCase "isPathSuffixOf does not match non-suffix" $ do
            "/admin" `isPathSuffixOf` "/api/v1/users" @?= False
        ]

-- -------------------------------------------------------------------------
-- 3. Boolean Logic Tests
-- -------------------------------------------------------------------------

booleanLogicTests :: TestTree
booleanLogicTests =
    testGroup
        "Boolean Logic"
        [ testCase "And with both true" $ do
            let info = EndpointInfo "/api/v1/users" "GET" Nothing Nothing
                predicate = And (PathPrefix "/api") (PathSuffix "/users")
            matchesEndpoint predicate info @?= True
        , testCase "And with first false" $ do
            let info = EndpointInfo "/other/v1/users" "GET" Nothing Nothing
                predicate = And (PathPrefix "/api") (PathSuffix "/users")
            matchesEndpoint predicate info @?= False
        , testCase "And with second false" $ do
            let info = EndpointInfo "/api/v1/other" "GET" Nothing Nothing
                predicate = And (PathPrefix "/api") (PathSuffix "/users")
            matchesEndpoint predicate info @?= False
        , testCase "Or with first true" $ do
            let info = EndpointInfo "/api/v1/other" "GET" Nothing Nothing
                predicate = Or (PathPrefix "/api") (PathSuffix "/users")
            matchesEndpoint predicate info @?= True
        , testCase "Or with second true" $ do
            let info = EndpointInfo "/other/v1/users" "GET" Nothing Nothing
                predicate = Or (PathPrefix "/api") (PathSuffix "/users")
            matchesEndpoint predicate info @?= True
        , testCase "Or with both false" $ do
            let info = EndpointInfo "/other/v1/other" "GET" Nothing Nothing
                predicate = Or (PathPrefix "/api") (PathSuffix "/users")
            matchesEndpoint predicate info @?= False
        , testCase "Not inverts result" $ do
            let info = EndpointInfo "/api/v1/users" "GET" Nothing Nothing
                predicate = Not (PathPrefix "/other")
            matchesEndpoint predicate info @?= True
        , testCase "Not with matching predicate" $ do
            let info = EndpointInfo "/api/v1/users" "GET" Nothing Nothing
                predicate = Not (PathPrefix "/api")
            matchesEndpoint predicate info @?= False
        , testCase "complex nested predicate" $ do
            let info = EndpointInfo "/api/v1/users" "GET" Nothing Nothing
                -- (prefix /api AND suffix /users) OR (contains admin)
                predicate = Or (And (PathPrefix "/api") (PathSuffix "/users")) (PathContains "admin")
            matchesEndpoint predicate info @?= True
        ]

-- -------------------------------------------------------------------------
-- 4. Method Matching Tests
-- -------------------------------------------------------------------------

methodMatchingTests :: TestTree
methodMatchingTests =
    testGroup
        "Method Matching"
        [ testCase "MethodEquals with GET" $ do
            let info = EndpointInfo "/api/users" "GET" Nothing Nothing
            matchesEndpoint (MethodEquals "GET") info @?= True
        , testCase "MethodEquals is case-insensitive" $ do
            let info = EndpointInfo "/api/users" "POST" Nothing Nothing
            matchesEndpoint (MethodEquals "post") info @?= True
        , testCase "MethodEquals with non-matching method" $ do
            let info = EndpointInfo "/api/users" "GET" Nothing Nothing
            matchesEndpoint (MethodEquals "POST") info @?= False
        , testCase "And with method and path" $ do
            let info = EndpointInfo "/api/v1/users" "POST" Nothing Nothing
                predicate = And (PathPrefix "/api") (MethodEquals "POST")
            matchesEndpoint predicate info @?= True
        ]

-- -------------------------------------------------------------------------
-- 5. JSON Serialization Tests
-- -------------------------------------------------------------------------

jsonSerializationTests :: TestTree
jsonSerializationTests =
    testGroup
        "JSON Serialization"
        [ testCase "round-trip PathPrefix" $ do
            let pred = PathPrefix "/api/v1"
            decode (encode pred) @?= Just pred
        , testCase "round-trip PathSuffix" $ do
            let pred = PathSuffix "/users"
            decode (encode pred) @?= Just pred
        , testCase "round-trip PathEqual" $ do
            let pred = PathEqual "/api/users"
            decode (encode pred) @?= Just pred
        , testCase "round-trip PathContains" $ do
            let pred = PathContains "admin"
            decode (encode pred) @?= Just pred
        , testCase "round-trip MethodEquals" $ do
            let pred = MethodEquals "GET"
            decode (encode pred) @?= Just pred
        , testCase "round-trip OperationIdEquals" $ do
            let pred = OperationIdEquals "listUsers"
            decode (encode pred) @?= Just pred
        , testCase "round-trip TableNameEquals" $ do
            let pred = TableNameEquals "public.users"
            decode (encode pred) @?= Just pred
        , testCase "round-trip AlwaysTrue" $ do
            let pred = AlwaysTrue
            decode (encode pred) @?= Just pred
        , testCase "round-trip AlwaysFalse" $ do
            let pred = AlwaysFalse
            decode (encode pred) @?= Just pred
        , testCase "round-trip Not" $ do
            let pred = Not (PathPrefix "/admin")
            decode (encode pred) @?= Just pred
        , testCase "round-trip And" $ do
            let pred = And (PathPrefix "/api") (PathSuffix "/users")
            decode (encode pred) @?= Just pred
        , testCase "round-trip Or" $ do
            let pred = Or (PathPrefix "/api") (PathContains "public")
            decode (encode pred) @?= Just pred
        , testCase "round-trip complex nested predicate" $ do
            let pred = And (PathPrefix "/api") (Or (MethodEquals "GET") (MethodEquals "POST"))
            decode (encode pred) @?= Just pred
        , testCase "parse PathPrefix from JSON" $ do
            let json = "{\"tag\": \"PathPrefix\", \"contents\": \"/api/v1\"}"
            let result = decode json :: Maybe EndpointPredicate
            result @?= Just (PathPrefix "/api/v1")
        , testCase "parse And from JSON" $ do
            let json = "{\"tag\": \"And\", \"contents\": [{\"tag\": \"PathPrefix\", \"contents\": \"/api\"}, {\"tag\": \"PathSuffix\", \"contents\": \"/users\"}]}"
            let result = decode json :: Maybe EndpointPredicate
            result @?= Just (And (PathPrefix "/api") (PathSuffix "/users"))
        , testCase "parse Not from JSON" $ do
            let json = "{\"tag\": \"Not\", \"contents\": {\"tag\": \"PathContains\", \"contents\": \"admin\"}}"
            let result = decode json :: Maybe EndpointPredicate
            result @?= Just (Not (PathContains "admin"))
        ]

-- -------------------------------------------------------------------------
-- 6. OpenAPI Tool Matching Tests
-- -------------------------------------------------------------------------

-- | Mock OpenAPI tool for testing
data MockOpenAPITool = MockOpenAPITool
    { mockPath :: Text
    , mockMethod :: Text
    , mockOpId :: Maybe Text
    }

openAPIToolMatchingTests :: TestTree
openAPIToolMatchingTests =
    testGroup
        "OpenAPI Tool Matching (via EndpointInfo)"
        [ testCase "matches path and method" $ do
            let info = EndpointInfo "/api/v1/users" "GET" (Just "listUsers") Nothing
            matchesEndpoint (PathPrefix "/api") info @?= True
        , testCase "matches operation ID" $ do
            let info = EndpointInfo "/api/v1/users" "GET" (Just "listUsers") Nothing
            matchesEndpoint (OperationIdEquals "listUsers") info @?= True
        , testCase "complex filter for public API" $ do
            let info = EndpointInfo "/api/v1/public/users" "GET" (Just "listUsers") Nothing
                -- Include /api/v1/public but exclude admin paths
                predicate = And (PathPrefix "/api/v1/public") (Not (PathContains "admin"))
            matchesEndpoint predicate info @?= True
        , testCase "excludes admin paths" $ do
            let info = EndpointInfo "/api/v1/admin/users" "GET" (Just "adminListUsers") Nothing
                predicate = Not (PathContains "admin")
            matchesEndpoint predicate info @?= False
        ]

-- -------------------------------------------------------------------------
-- 7. PostgREST Tool Matching Tests
-- -------------------------------------------------------------------------

postgRESToolMatchingTests :: TestTree
postgRESToolMatchingTests =
    testGroup
        "PostgREST Tool Matching (via EndpointInfo)"
        [ testCase "matches table by path prefix" $ do
            let info = EndpointInfo "/public/users" "GET" Nothing (Just "public.users")
            matchesEndpoint (PathPrefix "/public") info @?= True
        , testCase "matches table name" $ do
            let info = EndpointInfo "/public/users" "GET" Nothing (Just "public.users")
            matchesEndpoint (TableNameEquals "public.users") info @?= True
        , testCase "filter for specific schema" $ do
            let info = EndpointInfo "/public/users" "GET" Nothing (Just "public.users")
                predicate = PathPrefix "/public"
            matchesEndpoint predicate info @?= True
        , testCase "exclude internal tables" $ do
            let info = EndpointInfo "/public/_internal_logs" "GET" Nothing (Just "public._internal_logs")
                predicate = Not (PathContains "_internal")
            matchesEndpoint predicate info @?= False
        ]

