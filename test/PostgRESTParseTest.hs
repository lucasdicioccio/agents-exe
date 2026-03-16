{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Test to verify PostgREST monstergame.json parsing works

module PostgRESTParseTest where

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, fromJust, listToMaybe, isNothing)
import Test.Tasty
import Test.Tasty.HUnit

import System.Agents.Tools.OpenAPI.Types (OpenAPISpec(..), Operation(..), Parameter(..), Schema(..), ParamLocation(..))
import System.Agents.Tools.PostgREST.Converter (
    convertPostgRESToTools, 
    extractRequestBodySchema, 
    resolveBodyParameterRef,
    isBodyParamRef,
    PostgRESTool(..)
 )
import System.Agents.Tools.PostgREST.Types (HttpMethod (..), defaultAllowedMethods, methodToText)

tests :: TestTree
tests = testGroup "PostgREST Parsing"
    [ test_parseMonsterGameSpec
    , test_convertMonsterGameToTools
    , test_parsePostgRESTBodyPattern
    , test_extractBodyParameterRef
    , test_resolveBodyParameterRef
    , test_convertPostgRESTToToolsWithBodySchema
    ]

test_parseMonsterGameSpec :: TestTree
test_parseMonsterGameSpec = testCase "Can parse PostgREST monstergame.json spec" $ do
    content <- LBS.readFile "test/data/monstergame.json"
    case decode content :: Maybe OpenAPISpec of
        Nothing -> assertFailure "Failed to parse monstergame.json as OpenAPISpec"
        Just spec -> do
            -- Should have multiple paths
            assertBool "Should have paths" (not $ null $ specPaths spec)
            -- Should have components from definitions
            assertBool "Should have components" (specComponents spec /= Nothing)
            -- Should have parameters section (for PostgREST body resolution)
            assertBool "Should have parameters" (specParameters spec /= Nothing)

test_convertMonsterGameToTools :: TestTree
test_convertMonsterGameToTools = testCase "Can convert monstergame spec to tools" $ do
    content <- LBS.readFile "test/data/monstergame.json"
    case decode content :: Maybe OpenAPISpec of
        Nothing -> assertFailure "Failed to parse monstergame.json"
        Just spec -> do
            let tools = convertPostgRESToTools "monstergame" defaultAllowedMethods spec
            -- Should generate tools for tables
            assertBool "Should generate tools" (not $ null tools)
            -- Print how many tools for debugging
            let toolCount = length tools
            assertBool ("Should have multiple tools, got: " ++ show toolCount) (toolCount > 5)

test_parsePostgRESTBodyPattern :: TestTree
test_parsePostgRESTBodyPattern = testCase "Can parse PostgREST body parameter pattern" $ do
    content <- LBS.readFile "test/data/postgrest-body-test.json"
    case decode content :: Maybe OpenAPISpec of
        Nothing -> assertFailure "Failed to parse postgrest-body-test.json"
        Just spec -> do
            -- Should have specParameters
            assertBool "Should have parameters" (isJust $ specParameters spec)
            let params = fromJust $ specParameters spec
            -- Should have body.users parameter
            assertBool "Should have body.users parameter" 
                (Map.member "body.users" params)
            -- Get the body.users parameter
            let bodyParam = params Map.! "body.users"
            -- Should be a body parameter
            assertEqual "Parameter should be in body" ParamInBody (paramIn bodyParam)
            -- Should have a schema
            assertBool "Parameter should have schema" (isJust $ paramSchema bodyParam)

test_extractBodyParameterRef :: TestTree
test_extractBodyParameterRef = testCase "Can identify body parameter references" $ do
    content <- LBS.readFile "test/data/postgrest-body-test.json"
    case decode content :: Maybe OpenAPISpec of
        Nothing -> assertFailure "Failed to parse postgrest-body-test.json"
        Just spec -> do
            -- Get the POST /users operation
            let paths = specPaths spec
            case Map.lookup "/users" paths of
                Nothing -> assertFailure "Should have /users path"
                Just methods -> case Map.lookup "POST" methods of
                    Nothing -> assertFailure "Should have POST method"
                    Just op -> do
                        -- Check that we have parameters
                        assertBool "Should have parameters" (not $ null $ opParameters op)
                        -- Check that one of the parameters is a body param ref
                        let hasBodyParamRef = any isBodyParamRef (opParameters op)
                        assertBool "Should have body parameter reference" hasBodyParamRef

test_resolveBodyParameterRef :: TestTree
test_resolveBodyParameterRef = testCase "Can resolve body parameter references to schemas" $ do
    content <- LBS.readFile "test/data/postgrest-body-test.json"
    case decode content :: Maybe OpenAPISpec of
        Nothing -> assertFailure "Failed to parse postgrest-body-test.json"
        Just spec -> do
            let params = fromJust $ specParameters spec
            -- Resolve body.users
            case resolveBodyParameterRef spec "#/parameters/body.users" of
                Nothing -> assertFailure "Should resolve body.users reference"
                Just schema -> do
                    -- Schema should have a $ref to #/definitions/users
                    assertEqual "Schema should ref users" 
                        (Just "#/definitions/users") 
                        (schemaRef schema)

test_convertPostgRESTToToolsWithBodySchema :: TestTree
test_convertPostgRESTToToolsWithBodySchema = testCase "Converts PostgREST to tools with request body schemas" $ do
    content <- LBS.readFile "test/data/postgrest-body-test.json"
    case decode content :: Maybe OpenAPISpec of
        Nothing -> assertFailure "Failed to parse postgrest-body-test.json"
        Just spec -> do
            let tools = convertPostgRESToTools "testdb" [POST, PATCH] spec
            -- Should have tools for POST and PATCH
            assertBool "Should generate tools" (not $ null tools)
            -- Find the POST tool
            let postTools = filter (\t -> prtMethod t == POST) tools
            assertEqual "Should have one POST tool" 1 (length postTools)
            -- The POST tool should have a request body schema
            let postTool = listToMaybe postTools
            assertBool "POST tool should have request body schema" 
                (isJust $ prtRequestBodySchema =<< postTool)
            -- Find the PATCH tool
            let patchTools = filter (\t -> prtMethod t == PATCH) tools
            assertEqual "Should have one PATCH tool" 1 (length patchTools)
            -- The PATCH tool should have a request body schema
            let patchTool = listToMaybe patchTools
            assertBool "PATCH tool should have request body schema" 
                (isJust $ prtRequestBodySchema =<< patchTool)

