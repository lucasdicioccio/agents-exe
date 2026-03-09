{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Test to verify PostgREST monstergame.json parsing works

module PostgRESTParseTest where

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as LBS
import Test.Tasty
import Test.Tasty.HUnit

import System.Agents.Tools.OpenAPI.Types (OpenAPISpec(..))
import System.Agents.Tools.PostgREST.Converter (convertPostgRESToTools)

tests :: TestTree
tests = testGroup "PostgREST Parsing"
    [ test_parseMonsterGameSpec
    , test_convertMonsterGameToTools
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

test_convertMonsterGameToTools :: TestTree
test_convertMonsterGameToTools = testCase "Can convert monstergame spec to tools" $ do
    content <- LBS.readFile "test/data/monstergame.json"
    case decode content :: Maybe OpenAPISpec of
        Nothing -> assertFailure "Failed to parse monstergame.json"
        Just spec -> do
            let tools = convertPostgRESToTools "monstergame" spec
            -- Should generate tools for tables
            assertBool "Should generate tools" (not $ null tools)
            -- Print how many tools for debugging
            let toolCount = length tools
            assertBool ("Should have multiple tools, got: " ++ show toolCount) (toolCount > 5)

