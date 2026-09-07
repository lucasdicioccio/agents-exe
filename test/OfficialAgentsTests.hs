{-# LANGUAGE OverloadedStrings #-}

-- | Validates that the official feature-test agents in agents/test/ parse
-- correctly as AgentDescription values.  This catches JSON mistakes and
-- schema drift without requiring a live LLM or network services.
module OfficialAgentsTests (officialAgentsTestSuite) where

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import System.Agents.Base (Agent (..), AgentDescription (..))
import Test.Tasty
import Test.Tasty.HUnit

-- | Official test agents and the slug they are expected to declare.
officialAgents :: [(FilePath, Text)]
officialAgents =
    [ ("agents/test/file-system-tester.json", "file-system-tester")
    , ("agents/test/sqlite-tester.json", "sqlite-tester")
    , ("agents/test/progressive-disclosure-tester.json", "progressive-disclosure-tester")
    , ("agents/test/openapi-tester.json", "openapi-tester")
    , ("agents/test/postgrest-tester.json", "postgrest-tester")
    , ("agents/test/lua-tester.json", "lua-tester")
    , ("agents/test/introspection-tester.json", "introspection-tester")
    ]

officialAgentsTestSuite :: TestTree
officialAgentsTestSuite =
    testGroup
        "Official feature-test agents"
        [ testCase ("parses " <> path) $ do
            result <- Aeson.eitherDecodeFileStrict' path
            case result of
                Left err -> assertFailure $ "Failed to parse " <> path <> ": " <> err
                Right (AgentDescription agent) ->
                    slug agent @?= expectedSlug
        | (path, expectedSlug) <- officialAgents
        ]

