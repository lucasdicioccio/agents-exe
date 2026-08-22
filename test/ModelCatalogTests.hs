{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module ModelCatalogTests where

import Data.List (find)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Test.Tasty
import Test.Tasty.HUnit

import System.Agents.CLI.New.ModelCatalog (
    CatalogEntry (..),
    MatchPattern (..),
    ModelCatalog (..),
    defaultModelCatalog,
    lookupPresetForModel,
 )

tests :: TestTree
tests =
    testGroup
        "ModelCatalog"
        [ testGroup
            "default catalog lookups"
            [ testCase "kimi-k2.5 matches kimi preset" $
                lookupPresetForModel defaultModelCatalog "kimi-k2.5"
                    @?= Just ("kimi", findEntryByPreset "kimi")
            , testCase "gpt-4 matches openai preset" $
                lookupPresetForModel defaultModelCatalog "gpt-4"
                    @?= Just ("openai", findEntryByPreset "openai")
            , testCase "o1-preview matches openai preset" $
                lookupPresetForModel defaultModelCatalog "o1-preview"
                    @?= Just ("openai", findEntryByPreset "openai")
            , testCase "o3-mini matches openai preset" $
                lookupPresetForModel defaultModelCatalog "o3-mini"
                    @?= Just ("openai", findEntryByPreset "openai")
            , testCase "chatgpt-4o matches openai preset" $
                lookupPresetForModel defaultModelCatalog "chatgpt-4o"
                    @?= Just ("openai", findEntryByPreset "openai")
            , testCase "mistral-large matches mistral preset" $
                lookupPresetForModel defaultModelCatalog "mistral-large"
                    @?= Just ("mistral", findEntryByPreset "mistral")
            , testCase "codestral-latest matches mistral preset" $
                lookupPresetForModel defaultModelCatalog "codestral-latest"
                    @?= Just ("mistral", findEntryByPreset "mistral")
            , testCase "pixtral-large matches mistral preset" $
                lookupPresetForModel defaultModelCatalog "pixtral-large"
                    @?= Just ("mistral", findEntryByPreset "mistral")
            , testCase "llama3.1 matches ollama preset" $
                lookupPresetForModel defaultModelCatalog "llama3.1"
                    @?= Just ("ollama", findEntryByPreset "ollama")
            , testCase "qwen2.5 matches ollama preset" $
                lookupPresetForModel defaultModelCatalog "qwen2.5"
                    @?= Just ("ollama", findEntryByPreset "ollama")
            , testCase "unknown model returns Nothing" $
                lookupPresetForModel defaultModelCatalog "totally-unknown-model-xyz"
                    @?= Nothing
            ]
        ]
  where
    findEntryByPreset :: Text.Text -> CatalogEntry
    findEntryByPreset preset =
        fromMaybe
            (error $ "Missing preset in default catalog: " ++ Text.unpack preset)
            (find ((== preset) . entryPreset) defaultModelCatalog.catalogEntries)

