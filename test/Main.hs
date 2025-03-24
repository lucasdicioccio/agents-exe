module Main where

import System.Agents.LLMs.OpenAI as OpenAI

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "OpenAI rate-limits"
        [ testCase "1d1h1m1s" $
            parseRateLimitDelay "1d1h1m1s" @?= 1 + 60 + 3600 + 86400
        , testCase "1h1m1s" $
            parseRateLimitDelay "1h1m1s" @?= 1 + 60 + 3600
        , testCase "1m1s" $
            parseRateLimitDelay "1m1s" @?= 1 + 60
        , testCase "1s" $
            parseRateLimitDelay "1s" @?= 1
        , testCase "1.376s" $
            parseRateLimitDelay "1.376s" @?= 2
        , testCase "120ms" $
            parseRateLimitDelay "120ms" @?= 1
        , testCase "26.001s" $
            parseRateLimitDelay "26.001s" @?= 27
        ]
