#!/usr/bin/env cabal
{- cabal:
build-depends: base >= 4.14 && < 5
            , aeson
            , bytestring
            , text
            , containers
            , agents
-}

{-# LANGUAGE OverloadedStrings #-}

-- | Validation script to verify PostgREST body schema resolution.
--
-- This script checks that POST operations in PostgREST specs correctly
-- expose their request body schemas.
--
-- Run with: cabal run validate-postgrest-body-schema.hs

module Main where

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (isJust, fromJust)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO

import System.Agents.Tools.OpenAPI.Types (OpenAPISpec(..))
import System.Agents.Tools.PostgREST.Converter (
    convertPostgRESToTools,
    PostgRESTool(..),
    extractRequestBodySchema
  )
import System.Agents.Tools.PostgREST.Types (
    HttpMethod(..),
    defaultAllowedMethods
  )

-- | Test files to validate
testFiles :: [(String, String)]
testFiles =
    [ ("OpenAPI 3.0 (should work)", "test/data/toy-openapi3-body.json")
    , ("Swagger 2.0 inline body (should work)", "test/data/toy-swagger2-inline-body.json")
    , ("PostgREST body ref (BROKEN - needs fix)", "test/data/toy-postgrest-body.json")
    , ("PostgREST monstergame (BROKEN - needs fix)", "test/data/monstergame.json")
    ]

main :: IO ()
main = do
    TIO.putStrLn "========================================"
    TIO.putStrLn "PostgREST Body Schema Validation"
    TIO.putStrLn "========================================"
    TIO.putStrLn ""

    mapM_ validateFile testFiles

    TIO.putStrLn ""
    TIO.putStrLn "========================================"
    TIO.putStrLn "Summary:"
    TIO.putStrLn "  - OpenAPI 3.0 body: Should PASS"
    TIO.putStrLn "  - Swagger 2.0 inline body: Should PASS"
    TIO.putStrLn "  - PostgREST body ref: Currently BROKEN"
    TIO.putStrLn "  - monstergame.json POST: Currently BROKEN"
    TIO.putStrLn ""
    TIO.putStrLn "Expected behavior after fix:"
    TIO.putStrLn "  All POST tools should have requestBodySchema = Just Schema{...}"
    TIO.putStrLn "========================================"

validateFile :: (String, String) -> IO ()
validateFile (desc, filepath) = do
    TIO.putStrLn $ "Testing: " <> Text.pack desc
    TIO.putStrLn $ "  File: " <> Text.pack filepath

    content <- LBS.readFile filepath
    case decode content :: Maybe OpenAPISpec of
        Nothing -> do
            TIO.putStrLn "  ERROR: Failed to parse spec"
        Just spec -> do
            -- Get all tools including POST
            let allMethods = [GET, POST, PATCH, PUT]
            let tools = convertPostgRESToTools "test" allMethods spec
            let postTools = filter ((== POST) . prtMethod) tools

            TIO.putStrLn $ "  Total tools: " <> Text.pack (show $ length tools)
            TIO.putStrLn $ "  POST tools: " <> Text.pack (show $ length postTools)

            if null postTools
                then TIO.putStrLn "  SKIP: No POST tools found"
                else do
                    mapM_ (checkPostTool desc) postTools
    TIO.putStrLn ""

checkPostTool :: String -> PostgRESTool -> IO ()
checkPostTool fileDesc tool = do
    let hasBodySchema = isJust (prtRequestBodySchema tool)
    let status = if hasBodySchema then "✓ PASS" else "✗ FAIL (missing body schema)"
    TIO.putStrLn $ "    " <> prtName tool <> ": " <> status

    -- Additional diagnostics for failing cases
    if not hasBodySchema && "PostgREST" `elem` words fileDesc
        then do
            TIO.putStrLn "      -> This is the known issue: #/parameters/body.xxx refs not resolved"
            TIO.putStrLn "      -> Expected: schema from #/definitions/users"
        else return ()

