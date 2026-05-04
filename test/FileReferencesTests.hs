{-# LANGUAGE OverloadedStrings #-}

{- | Tests for file reference parsing functionality.

This module tests the [[file:path]] syntax parsing and resolution.
-}
module FileReferencesTests where

import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (createDirectoryIfMissing, removeFile)
import System.FilePath (takeDirectory)
import System.IO.Temp (withSystemTempDirectory)
import System.Agents.Prompt.FileReferences
import Test.Tasty
import Test.Tasty.HUnit

-- | Test suite for file references.
tests :: TestTree
tests =
    testGroup
        "FileReferences"
        [ parseTests
        , rangeTests
        , resolutionTests
        , inlineTests
        ]

-- | Tests for parsing file references.
parseTests :: TestTree
parseTests =
    testGroup
        "Parsing"
        [ testCase "Parse simple file reference" $ do
            let refs = parseFileReferences "Check [[file:src/Main.hs]] please"
            length refs @?= 1
            filePath (head refs) @?= "src/Main.hs"
            isNothing (lineRange $ head refs) @? "Should not have line range"
        , testCase "Parse file reference with line range" $ do
            let refs = parseFileReferences "Fix [[file:src/Parser.hs#L10-L20]] bug"
            length refs @?= 1
            filePath (head refs) @?= "src/Parser.hs"
            isJust (lineRange $ head refs) @? "Should have line range"
            let Just (LineRange s e) = lineRange $ head refs
            s @?= 10
            e @?= 20
        , testCase "Parse multiple file references" $ do
            let refs = parseFileReferences "Compare [[file:a.txt]] and [[file:b.txt#L1-L5]]"
            length refs @?= 2
        , testCase "Parse single line reference" $ do
            let refs = parseFileReferences "See [[file:code.hs#L42]]"
            let Just (LineRange s e) = lineRange $ head refs
            s @?= 42
            e @?= 42
        , testCase "Parse open-ended range" $ do
            let refs = parseFileReferences "From [[file:log.txt#L100-]]"
            let Just (LineRange s e) = lineRange $ head refs
            s @?= 100
            e @?= (-1)
        , testCase "Ignore non-file brackets" $ do
            let refs = parseFileReferences "[[not:a:file]] and [[file:valid.txt]]"
            length refs @?= 1
            filePath (head refs) @?= "valid.txt"
        , testCase "Ignore empty file path" $ do
            let refs = parseFileReferences "[[file: ]] invalid"
            length refs @?= 0
        ]

-- | Tests for line range parsing.
rangeTests :: TestTree
rangeTests =
    testGroup
        "LineRanges"
        [ testCase "Extract full file content" $ do
            let content = "line1\nline2\nline3\n"
            let result = readFileRange content (LineRange 1 3)
            result @?= Right "line1\nline2\nline3\n"
        , testCase "Extract partial range" $ do
            let content = "line1\nline2\nline3\nline4\nline5\n"
            let result = readFileRange content (LineRange 2 4)
            result @?= Right "line2\nline3\nline4\n"
        , testCase "Extract single line" $ do
            let content = "line1\nline2\nline3\n"
            let result = readFileRange content (LineRange 2 2)
            result @?= Right "line2\n"
        , testCase "Open-ended range to end" $ do
            let content = "line1\nline2\nline3\n"
            let result = readFileRange content (LineRange 2 (-1))
            result @?= Right "line2\nline3\n"
        , testCase "Reject invalid start line" $ do
            let content = "line1\nline2\n"
            let result = readFileRange content (LineRange 0 1)
            case result of
                Left _ -> return () -- Expected
                Right _ -> assertFailure "Should fail with invalid line number"
        , testCase "Reject out of bounds" $ do
            let content = "line1\nline2\n"
            let result = readFileRange content (LineRange 5 10)
            case result of
                Left _ -> return () -- Expected
                Right _ -> assertFailure "Should fail with out of bounds"
        , testCase "Reject end before start" $ do
            let content = "line1\nline2\nline3\n"
            let result = readFileRange content (LineRange 3 2)
            case result of
                Left _ -> return () -- Expected
                Right _ -> assertFailure "Should fail with end before start"
        ]

-- | Tests for file resolution.
resolutionTests :: TestTree
resolutionTests =
    testGroup
        "Resolution"
        [ testCase "Resolve existing file" $ do
            withSystemTempDirectory "file-refs-test" $ \dir -> do
                let testFile = dir ++ "/test.txt"
                writeFile testFile "Hello, World!"
                let ref = FileReference testFile Nothing
                result <- resolveReference ref
                case result of
                    ResolutionSuccess content -> content @?= "Hello, World!"
                    ResolutionError err -> assertFailure $ "Should succeed: " ++ Text.unpack err
        , testCase "Fail on missing file" $ do
            let ref = FileReference "/nonexistent/file.txt" Nothing
            result <- resolveReference ref
            case result of
                ResolutionSuccess _ -> assertFailure "Should fail on missing file"
                ResolutionError _ -> return () -- Expected
        , testCase "Resolve with line range" $ do
            withSystemTempDirectory "file-refs-test" $ \dir -> do
                let testFile = dir ++ "/test.txt"
                writeFile testFile "line1\nline2\nline3\n"
                let ref = FileReference testFile (Just $ LineRange 2 2)
                result <- resolveReference ref
                case result of
                    ResolutionSuccess content -> content @?= "line2\n"
                    ResolutionError err -> assertFailure $ "Should succeed: " ++ Text.unpack err
        ]

-- | Tests for inlining content.
inlineTests :: TestTree
inlineTests =
    testGroup
        "Inlining"
        [ testCase "Inline file content" $ do
            let prompt = "Review: [[file:src/Main.hs]]"
            let ref = FileReference "src/Main.hs" Nothing
            let resolution = ResolutionSuccess "module Main where\nmain = putStrLn \"Hello\"\n"
            let result = inlineReferences prompt [(ref, resolution)]
            assertBool "Should contain file content" ("module Main where" `Text.isInfixOf` result)
            assertBool "Should have markdown code block" ("```haskell" `Text.isInfixOf` result)
        , testCase "Inline error message" $ do
            let prompt = "Check [[file:missing.txt]]"
            let ref = FileReference "missing.txt" Nothing
            let resolution = ResolutionError "File not found"
            let result = inlineReferences prompt [(ref, resolution)]
            assertBool "Should contain error" ("Error including" `Text.isInfixOf` result)
        , testCase "Multiple references" $ do
            let prompt = "Compare [[file:a.hs]] and [[file:b.hs]]"
            let ref1 = FileReference "a.hs" Nothing
            let ref2 = FileReference "b.hs" Nothing
            let resolutions =
                    [ (ref1, ResolutionSuccess "content A")
                    , (ref2, ResolutionSuccess "content B")
                    ]
            let result = inlineReferences prompt resolutions
            assertBool "Should contain both files" ("content A" `Text.isInfixOf` result)
            assertBool "Should contain both files" ("content B" `Text.isInfixOf` result)
        ]

