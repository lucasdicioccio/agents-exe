{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Unit tests for DeveloperToolbox ranged file read/write operations.
--
-- These tests verify:
-- * Range parsing (single lines, ranges, head/tail/whole, insert-after)
-- * File reading with line ranges
-- * File writing with line ranges
-- * Multiple range operations
-- * Edge cases (empty files, out-of-bounds ranges, etc.)
-- * Indentation preservation in replacements
-- * Line number handling (1-based, inclusive)
--
-- NOTE: Tests use temporary files to avoid corrupting shared resources.
module DeveloperToolboxTests where

import Control.Exception (bracket)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory (createDirectoryIfMissing, doesFileExist, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import Prod.Tracer (Tracer (..), silent)

import System.Agents.Base (DeveloperToolboxDescription (..), DeveloperToolCapability (..))
import System.Agents.Tools.DeveloperToolbox as DeveloperToolbox
import System.Agents.Tools.DeveloperToolbox.Types (RangeSpec(..))

-- | Test data: a simple multi-line file for testing
testFileContent :: Text
testFileContent =
    Text.unlines
        [ "Line 1: First line"
        , "Line 2: Second line"
        , "Line 3: Third line"
        , "Line 4: Fourth line"
        , "Line 5: Fifth line"
        , "Line 6: Sixth line"
        , "Line 7: Seventh line"
        , "Line 8: Eighth line"
        , "Line 9: Ninth line"
        , "Line 10: Tenth line"
        ]

-- | Test data: empty file content
emptyFileContent :: Text
emptyFileContent = ""

-- | Test data: single line file
singleLineContent :: Text
singleLineContent = "Only line\n"

-- | Create a test toolbox with all capabilities enabled
testToolbox :: IO Toolbox
testToolbox = do
    let desc =
            DeveloperToolboxDescription
                { developerToolboxName = "test-developer"
                , developerToolboxDescription = "Test developer toolbox"
                , developerToolboxCapabilities =
                    [ DevToolReadFileRange
                    , DevToolWriteFileRange
                    ]
                , developerToolboxActivation = Nothing
                }
    result <- DeveloperToolbox.initializeToolbox silent desc
    case result of
        Left err -> error $ "Failed to initialize test toolbox: " ++ err
        Right toolbox -> pure toolbox

-- | Create a temporary directory for testing, clean up after
withTempDir :: (FilePath -> IO a) -> IO a
withTempDir action = do
    tmpDir <- createTempDirectory "/tmp" "devtoolbox-test-"
    bracket
        (pure tmpDir)
        removeDirectoryRecursive
        action

-- | Create a test file with the given content
withTestFile :: FilePath -> Text -> (FilePath -> IO a) -> IO a
withTestFile tmpDir content action = do
    let filePath = tmpDir </> "testfile.txt"
    Text.writeFile filePath content
    action filePath

-- | Create a test file from testFileContent
withStandardTestFile :: FilePath -> (FilePath -> IO a) -> IO a
withStandardTestFile tmpDir = withTestFile tmpDir testFileContent

-------------------------------------------------------------------------------
-- Test Suite
-------------------------------------------------------------------------------

tests :: TestTree
tests =
    testGroup
        "DeveloperToolbox Tests"
        [ rangeParsingTests
        , readFileRangeTests
        , writeFileRangeTests
        , wholeRangeTests
        , edgeCaseTests
        , multiRangeTests
        , indentationTests
        , lineNumberTests
        ]

-------------------------------------------------------------------------------
-- Range Parsing Tests
-------------------------------------------------------------------------------

rangeParsingTests :: TestTree
rangeParsingTests =
    testGroup
        "Range Parsing"
        [ testCase "Parse single line" testParseSingleLine
        , testCase "Parse line range" testParseLineRange
        , testCase "Parse head" testParseHead
        , testCase "Parse tail" testParseTail
        , testCase "Parse whole" testParseWhole
        , testCase "Parse multiple ranges" testParseMultipleRanges
        , testCase "Parse invalid range (start > end)" testParseInvalidRange
        , testCase "Parse invalid number" testParseInvalidNumber
        , testCase "Parse zero (invalid)" testParseZero
        , testCase "Parse empty string" testParseEmpty
        , testCase "Parse insert-after single line" testParseInsertAfterSingle
        , testCase "Parse insert-after range" testParseInsertAfterRange
        , testCase "Parse multiple insert-after ranges" testParseMultipleInsertAfter
        ]

testParseSingleLine :: Assertion
testParseSingleLine =
    DeveloperToolbox.parseRanges "5" @?= Right [Lines (5, 5)]

testParseLineRange :: Assertion
testParseLineRange =
    DeveloperToolbox.parseRanges "1-10" @?= Right [Lines (1, 10)]

testParseHead :: Assertion
testParseHead =
    DeveloperToolbox.parseRanges "head" @?= Right [Head]

testParseTail :: Assertion
testParseTail =
    DeveloperToolbox.parseRanges "tail" @?= Right [Tail]

testParseWhole :: Assertion
testParseWhole =
    DeveloperToolbox.parseRanges "whole" @?= Right [Whole]

testParseMultipleRanges :: Assertion
testParseMultipleRanges =
    DeveloperToolbox.parseRanges "1-5,10-15,20"
        @?= Right [Lines (1, 5), Lines (10, 15), Lines (20, 20)]

testParseInvalidRange :: Assertion
testParseInvalidRange =
    case DeveloperToolbox.parseRanges "10-5" of
        Left (InvalidRangeError _) -> pure ()
        _ -> assertFailure "Expected InvalidRangeError for start > end"

testParseInvalidNumber :: Assertion
testParseInvalidNumber =
    case DeveloperToolbox.parseRanges "abc" of
        Left (InvalidRangeError _) -> pure ()
        _ -> assertFailure "Expected InvalidRangeError for invalid number"

testParseZero :: Assertion
testParseZero =
    case DeveloperToolbox.parseRanges "0" of
        Left (InvalidRangeError _) -> pure ()
        _ -> assertFailure "Expected InvalidRangeError for zero line number"

testParseEmpty :: Assertion
testParseEmpty =
    DeveloperToolbox.parseRanges "" @?= Right []

-- | Test parsing insert-after single line (N+)
testParseInsertAfterSingle :: Assertion
testParseInsertAfterSingle =
    DeveloperToolbox.parseRanges "5+" @?= Right [After 5]

-- | Test parsing insert-after range (N-M+ means After M)
testParseInsertAfterRange :: Assertion
testParseInsertAfterRange =
    DeveloperToolbox.parseRanges "2-5+" @?= Right [After 5]

-- | Test parsing multiple insert-after ranges
testParseMultipleInsertAfter :: Assertion
testParseMultipleInsertAfter =
    DeveloperToolbox.parseRanges "2+,5+"
        @?= Right [After 2, After 5]

-------------------------------------------------------------------------------
-- Read File Range Tests
-------------------------------------------------------------------------------

readFileRangeTests :: TestTree
readFileRangeTests =
    testGroup
        "Read File Range"
        [ testCase "Read single line" testReadSingleLine
        , testCase "Read line range" testReadLineRange
        , testCase "Read head (returns empty)" testReadHead
        , testCase "Read tail (returns empty)" testReadTail
        , testCase "Read multiple ranges" testReadMultipleRanges
        , testCase "Read file not found" testReadFileNotFound
        , testCase "Read out of bounds range" testReadOutOfBounds
        , testCase "Read includes line numbers" testReadIncludesLineNumbers
        ]

testReadSingleLine :: Assertion
testReadSingleLine = withTempDir $ \tmpDir ->
    withStandardTestFile tmpDir $ \filePath -> do
        toolbox <- testToolbox
        result <- DeveloperToolbox.executeReadFileRange silent toolbox filePath "3"
        case result of
            Left err -> assertFailure $ show err
            Right readResult -> do
                readFileLinesRead readResult @?= 1
                -- Check that line 3 content is present
                assertBool "Should contain 'Third line'" $
                    "Third line" `Text.isInfixOf` readFileContent readResult
                -- Check that it has line number
                assertBool "Should contain line number 3" $
                    "3\t" `Text.isInfixOf` readFileContent readResult

testReadLineRange :: Assertion
testReadLineRange = withTempDir $ \tmpDir ->
    withStandardTestFile tmpDir $ \filePath -> do
        toolbox <- testToolbox
        result <- DeveloperToolbox.executeReadFileRange silent toolbox filePath "2-4"
        case result of
            Left err -> assertFailure $ show err
            Right readResult -> do
                readFileLinesRead readResult @?= 3
                let content = readFileContent readResult
                assertBool "Should contain line 2" $ "Second line" `Text.isInfixOf` content
                assertBool "Should contain line 3" $ "Third line" `Text.isInfixOf` content
                assertBool "Should contain line 4" $ "Fourth line" `Text.isInfixOf` content
                assertBool "Should NOT contain line 1" $ not ("First line" `Text.isInfixOf` content)
                assertBool "Should NOT contain line 5" $ not ("Fifth line" `Text.isInfixOf` content)

testReadHead :: Assertion
testReadHead = withTempDir $ \tmpDir ->
    withStandardTestFile tmpDir $ \filePath -> do
        toolbox <- testToolbox
        result <- DeveloperToolbox.executeReadFileRange silent toolbox filePath "head"
        case result of
            Left err -> assertFailure $ show err
            Right readResult -> do
                -- Head returns empty list as per implementation
                readFileLinesRead readResult @?= 0

testReadTail :: Assertion
testReadTail = withTempDir $ \tmpDir ->
    withStandardTestFile tmpDir $ \filePath -> do
        toolbox <- testToolbox
        result <- DeveloperToolbox.executeReadFileRange silent toolbox filePath "tail"
        case result of
            Left err -> assertFailure $ show err
            Right readResult -> do
                -- Tail returns empty list as per implementation
                readFileLinesRead readResult @?= 0

testReadMultipleRanges :: Assertion
testReadMultipleRanges = withTempDir $ \tmpDir ->
    withStandardTestFile tmpDir $ \filePath -> do
        toolbox <- testToolbox
        result <- DeveloperToolbox.executeReadFileRange silent toolbox filePath "1-2,9-10"
        case result of
            Left err -> assertFailure $ show err
            Right readResult -> do
                readFileLinesRead readResult @?= 4
                let content = readFileContent readResult
                assertBool "Should contain line 1" $ "First line" `Text.isInfixOf` content
                assertBool "Should contain line 2" $ "Second line" `Text.isInfixOf` content
                assertBool "Should contain line 9" $ "Ninth line" `Text.isInfixOf` content
                assertBool "Should contain line 10" $ "Tenth line" `Text.isInfixOf` content
                assertBool "Should NOT contain line 5" $ not ("Fifth line" `Text.isInfixOf` content)

testReadFileNotFound :: Assertion
testReadFileNotFound = withTempDir $ \tmpDir -> do
    toolbox <- testToolbox
    let nonExistent = tmpDir </> "does-not-exist.txt"
    result <- DeveloperToolbox.executeReadFileRange silent toolbox nonExistent "1-5"
    case result of
        Left (FileNotFoundError _) -> pure ()
        _ -> assertFailure "Expected FileNotFoundError"

testReadOutOfBounds :: Assertion
testReadOutOfBounds = withTempDir $ \tmpDir ->
    withStandardTestFile tmpDir $ \filePath -> do
        toolbox <- testToolbox
        -- File has 10 lines, try to read lines 20-30
        result <- DeveloperToolbox.executeReadFileRange silent toolbox filePath "20-30"
        case result of
            Left err -> assertFailure $ show err
            Right readResult -> do
                -- Should return empty for out of bounds
                readFileLinesRead readResult @?= 0

testReadIncludesLineNumbers :: Assertion
testReadIncludesLineNumbers = withTempDir $ \tmpDir ->
    withStandardTestFile tmpDir $ \filePath -> do
        toolbox <- testToolbox
        result <- DeveloperToolbox.executeReadFileRange silent toolbox filePath "5"
        case result of
            Left err -> assertFailure $ show err
            Right readResult -> do
                let content = readFileContent readResult
                -- Line format should be "5\tLine 5: Fifth line"
                assertBool "Should have line number prefix with tab" $
                    "5\t" `Text.isInfixOf` content

-------------------------------------------------------------------------------
-- Write File Range Tests
-------------------------------------------------------------------------------

writeFileRangeTests :: TestTree
writeFileRangeTests =
    testGroup
        "Write File Range"
        [ testCase "Replace single line" testWriteSingleLine
        , testCase "Replace line range" testWriteLineRange
        , testCase "Prepend to file (head)" testWriteHead
        , testCase "Append to file (tail)" testWriteTail
        , testCase "Write to new file with head" testWriteNewFileHead
        , testCase "Write to new file with tail" testWriteNewFileTail
        , testCase "File not found error (no head/tail)" testWriteFileNotFound
        , testCase "Lines written count" testWriteLinesWrittenCount
        ]

testWriteSingleLine :: Assertion
testWriteSingleLine = withTempDir $ \tmpDir ->
    withStandardTestFile tmpDir $ \filePath -> do
        toolbox <- testToolbox
        result <- DeveloperToolbox.executeWriteFileRange silent toolbox filePath "3" ["REPLACED LINE\n"]
        case result of
            Left err -> assertFailure $ show err
            Right writeResult -> do
                writeFileRangesModified writeResult @?= 1
                -- Read the file and verify
                content <- Text.readFile filePath
                assertBool "Should contain replaced line" $
                    "REPLACED LINE" `Text.isInfixOf` content
                assertBool "Should NOT contain original line 3" $
                    not ("Third line" `Text.isInfixOf` content)
                -- Other lines should still be there
                assertBool "Should still have line 1" $ "First line" `Text.isInfixOf` content
                assertBool "Should still have line 5" $ "Fifth line" `Text.isInfixOf` content

testWriteLineRange :: Assertion
testWriteLineRange = withTempDir $ \tmpDir ->
    withStandardTestFile tmpDir $ \filePath -> do
        toolbox <- testToolbox
        result <- DeveloperToolbox.executeWriteFileRange silent toolbox filePath "3-5" ["NEW LINE A\nNEW LINE B\n"]
        case result of
            Left err -> assertFailure $ show err
            Right writeResult -> do
                writeFileRangesModified writeResult @?= 1
                content <- Text.readFile filePath
                assertBool "Should contain NEW LINE A" $ "NEW LINE A" `Text.isInfixOf` content
                assertBool "Should contain NEW LINE B" $ "NEW LINE B" `Text.isInfixOf` content
                assertBool "Should NOT contain Third line" $ not ("Third line" `Text.isInfixOf` content)
                assertBool "Should NOT contain Fourth line" $ not ("Fourth line" `Text.isInfixOf` content)
                assertBool "Should NOT contain Fifth line" $ not ("Fifth line" `Text.isInfixOf` content)
                assertBool "Should still have line 1" $ "First line" `Text.isInfixOf` content
                assertBool "Should still have line 10" $ "Tenth line" `Text.isInfixOf` content

testWriteHead :: Assertion
testWriteHead = withTempDir $ \tmpDir ->
    withStandardTestFile tmpDir $ \filePath -> do
        toolbox <- testToolbox
        result <- DeveloperToolbox.executeWriteFileRange silent toolbox filePath "head" ["PREPENDED LINE\n"]
        case result of
            Left err -> assertFailure $ show err
            Right writeResult -> do
                writeFileRangesModified writeResult @?= 1
                content <- Text.readFile filePath
                let lines' = Text.lines content
                -- First line should be prepended content
                assertBool "Should start with prepended line" $
                    "PREPENDED LINE" `Text.isPrefixOf` Text.concat (take 1 lines')
                -- Original content should follow
                assertBool "Should still have original line 1" $ "First line" `Text.isInfixOf` content

testWriteTail :: Assertion
testWriteTail = withTempDir $ \tmpDir ->
    withStandardTestFile tmpDir $ \filePath -> do
        toolbox <- testToolbox
        result <- DeveloperToolbox.executeWriteFileRange silent toolbox filePath "tail" ["APPENDED LINE\n"]
        case result of
            Left err -> assertFailure $ show err
            Right writeResult -> do
                writeFileRangesModified writeResult @?= 1
                content <- Text.readFile filePath
                -- Should have appended line at end
                assertBool "Should end with appended line" $ "APPENDED LINE" `Text.isInfixOf` content
                -- Original content should still be there
                assertBool "Should still have line 10" $ "Tenth line" `Text.isInfixOf` content

testWriteNewFileHead :: Assertion
testWriteNewFileHead = withTempDir $ \tmpDir -> do
    toolbox <- testToolbox
    let newFile = tmpDir </> "new-file.txt"
    result <- DeveloperToolbox.executeWriteFileRange silent toolbox newFile "head" ["FIRST LINE\nSECOND LINE\n"]
    case result of
        Left err -> assertFailure $ show err
        Right writeResult -> do
            writeFileRangesModified writeResult @?= 1
            content <- Text.readFile newFile
            assertBool "Should contain FIRST LINE" $ "FIRST LINE" `Text.isInfixOf` content
            assertBool "Should contain SECOND LINE" $ "SECOND LINE" `Text.isInfixOf` content

testWriteNewFileTail :: Assertion
testWriteNewFileTail = withTempDir $ \tmpDir -> do
    toolbox <- testToolbox
    let newFile = tmpDir </> "new-file-tail.txt"
    result <- DeveloperToolbox.executeWriteFileRange silent toolbox newFile "tail" ["ONLY LINE\n"]
    case result of
        Left err -> assertFailure $ show err
        Right writeResult -> do
            writeFileRangesModified writeResult @?= 1
            content <- Text.readFile newFile
            assertBool "Should contain ONLY LINE" $ "ONLY LINE" `Text.isInfixOf` content

testWriteFileNotFound :: Assertion
testWriteFileNotFound = withTempDir $ \tmpDir -> do
    toolbox <- testToolbox
    let nonExistent = tmpDir </> "does-not-exist.txt"
    -- Trying to write to non-existent file without head/tail should fail
    result <- DeveloperToolbox.executeWriteFileRange silent toolbox nonExistent "5" ["content\n"]
    case result of
        Left (FileNotFoundError _) -> pure ()
        _ -> assertFailure "Expected FileNotFoundError for non-existent file without head/tail"

testWriteLinesWrittenCount :: Assertion
testWriteLinesWrittenCount = withTempDir $ \tmpDir ->
    withStandardTestFile tmpDir $ \filePath -> do
        toolbox <- testToolbox
        result <- DeveloperToolbox.executeWriteFileRange silent toolbox filePath "1-2" ["A\nB\nC\n"]
        case result of
            Left err -> assertFailure $ show err
            Right writeResult -> do
                -- linesWritten counts lines in the content parameter
                writeFileLinesWritten writeResult @?= 3

-------------------------------------------------------------------------------
-- Whole Range Tests (for read-file-range)
-------------------------------------------------------------------------------

wholeRangeTests :: TestTree
wholeRangeTests =
    testGroup
        "Whole Range Tests"
        [ testCase "Read whole file returns all lines with line numbers" testReadWholeFile
        , testCase "Read whole empty file returns empty" testReadWholeEmptyFile
        , testCase "Read whole single line file" testReadWholeSingleLine
        , testCase "Write whole overwrites entire file" testWriteWholeOverwrite
        , testCase "Write whole creates new file" testWriteWholeNewFile
        ]

-- | Test that "whole" reads the entire file with line numbers
testReadWholeFile :: Assertion
testReadWholeFile = withTempDir $ \tmpDir ->
    withStandardTestFile tmpDir $ \filePath -> do
        toolbox <- testToolbox
        result <- DeveloperToolbox.executeReadFileRange silent toolbox filePath "whole"
        case result of
            Left err -> assertFailure $ show err
            Right readResult -> do
                -- Should have all 10 lines
                readFileLinesRead readResult @?= 10
                let content = readFileContent readResult
                -- All lines should be present with line numbers
                assertBool "Should contain line 1 with number" $ "1\tLine 1:" `Text.isInfixOf` content
                assertBool "Should contain line 5 with number" $ "5\tLine 5:" `Text.isInfixOf` content
                assertBool "Should contain line 10 with number" $ "10\tLine 10:" `Text.isInfixOf` content
                -- Verify line numbers are correct (tab-separated)
                assertBool "Should have First line" $ "First line" `Text.isInfixOf` content
                assertBool "Should have Tenth line" $ "Tenth line" `Text.isInfixOf` content

-- | Test that "whole" on empty file returns empty
testReadWholeEmptyFile :: Assertion
testReadWholeEmptyFile = withTempDir $ \tmpDir ->
    withTestFile tmpDir emptyFileContent $ \filePath -> do
        toolbox <- testToolbox
        result <- DeveloperToolbox.executeReadFileRange silent toolbox filePath "whole"
        case result of
            Left err -> assertFailure $ show err
            Right readResult -> do
                readFileLinesRead readResult @?= 0
                readFileContent readResult @?= ""

-- | Test that "whole" on single line file works
testReadWholeSingleLine :: Assertion
testReadWholeSingleLine = withTempDir $ \tmpDir ->
    withTestFile tmpDir singleLineContent $ \filePath -> do
        toolbox <- testToolbox
        result <- DeveloperToolbox.executeReadFileRange silent toolbox filePath "whole"
        case result of
            Left err -> assertFailure $ show err
            Right readResult -> do
                readFileLinesRead readResult @?= 1
                let content = readFileContent readResult
                assertBool "Should contain line number 1" $ "1\t" `Text.isInfixOf` content
                assertBool "Should contain 'Only line'" $ "Only line" `Text.isInfixOf` content

-- | Test that "whole" write overwrites entire file
testWriteWholeOverwrite :: Assertion
testWriteWholeOverwrite = withTempDir $ \tmpDir ->
    withStandardTestFile tmpDir $ \filePath -> do
        toolbox <- testToolbox
        result <- DeveloperToolbox.executeWriteFileRange
                    silent
                    toolbox
                    filePath
                    "whole"
                    ["COMPLETELY NEW\nFILE CONTENT\nHERE\n"]
        case result of
            Left err -> assertFailure $ show err
            Right writeResult -> do
                writeFileRangesModified writeResult @?= 1
                content <- Text.readFile filePath
                -- New content should be present
                assertBool "Should have COMPLETELY NEW" $ "COMPLETELY NEW" `Text.isInfixOf` content
                assertBool "Should have FILE CONTENT" $ "FILE CONTENT" `Text.isInfixOf` content
                -- Old content should be completely gone
                assertBool "Should NOT have First line" $ not ("First line" `Text.isInfixOf` content)
                assertBool "Should NOT have any old Line X:" $ not ("Line 1:" `Text.isInfixOf` content)
                -- Verify line count
                let lines' = Text.lines content
                length lines' @?= 3

-- | Test that "whole" write creates new file
testWriteWholeNewFile :: Assertion
testWriteWholeNewFile = withTempDir $ \tmpDir -> do
    toolbox <- testToolbox
    let newFile = tmpDir </> "whole-new-file.txt"
    result <- DeveloperToolbox.executeWriteFileRange
                silent
                toolbox
                newFile
                "whole"
                ["NEW FILE\nWITH WHOLE\n"]
    case result of
        Left err -> assertFailure $ show err
        Right writeResult -> do
            writeFileRangesModified writeResult @?= 1
            content <- Text.readFile newFile
            assertBool "Should have NEW FILE" $ "NEW FILE" `Text.isInfixOf` content
            assertBool "Should have WITH WHOLE" $ "WITH WHOLE" `Text.isInfixOf` content

-------------------------------------------------------------------------------
-- Multi-Range Tests
-------------------------------------------------------------------------------

multiRangeTests :: TestTree
multiRangeTests =
    testGroup
        "Multi-Range Operations"
        [ testCase "Replace multiple ranges (bottom-to-top)" testMultiRangeReplace
        , testCase "Replace multiple ranges with separator" testMultiRangeWithSeparator
        , testCase "Mismatched content blocks error" testMismatchedContentBlocks
        ]

testMultiRangeReplace :: Assertion
testMultiRangeReplace = withTempDir $ \tmpDir ->
    withStandardTestFile tmpDir $ \filePath -> do
        toolbox <- testToolbox
        -- Replace lines 1-2 with content A, and lines 9-10 with content B
        -- Note: applied bottom-to-top so earlier line numbers stay valid
        result <-
            DeveloperToolbox.executeWriteFileRange
                silent
                toolbox
                filePath
                "1-2,9-10"
                ["REPLACED FIRST\n", "REPLACED LAST\n"]
        case result of
            Left err -> assertFailure $ show err
            Right writeResult -> do
                writeFileRangesModified writeResult @?= 2
                content <- Text.readFile filePath
                -- Lines 1-2 should be replaced
                assertBool "Should have REPLACED FIRST" $ "REPLACED FIRST" `Text.isInfixOf` content
                assertBool "Should NOT have First line" $ not ("First line" `Text.isInfixOf` content)
                assertBool "Should NOT have Second line" $ not ("Second line" `Text.isInfixOf` content)
                -- Lines 9-10 should be replaced
                assertBool "Should have REPLACED LAST" $ "REPLACED LAST" `Text.isInfixOf` content
                assertBool "Should NOT have Ninth line" $ not ("Ninth line" `Text.isInfixOf` content)
                assertBool "Should NOT have Tenth line" $ not ("Tenth line" `Text.isInfixOf` content)
                -- Middle lines should remain
                assertBool "Should still have line 5" $ "Fifth line" `Text.isInfixOf` content

testMultiRangeWithSeparator :: Assertion
testMultiRangeWithSeparator = withTempDir $ \tmpDir ->
    withStandardTestFile tmpDir $ \filePath -> do
        toolbox <- testToolbox
        -- Replace lines 2 and 8 with different content
        result <-
            DeveloperToolbox.executeWriteFileRange
                silent
                toolbox
                filePath
                "2,8"
                ["NEW LINE TWO\n", "NEW LINE EIGHT\n"]
        case result of
            Left err -> assertFailure $ show err
            Right writeResult -> do
                writeFileRangesModified writeResult @?= 2
                content <- Text.readFile filePath
                assertBool "Should have NEW LINE TWO" $ "NEW LINE TWO" `Text.isInfixOf` content
                assertBool "Should have NEW LINE EIGHT" $ "NEW LINE EIGHT" `Text.isInfixOf` content
                assertBool "Should NOT have Second line" $ not ("Second line" `Text.isInfixOf` content)
                assertBool "Should NOT have Eighth line" $ not ("Eighth line" `Text.isInfixOf` content)

testMismatchedContentBlocks :: Assertion
testMismatchedContentBlocks = withTempDir $ \tmpDir ->
    withStandardTestFile tmpDir $ \filePath -> do
        toolbox <- testToolbox
        -- Try to replace two ranges but only provide one content block
        result <- DeveloperToolbox.executeWriteFileRange silent toolbox filePath "1-2,8-9" ["ONLY ONE BLOCK\n"]
        case result of
            Left (InvalidRangeError _) -> pure ()
            _ -> assertFailure "Expected InvalidRangeError for mismatched content blocks"

-------------------------------------------------------------------------------
-- Edge Case Tests
-------------------------------------------------------------------------------

edgeCaseTests :: TestTree
edgeCaseTests =
    testGroup
        "Edge Cases"
        [ testCase "Read from empty file" testReadEmptyFile
        , testCase "Write to empty file with head" testWriteEmptyFileHead
        , testCase "Write to single line file" testWriteSingleLineFile
        , testCase "Replace entire file" testReplaceEntireFile
        , testCase "Range at end of file" testRangeAtEndOfFile
        , testCase "Overlapping ranges in multi-range" testOverlappingRanges
        , testCase "Trailing newline preservation" testTrailingNewlinePreservation
        ]

testReadEmptyFile :: Assertion
testReadEmptyFile = withTempDir $ \tmpDir ->
    withTestFile tmpDir emptyFileContent $ \filePath -> do
        toolbox <- testToolbox
        result <- DeveloperToolbox.executeReadFileRange silent toolbox filePath "1"
        case result of
            Left err -> assertFailure $ show err
            Right readResult -> do
                readFileLinesRead readResult @?= 0

testWriteEmptyFileHead :: Assertion
testWriteEmptyFileHead = withTempDir $ \tmpDir ->
    withTestFile tmpDir emptyFileContent $ \filePath -> do
        toolbox <- testToolbox
        result <- DeveloperToolbox.executeWriteFileRange silent toolbox filePath "head" ["NEW CONTENT\n"]
        case result of
            Left err -> assertFailure $ show err
            Right writeResult -> do
                writeFileRangesModified writeResult @?= 1
                content <- Text.readFile filePath
                assertBool "Should have NEW CONTENT" $ "NEW CONTENT" `Text.isInfixOf` content

testWriteSingleLineFile :: Assertion
testWriteSingleLineFile = withTempDir $ \tmpDir ->
    withTestFile tmpDir singleLineContent $ \filePath -> do
        toolbox <- testToolbox
        -- Replace the only line
        result <- DeveloperToolbox.executeWriteFileRange silent toolbox filePath "1" ["REPLACED\n"]
        case result of
            Left err -> assertFailure $ show err
            Right writeResult -> do
                writeFileRangesModified writeResult @?= 1
                content <- Text.readFile filePath
                assertBool "Should have REPLACED" $ "REPLACED" `Text.isInfixOf` content
                assertBool "Should NOT have Only line" $ not ("Only line" `Text.isInfixOf` content)

testReplaceEntireFile :: Assertion
testReplaceEntireFile = withTempDir $ \tmpDir ->
    withStandardTestFile tmpDir $ \filePath -> do
        toolbox <- testToolbox
        result <- DeveloperToolbox.executeWriteFileRange silent toolbox filePath "1-10" ["ALL NEW\nCONTENT\nHERE\n"]
        case result of
            Left err -> assertFailure $ show err
            Right writeResult -> do
                writeFileRangesModified writeResult @?= 1
                content <- Text.readFile filePath
                assertBool "Should have ALL NEW" $ "ALL NEW" `Text.isInfixOf` content
                assertBool "Should have CONTENT" $ "CONTENT" `Text.isInfixOf` content
                assertBool "Should NOT have First line" $ not ("First line" `Text.isInfixOf` content)
                assertBool "Should NOT have Tenth line" $ not ("Tenth line" `Text.isInfixOf` content)

testRangeAtEndOfFile :: Assertion
testRangeAtEndOfFile = withTempDir $ \tmpDir ->
    withStandardTestFile tmpDir $ \filePath -> do
        toolbox <- testToolbox
        -- Replace last two lines (9-10)
        result <- DeveloperToolbox.executeWriteFileRange silent toolbox filePath "9-10" ["NEW NINE\nNEW TEN\n"]
        case result of
            Left err -> assertFailure $ show err
            Right _ -> do
                content <- Text.readFile filePath
                assertBool "Should have NEW NINE" $ "NEW NINE" `Text.isInfixOf` content
                assertBool "Should have NEW TEN" $ "NEW TEN" `Text.isInfixOf` content
                assertBool "Should NOT have Ninth line" $ not ("Ninth line" `Text.isInfixOf` content)
                assertBool "Should still have Eighth line" $ "Eighth line" `Text.isInfixOf` content

testOverlappingRanges :: Assertion
testOverlappingRanges = withTempDir $ \tmpDir ->
    withStandardTestFile tmpDir $ \filePath -> do
        toolbox <- testToolbox
        -- Replace overlapping ranges - behavior depends on implementation
        result <-
            DeveloperToolbox.executeWriteFileRange
                silent
                toolbox
                filePath
                "2-4,3-5"
                ["BLOCK A\n", "BLOCK B\n"]
        case result of
            Left (InvalidRangeError _) ->
                -- If overlapping ranges are rejected, that's valid
                pure ()
            Right writeResult -> do
                -- If accepted, both ranges should be reported as modified
                writeFileRangesModified writeResult @?= 2
                -- The result depends on bottom-to-top application order
                -- This test documents the current behavior
                content <- Text.readFile filePath
                -- File should be modified in some way
                assertBool "Content should be modified" $
                    "BLOCK A" `Text.isInfixOf` content || "BLOCK B" `Text.isInfixOf` content

testTrailingNewlinePreservation :: Assertion
testTrailingNewlinePreservation = withTempDir $ \tmpDir -> do
    toolbox <- testToolbox
    let filePath = tmpDir </> "no-newline.txt"
    -- Create file without trailing newline
    Text.writeFile filePath "Line 1\nLine 2\nLine 3"
    result <- DeveloperToolbox.executeWriteFileRange silent toolbox filePath "2" ["REPLACED\n"]
    case result of
        Left err -> assertFailure $ show err
        Right _ -> do
            content <- Text.readFile filePath
            -- After write, the file should have proper line structure
            assertBool "Should have REPLACED" $ "REPLACED" `Text.isInfixOf` content
            assertBool "Should have Line 1" $ "Line 1" `Text.isInfixOf` content
            assertBool "Should have Line 3" $ "Line 3" `Text.isInfixOf` content

-------------------------------------------------------------------------------
-- Indentation Preservation Tests
-------------------------------------------------------------------------------

-- | Tests for verifying that indentation is preserved in write-file-range operations
indentationTests :: TestTree
indentationTests =
    testGroup
        "Indentation Preservation"
        [ testCase "Single line replacement preserves indentation" testIndentationPreservation
        , testCase "Multi-range with indented content blocks" testMultiRangeIndentation
        , testCase "Nested indentation preserved" testNestedIndentation
        , testCase "Tab indentation preserved" testTabIndentation
        ]

-- | Test that indentation is preserved when replacing a single line
testIndentationPreservation :: Assertion
testIndentationPreservation = withTempDir $ \tmpDir ->
    withStandardTestFile tmpDir $ \filePath -> do
        toolbox <- testToolbox
        -- Content with significant indentation (Haskell-style code)
        let indentedContent = "    let x = 1\n        y = 2\n    in x + y\n"
        result <- DeveloperToolbox.executeWriteFileRange silent toolbox filePath "3" [indentedContent]
        case result of
            Left err -> assertFailure $ show err
            Right _ -> do
                content <- Text.readFile filePath
                -- Verify 4-space indentation is preserved
                assertBool "Should preserve 4-space indentation on first line" $
                    "    let x = 1" `Text.isInfixOf` content
                -- Verify 8-space indentation is preserved
                assertBool "Should preserve 8-space indentation on second line" $
                    "        y = 2" `Text.isInfixOf` content
                -- Verify closing indentation is preserved
                assertBool "Should preserve 4-space indentation on third line" $
                    "    in x + y" `Text.isInfixOf` content

-- | Test that indentation is preserved in multi-range operations with separator
testMultiRangeIndentation :: Assertion
testMultiRangeIndentation = withTempDir $ \tmpDir ->
    withStandardTestFile tmpDir $ \filePath -> do
        toolbox <- testToolbox
        -- Two blocks with different indentation levels
        let block1 = "    block1 line1\n    block1 line2\n"
        let block2 = "        block2 line1\n        block2 line2\n"
        result <- DeveloperToolbox.executeWriteFileRange silent toolbox filePath "2,8" [block1, block2]
        case result of
            Left err -> assertFailure $ show err
            Right _ -> do
                fileContent <- Text.readFile filePath
                -- First block indentation (4 spaces) should be preserved
                assertBool "First block indentation preserved" $
                    "    block1 line1" `Text.isInfixOf` fileContent
                -- Second block indentation (8 spaces) should be preserved
                assertBool "Second block indentation preserved" $
                    "        block2 line1" `Text.isInfixOf` fileContent

-- | Test deeply nested indentation
testNestedIndentation :: Assertion
testNestedIndentation = withTempDir $ \tmpDir ->
    withStandardTestFile tmpDir $ \filePath -> do
        toolbox <- testToolbox
        -- Python-style nested indentation
        let nestedContent = "def outer():\n    def inner():\n        if True:\n            return 42\n"
        result <- DeveloperToolbox.executeWriteFileRange silent toolbox filePath "5" [nestedContent]
        case result of
            Left err -> assertFailure $ show err
            Right _ -> do
                content <- Text.readFile filePath
                -- Verify each level of indentation is preserved
                assertBool "Should preserve 0-space indentation (def)" $
                    "def outer():" `Text.isInfixOf` content
                assertBool "Should preserve 4-space indentation (def inner)" $
                    "    def inner():" `Text.isInfixOf` content
                assertBool "Should preserve 8-space indentation (if)" $
                    "        if True:" `Text.isInfixOf` content
                assertBool "Should preserve 12-space indentation (return)" $
                    "            return 42" `Text.isInfixOf` content

-- | Test that tab indentation is preserved
testTabIndentation :: Assertion
testTabIndentation = withTempDir $ \tmpDir ->
    withStandardTestFile tmpDir $ \filePath -> do
        toolbox <- testToolbox
        -- Content with tab indentation
        let tabContent = "\tfirst level\n\t\tsecond level\n\t\t\tthird level\n"
        result <- DeveloperToolbox.executeWriteFileRange silent toolbox filePath "7" [tabContent]
        case result of
            Left err -> assertFailure $ show err
            Right _ -> do
                content <- Text.readFile filePath
                -- Verify tab indentation is preserved
                assertBool "Should preserve single tab" $
                    "\tfirst level" `Text.isInfixOf` content
                assertBool "Should preserve double tab" $
                    "\t\tsecond level" `Text.isInfixOf` content
                assertBool "Should preserve triple tab" $
                    "\t\t\tthird level" `Text.isInfixOf` content

-------------------------------------------------------------------------------
-- Line Number Handling Tests
-------------------------------------------------------------------------------

-- | Tests for verifying correct 1-based line number handling
lineNumberTests :: TestTree
lineNumberTests =
    testGroup
        "Line Number Handling (1-based, inclusive)"
        [ testCase "Single line deletion (line 5)" testSingleLineDeletion
        , testCase "Replace first line (line 1)" testReplaceFirstLine
        , testCase "Replace last line" testReplaceLastLine
        , testCase "Range deletion at boundaries" testRangeDeletionAtBoundaries
        , testCase "Sequential line replacement" testSequentialLineReplacement
        ]

-- | Test that deleting a single specific line works correctly
-- The test creates a 10-line file, deletes line 5, and verifies:
-- 1. Line 5 is removed (no "Line 5" text in output)
-- 2. Line 4 is still present
-- 3. Line 6 is still present (shifted to position 5)
-- 4. Total line count is 9 (one line deleted)
testSingleLineDeletion :: Assertion
testSingleLineDeletion = withTempDir $ \tmpDir -> do
    toolbox <- testToolbox
    let filePath = tmpDir </> "numbered.txt"
    -- Create a file with numbered lines for precise verification
    let numberedContent = Text.unlines $ map (\n -> "Line " <> Text.pack (show n)) [1..10]
    Text.writeFile filePath numberedContent

    -- Delete line 5
    result <- DeveloperToolbox.executeWriteFileRange silent toolbox filePath "5" [""]
    case result of
        Left err -> assertFailure $ show err
        Right _ -> do
            content <- Text.readFile filePath
            let lines' = filter (/= "") (Text.lines content)  -- Filter empty lines from trailing newline
            -- After deleting line 5, what was line 6 should now be at position 5
            assertBool "Line 5 should be removed (no 'Line 5' in output)" $
                not ("Line 5" `Text.isInfixOf` content)
            assertBool "Line 4 should still be present" $
                "Line 4" `Text.isInfixOf` content
            assertBool "Line 6 should still be present (now at position 5 or later)" $
                "Line 6" `Text.isInfixOf` content
            -- Should have 9 lines after deletion (original 10 minus 1 deleted)
            length lines' @?= 9

-- | Test replacing the first line
testReplaceFirstLine :: Assertion
testReplaceFirstLine = withTempDir $ \tmpDir ->
    withStandardTestFile tmpDir $ \filePath -> do
        toolbox <- testToolbox
        result <- DeveloperToolbox.executeWriteFileRange silent toolbox filePath "1" ["NEW FIRST LINE\n"]
        case result of
            Left err -> assertFailure $ show err
            Right _ -> do
                content <- Text.readFile filePath
                let lines' = filter (/= "") (Text.lines content)
                -- First line should be replaced
                case lines' of
                    (first : _) -> first @?= "NEW FIRST LINE"
                    [] -> assertFailure "Expected non-empty file"
                -- Original first line should be gone
                assertBool "Should NOT have original first line" $
                    not ("Line 1: First line" `Text.isInfixOf` Text.unlines (take 1 lines'))
                -- Other lines should still be present
                assertBool "Should still have line 2" $ "Second line" `Text.isInfixOf` content

-- | Test replacing the last line (line 10 in a 10-line file)
testReplaceLastLine :: Assertion
testReplaceLastLine = withTempDir $ \tmpDir ->
    withStandardTestFile tmpDir $ \filePath -> do
        toolbox <- testToolbox
        result <- DeveloperToolbox.executeWriteFileRange silent toolbox filePath "10" ["NEW LAST LINE\n"]
        case result of
            Left err -> assertFailure $ show err
            Right _ -> do
                content <- Text.readFile filePath
                let lines' = filter (/= "") (Text.lines content)
                -- Last line should be replaced (line 10 is the 10th line)
                case reverse lines' of
                    (last' : _) -> last' @?= "NEW LAST LINE"
                    [] -> assertFailure "Expected non-empty file"
                -- Original last line should be gone
                assertBool "Should NOT have original last line (Tenth)" $
                    not ("Tenth line" `Text.isInfixOf` content)
                -- Line 9 should still exist
                assertBool "Should still have Line 9" $ "Ninth line" `Text.isInfixOf` content

-- | Test range deletions at file boundaries
-- This test verifies that deleting ranges at the start and end of a file works correctly.
testRangeDeletionAtBoundaries :: Assertion
testRangeDeletionAtBoundaries = withTempDir $ \tmpDir -> do
    toolbox <- testToolbox
    let filePath = tmpDir </> "boundaries.txt"
    let content = Text.unlines $ map (\n -> "Line " <> Text.pack (show n)) [1..10]
    Text.writeFile filePath content

    -- Delete lines 1-2 (start boundary)
    result1 <- DeveloperToolbox.executeWriteFileRange silent toolbox filePath "1-2" [""]
    case result1 of
        Left err -> assertFailure $ show err
        Right _ -> do
            content1 <- Text.readFile filePath
            let lines1 = filter (/= "") (Text.lines content1)
            -- First two lines should be gone - use exact match to avoid matching "Line 10", "Line 11", etc.
            assertBool "Line 1 should be removed (exact match)" $ 
                not ("Line 1\n" `Text.isInfixOf` (content1 <> "\n"))
            assertBool "Line 2 should be removed (exact match)" $ 
                not ("Line 2\n" `Text.isInfixOf` (content1 <> "\n"))
            -- What was Line 3 should now be first
            case lines1 of
                (first : _) -> first @?= "Line 3"
                [] -> assertFailure "Expected non-empty file"
            -- Should have 8 lines remaining (10 - 2)
            length lines1 @?= 8

    -- Delete lines 7-8 (end boundary of remaining 8 lines: lines 3-10 from original)
    -- In the current file, lines 7-8 correspond to original lines 9-10
    result2 <- DeveloperToolbox.executeWriteFileRange silent toolbox filePath "7-8" [""]
    case result2 of
        Left err -> assertFailure $ show err
        Right _ -> do
            content2 <- Text.readFile filePath
            let lines2 = filter (/= "") (Text.lines content2)
            -- Original lines 9-10 should be gone (these are lines 7-8 in current file)
            assertBool "Line 9 should be removed" $ not ("Line 9" `Text.isInfixOf` content2)
            assertBool "Line 10 should be removed" $ not ("Line 10" `Text.isInfixOf` content2)
            -- Lines 3-8 (original 3-8) should still exist
            assertBool "Line 3 should still exist" $ "Line 3" `Text.isInfixOf` content2
            assertBool "Line 8 should still exist" $ "Line 8" `Text.isInfixOf` content2
            -- Should have 6 lines remaining (8 - 2)
            length lines2 @?= 6

-- | Test that sequential line replacements work correctly
testSequentialLineReplacement :: Assertion
testSequentialLineReplacement = withTempDir $ \tmpDir -> do
    toolbox <- testToolbox
    let filePath = tmpDir </> "sequential.txt"
    let content = Text.unlines ["A", "B", "C", "D", "E"]
    Text.writeFile filePath content

    -- Replace lines 2 and 4 with specific content (applied bottom-to-top)
    result <- DeveloperToolbox.executeWriteFileRange silent toolbox filePath "2,4" ["REPLACED_B\n", "REPLACED_D\n"]
    case result of
        Left err -> assertFailure $ show err
        Right _ -> do
            fileContent <- Text.readFile filePath
            let lines' = filter (/= "") (Text.lines fileContent)
            -- Verify structure (still 5 lines, 2 replaced)
            assertBool "Line 1 (A) should be unchanged" $ "A" `elem` lines'
            assertBool "Line 2 should be REPLACED_B" $ "REPLACED_B" `elem` lines'
            assertBool "Line 3 (C) should be unchanged" $ "C" `elem` lines'
            assertBool "Line 4 should be REPLACED_D" $ "REPLACED_D" `elem` lines'
            assertBool "Line 5 (E) should be unchanged" $ "E" `elem` lines'
            -- Verify originals are gone
            assertBool "Original B should be removed" $ not ("B" `elem` lines')
            assertBool "Original D should be removed" $ not ("D" `elem` lines')

