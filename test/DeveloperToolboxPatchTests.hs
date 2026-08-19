{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the DeveloperToolbox patch-file capability.
--
-- These tests cover both the happy path and the exact-context failure modes
-- that make 'patch-file' tricky to use in practice.  They are intended to
-- catch regressions in context matching, blank-line handling, trailing
-- context, multi-hunk application, and snapshot-based optimistic locking.
module DeveloperToolboxPatchTests where

import Control.Exception (bracket)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory (removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import Prod.Tracer (silent)

import System.Agents.Base (DeveloperToolboxDescription (..), DeveloperToolCapability (..), FileSandboxConfig (..))
import System.Agents.FileSandbox.Predicate (PathPredicate (..))
import System.Agents.Tools.DeveloperToolbox as DeveloperToolbox
import System.Agents.Tools.DeveloperToolbox.Types (PatchError (..), SnapshotRef (..))

-- | Toolbox with the patch-file capability enabled.
patchToolbox :: IO Toolbox
patchToolbox = do
    let desc =
            DeveloperToolboxDescription
                { developerToolboxName = "test-patch-developer"
                , developerToolboxDescription = "Test developer toolbox with patch-file"
                , developerToolboxCapabilities =
                    [ DevToolReadFileRange
                    , DevToolWriteFileRange
                    , DevToolPatchFile
                    ]
                , developerToolboxActivation = Nothing
                , developerToolboxFileSandbox =
                    Just
                        FileSandboxConfig
                            { fsbPredicate = AlwaysAllow
                            , fsbMaxFileSize = Nothing
                            , fsbName = Nothing
                            }
                }
    result <- DeveloperToolbox.initializeToolbox silent desc
    case result of
        Left err -> error $ "Failed to initialize patch toolbox: " ++ err
        Right toolbox -> pure toolbox

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir action = do
    tmpDir <- createTempDirectory "/tmp" "devtoolbox-patch-test-"
    bracket
        (pure tmpDir)
        removeDirectoryRecursive
        action

withTestFile :: FilePath -> Text -> (FilePath -> IO a) -> IO a
withTestFile tmpDir content action = do
    let filePath = tmpDir </> "testfile.txt"
    Text.writeFile filePath content
    action filePath

applyPatch :: Toolbox -> FilePath -> Text -> Maybe SnapshotRef -> IO (Either DeveloperToolError PatchResult)
applyPatch = DeveloperToolbox.executePatchFile silent

tests :: TestTree
tests =
    testGroup
        "DeveloperToolbox Patch Tests"
        [ testCase "Simple replacement patch succeeds" testSimpleReplacementPatch
        , testCase "Missing blank line in context fails" testBlankLineContextMismatch
        , testCase "Wrong trailing context fails" testTrailingContextMismatch
        , testCase "Haskell comment removal is parsed correctly" testHaskellCommentRemoval
        , testCase "Multi-hunk patch with line insertions succeeds" testMultiHunkNoDrift
        , testCase "Expected snapshot mismatch is rejected" testExpectedSnapshotMismatch
        , testCase "Expected snapshot match succeeds" testExpectedSnapshotMatch
        ]

-- | Baseline: a well-formed patch with exact context applies cleanly.
testSimpleReplacementPatch :: Assertion
testSimpleReplacementPatch = withTempDir $ \tmpDir ->
    withTestFile tmpDir (Text.unlines ["line 1", "line 2", "line 3", "line 4"]) $ \filePath -> do
        toolbox <- patchToolbox
        let patch =
                Text.unlines
                    [ "--- a/file"
                    , "+++ b/file"
                    , "@@ -2,3 +2,4 @@"
                    , " line 2"
                    , "-line 3"
                    , "+line 3 new"
                    , "+line 3 extra"
                    , " line 4"
                    ]
        result <- applyPatch toolbox filePath patch Nothing
        case result of
            Left err -> assertFailure $ show err
            Right patchResult -> do
                patchHunksApplied patchResult @?= 1
                patchHunksRejected patchResult @?= 0
                content <- Text.readFile filePath
                let lines' = Text.lines content
                lines'
                    @?= [ "line 1"
                        , "line 2"
                        , "line 3 new"
                        , "line 3 extra"
                        , "line 4"
                        ]

-- | Reproduces the blank-line failure mode: a patch that omits a blank line
-- from the surrounding context is rejected with a clear context-mismatch
-- error instead of silently corrupting the file.
testBlankLineContextMismatch :: Assertion
testBlankLineContextMismatch = withTempDir $ \tmpDir ->
    withTestFile
        tmpDir
        (Text.unlines ["module Main where", "", "import Foo", "", "main = return ()"])
        $ \filePath -> do
            toolbox <- patchToolbox
            -- The patch context skips the blank line at line 2.
            let patch =
                    Text.unlines
                        [ "--- a/file"
                        , "+++ b/file"
                        , "@@ -1,3 +1,3 @@"
                        , " module Main where"
                        , "-import Foo"
                        , "+import Bar"
                        , " main = return ()"
                        ]
            result <- applyPatch toolbox filePath patch Nothing
            case result of
                Right _ -> assertFailure "Expected PatchContextMismatch for missing blank line"
                Left (PatchValidationError (PatchContextMismatch line _ expected actual)) -> do
                    line @?= 2
                    actual @?= [""]
                    expected @?= ["import Foo"]
                Left err ->
                    assertFailure $
                        "Expected PatchContextMismatch, got: " ++ show err
            -- The file must be left untouched.
            content <- Text.readFile filePath
            content
                @?= Text.unlines
                    ["module Main where", "", "import Foo", "", "main = return ()"]

-- | Reproduces the trailing-context failure mode: a patch that guesses the
-- closing context incorrectly (e.g. a ')' that does not exist in the file) is
-- rejected.
testTrailingContextMismatch :: Assertion
testTrailingContextMismatch = withTempDir $ \tmpDir ->
    withTestFile
        tmpDir
        (Text.unlines ["takeParent :: Path -> Path", "takeParent path = dirname path", "", "filterPath :: Path -> Bool"])
        $ \filePath -> do
            toolbox <- patchToolbox
            -- The patch claims the changed line is followed by ')', but the
            -- file has a blank line there.
            let patch =
                    Text.unlines
                        [ "--- a/file"
                        , "+++ b/file"
                        , "@@ -1,3 +1,3 @@"
                        , " takeParent :: Path -> Path"
                        , "-takeParent path = dirname path"
                        , "+takeParent path = takeDirectory path"
                        , " )"
                        ]
            result <- applyPatch toolbox filePath patch Nothing
            case result of
                Right _ -> assertFailure "Expected PatchContextMismatch for wrong trailing context"
                Left (PatchValidationError (PatchContextMismatch line _ _ _)) ->
                    line @?= 3
                Left err ->
                    assertFailure $
                        "Expected PatchContextMismatch, got: " ++ show err

-- | Removed lines that start with '--' (typical of Haskell comments) must not
-- be confused with the '--- a/file' header.  This is a regression test for a
-- parser bug where any line beginning with '---' was treated as a file header
-- rather than a removed line.
testHaskellCommentRemoval :: Assertion
testHaskellCommentRemoval = withTempDir $ \tmpDir ->
    withTestFile
        tmpDir
        (Text.unlines ["module Main where", "-- old comment", "import Foo"])
        $ \filePath -> do
            toolbox <- patchToolbox
            let patch =
                    Text.unlines
                        [ "--- a/file"
                        , "+++ b/file"
                        , "@@ -2,1 +2,0 @@"
                        , "--- old comment"
                        ]
            result <- applyPatch toolbox filePath patch Nothing
            case result of
                Left err -> assertFailure $ show err
                Right patchResult -> do
                    patchHunksApplied patchResult @?= 1
                    content <- Text.readFile filePath
                    Text.lines content
                        @?= ["module Main where", "import Foo"]
-- | A single patch can contain several hunks that change line counts.  The
-- implementation applies them bottom-to-top so earlier hunks do not shift
-- the coordinates of later hunks.
testMultiHunkNoDrift :: Assertion
testMultiHunkNoDrift = withTempDir $ \tmpDir ->
    withTestFile tmpDir (Text.unlines ["one", "two", "three", "four", "five", "six"]) $ \filePath -> do
        toolbox <- patchToolbox
        let patch =
                Text.unlines
                    [ "--- a/file"
                    , "+++ b/file"
                    , "@@ -2,2 +2,3 @@"
                    , " two"
                    , "+two and a half"
                    , " three"
                    , "@@ -5,2 +6,3 @@"
                    , " five"
                    , "+five and a half"
                    , " six"
                    ]
        result <- applyPatch toolbox filePath patch Nothing
        case result of
            Left err -> assertFailure $ show err
            Right patchResult -> do
                patchHunksApplied patchResult @?= 2
                content <- Text.readFile filePath
                Text.lines content
                    @?= [ "one"
                        , "two"
                        , "two and a half"
                        , "three"
                        , "four"
                        , "five"
                        , "five and a half"
                        , "six"
                        ]

-- | If the caller passes an 'expected_snapshot_ref' that does not match the
-- current file, the patch is rejected before any context validation.
testExpectedSnapshotMismatch :: Assertion
testExpectedSnapshotMismatch = withTempDir $ \tmpDir ->
    withTestFile tmpDir (Text.unlines ["line 1", "line 2", "line 3"]) $ \filePath -> do
        toolbox <- patchToolbox
        let patch =
                Text.unlines
                    [ "--- a/file"
                    , "+++ b/file"
                    , "@@ -2,1 +2,1 @@"
                    , "-line 2"
                    , "+replaced"
                    ]
        let wrongRef = SnapshotRef "deadbeefdeadbeefdeadbeefdeadbeef"
        result <- applyPatch toolbox filePath patch (Just wrongRef)
        result <- applyPatch toolbox filePath patch (Just wrongRef)
        case result of
            Right _ -> assertFailure "Expected SnapshotMismatchError"
            Left (SnapshotMismatchError expected actual) -> do
                expected @?= wrongRef
                -- The actual ref should be the real MD5 of the file content.
                assertBool "actual snapshot ref should differ from wrong ref" $
                    actual /= wrongRef
            Left err ->
                assertFailure $
                    "Expected SnapshotMismatchError, got: " ++ show err

-- | When the caller passes the correct 'expected_snapshot_ref', the patch
-- succeeds and the result contains the before/after snapshot references.
testExpectedSnapshotMatch :: Assertion
testExpectedSnapshotMatch = withTempDir $ \tmpDir ->
    withTestFile tmpDir (Text.unlines ["line 1", "line 2", "line 3"]) $ \filePath -> do
        toolbox <- patchToolbox
        beforeBytes <- BS.readFile filePath
        let beforeRef = DeveloperToolbox.snapshotRef $ DeveloperToolbox.makeSnapshot beforeBytes
        let patch =
                Text.unlines
                    [ "--- a/file"
                    , "+++ b/file"
                    , "@@ -2,1 +2,1 @@"
                    , "-line 2"
                    , "+replaced"
                    ]
        result <- applyPatch toolbox filePath patch (Just beforeRef)
        case result of
            Left err -> assertFailure $ show err
            Right patchResult -> do
                patchBeforeSnapshotRef patchResult @?= Just beforeRef
                assertBool "after snapshot ref should differ from before" $
                    patchAfterSnapshotRef patchResult /= Just beforeRef
                content <- Text.readFile filePath
                assertBool "Should contain replaced line" $
                    "replaced" `Text.isInfixOf` content
                assertBool "Should not contain original line 2" $
                    not ("line 2" `Text.isInfixOf` content)

