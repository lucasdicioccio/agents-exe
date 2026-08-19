{-# LANGUAGE OverloadedStrings #-}

-- | Tests for "System.Agents.FileSandbox.Predicate".
--
-- The tests exercise the predicate evaluator against a small on-disk fixture
-- tree under @test/data/tree@.  They cover exact matches, recursive directory
-- access, filename prefix/suffix patterns, file extension filtering, size
-- limits, and the boolean combinators ("And", "Or", "Not", "Any", "All").
module FileSandboxPredicateTests
    ( tests
    ) where

import Data.Either (isLeft)
import System.Directory (canonicalizePath, withCurrentDirectory)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

import System.Agents.FileSandbox.Predicate

-- -------------------------------------------------------------------------
-- Test entry point
-- -------------------------------------------------------------------------

-- | All file-sandbox predicate tests.
tests :: TestTree
tests =
    testGroup
        "FileSandbox Predicate Tests"
        [ exactMatchTests
        , recursiveDirectoryTests
        , prefixSuffixTests
        , extensionTests
        , sizeTests
        , shallowDirectoryTests
        , childOfTests
        , booleanLogicTests
        , relativePredicateTests
        ]

-- | Absolute path to the fixture tree used by the tests.
fixtureRoot :: IO FilePath
fixtureRoot = canonicalizePath "test/data/tree"

-- -------------------------------------------------------------------------
-- Assertion helpers
-- -------------------------------------------------------------------------

-- | Assert that a predicate grants access to the supplied path.
assertAllowed :: PathPredicate -> FilePath -> Assertion
assertAllowed predicate path = do
    result <- evaluatePredicate predicate path
    result @?= Right ()

-- | Assert that a predicate denies access to the supplied path.
assertDenied :: PathPredicate -> FilePath -> Assertion
assertDenied predicate path = do
    result <- evaluatePredicate predicate path
    assertBool "expected predicate to deny access" (isLeft result)

-- -------------------------------------------------------------------------
-- 1. Exact path matches
-- -------------------------------------------------------------------------

exactMatchTests :: TestTree
exactMatchTests =
    testGroup
        "Exact path matches"
        [ testCase "FileExactly allows the exact file" $ do
            root <- fixtureRoot
            assertAllowed (FileExactly (root </> "src" </> "Main.hs")) (root </> "src" </> "Main.hs")
        , testCase "FileExactly denies a different file" $ do
            root <- fixtureRoot
            assertDenied (FileExactly (root </> "src" </> "Main.hs")) (root </> "src" </> "Utils.hs")
        , testCase "FileExactly denies a directory" $ do
            root <- fixtureRoot
            assertDenied (FileExactly (root </> "src" </> "Main.hs")) (root </> "src")
        , testCase "DirectoryExactly allows the exact directory" $ do
            root <- fixtureRoot
            assertAllowed (DirectoryExactly (root </> "docs")) (root </> "docs")
        , testCase "DirectoryExactly denies a child file" $ do
            root <- fixtureRoot
            assertDenied (DirectoryExactly (root </> "docs")) (root </> "docs" </> "README.md")
        , testCase "DirectoryExactly denies a sibling directory" $ do
            root <- fixtureRoot
            assertDenied (DirectoryExactly (root </> "docs")) (root </> "src")
        ]

-- -------------------------------------------------------------------------
-- 2. Recursive directories
-- -------------------------------------------------------------------------

recursiveDirectoryTests :: TestTree
recursiveDirectoryTests =
    testGroup
        "Recursive directory access"
        [ testCase "DirectoryRecursive allows the directory itself" $ do
            root <- fixtureRoot
            assertAllowed (DirectoryRecursive (root </> "src")) (root </> "src")
        , testCase "DirectoryRecursive allows direct children" $ do
            root <- fixtureRoot
            assertAllowed (DirectoryRecursive (root </> "src")) (root </> "src" </> "Main.hs")
            assertAllowed (DirectoryRecursive (root </> "src")) (root </> "src" </> "Utils.hs")
        , testCase "DirectoryRecursive denies paths outside the tree" $ do
            root <- fixtureRoot
            assertDenied (DirectoryRecursive (root </> "src")) (root </> "docs" </> "README.md")
        , testCase "DirectoryRecursive allows nested descendants" $ do
            root <- fixtureRoot
            assertAllowed (DirectoryRecursive (root </> "config")) (root </> "config" </> "nested" </> "deep.json")
        , testCase "DirectoryRecursive over the root allows everything" $ do
            root <- fixtureRoot
            assertAllowed (DirectoryRecursive root) (root </> "src" </> "Main.hs")
            assertAllowed (DirectoryRecursive root) (root </> "docs" </> "README.md")
            assertAllowed (DirectoryRecursive root) (root </> "secret.key")
        ]

-- -------------------------------------------------------------------------
-- 3. Prefix/suffix filename patterns
-- -------------------------------------------------------------------------

prefixSuffixTests :: TestTree
prefixSuffixTests =
    testGroup
        "Prefix/suffix filename patterns"
        [ testCase "FilePattern '*.hs' matches haskell files" $ do
            root <- fixtureRoot
            assertAllowed (FilePattern "*.hs") (root </> "src" </> "Main.hs")
            assertAllowed (FilePattern "*.hs") (root </> "src" </> "Utils.hs")
        , testCase "FilePattern '*.hs' denies non-haskell files" $ do
            root <- fixtureRoot
            assertDenied (FilePattern "*.hs") (root </> "docs" </> "README.md")
        , testCase "FilePattern 'README*' matches by prefix" $ do
            root <- fixtureRoot
            assertAllowed (FilePattern "README*") (root </> "docs" </> "README.md")
        , testCase "FilePattern 'README*' denies files with different prefix" $ do
            root <- fixtureRoot
            assertDenied (FilePattern "README*") (root </> "docs" </> "CHANGELOG.md")
        , testCase "FilePattern '*' matches any filename" $ do
            root <- fixtureRoot
            assertAllowed (FilePattern "*") (root </> "secret.key")
        ]

-- -------------------------------------------------------------------------
-- 4. File extension filtering
-- -------------------------------------------------------------------------

extensionTests :: TestTree
extensionTests =
    testGroup
        "File extension filtering"
        [ testCase "FileExtension allows listed extensions" $ do
            root <- fixtureRoot
            assertAllowed (FileExtension ["hs", "md"]) (root </> "src" </> "Main.hs")
            assertAllowed (FileExtension ["hs", "md"]) (root </> "docs" </> "README.md")
        , testCase "FileExtension denies unlisted extensions" $ do
            root <- fixtureRoot
            assertDenied (FileExtension ["hs", "md"]) (root </> "config" </> "settings.json")
        , testCase "FileExtension is sensitive to the leading dot" $ do
            root <- fixtureRoot
            assertDenied (FileExtension [".hs"]) (root </> "src" </> "Main.hs")
        ]

-- -------------------------------------------------------------------------
-- 5. File size limits
-- -------------------------------------------------------------------------

sizeTests :: TestTree
sizeTests =
    testGroup
        "File size limits"
        [ testCase "FileSizeLessThan allows small files" $ do
            root <- fixtureRoot
            assertAllowed (FileSizeLessThan 1024) (root </> "top.txt")
        , testCase "FileSizeLessThan denies files that are too large" $ do
            root <- fixtureRoot
            assertDenied (FileSizeLessThan 2) (root </> "top.txt")
        ]

-- -------------------------------------------------------------------------
-- 6. Shallow directory access
-- -------------------------------------------------------------------------

shallowDirectoryTests :: TestTree
shallowDirectoryTests =
    testGroup
        "Shallow directory access"
        [ testCase "DirectoryShallow allows direct children" $ do
            root <- fixtureRoot
            assertAllowed (DirectoryShallow (root </> "config")) (root </> "config" </> "settings.json")
        , testCase "DirectoryShallow denies nested descendants" $ do
            root <- fixtureRoot
            assertDenied (DirectoryShallow (root </> "config")) (root </> "config" </> "nested" </> "deep.json")
        , testCase "DirectoryShallow denies the directory itself" $ do
            root <- fixtureRoot
            assertDenied (DirectoryShallow (root </> "config")) (root </> "config")
        ]

-- -------------------------------------------------------------------------
-- 7. Child-of relationship
-- -------------------------------------------------------------------------

childOfTests :: TestTree
childOfTests =
    testGroup
        "Child-of relationship"
        [ testCase "ChildOf allows descendants" $ do
            root <- fixtureRoot
            assertAllowed (ChildOf root) (root </> "src" </> "Main.hs")
        , testCase "ChildOf allows the directory itself" $ do
            root <- fixtureRoot
            assertAllowed (ChildOf (root </> "src")) (root </> "src")
        , testCase "ChildOf denies paths outside the tree" $ do
            root <- fixtureRoot
            assertDenied (ChildOf (root </> "src")) (root </> "docs" </> "README.md")
        ]

-- -------------------------------------------------------------------------
-- 8. Boolean combinators
-- -------------------------------------------------------------------------

booleanLogicTests :: TestTree
booleanLogicTests =
    testGroup
        "Boolean predicate combinators"
        [ testCase "Or allows when left branch matches" $ do
            root <- fixtureRoot
            let predicate = Or (FileExtension ["hs"]) (FilePattern "README*")
            assertAllowed predicate (root </> "src" </> "Main.hs")
        , testCase "Or allows when right branch matches" $ do
            root <- fixtureRoot
            let predicate = Or (FileExtension ["hs"]) (FilePattern "README*")
            assertAllowed predicate (root </> "docs" </> "README.md")
        , testCase "Or denies when neither branch matches" $ do
            root <- fixtureRoot
            let predicate = Or (FileExtension ["hs"]) (FilePattern "README*")
            assertDenied predicate (root </> "secret.key")
        , testCase "And allows when both branches match" $ do
            root <- fixtureRoot
            let predicate = And (DirectoryRecursive (root </> "src")) (FilePattern "Main.*")
            assertAllowed predicate (root </> "src" </> "Main.hs")
        , testCase "And denies when only left branch matches" $ do
            root <- fixtureRoot
            let predicate = And (DirectoryRecursive (root </> "src")) (FilePattern "Main.*")
            assertDenied predicate (root </> "src" </> "Utils.hs")
        , testCase "And denies when only right branch matches" $ do
            root <- fixtureRoot
            let predicate = And (DirectoryRecursive (root </> "src")) (FilePattern "Main.*")
            assertDenied predicate (root </> "docs" </> "README.md")
        , testCase "Any allows when any member matches" $ do
            root <- fixtureRoot
            let predicate = Any [FileExtension ["md"], FileExtension ["json"]]
            assertAllowed predicate (root </> "docs" </> "README.md")
            assertAllowed predicate (root </> "config" </> "settings.json")
        , testCase "Any denies when no member matches" $ do
            root <- fixtureRoot
            let predicate = Any [FileExtension ["md"], FileExtension ["json"]]
            assertDenied predicate (root </> "src" </> "Main.hs")
        , testCase "All allows when every member matches" $ do
            root <- fixtureRoot
            let predicate = All [DirectoryRecursive root, FileExtension ["json"]]
            assertAllowed predicate (root </> "config" </> "settings.json")
        , testCase "All denies when one member fails" $ do
            root <- fixtureRoot
            let predicate = All [DirectoryRecursive root, FileExtension ["json"]]
            assertDenied predicate (root </> "src" </> "Main.hs")
        , testCase "Not inverts an allowed predicate" $ do
            root <- fixtureRoot
            assertDenied (Not (FileExtension ["hs"])) (root </> "src" </> "Main.hs")
        , testCase "Not inverts a denied predicate" $ do
            root <- fixtureRoot
            assertAllowed (Not (FileExtension ["hs"])) (root </> "secret.key")
        , testCase "complex nested predicate" $ do
            root <- fixtureRoot
            -- Allow haskell files in src OR anything under docs.
            let predicate =
                    Or
                        (And (DirectoryRecursive (root </> "src")) (FileExtension ["hs"]))
                        (DirectoryRecursive (root </> "docs"))
            assertAllowed predicate (root </> "src" </> "Main.hs")
            assertAllowed predicate (root </> "docs" </> "CHANGELOG.md")
            assertDenied predicate (root </> "config" </> "settings.json")
        ]

-- -------------------------------------------------------------------------
-- 9. Relative predicate resolution
-- -------------------------------------------------------------------------

relativePredicateTests :: TestTree
relativePredicateTests =
    testGroup
        "Relative predicate resolution"
        [ testCase "DirectoryRecursive with relative path resolves from cwd" $ do
            root <- fixtureRoot
            withCurrentDirectory root $ do
                assertAllowed (DirectoryRecursive "src") (root </> "src" </> "Main.hs")
                assertDenied (DirectoryRecursive "src") (root </> "docs" </> "README.md")
        , testCase "FileExactly with relative path resolves from cwd" $ do
            root <- fixtureRoot
            withCurrentDirectory root $ do
                assertAllowed (FileExactly "top.txt") (root </> "top.txt")
                assertDenied (FileExactly "top.txt") (root </> "src" </> "Main.hs")
        ]

