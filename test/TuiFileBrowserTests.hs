{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the attachment file browser's navigation logic (issue #506):
what Enter does under the cursor, going to the parent directory, and what
the dialog says about the current directory. They run on a real temporary
directory through Brick's own 'FileBrowser'.
-}
module TuiFileBrowserTests (tests) where

import Brick.Widgets.FileBrowser (
    FileBrowser,
    FileInfo (..),
    FileStatus (..),
    FileType (..),
    fileBrowserCursor,
    getWorkingDirectory,
    newFileBrowser,
    selectNonDirectories,
 )
import qualified Data.Text as Text
import System.Directory (createDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import System.Agents.TUI.FileBrowserNav

openAt :: FilePath -> IO (FileBrowser ())
openAt dir = newFileBrowser selectNonDirectories () (Just dir)

-- | A listing entry with the given type, built by hand: the browser exposes no way to list its entries.
entryOf :: FilePath -> FileType -> Maybe FileType -> FileInfo
entryOf path ty linkTarget =
    FileInfo
        { fileInfoFilename = path
        , fileInfoSanitizedFilename = path
        , fileInfoFilePath = path
        , fileInfoFileStatus = Right (FileStatus{fileStatusSize = 0, fileStatusFileType = Just ty})
        , fileInfoLinkTargetType = linkTarget
        }

-- | A directory holding @sub/@ (with @inner.txt@), @a.txt@ and an empty @empty/@.
withTree :: (FilePath -> IO a) -> IO a
withTree k = withSystemTempDirectory "fb-nav" $ \root -> do
    createDirectory (root </> "sub")
    createDirectory (root </> "empty")
    writeFile (root </> "a.txt") "hello"
    writeFile (root </> "sub" </> "inner.txt") "inner"
    k root

tests :: TestTree
tests =
    testGroup
        "TUI file browser navigation (#506)"
        [ testCase "parentDirectory goes up one level, and the root stays the root" $ do
            parentDirectory "/a/b/c" @?= "/a/b"
            parentDirectory "/a/b/c/" @?= "/a/b"
            parentDirectory "/a" @?= "/"
            parentDirectory "/" @?= "/"
        , testCase "Enter on a directory enters it, on a file attaches it" $ do
            classifyEnter (Just (entryOf "/x/sub" Directory Nothing)) @?= EnterDirectory
            classifyEnter (Just (entryOf "/x/a.txt" RegularFile Nothing)) @?= AttachFile "/x/a.txt"
        , testCase "a symbolic link is entered when it points to a directory, attached otherwise" $ do
            classifyEnter (Just (entryOf "/x/ln" SymbolicLink (Just Directory))) @?= EnterDirectory
            classifyEnter (Just (entryOf "/x/ln" SymbolicLink (Just RegularFile))) @?= AttachFile "/x/ln"
        , testCase "Enter with nothing under the cursor does nothing" $
            classifyEnter Nothing @?= NothingUnderCursor
        , testCase "navigateUp returns from a nested directory to its parent" $
            withTree $ \root -> do
                fb <- openAt (root </> "sub")
                fb' <- navigateUp fb
                getWorkingDirectory fb' @?= root
        , testCase "the status line always shows the current path" $
            withTree $ \root -> do
                fb <- openAt (root </> "sub")
                assertBool "path shown" $ Text.pack (root </> "sub") `Text.isInfixOf` browserStatusLine fb
                assertBool "no problem reported" $ not ("(" `Text.isInfixOf` browserStatusLine fb)
        , testCase "an empty directory is not a dead end: its '..' entry can be entered" $
            withTree $ \root -> do
                fb <- openAt (root </> "empty")
                classifyEnter (fileBrowserCursor fb) @?= EnterDirectory
                assertBool "no problem reported" $ not ("cannot read" `Text.isInfixOf` browserStatusLine fb)
        , testCase "a directory that cannot be listed is reported, and can be left" $
            withTree $ \root -> do
                fb <- openAt (root </> "missing")
                assertBool "says cannot read" $ "cannot read" `Text.isInfixOf` browserStatusLine fb
                fb' <- navigateUp fb
                getWorkingDirectory fb' @?= root
                assertBool "no error after going up" $ not ("cannot read" `Text.isInfixOf` browserStatusLine fb')
        ]
