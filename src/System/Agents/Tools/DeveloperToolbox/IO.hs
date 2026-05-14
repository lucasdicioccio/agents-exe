{-# LANGUAGE OverloadedStrings #-}

{- |
IO utilities for the DeveloperToolbox.

This module provides file system operations used by various DeveloperToolbox
capabilities, including atomic file writes.
-}
module System.Agents.Tools.DeveloperToolbox.IO (
    -- * File operations
    writeFileAtomic,
) where

import System.Directory (doesFileExist, removeFile, renameFile)
import System.FilePath (takeDirectory)
import System.IO (hClose, hPutStr)
import System.IO.Temp (withTempFile)
import Data.Text (Text)
import qualified Data.Text as Text

{- | Write a file atomically by writing to a temp file and renaming.
This ensures that readers never see a partially-written file.
-}
writeFileAtomic :: FilePath -> Text -> IO ()
writeFileAtomic filePath content = do
    let dir = takeDirectory filePath
    -- Use a temp file in the same directory for atomic rename
    withTempFile dir ".write-file-range-tmp-" $ \tmpPath tmpHandle -> do
        hPutStr tmpHandle (Text.unpack content)
        hClose tmpHandle
        -- Atomic rename on POSIX systems
        renameFileOverwrite tmpPath filePath
  where
    -- Cross-platform file rename that overwrites existing file
    renameFileOverwrite :: FilePath -> FilePath -> IO ()
    renameFileOverwrite src dst = do
        dstExists <- doesFileExist dst
        if dstExists then removeFile dst else return ()
        renameFile src dst

