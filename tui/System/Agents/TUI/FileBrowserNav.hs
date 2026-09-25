{- | Navigation logic of the attachment file browser (issue #506), kept apart
from the Brick event loop so it can be tested on its own: what Enter does
under the cursor (open a directory or attach a file), how Backspace goes to
the parent directory, and what the dialog says about the current directory.
-}
module System.Agents.TUI.FileBrowserNav (
    EnterAction (..),
    classifyEnter,
    isDirectoryEntry,
    parentDirectory,
    navigateUp,
    browserStatusLine,
    browserHint,
) where

import Brick.Widgets.FileBrowser (
    FileBrowser,
    FileInfo (..),
    FileStatus (..),
    FileType (..),
    fileBrowserCursor,
    fileBrowserException,
    fileBrowserIsSearching,
    getWorkingDirectory,
    setWorkingDirectory,
 )
import Data.Text (Text)
import qualified Data.Text as Text
import System.FilePath (dropTrailingPathSeparator, takeDirectory)

-- | What Enter does for the entry under the cursor.
data EnterAction
    = -- | A directory (or a symlink to one): descend into it.
      EnterDirectory
    | -- | A file: attach it.
      AttachFile FilePath
    | -- | An empty listing (a search with no match, an unreadable directory): nothing to act on.
      NothingUnderCursor
    deriving (Show, Eq)

-- | Whether an entry is a directory, following a symbolic link to its target.
isDirectoryEntry :: FileInfo -> Bool
isDirectoryEntry fi =
    fileType == Just Directory
        || (fileType == Just SymbolicLink && fileInfoLinkTargetType fi == Just Directory)
  where
    fileType = either (const Nothing) fileStatusFileType (fileInfoFileStatus fi)

-- | Decide what Enter does, from the entry under the cursor.
classifyEnter :: Maybe FileInfo -> EnterAction
classifyEnter Nothing = NothingUnderCursor
classifyEnter (Just fi)
    | isDirectoryEntry fi = EnterDirectory
    | otherwise = AttachFile (fileInfoFilePath fi)

-- | The parent of a directory; the root is its own parent.
parentDirectory :: FilePath -> FilePath
parentDirectory = takeDirectory . dropTrailingPathSeparator

{- | Go to the parent of the browser's working directory. A listing error
(for instance a permission problem) is left in 'fileBrowserException' for
'browserStatusLine' to show, as 'setWorkingDirectory' does.
-}
navigateUp :: FileBrowser n -> IO (FileBrowser n)
navigateUp fb = setWorkingDirectory (parentDirectory (getWorkingDirectory fb)) fb

-- | The line under the listing: always the current path, and why the listing is empty or unreadable, if it is.
browserStatusLine :: FileBrowser n -> Text
browserStatusLine fb =
    "Current: " <> Text.pack (getWorkingDirectory fb) <> problem
  where
    problem = case (fileBrowserException fb, fileBrowserCursor fb) of
        (Just err, _) -> "  (cannot read: " <> Text.pack (show err) <> ")"
        -- Brick lists a ".." entry in every directory but the root, so an
        -- empty directory still has one to enter; nothing at all is a search
        -- that matches no entry (or an empty root).
        (Nothing, Nothing) -> if fileBrowserIsSearching fb then "  (no match)" else "  (nothing to show)"
        _ -> ""

-- | The key help line, which depends on whether a search is being typed.
browserHint :: FileBrowser n -> Text
browserHint fb
    | fileBrowserIsSearching fb = "type to filter | Enter: keep filter | Esc: stop searching"
    | otherwise = "Enter: open directory / attach file | Backspace: parent | /: search | Esc: cancel"
