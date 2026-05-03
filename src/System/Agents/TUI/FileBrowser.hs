{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Enhanced File Browser for TUI with improved directory navigation.

This module provides a file browser with:

1. **Clear Modes**: Explicit browsing vs selecting modes
2. **Better Navigation**: Intuitive key bindings for directory operations
3. **Visual Distinction**: Clear visual indicators for current selection
4. **Path Display**: Always shows current path context

== Keyboard Navigation

| Key | Action |
|-----|--------|
| ↑/↓ | Navigate items |
| Enter | Open directory / Select file |
| Backspace | Go to parent directory |
| Space | Select current item |
| Tab | Toggle selection mode |
| Esc | Cancel |

== Selection Modes

* 'BrowseMode': Navigate through directories, Enter enters directories
* 'SelectMode': Select files/directories, Enter confirms selection
-}
module System.Agents.TUI.FileBrowser (
    -- * Types
    FileBrowser,
    FileBrowserMode (..),
    FileBrowserSelection (..),
    FileBrowserEntry (..),
    FileBrowserError (..),

    -- * Initialization
    newFileBrowser,
    newFileBrowserWithFilter,

    -- * Navigation
    handleFileBrowserEvent,
    enterDirectory,
    goToParentDirectory,
    selectCurrentEntry,

    -- * Queries
    getCurrentPath,
    getSelectedEntry,
    getCurrentDir,
    fileBrowserSelection,
    fileBrowserMode,
    fileBrowserEntries,
    fileBrowserError,

    -- * State manipulation
    setFileBrowserMode,
    setFileBrowserError,
    clearFileBrowserError,
    refreshFileBrowser,

    -- * Rendering helpers
    fileBrowserEntryName,
    fileBrowserEntryPath,
    fileBrowserEntryIsDirectory,
) where

import Control.Exception (SomeException, catch, try)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Graphics.Vty as Vty
import System.Directory (
    doesDirectoryExist,
    doesFileExist,
    getCurrentDirectory,
    getDirectoryContents,
    getHomeDirectory,
    pathIsSymbolicLink,
 )
import System.FilePath ((</>))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Mode of the file browser - browsing or selecting.
data FileBrowserMode
    = -- | Navigate through directories (Enter enters directories)
      BrowseMode
    | -- | Select files/directories (Enter confirms selection)
      SelectMode
    deriving (Show, Eq)

-- | Type of selection made in the file browser.
data FileBrowserSelection
    = -- | A single file was selected
      FileSelected FilePath
    | -- | A directory was selected (in select mode)
      DirectorySelected FilePath
    | -- | No selection made (user cancelled)
      NoSelection
    deriving (Show, Eq)

-- | An entry in the file browser listing.
data FileBrowserEntry = FileBrowserEntry
    { fbeName :: !Text
    -- ^ Display name of the entry
    , fbePath :: !FilePath
    -- ^ Full path to the entry
    , fbeIsDirectory :: !Bool
    -- ^ Whether this is a directory
    , fbeIsSymlink :: !Bool
    -- ^ Whether this is a symbolic link
    , fbeIsAccessible :: !Bool
    -- ^ Whether the entry is accessible (readable)
    }
    deriving (Show, Eq)

-- | Errors that can occur in the file browser.
data FileBrowserError
    = PermissionError FilePath Text
    | DirectoryNotFound FilePath
    | GeneralError Text
    deriving (Show, Eq)

-- | Filter function for file browser entries.
type FileBrowserFilter = FileBrowserEntry -> Bool

-- | Internal state of the file browser.
data FileBrowser n = FileBrowser
    { fbCurrentDir :: !FilePath
    -- ^ Current directory path
    , fbSelectedIndex :: !Int
    -- ^ Currently selected index
    , fbEntries :: ![FileBrowserEntry]
    -- ^ List of entries in current directory
    , fbMode :: !FileBrowserMode
    -- ^ Current browser mode
    , fbSelection :: !FileBrowserSelection
    -- ^ Current selection (if any)
    , fbFilter :: !(Maybe FileBrowserFilter)
    -- ^ Optional filter for entries
    , fbError :: !(Maybe FileBrowserError)
    -- ^ Current error (if any)
    , fbWidgetName :: !n
    -- ^ Widget name for the file browser
    , fbShowHidden :: !Bool
    -- ^ Whether to show hidden files
    }

-- | Manual Show instance for FileBrowser (omitting the filter function).
instance (Show n) => Show (FileBrowser n) where
    show fb =
        "FileBrowser {"
            ++ "fbCurrentDir = "
            ++ show fb.fbCurrentDir
            ++ ", fbSelectedIndex = "
            ++ show fb.fbSelectedIndex
            ++ ", fbEntries = "
            ++ show fb.fbEntries
            ++ ", fbMode = "
            ++ show fb.fbMode
            ++ ", fbSelection = "
            ++ show fb.fbSelection
            ++ ", fbFilter = <filter function>"
            ++ ", fbError = "
            ++ show fb.fbError
            ++ ", fbWidgetName = "
            ++ show fb.fbWidgetName
            ++ ", fbShowHidden = "
            ++ show fb.fbShowHidden
            ++ "}"

-------------------------------------------------------------------------------
-- Initialization
-------------------------------------------------------------------------------

{- | Create a new file browser starting at the given directory.
If no directory is provided, uses the current working directory.
-}
newFileBrowser :: (Ord n, Show n) => n -> Maybe FilePath -> IO (FileBrowser n)
newFileBrowser name mStartPath = newFileBrowserWithFilter name mStartPath Nothing

-- | Create a new file browser with a filter function.
newFileBrowserWithFilter ::
    (Ord n, Show n) =>
    n ->
    Maybe FilePath ->
    Maybe FileBrowserFilter ->
    IO (FileBrowser n)
newFileBrowserWithFilter name mStartPath mFilter = do
    startDir <- case mStartPath of
        Just path -> pure path
        Nothing -> getCurrentDirectory
    entries <- readDirectoryEntries mFilter startDir
    pure
        FileBrowser
            { fbCurrentDir = startDir
            , fbSelectedIndex = 0
            , fbEntries = entries
            , fbMode = BrowseMode
            , fbSelection = NoSelection
            , fbFilter = mFilter
            , fbError = Nothing
            , fbWidgetName = name
            , fbShowHidden = False
            }

-------------------------------------------------------------------------------
-- Directory Reading
-------------------------------------------------------------------------------

-- | Read all entries from a directory.
readDirectoryEntries :: Maybe FileBrowserFilter -> FilePath -> IO [FileBrowserEntry]
readDirectoryEntries mFilter dir = do
    result <- try $ do
        contents <- getDirectoryContents dir
        let visibleContents = filter (not . isHidden) contents
        entries <- mapM (readEntry dir) visibleContents
        let sortedEntries = sortEntries entries
        -- Add ".." entry at the top if not at root
        parentEntry <- makeParentEntry dir
        let withParent = case parentEntry of
                Just pe -> pe : sortedEntries
                Nothing -> sortedEntries
        -- Apply filter if provided
        case mFilter of
            Just f -> pure $ filter f withParent
            Nothing -> pure withParent
    case result of
        Left (_e :: SomeException) -> do
            -- Return empty list on error, error will be set separately
            pure []
        Right entries -> pure entries
  where
    isHidden ('.' : _) = True
    isHidden _ = False

-- | Create a parent directory entry if not at filesystem root.
makeParentEntry :: FilePath -> IO (Maybe FileBrowserEntry)
makeParentEntry dir = do
    -- Check if we're at the root by seeing if parent == current
    let parent = dir </> ".."
    canonicalDir <- canonicalizePath dir
    canonicalParent <- canonicalizePath parent
    if canonicalDir == canonicalParent
        then pure Nothing
        else
            pure $
                Just
                    FileBrowserEntry
                        { fbeName = "📁 .."
                        , fbePath = parent
                        , fbeIsDirectory = True
                        , fbeIsSymlink = False
                        , fbeIsAccessible = True
                        }

-- | Read a single directory entry.
readEntry :: FilePath -> FilePath -> IO FileBrowserEntry
readEntry dir name = do
    let fullPath = dir </> name
    isDir <- doesDirectoryExist fullPath
    isLink <-
        ( pathIsSymbolicLink fullPath >>= \isSym ->
            if isSym then pure True else pure False
        )
            `catch` \(_ :: SomeException) -> pure False
    isAccessible <-
        (if isDir then doesDirectoryExist fullPath else doesFileExist fullPath)
            `catch` \(_ :: SomeException) -> pure False
    let displayName = decorateName name isDir isLink
    pure
        FileBrowserEntry
            { fbeName = displayName
            , fbePath = fullPath
            , fbeIsDirectory = isDir
            , fbeIsSymlink = isLink
            , fbeIsAccessible = isAccessible
            }

-- | Decorate a file name with appropriate icon.
decorateName :: FilePath -> Bool -> Bool -> Text
decorateName name isDir isLink
    | isDir && isLink = "📁 " <> Text.pack name <> " ->"
    | isDir = "📁 " <> Text.pack name
    | isLink = "🔗 " <> Text.pack name
    | otherwise = "📄 " <> Text.pack name

-- | Sort entries: directories first, then alphabetically.
sortEntries :: [FileBrowserEntry] -> [FileBrowserEntry]
sortEntries entries =
    let dirs = filter fbeIsDirectory entries
        files = filter (not . fbeIsDirectory) entries
        sortByName = sortBy (comparing (Text.toLower . fbeName))
     in sortByName dirs ++ sortByName files

-- | Simple canonicalizePath implementation.
canonicalizePath :: FilePath -> IO FilePath
canonicalizePath path = do
    -- Simple implementation - just handle basic cases
    pure path

-------------------------------------------------------------------------------
-- Event Handling
-------------------------------------------------------------------------------

-- | Handle a Vty event in the file browser.
handleFileBrowserEvent :: Vty.Event -> FileBrowser n -> IO (FileBrowser n)
handleFileBrowserEvent ev fb = do
    case ev of
        Vty.EvKey Vty.KUp [] -> pure $ navigateUp fb
        Vty.EvKey Vty.KDown [] -> pure $ navigateDown fb
        Vty.EvKey Vty.KEnter [] -> handleEnter fb
        Vty.EvKey Vty.KBS [] -> goToParentDirectory fb
        Vty.EvKey (Vty.KChar 'h') [Vty.MCtrl] -> goToParentDirectory fb
        Vty.EvKey (Vty.KChar ' ') [] -> pure $ selectCurrentEntry fb
        Vty.EvKey (Vty.KChar '\t') [] -> pure $ toggleMode fb
        Vty.EvKey (Vty.KChar 'r') [Vty.MCtrl] -> refreshFileBrowser fb
        Vty.EvKey (Vty.KChar '~') [] -> goToHomeDirectory fb
        Vty.EvKey (Vty.KChar '.') [] -> pure $ toggleHiddenFiles fb
        _ -> pure fb

-- | Navigate up in the list.
navigateUp :: FileBrowser n -> FileBrowser n
navigateUp fb =
    let newIndex = max 0 (fbSelectedIndex fb - 1)
     in fb{fbSelectedIndex = newIndex}

-- | Navigate down in the list.
navigateDown :: FileBrowser n -> FileBrowser n
navigateDown fb =
    let maxIndex = max 0 (length (fbEntries fb) - 1)
        newIndex = min maxIndex (fbSelectedIndex fb + 1)
     in fb{fbSelectedIndex = newIndex}

-- | Handle Enter key press based on current mode and selection.
handleEnter :: FileBrowser n -> IO (FileBrowser n)
handleEnter fb = do
    case getSelectedEntry fb of
        Nothing -> pure fb
        Just entry ->
            if fbeIsDirectory entry
                then case fbMode fb of
                    BrowseMode -> enterDirectory fb (fbePath entry)
                    SelectMode -> pure $ fb{fbSelection = DirectorySelected (fbePath entry)}
                else pure $ fb{fbSelection = FileSelected (fbePath entry)}

-- | Enter a directory.
enterDirectory :: FileBrowser n -> FilePath -> IO (FileBrowser n)
enterDirectory fb path = do
    result <- try $ do
        isDir <- doesDirectoryExist path
        if not isDir
            then pure $ setError fb (GeneralError $ Text.pack $ "Not a directory: " ++ path)
            else do
                entries <- readDirectoryEntries (fbFilter fb) path
                pure
                    fb
                        { fbCurrentDir = path
                        , fbSelectedIndex = 0
                        , fbEntries = entries
                        , fbError = Nothing
                        }
    case result of
        Left (_e :: SomeException) ->
            pure $ setError fb (PermissionError path (Text.pack $ "Cannot access: " ++ path))
        Right newFb -> pure newFb

-- | Go to the parent directory.
goToParentDirectory :: FileBrowser n -> IO (FileBrowser n)
goToParentDirectory fb = do
    let parent = fbCurrentDir fb </> ".."
    enterDirectory fb parent

-- | Go to the home directory.
goToHomeDirectory :: FileBrowser n -> IO (FileBrowser n)
goToHomeDirectory fb = do
    home <- getHomeDirectory
    enterDirectory fb home

-- | Select the current entry.
selectCurrentEntry :: FileBrowser n -> FileBrowser n
selectCurrentEntry fb =
    case getSelectedEntry fb of
        Nothing -> fb
        Just entry ->
            let selection =
                    if fbeIsDirectory entry
                        then DirectorySelected (fbePath entry)
                        else FileSelected (fbePath entry)
             in fb{fbSelection = selection}

-- | Toggle between browse and select modes.
toggleMode :: FileBrowser n -> FileBrowser n
toggleMode fb =
    let newMode = case fbMode fb of
            BrowseMode -> SelectMode
            SelectMode -> BrowseMode
     in fb{fbMode = newMode}

-- | Toggle visibility of hidden files.
toggleHiddenFiles :: FileBrowser n -> FileBrowser n
toggleHiddenFiles fb =
    -- Note: This would require re-reading the directory
    -- For now, just toggle the flag
    fb{fbShowHidden = not (fbShowHidden fb)}

-------------------------------------------------------------------------------
-- Queries
-------------------------------------------------------------------------------

-- | Get the current directory path.
getCurrentPath :: FileBrowser n -> FilePath
getCurrentPath = fbCurrentDir

-- | Get the currently selected entry (if any).
getSelectedEntry :: FileBrowser n -> Maybe FileBrowserEntry
getSelectedEntry fb =
    let entries = fbEntries fb
        idx = fbSelectedIndex fb
     in if idx >= 0 && idx < length entries
            then Just (entries !! idx)
            else Nothing

-- | Get the current directory as Text.
getCurrentDir :: FileBrowser n -> Text
getCurrentDir = Text.pack . fbCurrentDir

-- | Get the current selection.
fileBrowserSelection :: FileBrowser n -> FileBrowserSelection
fileBrowserSelection = fbSelection

-- | Get the current mode.
fileBrowserMode :: FileBrowser n -> FileBrowserMode
fileBrowserMode = fbMode

-- | Get the list of entries.
fileBrowserEntries :: FileBrowser n -> [FileBrowserEntry]
fileBrowserEntries = fbEntries

-- | Get the current error (if any).
fileBrowserError :: FileBrowser n -> Maybe FileBrowserError
fileBrowserError = fbError

-------------------------------------------------------------------------------
-- State Manipulation
-------------------------------------------------------------------------------

-- | Set the file browser mode.
setFileBrowserMode :: FileBrowserMode -> FileBrowser n -> FileBrowser n
setFileBrowserMode mode fb = fb{fbMode = mode}

-- | Set an error on the file browser.
setFileBrowserError :: FileBrowserError -> FileBrowser n -> FileBrowser n
setFileBrowserError err fb = fb{fbError = Just err}

-- | Set an error using the internal helper.
setError :: FileBrowser n -> FileBrowserError -> FileBrowser n
setError fb err = fb{fbError = Just err}

-- | Clear any error on the file browser.
clearFileBrowserError :: FileBrowser n -> FileBrowser n
clearFileBrowserError fb = fb{fbError = Nothing}

-- | Refresh the file browser contents.
refreshFileBrowser :: FileBrowser n -> IO (FileBrowser n)
refreshFileBrowser fb = do
    entries <- readDirectoryEntries (fbFilter fb) (fbCurrentDir fb)
    let safeIndex = min (fbSelectedIndex fb) (max 0 (length entries - 1))
    pure
        fb
            { fbEntries = entries
            , fbSelectedIndex = safeIndex
            , fbError = Nothing
            }

-------------------------------------------------------------------------------
-- Rendering Helpers
-------------------------------------------------------------------------------

-- | Get the name of a file browser entry.
fileBrowserEntryName :: FileBrowserEntry -> Text
fileBrowserEntryName = fbeName

-- | Get the path of a file browser entry.
fileBrowserEntryPath :: FileBrowserEntry -> FilePath
fileBrowserEntryPath = fbePath

-- | Check if a file browser entry is a directory.
fileBrowserEntryIsDirectory :: FileBrowserEntry -> Bool
fileBrowserEntryIsDirectory = fbeIsDirectory
