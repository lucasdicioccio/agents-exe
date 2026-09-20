{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Clipboard and drag-and-drop support for the TUI.

This module provides cross-platform clipboard access and file drop handling
for the TUI application. It supports:

* Clipboard paste detection (images, file paths, text)
* Platform-specific clipboard backends (X11, Wayland, macOS, Windows)
* Terminal-specific file drop protocols (iTerm2, Kitty escape sequences)
* Smart content detection and MIME type inference

Design principles:
* Graceful degradation when clipboard tools are unavailable
* No external dependencies beyond common system tools
* Safe handling of binary data and temporary files
* Security-conscious path validation
-}
module System.Agents.TUI.Clipboard (
    -- * Clipboard Content Types
    ClipboardContent (..),
    ClipboardBackend (..),
    ContentAction (..),
    FileDropEvent (..),

    -- * Backend Detection
    detectBackend,
    hasClipboardSupport,

    -- * Clipboard Operations
    readClipboard,
    detectClipboardContent,
    analyzeContent,

    -- * File Drop Support
    enableFileDropMode,
    disableFileDropMode,
    parseTerminalSequence,

    -- * Content Processing
    createTempAttachment,
    loadMediaAttachmentFromPath,
    isSingleFilePath,
    detectImageFormat,

    -- * Constants
    maxClipboardImageSize,
    clipboardTempDir,

    -- * Cleanup
    cleanupClipboardTempFiles,
) where

import Control.Applicative ((<|>))
import Control.Exception (IOException, catch, handle)
import Control.Monad (filterM)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as BSL
import Data.Char (isSpace)
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Word as Word
import System.Directory (createDirectoryIfMissing, doesFileExist, getTemporaryDirectory, removeFile)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeBaseName, takeExtension, (<.>), (</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Temp (emptyTempFile)
import System.Info (os)
import System.Process (proc, readProcessWithExitCode)
import System.Process.ByteString.Lazy (readCreateProcessWithExitCode)

import System.Agents.Media.Types (MediaAttachment (..))

-------------------------------------------------------------------------------
-- Constants
-------------------------------------------------------------------------------

-- | Maximum size for clipboard images (50MB).
maxClipboardImageSize :: Int
maxClipboardImageSize = 50 * 1024 * 1024

-- | Default directory for temporary clipboard files.
clipboardTempDir :: String
clipboardTempDir = "agents-exe-clipboard"

-------------------------------------------------------------------------------
-- Clipboard Content Types
-------------------------------------------------------------------------------

-- | Represents the different types of content that can be in the clipboard.
data ClipboardContent
    = -- | Binary image data with detected MIME type
      ClipboardImage ByteString Text
    | -- | A file path (absolute or relative)
      ClipboardFilePath FilePath
    | -- | Plain text content
      ClipboardText Text
    | -- | Multiple file paths (from multi-select in file manager)
      ClipboardFilePaths [FilePath]
    | -- | Unknown or unsupported content
      ClipboardUnknown
    deriving (Show, Eq)

-- | Action to take based on clipboard content analysis.
data ContentAction
    = -- | Attach as media attachment
      AttachAsMedia MediaAttachment
    | -- | Paste as text into the editor
      PasteAsText Text
    | -- | Attach multiple files
      AttachMultipleFiles [MediaAttachment]
    | -- | Content should be ignored
      IgnoreContent
    deriving (Show, Eq)

-- | File drop event from terminal drag-and-drop.
data FileDropEvent = FileDropEvent
    { dropFilename :: FilePath
    -- ^ Filename of the dropped file
    , dropContent :: Maybe ByteString
    -- ^ Content if inline, Nothing if just a reference
    , dropMimeType :: Maybe Text
    -- ^ Optional MIME type hint from terminal
    }
    deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Clipboard Backend Types
-------------------------------------------------------------------------------

-- | Platform-specific clipboard backend.
data ClipboardBackend
    = X11Backend
    | WaylandBackend
    | MacOSBackend
    | WindowsBackend
    | OSC52Backend
    | NoBackend
    deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Backend Detection
-------------------------------------------------------------------------------

{- | Detect the best available clipboard backend for the current platform.

The detection order is:
1. Check for Wayland (WAYLAND_DISPLAY env var + wl-clipboard)
2. Check for X11 (DISPLAY env var + xclip/xsel)
3. Check for macOS (pbcopy/pbpaste)
4. Check for Windows (PowerShell)
5. Fall back to OSC 52 if supported by terminal
6. NoBackend if nothing available
-}
detectBackend :: IO ClipboardBackend
detectBackend = case os of
    "linux" -> detectLinuxBackend
    "darwin" -> detectMacBackend
    "mingw32" -> detectWindowsBackend
    "cygwin" -> detectWindowsBackend
    _ -> detectGenericBackend

-- | Detect Linux clipboard backend (Wayland or X11).
detectLinuxBackend :: IO ClipboardBackend
detectLinuxBackend = do
    waylandDisplay <- lookupEnv "WAYLAND_DISPLAY"
    x11Display <- lookupEnv "DISPLAY"

    hasWlPaste <- hasCommand "wl-paste"
    hasXclip <- hasCommand "xclip"
    hasXsel <- hasCommand "xsel"

    if
        | x11Display /= Nothing && (hasXclip || hasXsel) ->
            pure X11Backend
        | hasXclip || hasXsel ->
            -- X11 tool available but no DISPLAY - might work anyway
            pure X11Backend
        | waylandDisplay /= Nothing && hasWlPaste ->
            pure WaylandBackend
        | hasWlPaste ->
            -- wl-paste available but no WAYLAND_DISPLAY - might work anyway
            pure WaylandBackend
        | otherwise ->
            pure NoBackend

-- | Detect macOS clipboard backend.
detectMacBackend :: IO ClipboardBackend
detectMacBackend = do
    hasPb <- hasCommand "pbpaste"
    pure $ if hasPb then MacOSBackend else NoBackend

-- | Detect Windows clipboard backend.
detectWindowsBackend :: IO ClipboardBackend
detectWindowsBackend = do
    hasPowershell <- hasCommand "powershell.exe"
    pure $ if hasPowershell then WindowsBackend else NoBackend

-- | Detect generic backend (fallback to OSC 52).
detectGenericBackend :: IO ClipboardBackend
detectGenericBackend = do
    -- Check if terminal supports OSC 52
    term <- lookupEnv "TERM"
    pure $ case term of
        Just t
            | "screen" `isPrefixOf` t -> OSC52Backend
            | "tmux" `isPrefixOf` t -> OSC52Backend
            | "xterm" `isPrefixOf` t -> OSC52Backend
            | otherwise -> NoBackend
        Nothing -> NoBackend

-- | Check if clipboard support is available.
hasClipboardSupport :: IO Bool
hasClipboardSupport = do
    backend <- detectBackend
    pure $ backend /= NoBackend

-- | Check if a command exists in PATH.
hasCommand :: String -> IO Bool
hasCommand cmd = do
    (exitCode, _, _) <- readProcessWithExitCode "which" [cmd] ""
    pure $ exitCode == ExitSuccess

-------------------------------------------------------------------------------
-- Clipboard Reading
-------------------------------------------------------------------------------

{- | Read raw clipboard content using the specified backend.

Returns 'Nothing' if the clipboard is empty or cannot be read.
-}
readClipboard :: ClipboardBackend -> IO (Maybe ByteString)
readClipboard NoBackend = pure Nothing
readClipboard OSC52Backend = readOSC52Clipboard
readClipboard X11Backend = readX11Clipboard
readClipboard WaylandBackend = readWaylandClipboard
readClipboard MacOSBackend = readMacOSClipboard
readClipboard WindowsBackend = readWindowsClipboard

{- | Read from X11 clipboard using xclip or xsel.
Uses TARGETS to detect image formats first, falling back to default text.
-}
readX11Clipboard :: IO (Maybe ByteString)
readX11Clipboard = do
    hasXclip <- hasCommand "xclip"
    if hasXclip
        then readX11WithXclip
        else do
            hasXsel <- hasCommand "xsel"
            if hasXsel
                then readWithTool "xsel" ["--clipboard", "--output"]
                else pure Nothing

{- | Read X11 clipboard using xclip with image target detection.
First queries available TARGETS, then prioritizes image targets.
-}
readX11WithXclip :: IO (Maybe ByteString)
readX11WithXclip = do
    -- Query available targets
    mTargets <- readX11Targets
    case mTargets of
        Just targets -> do
            -- Look for image targets
            let imageTarget = findImageTarget targets
            case imageTarget of
                Just target -> readWithTool "xclip" ["-selection", "clipboard", "-o", "-target", target]
                Nothing -> readWithTool "xclip" ["-selection", "clipboard", "-o"]
        Nothing -> readWithTool "xclip" ["-selection", "clipboard", "-o"]

{- | Query available X11 clipboard targets using xclip.
Returns a list of available target strings.
-}
readX11Targets :: IO (Maybe [String])
readX11Targets = do
    result <- readProcessWithExitCode "xclip" ["-selection", "clipboard", "-o", "-target", "TARGETS"] ""
    case result of
        (ExitSuccess, stdout, _) -> do
            let targets = filter (not . null) $ lines stdout
            pure $ Just targets
        _ -> pure Nothing

{- | Find an image target from the list of available targets.
Prioritizes common image formats like image/png, image/jpeg, etc.
-}
findImageTarget :: [String] -> Maybe String
findImageTarget targets =
    -- Priority order for image formats
    let preferredOrder = ["image/png", "image/jpeg", "image/jpg", "image/gif", "image/webp", "image/bmp"]
        -- First check preferred order
        preferred = listToMaybe $ filter (`elem` targets) preferredOrder
        -- Then check any target containing "image"
        anyImage = listToMaybe $ filter ("image" `isInfixOf`) targets
     in preferred <|> anyImage

-- | Read from Wayland clipboard using wl-paste.
readWaylandClipboard :: IO (Maybe ByteString)
readWaylandClipboard = readWithTool "wl-paste" ["--no-newline"]

-- | Read from macOS clipboard using pbpaste.
readMacOSClipboard :: IO (Maybe ByteString)
readMacOSClipboard = readWithTool "pbpaste" []

-- | Read from Windows clipboard using PowerShell.
readWindowsClipboard :: IO (Maybe ByteString)
readWindowsClipboard = do
    let psCmd = "Get-Clipboard -Raw | ForEach-Object { [System.Text.Encoding]::UTF8.GetBytes($_) } | ForEach-Object { $_.ToString('X2') } | Join-String"
    result <- readProcessWithExitCode "powershell.exe" ["-Command", psCmd] ""
    case result of
        (ExitSuccess, stdout, _) -> do
            -- Parse hex-encoded output
            pure $ parseHexBytes (Text.pack stdout)
        _ -> pure Nothing
  where
    parseHexBytes :: Text -> Maybe ByteString
    parseHexBytes txt =
        let hexPairs = chunksOf 2 (filter (not . isSpace) (Text.unpack txt))
            bytes = mapM hexToByte hexPairs
         in BS.pack <$> bytes

    chunksOf :: Int -> [a] -> [[a]]
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)

    hexToByte :: String -> Maybe Word.Word8
    hexToByte [h1, h2] = do
        w1 <- hexDigit h1
        w2 <- hexDigit h2
        pure $ fromIntegral (w1 * 16 + w2)
    hexToByte _ = Nothing

    hexDigit :: Char -> Maybe Int
    hexDigit c
        | c >= '0' && c <= '9' = Just $ fromEnum c - fromEnum '0'
        | c >= 'a' && c <= 'f' = Just $ fromEnum c - fromEnum 'a' + 10
        | c >= 'A' && c <= 'F' = Just $ fromEnum c - fromEnum 'A' + 10
        | otherwise = Nothing

{- | Read from OSC 52 clipboard (terminal-integrated).
Note: This requires terminal support for reading clipboard via OSC 52,
which is less common than writing. Most terminals only support OSC 52 write.
-}
readOSC52Clipboard :: IO (Maybe ByteString)
readOSC52Clipboard = do
    -- OSC 52 read is not widely supported, return Nothing for now
    -- In the future, this could send an OSC 52 query and wait for response
    pure Nothing

-- | Helper to read clipboard using an external tool.
readWithTool :: String -> [String] -> IO (Maybe ByteString)
readWithTool tool args =
    do
        (exitCode, stdout, _) <- readCreateProcessWithExitCode (proc tool args) BSL.empty
        case exitCode of
            ExitSuccess ->
                pure $ Just $ BSL.toStrict stdout
            ExitFailure _ ->
                pure Nothing
        `catch` \(_ :: IOException) -> pure Nothing

-------------------------------------------------------------------------------
-- Content Detection
-------------------------------------------------------------------------------

{- | Detect the type of content in the clipboard.

This function reads the clipboard and attempts to classify the content:
* Image data (by magic bytes)
* File paths (valid paths that exist)
* Text content
-}
detectClipboardContent :: IO (Maybe ClipboardContent)
detectClipboardContent = do
    backend <- detectBackend
    mContent <- readClipboard backend
    case mContent of
        Nothing -> pure Nothing
        Just content -> do
            -- Check size limit
            if BS.length content > maxClipboardImageSize
                then do
                    hPutStrLn stderr "Clipboard content too large (>50MB)"
                    pure Nothing
                else Just <$> classifyContent content

-- | Classify raw clipboard content.
classifyContent :: ByteString -> IO ClipboardContent
classifyContent content
    | BS.null content = pure ClipboardUnknown
    | isImageData content = do
        let mime = detectImageFormat content
        pure $ ClipboardImage content mime
    | isTextContent content = do
        let text = Text.Encoding.decodeUtf8 content
        -- Check if it's a single file path
        if isSingleFilePath text
            then do
                let path = Text.unpack (Text.strip text)
                exists <- doesFileExist path
                pure $
                    if exists
                        then ClipboardFilePath path
                        else ClipboardText text
            else do
                -- Check if it's multiple file paths (one per line)
                let paths = map Text.unpack (Text.lines text)
                existingPaths <- filterM doesFileExist paths
                if length existingPaths > 1
                    then pure $ ClipboardFilePaths existingPaths
                    else pure $ ClipboardText text
    | otherwise = pure ClipboardUnknown

-- | Check if content appears to be image data by magic bytes.
isImageData :: ByteString -> Bool
isImageData bs
    | BS.length bs < 8 = False
    | otherwise =
        let pngMagic = BS.pack [0x89, 0x50, 0x4E, 0x47] -- \x89PNG
            jpegMagic = BS.pack [0xFF, 0xD8, 0xFF] -- \xFF\xD8\xFF
            gifMagic1 = BS.pack [0x47, 0x49, 0x46, 0x38, 0x37, 0x61] -- GIF87a
            gifMagic2 = BS.pack [0x47, 0x49, 0x46, 0x38, 0x39, 0x61] -- GIF89a
            webpMagic = BS.pack [0x52, 0x49, 0x46, 0x46] -- RIFF (WebP)
         in BS.isPrefixOf pngMagic bs
                || BS.isPrefixOf jpegMagic bs
                || BS.isPrefixOf gifMagic1 bs
                || BS.isPrefixOf gifMagic2 bs
                || BS.isPrefixOf webpMagic bs

-- | Detect image format from magic bytes.
detectImageFormat :: ByteString -> Text
detectImageFormat bs
    | BS.length bs < 8 = "application/octet-stream"
    | otherwise =
        let pngMagic = BS.pack [0x89, 0x50, 0x4E, 0x47]
            jpegMagic = BS.pack [0xFF, 0xD8, 0xFF]
            gifMagic1 = BS.pack [0x47, 0x49, 0x46, 0x38, 0x37, 0x61]
            gifMagic2 = BS.pack [0x47, 0x49, 0x46, 0x38, 0x39, 0x61]
            webpMagic = BS.pack [0x52, 0x49, 0x46, 0x46]
         in if
                | BS.isPrefixOf pngMagic bs -> "image/png"
                | BS.isPrefixOf jpegMagic bs -> "image/jpeg"
                | BS.isPrefixOf gifMagic1 bs -> "image/gif"
                | BS.isPrefixOf gifMagic2 bs -> "image/gif"
                | BS.isPrefixOf webpMagic bs -> "image/webp"
                | otherwise -> "application/octet-stream"

-- | Check if content appears to be text (no null bytes in first 1KB).
isTextContent :: ByteString -> Bool
isTextContent bs =
    let sample = BS.take 1024 bs
     in not (BS.elem 0 sample)

-- | Check if text is a single file path.
isSingleFilePath :: Text -> Bool
isSingleFilePath txt =
    let stripped = Text.strip txt
        textLines = Text.lines stripped
     in length textLines == 1
            && not (Text.null stripped)
            && ( Text.isPrefixOf "/" stripped
                    || Text.isPrefixOf "~" stripped
                    || Text.isPrefixOf "." stripped
                    || Text.isPrefixOf "file://" stripped
               )

-------------------------------------------------------------------------------
-- Content Analysis
-------------------------------------------------------------------------------

{- | Analyze clipboard content and determine the appropriate action.

This function:
* Saves images to temporary files and creates attachments
* Loads file paths as media attachments
* Returns text content for editor insertion
-}
analyzeContent :: ClipboardContent -> IO ContentAction
analyzeContent ClipboardUnknown = pure IgnoreContent
analyzeContent (ClipboardText text) = pure $ PasteAsText text
analyzeContent (ClipboardFilePath path) = do
    result <- loadMediaAttachmentFromPath path
    pure $ either (const IgnoreContent) AttachAsMedia result
analyzeContent (ClipboardFilePaths paths) = do
    attachments <- mapM loadMediaAttachmentFromPath paths
    let validAttachments = [att | Right att <- attachments]
    pure $ AttachMultipleFiles validAttachments
analyzeContent (ClipboardImage data_ mime) = do
    result <- createTempAttachment data_ mime
    pure $ either (const IgnoreContent) AttachAsMedia result

-------------------------------------------------------------------------------
-- Media Attachment Creation
-------------------------------------------------------------------------------

{- | Create a temporary file attachment from binary data.

Saves the binary data to a temporary file with appropriate extension
based on the MIME type, then creates a MediaAttachment.
-}
createTempAttachment :: ByteString -> Text -> IO (Either String MediaAttachment)
createTempAttachment content mime = handle handleIOError $ do
    -- Create temp directory if needed
    tempDir <- getTemporaryDirectory
    let clipDir = tempDir </> clipboardTempDir
    createDirectoryIfMissing True clipDir

    -- Determine file extension from MIME type
    let ext = mimeToExtension mime
    tempFile <- emptyTempFile clipDir ("clipboard-" ++ ext)

    -- Write the content
    BS.writeFile tempFile content

    -- Create attachment
    let base64Data = Text.Encoding.decodeUtf8 $ Base64.encode content
        filename = Just $ Text.pack $ takeBaseName tempFile <.> ext

    pure $ Right $ MediaAttachment mime base64Data filename
  where
    handleIOError :: IOException -> IO (Either String MediaAttachment)
    handleIOError e = pure $ Left $ "Failed to create temporary file: " ++ show e

-- | Load a media attachment from a file path.
loadMediaAttachmentFromPath :: FilePath -> IO (Either String MediaAttachment)
loadMediaAttachmentFromPath path = handle handleIOError $ do
    exists <- doesFileExist path
    if not exists
        then pure $ Left $ "File not found: " ++ path
        else do
            content <- BS.readFile path
            if BS.length content > maxClipboardImageSize
                then pure $ Left "File too large (>50MB)"
                else do
                    let mime = detectMimeFromPath path
                        base64Data = Text.Encoding.decodeUtf8 $ Base64.encode content
                        filename = Just $ Text.pack $ takeBaseName path
                    pure $ Right $ MediaAttachment mime base64Data filename
  where
    handleIOError :: IOException -> IO (Either String MediaAttachment)
    handleIOError e = pure $ Left $ "Failed to read file: " ++ show e

-- | Map MIME type to file extension.
mimeToExtension :: Text -> String
mimeToExtension mime
    | "image/png" `Text.isPrefixOf` mime = ".png"
    | "image/jpeg" `Text.isPrefixOf` mime = ".jpg"
    | "image/gif" `Text.isPrefixOf` mime = ".gif"
    | "image/webp" `Text.isPrefixOf` mime = ".webp"
    | "image/svg" `Text.isPrefixOf` mime = ".svg"
    | "application/pdf" `Text.isPrefixOf` mime = ".pdf"
    | otherwise = ".bin"

-- | Detect MIME type from file path extension.
detectMimeFromPath :: FilePath -> Text
detectMimeFromPath path =
    let ext = map (\c -> if c == '.' then '.' else c) (takeExtension path)
        mimeMap =
            [ (".png", "image/png")
            , (".jpg", "image/jpeg")
            , (".jpeg", "image/jpeg")
            , (".gif", "image/gif")
            , (".webp", "image/webp")
            , (".svg", "image/svg+xml")
            , (".pdf", "application/pdf")
            , (".txt", "text/plain")
            , (".md", "text/markdown")
            , (".json", "application/json")
            , (".xml", "application/xml")
            ]
     in fromMaybe "application/octet-stream" (lookup ext mimeMap)

-------------------------------------------------------------------------------
-- File Drop Support
-------------------------------------------------------------------------------

{- | Enable file drop mode in the terminal.

Sends escape sequences to enable drag-and-drop support in terminals
that support it (iTerm2, Kitty, WezTerm).
-}
enableFileDropMode :: IO ()
enableFileDropMode = do
    -- iTerm2 file drop protocol enable
    putStr "\ESC]1337;EnableFileDrop=1\007"
    -- Kitty keyboard protocol (if supported)
    putStr "\ESC[>1u"

-- | Disable file drop mode in the terminal.
disableFileDropMode :: IO ()
disableFileDropMode = do
    -- iTerm2 file drop protocol disable
    putStr "\ESC]1337;EnableFileDrop=0\007"
    -- Reset Kitty keyboard protocol
    putStr "\ESC[<1u"

{- | Parse terminal escape sequences for file drop events.

Currently supports:
* iTerm2 file drop protocol
* Kitty graphics protocol (basic)
* Generic OSC 52 sequences

Returns 'Nothing' if the event is not a recognized file drop sequence.
-}
parseTerminalSequence :: Text -> Maybe FileDropEvent
parseTerminalSequence txt =
    parseITerm2FileDrop txt
        <|> parseKittyFileDrop txt
        <|> parseGenericFileDrop txt

{- | Parse iTerm2 file drop sequence.
Format: \033]1337;File=name=filename;size=N;inline=1:base64data\007
-}
parseITerm2FileDrop :: Text -> Maybe FileDropEvent
parseITerm2FileDrop txt = do
    -- Check for iTerm2 file protocol prefix
    let prefix = "\ESC]1337;File="
    if not (prefix `Text.isPrefixOf` txt)
        then Nothing
        else do
            -- Extract the parameters and data
            let rest = Text.drop (Text.length prefix) txt
                (params, dataAndEnd) = Text.breakOn ":" rest
                data_ = Text.takeWhile (/= '\007') (Text.drop 1 dataAndEnd)

            -- Parse filename from params
            filename <- extractParam "name=" params

            -- Check if inline content is present
            let hasInline = "inline=1" `Text.isInfixOf` params
                content =
                    if hasInline && not (Text.null data_)
                        then Just $ Base64.decodeLenient $ Text.Encoding.encodeUtf8 data_
                        else Nothing

            pure $
                FileDropEvent
                    { dropFilename = Text.unpack filename
                    , dropContent = content
                    , dropMimeType = extractParam "type=" params
                    }

{- | Parse Kitty graphics protocol file drop.
Format: \033_Ga=T,f=100,t=d,d=base64data\033\\
-}
parseKittyFileDrop :: Text -> Maybe FileDropEvent
parseKittyFileDrop txt = do
    let prefix = "\033_G"
    if not (prefix `Text.isPrefixOf` txt)
        then Nothing
        else do
            -- This is a simplified parser - full Kitty protocol is more complex
            let rest = Text.drop (Text.length prefix) txt
                filename = Text.takeWhile (/= '\033') rest
            if Text.null filename
                then Nothing
                else
                    pure $
                        FileDropEvent
                            { dropFilename = Text.unpack filename
                            , dropContent = Nothing
                            , dropMimeType = Nothing
                            }

-- | Parse generic file drop (file:// URL format).
parseGenericFileDrop :: Text -> Maybe FileDropEvent
parseGenericFileDrop txt = do
    let filePrefix = "file://"
    if not (filePrefix `Text.isPrefixOf` txt)
        then Nothing
        else do
            let path = Text.drop (Text.length filePrefix) txt
                filename = Text.takeWhile (/= '\n') path
            pure $
                FileDropEvent
                    { dropFilename = Text.unpack filename
                    , dropContent = Nothing
                    , dropMimeType = Nothing
                    }

-- | Extract a parameter value from a semicolon-separated list.
extractParam :: Text -> Text -> Maybe Text
extractParam key params =
    let parts = Text.splitOn ";" params
        findValue [] = Nothing
        findValue (p : ps)
            | key `Text.isPrefixOf` p = Just $ Text.drop (Text.length key) p
            | otherwise = findValue ps
     in findValue parts

-------------------------------------------------------------------------------
-- Cleanup
-------------------------------------------------------------------------------

{- | Clean up temporary clipboard files.

This should be called on application exit to remove any temporary
files created from clipboard paste operations.
-}
cleanupClipboardTempFiles :: IO ()
cleanupClipboardTempFiles = handle (\(_ :: IOException) -> pure ()) $ do
    tempDir <- getTemporaryDirectory
    let clipDir = tempDir </> clipboardTempDir
    removeFile clipDir
