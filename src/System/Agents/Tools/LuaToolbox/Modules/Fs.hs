{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Sandboxed filesystem module for LuaToolbox with enhanced path validation.

This module provides filesystem operations that are sandboxed to specific
allowed paths. Key security features:

* Path canonicalization to prevent symlink traversal attacks
* Absolute path requirement
* Whitelist-based access control
* Secure default: empty whitelist means NO access
* fs.patch() for code patching with diff generation

Path validation uses canonicalizePath which resolves symlinks, making it
resistant to traversal attacks through symlinks.
-}
module System.Agents.Tools.LuaToolbox.Modules.Fs (
    FsConfig (..),
    PathError (..),
    registerFsModule,
    validatePath,
) where

import Control.Exception (IOException, try)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as TextIO
import Foreign.C.Types (CInt (..))
import qualified HsLua as Lua
import System.Directory (
    canonicalizePath,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
 )
import System.FilePath (isAbsolute, isPathSeparator, normalise, takeDirectory, (</>))

stackIdxToInt :: Lua.StackIndex -> Int
stackIdxToInt (Lua.StackIndex n) = fromIntegral n

getStackInt :: Lua.StackIndex -> Int
getStackInt = stackIdxToInt

{- | Path validation error types.

These errors provide detailed information about why a path was rejected,
useful for debugging and logging.
-}
data PathError
    = -- | Path is not in the allowed whitelist
      PathNotAllowed FilePath
    | -- | Path is not absolute (must start with /)
      PathNotAbsolute FilePath
    | -- | Path escapes the sandbox directory (child, parent)
      PathOutsideSandbox FilePath FilePath
    | -- | IO error during path validation
      PathIOError FilePath String
    deriving (Show, Eq)

{- | Filesystem module configuration with path sandboxing.

Security defaults:
* Empty allowedPaths means NO filesystem access
* All paths in allowedPaths should be absolute and canonical
* Canonicalization is performed on each path check

Example:
@
FsConfig
    { fsAllowedPaths = ["/home/user/allowed", "/tmp/sandbox"]
    }
@
-}
data FsConfig = FsConfig
    { fsAllowedPaths :: [FilePath]
    {- ^ Whitelist of allowed paths. If empty, NO paths are allowed (secure default).
    Paths should be absolute and canonical.
    -}
    }
    deriving (Show, Eq)

-- | Register the fs module in the Lua state.
registerFsModule :: Lua.State -> FsConfig -> IO ()
registerFsModule lstate config = Lua.runWith lstate $ do
    Lua.newtable

    Lua.pushName "read"
    Lua.pushHaskellFunction (luaRead config)
    Lua.settable (Lua.nthTop 3)

    Lua.pushName "write"
    Lua.pushHaskellFunction (luaWrite config)
    Lua.settable (Lua.nthTop 3)

    Lua.pushName "exists"
    Lua.pushHaskellFunction (luaExists config)
    Lua.settable (Lua.nthTop 3)

    Lua.pushName "list"
    Lua.pushHaskellFunction (luaList config)
    Lua.settable (Lua.nthTop 3)

    Lua.pushName "mkdir"
    Lua.pushHaskellFunction (luaMkdir config)
    Lua.settable (Lua.nthTop 3)

    Lua.pushName "isdir"
    Lua.pushHaskellFunction (luaIsDir config)
    Lua.settable (Lua.nthTop 3)

    Lua.pushName "isfile"
    Lua.pushHaskellFunction (luaIsFile config)
    Lua.settable (Lua.nthTop 3)

    Lua.pushName "patch"
    Lua.pushHaskellFunction (luaPatch config)
    Lua.settable (Lua.nthTop 3)

    Lua.setglobal (Lua.Name "fs")

{- | Validate and canonicalize a path against allowed paths.

This function performs the following security checks:
1. Path must be absolute
2. Path is canonicalized (resolves symlinks, .., etc.)
3. Canonical path must be within one of the allowed paths

Returns the canonical path if valid, or an error.
-}
validatePath :: FsConfig -> FilePath -> IO (Either PathError FilePath)
validatePath config path = do
    -- Path must be absolute
    if not (isAbsolute path)
        then pure $ Left $ PathNotAbsolute path
        else do
            -- Canonicalize to resolve symlinks, .., etc.
            -- This is crucial for preventing symlink traversal attacks
            canonical <- canonicalizePath path

            -- Check against allowed paths
            if null (fsAllowedPaths config)
                then pure $ Left $ PathNotAllowed canonical
                else
                    if any (isPathWithin canonical) (fsAllowedPaths config)
                        then pure $ Right canonical
                        else pure $ Left $ PathOutsideSandbox canonical (show $ fsAllowedPaths config)
  where
    -- Check if child is within parent (handles path separator edge cases)
    isPathWithin :: FilePath -> FilePath -> Bool
    isPathWithin child parent =
        let parent' = if last parent == '/' then parent else parent ++ "/"
         in child == parent || take (length parent') child == parent'

-- | Read file contents with enhanced error handling.
luaRead :: FsConfig -> Lua.LuaE Lua.Exception Lua.NumResults
luaRead config = do
    -- Get path from top of stack
    pathBytes <- Lua.tostring' (Lua.nthTop 1)
    Lua.pop 1

    let path = Text.unpack $ Text.decodeUtf8 pathBytes

    validation <- liftIO $ validatePath config path
    case validation of
        Left err -> do
            Lua.pushnil
            Lua.pushstring (Text.encodeUtf8 $ Text.pack $ show err)
            pure 2
        Right validPath -> do
            result <- liftIO $ try $ BS.readFile validPath
            case result of
                Left (e :: IOException) -> do
                    Lua.pushnil
                    Lua.pushstring (Text.encodeUtf8 $ Text.pack $ show e)
                    pure 2
                Right content -> do
                    Lua.pushstring content
                    pure 1

-- | Write file contents with enhanced error handling.
luaWrite :: FsConfig -> Lua.LuaE Lua.Exception Lua.NumResults
luaWrite config = do
    top <- Lua.gettop
    let topInt = getStackInt top
    if topInt < 2
        then do
            Lua.pop topInt
            Lua.pushboolean False
            Lua.pushstring "Usage: fs.write(path, content)"
            pure 2
        else do
            let pathIdx = topInt - 1
            pathBytes <- Lua.tostring' (Lua.nthTop (fromIntegral pathIdx))
            content <- Lua.tostring' (Lua.nthTop 1)
            Lua.pop topInt

            let path = Text.unpack $ Text.decodeUtf8 pathBytes
            validation <- liftIO $ validatePath config path

            case validation of
                Left err -> do
                    Lua.pushboolean False
                    Lua.pushstring (Text.encodeUtf8 $ Text.pack $ show err)
                    pure 2
                Right validPath -> do
                    let parentDir = takeDirectory validPath
                    parentResult <- liftIO $ try $ createDirectoryIfMissing True parentDir
                    case parentResult of
                        Left (e :: IOException) -> do
                            Lua.pushboolean False
                            Lua.pushstring (Text.encodeUtf8 $ Text.pack $ show e)
                            pure 2
                        Right () -> do
                            result <- liftIO $ try $ BS.writeFile validPath content
                            case result of
                                Left (e :: IOException) -> do
                                    Lua.pushboolean False
                                    Lua.pushstring (Text.encodeUtf8 $ Text.pack $ show e)
                                    pure 2
                                Right () -> do
                                    Lua.pushboolean True
                                    pure 1

-- | Check if path exists.
luaExists :: FsConfig -> Lua.LuaE Lua.Exception Lua.NumResults
luaExists config = do
    -- Get path from top of stack
    pathBytes <- Lua.tostring' (Lua.nthTop 1)
    Lua.pop 1

    let path = Text.unpack $ Text.decodeUtf8 pathBytes
    validation <- liftIO $ validatePath config path

    case validation of
        Left _ -> do
            Lua.pushboolean False
            pure 1
        Right validPath -> do
            fileExists <- liftIO $ doesFileExist validPath
            dirExists <- liftIO $ doesDirectoryExist validPath
            Lua.pushboolean (fileExists || dirExists)
            pure 1

-- | List directory contents.
luaList :: FsConfig -> Lua.LuaE Lua.Exception Lua.NumResults
luaList config = do
    -- Get path from top of stack
    pathBytes <- Lua.tostring' (Lua.nthTop 1)
    Lua.pop 1

    let path = Text.unpack $ Text.decodeUtf8 pathBytes
    validation <- liftIO $ validatePath config path

    case validation of
        Left err -> do
            Lua.pushnil
            Lua.pushstring (Text.encodeUtf8 $ Text.pack $ show err)
            pure 2
        Right validPath -> do
            isDir <- liftIO $ doesDirectoryExist validPath
            if not isDir
                then do
                    Lua.pushnil
                    Lua.pushstring "Not a directory"
                    pure 2
                else do
                    result <- liftIO $ try $ listDirectory validPath
                    case result of
                        Left (e :: IOException) -> do
                            Lua.pushnil
                            Lua.pushstring (Text.encodeUtf8 $ Text.pack $ show e)
                            pure 2
                        Right entries -> do
                            pushStringList entries
                            pure 1

-- | Create directory.
luaMkdir :: FsConfig -> Lua.LuaE Lua.Exception Lua.NumResults
luaMkdir config = do
    -- Get path from top of stack
    pathBytes <- Lua.tostring' (Lua.nthTop 1)
    Lua.pop 1

    let path = Text.unpack $ Text.decodeUtf8 pathBytes
    validation <- liftIO $ validatePath config path

    case validation of
        Left err -> do
            Lua.pushboolean False
            Lua.pushstring (Text.encodeUtf8 $ Text.pack $ show err)
            pure 2
        Right validPath -> do
            result <- liftIO $ try $ createDirectoryIfMissing True validPath
            case result of
                Left (e :: IOException) -> do
                    Lua.pushboolean False
                    Lua.pushstring (Text.encodeUtf8 $ Text.pack $ show e)
                    pure 2
                Right () -> do
                    Lua.pushboolean True
                    pure 1

-- | Check if path is a directory.
luaIsDir :: FsConfig -> Lua.LuaE Lua.Exception Lua.NumResults
luaIsDir config = do
    -- Get path from top of stack
    pathBytes <- Lua.tostring' (Lua.nthTop 1)
    Lua.pop 1

    let path = Text.unpack $ Text.decodeUtf8 pathBytes
    validation <- liftIO $ validatePath config path

    case validation of
        Left _ -> do
            Lua.pushboolean False
            pure 1
        Right validPath -> do
            isDir <- liftIO $ doesDirectoryExist validPath
            Lua.pushboolean isDir
            pure 1

-- | Check if path is a file.
luaIsFile :: FsConfig -> Lua.LuaE Lua.Exception Lua.NumResults
luaIsFile config = do
    -- Get path from top of stack
    pathBytes <- Lua.tostring' (Lua.nthTop 1)
    Lua.pop 1

    let path = Text.unpack $ Text.decodeUtf8 pathBytes
    validation <- liftIO $ validatePath config path

    case validation of
        Left _ -> do
            Lua.pushboolean False
            pure 1
        Right validPath -> do
            isFile <- liftIO $ doesFileExist validPath
            Lua.pushboolean isFile
            pure 1

{- | Patch a file by replacing search pattern with replacement.

fs.patch(path, search, replace) -> success, error, diff

Returns:
* success: boolean indicating if any replacements were made
* error: empty string on success, error message on failure
* diff: unified diff of changes (or nil if no changes)
-}
luaPatch :: FsConfig -> Lua.LuaE Lua.Exception Lua.NumResults
luaPatch config = do
    top <- Lua.gettop
    let topInt = getStackInt top

    if topInt < 3
        then do
            Lua.pop topInt
            Lua.pushboolean False
            Lua.pushstring "Usage: fs.patch(path, search, replace)"
            Lua.pushnil
            pure 3
        else do
            pathBytes <- Lua.tostring' (Lua.nthTop 3)
            searchBytes <- Lua.tostring' (Lua.nthTop 2)
            replaceBytes <- Lua.tostring' (Lua.nthTop 1)
            Lua.pop 3

            let path = Text.unpack $ Text.decodeUtf8 pathBytes
            let search = Text.decodeUtf8 searchBytes
            let replace = Text.decodeUtf8 replaceBytes

            validation <- liftIO $ validatePath config path
            case validation of
                Left err -> do
                    Lua.pushboolean False
                    Lua.pushstring (Text.encodeUtf8 $ Text.pack $ show err)
                    Lua.pushnil
                    pure 3
                Right validPath -> do
                    result <- liftIO $ try $ do
                        content <- TextIO.readFile validPath
                        let newContent = Text.replace search replace content
                        let count = Text.count search content
                        if count > 0
                            then do
                                TextIO.writeFile validPath newContent
                                let diff = generateDiff search replace content newContent count
                                pure (True, count, diff)
                            else
                                pure (False, 0, Text.empty)
                    case result of
                        Left (e :: IOException) -> do
                            Lua.pushboolean False
                            Lua.pushstring (Text.encodeUtf8 $ Text.pack $ show e)
                            Lua.pushnil
                            pure 3
                        Right (success, count, diff) -> do
                            Lua.pushboolean success
                            if success
                                then do
                                    Lua.pushstring ""
                                    Lua.pushstring
                                        ( Text.encodeUtf8 $
                                            Text.pack $
                                                "Replaced " ++ show count ++ " occurrence(s)\n\n" ++ Text.unpack diff
                                        )
                                else do
                                    Lua.pushstring "Pattern not found"
                                    Lua.pushnil
                            pure 3

{- | Generate a simple unified diff for the patch.

This generates a basic unified diff showing the change made.
For more complex diffs, a proper diff library would be needed.
-}
generateDiff :: Text -> Text -> Text -> Text -> Int -> Text
generateDiff search replace oldContent newContent count =
    let oldLines = Text.lines oldContent
        newLines = Text.lines newContent
        -- Find context (simplified: show first few lines around change)
        contextLines = 3
     in Text.unlines
            [ "--- a/original"
            , "+++ b/modified"
            , "@@ -1," <> Text.pack (show (min contextLines (length oldLines))) <> " +1," <> Text.pack (show (min contextLines (length newLines))) <> " @@"
            , "-" <> search
            , "+" <> replace
            , ""
            , "(Changed " <> Text.pack (show count) <> " occurrence(s))"
            ]

-- | Push a list of strings as a Lua table.
pushStringList :: [String] -> Lua.LuaE Lua.Exception ()
pushStringList items = do
    Lua.newtable
    mapM_
        ( \(i, s) -> do
            Lua.pushstring (Text.encodeUtf8 $ Text.pack s)
            Lua.pushinteger (fromIntegral i)
            Lua.insert (Lua.nthTop 2)
            Lua.settable (Lua.nthTop 3)
        )
        (zip [1 ..] items)
