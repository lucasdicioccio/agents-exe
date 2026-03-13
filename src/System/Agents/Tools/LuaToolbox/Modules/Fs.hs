{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Sandboxed filesystem module for LuaToolbox.
-}
module System.Agents.Tools.LuaToolbox.Modules.Fs (
    FsConfig (..),
    registerFsModule,
) where

import qualified Data.ByteString as BS
import Control.Exception (try, IOException)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified HsLua as Lua
import System.Directory (
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
 )
import System.FilePath (isAbsolute, normalise, (</>), takeDirectory, isPathSeparator)
import Data.List (isPrefixOf)
import Foreign.C.Types (CInt(..))

stackIdxToInt :: Lua.StackIndex -> Int
stackIdxToInt (Lua.StackIndex n) = fromIntegral n

getStackInt :: Lua.StackIndex -> Int
getStackInt = stackIdxToInt

-- | Filesystem module configuration with path sandboxing.
data FsConfig = FsConfig
    { fsAllowedPaths :: [FilePath]
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

    Lua.setglobal (Lua.Name "fs")

-- | Read file contents.
luaRead :: FsConfig -> Lua.LuaE Lua.Exception Lua.NumResults
luaRead config = do
    mPath <- getPathArg config 1
    case mPath of
        Left err -> do
            Lua.pushnil
            Lua.pushstring (Text.encodeUtf8 err)
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

-- | Write file contents.
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
            mPath <- getPathArg config pathIdx
            content <- Lua.tostring' (Lua.nthTop 1)
            Lua.pop topInt
            case mPath of
                Left err -> do
                    Lua.pushboolean False
                    Lua.pushstring (Text.encodeUtf8 err)
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
    mPath <- getPathArg config 1
    case mPath of
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
    mPath <- getPathArg config 1
    case mPath of
        Left err -> do
            Lua.pushnil
            Lua.pushstring (Text.encodeUtf8 err)
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
    mPath <- getPathArg config 1
    case mPath of
        Left err -> do
            Lua.pushboolean False
            Lua.pushstring (Text.encodeUtf8 err)
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
    mPath <- getPathArg config 1
    case mPath of
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
    mPath <- getPathArg config 1
    case mPath of
        Left _ -> do
            Lua.pushboolean False
            pure 1
        Right validPath -> do
            isFile <- liftIO $ doesFileExist validPath
            Lua.pushboolean isFile
            pure 1

-- | Validate and normalize path.
validatePath :: FsConfig -> FilePath -> Either Text FilePath
validatePath config path
    | null (fsAllowedPaths config) = Left "No paths allowed"
    | not (isAbsolute normalised) = Left "Path must be absolute"
    | containsTraversal normalised = Left "Path contains directory traversal"
    | otherwise = 
        if any (`isPathPrefixOf` normalised) (fsAllowedPaths config)
            then Right normalised
            else Left "Path not in allowed paths"
  where
    normalised = normalise path
    containsTraversal p = ".." `elem` splitPath p
    
    splitPath :: FilePath -> [String]
    splitPath = go []
      where
        go acc [] = reverse acc
        go acc (c:cs)
            | isPathSeparator c = go acc cs
            | otherwise = 
                let (part, rest) = break isPathSeparator (c:cs)
                in go (part : acc) rest

    isPathPrefixOf :: FilePath -> FilePath -> Bool
    isPathPrefixOf prefix p = 
        prefix == p || 
        (prefix `isPrefixOf` p && 
         (null dropPrefix || isPathSeparator (head dropPrefix)))
      where
        dropPrefix = drop (length prefix) p

-- | Helper to get and validate a path argument.
getPathArg :: FsConfig -> Int -> Lua.LuaE Lua.Exception (Either Text FilePath)
getPathArg config offset = do
    top <- Lua.gettop
    let idx = getStackInt top - offset + 1
    bs <- Lua.tostring' (Lua.nthTop (fromIntegral idx))
    let path = Text.unpack $ Text.decodeUtf8 bs
    pure $ validatePath config path

-- | Push a list of strings as a Lua table.
pushStringList :: [String] -> Lua.LuaE Lua.Exception ()
pushStringList items = do
    Lua.newtable
    mapM_ (\(i, s) -> do
        Lua.pushstring (Text.encodeUtf8 $ Text.pack s)
        Lua.pushinteger (fromIntegral i)
        Lua.insert (Lua.nthTop 2)
        Lua.settable (Lua.nthTop 3)
        ) (zip [1..] items)

