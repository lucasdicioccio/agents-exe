{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

{- | File attachment capability for multi-modal LLM interactions.

This module implements the file attachment functionality, allowing
files to be read from disk and returned as base64-encoded attachments
suitable for LLM consumption.
-}
module System.Agents.Tools.SystemToolbox.Attachment (
    -- * File attachment
    executeAttachFile,
    maxFileSize,
) where

import Control.Exception (SomeException, try)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Prod.Tracer (Tracer (..), runTracer)
import System.Directory (doesFileExist)

import System.Agents.Base (SystemToolCapability (..))
import System.Agents.Tools.SystemToolbox.Media (detectMediaType, getFileExtension, mediaTypeToMime)
import System.Agents.Tools.SystemToolbox.Types (
    AttachFileResult (..),
    QueryError (..),
    Toolbox (..),
    Trace (..),
 )

-------------------------------------------------------------------------------
-- File Attachment
-------------------------------------------------------------------------------

-- | Maximum file size for attachment (50MB)
maxFileSize :: Int
maxFileSize = 50 * 1024 * 1024

{- | Execute file attachment.

This function reads a file from disk and returns it as a base64-encoded
attachment suitable for multi-modal LLM interactions.

Returns:
* 'Right AttachFileResult' on successful attachment
* 'Left QueryError' if capability not enabled, file not found, or file too large
-}
executeAttachFile ::
    Tracer IO Trace ->
    Toolbox ->
    FilePath ->
    IO (Either QueryError AttachFileResult)
executeAttachFile tracer toolbox filePath = do
    runTracer tracer (FileAttachRequestedTrace filePath)

    -- Check if capability is enabled
    if SystemToolAttachFile `notElem` toolboxCapabilities toolbox
        then do
            let err = CapabilityNotEnabledError "attach-file"
            runTracer tracer (FileAttachErrorTrace filePath "Capability not enabled")
            pure $ Left err
        else do
            -- Check if file exists
            fileExists <- doesFileExist filePath
            if not fileExists
                then do
                    let err = FileNotFoundError filePath
                    runTracer tracer (FileAttachErrorTrace filePath "File not found")
                    pure $ Left err
                else do
                    -- Read file
                    result <- try $ BS.readFile filePath
                    case result of
                        Left (e :: SomeException) -> do
                            let errMsg = Text.pack $ show e
                            runTracer tracer (FileAttachErrorTrace filePath errMsg)
                            pure $ Left $ SystemInfoError $ "Failed to read file: " <> errMsg
                        Right content -> do
                            let fileSize = BS.length content

                            -- Check file size
                            if fileSize > maxFileSize
                                then do
                                    runTracer tracer (FileAttachErrorTrace filePath "File too large")
                                    pure $ Left $ FileTooLargeError filePath maxFileSize
                                else do
                                    -- Detect media type
                                    case detectMediaType filePath of
                                        Nothing -> do
                                            let errMsg = "Unsupported file type: " <> getFileExtension filePath
                                            runTracer tracer (FileAttachErrorTrace filePath errMsg)
                                            pure $ Left $ UnsupportedFileTypeError filePath (getFileExtension filePath)
                                        Just mediaType -> do
                                            let mimeType = mediaTypeToMime mediaType
                                            let base64Data = Text.decodeUtf8 $ B64.encode content

                                            runTracer tracer (FileAttachSuccessTrace filePath mediaType fileSize)

                                            pure $
                                                Right $
                                                    AttachFileResult
                                                        { attachFilePath = filePath
                                                        , attachMimeType = mimeType
                                                        , attachBase64Data = base64Data
                                                        , attachFileSize = fileSize
                                                        }

