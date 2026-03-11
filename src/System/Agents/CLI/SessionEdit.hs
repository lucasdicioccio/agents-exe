{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module for the 'session-edit' command handler.
--
-- The session-edit command reads session JSON from stdin, applies
-- transformations, and outputs the result to stdout. This allows
-- composable editing of session files similar to Unix text utilities.
module System.Agents.CLI.SessionEdit
    ( -- * Types
      SessionEditOptions (..)
    , SessionEditOp (..)
      -- * Handler
    , handleSessionEdit
      -- * Transform conversion
    , editOpToTransform
    ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.ByteString.Lazy.Char8 as LByteChar8
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Exit (exitFailure)
import System.IO (stderr)

import qualified System.Agents.Session.Edit as SessionEdit
import qualified System.Agents.Session.Types as SessionTypes

-- | Options for the session-edit command
data SessionEditOptions = SessionEditOptions
    { sessionEditOperations :: [SessionEditOp]
    } deriving (Show)

-- | Session edit operations
data SessionEditOp
    = SessionEditTake Int
    | SessionEditTakeTail Int
    | SessionEditDrop Int
    | SessionEditDropTail Int
    | SessionEditCensorToolCalls
    | SessionEditCensorThinking
    deriving (Show)

-- | Handle the session-edit command
handleSessionEdit :: SessionEditOptions -> IO ()
handleSessionEdit opts = do
    input <- LByteString.getContents
    case Aeson.eitherDecode input of
        Left err -> do
            Text.hPutStrLn stderr $ "Error parsing session JSON: " <> Text.pack err
            exitFailure
        Right session -> do
            let transforms = map editOpToTransform opts.sessionEditOperations
            let editedSession = SessionEdit.applySessionEdits transforms session
            LByteChar8.putStrLn $ Aeson.encodePretty editedSession

-- | Convert a SessionEditOp to a session transformation function
editOpToTransform :: SessionEditOp -> (SessionTypes.Session -> SessionTypes.Session)
editOpToTransform op = case op of
    SessionEditTake n -> SessionEdit.sessionEditTake n
    SessionEditTakeTail n -> SessionEdit.sessionEditTakeTail n
    SessionEditDrop n -> SessionEdit.sessionEditDrop n
    SessionEditDropTail n -> SessionEdit.sessionEditDropTail n
    SessionEditCensorToolCalls -> SessionEdit.sessionEditCensorToolCalls
    SessionEditCensorThinking -> SessionEdit.sessionEditCensorThinking

