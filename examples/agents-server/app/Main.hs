{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Exception (SomeException, displayException, try)
import Data.Aeson ((.=))
import Options.Applicative
import System.Exit (exitFailure)
import System.IO (stderr)

import AgentsServer.Log (logLine, newHandleLogger)
import AgentsServer.Server (runServer, serverOptions)

main :: IO ()
main = do
    opts <- execParser $ info (serverOptions <**> helper) (fullDesc <> progDesc "Serve agents over HTTP, with sessions stored in SQLite")
    logger <- newHandleLogger stderr
    try (runServer opts logger) >>= \case
        Right () -> pure ()
        Left (e :: SomeException) -> do
            logLine logger "server.failed" ["message" .= displayException e]
            exitFailure
