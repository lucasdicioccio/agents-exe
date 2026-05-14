{-# LANGUAGE OverloadedStrings #-}

{- | Result formatting utilities for the SystemToolbox.

This module provides functions to format query results as JSON
for LLM consumption.
-}
module System.Agents.Tools.SystemToolbox.Formatting (
    -- * Result formatting
    formatResults,
) where

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy as LByteString

import System.Agents.Tools.SystemToolbox.Types (QueryResult (..), formatExecutionTime)

{- | Format query results as JSON.

Returns a ByteString containing a JSON object with:
* "capability": The capability name
* "data": The capability data
* "executionTime": Execution time in seconds

Example output:

> {
>   "capability": "date",
>   "data": {
>     "utc": "2024-01-15T10:30:00.123456Z",
>     "local": "2024-01-15T11:30:00.123456",
>     "timezone": "CET",
>     "timezoneOffset": "+0100"
>   },
>   "executionTime": 0.001
> }
-}
formatResults :: QueryResult -> ByteString
formatResults result =
    LByteString.toStrict $ Aeson.encode jsonObj
  where
    jsonObj =
        Aeson.object
            [ "capability" .= resultCapability result
            , "data" .= resultData result
            , "executionTime" .= formatExecutionTime (resultExecutionTime result)
            ]

