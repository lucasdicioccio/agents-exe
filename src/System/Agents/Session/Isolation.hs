{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Isolated deployment primitives for durable tool execution.

This module implements Phase 5 of the durable-workflows plan. It defines
a stable JSON envelope for passing tool calls to external runners and
provides concrete runners that execute tool calls outside the current
process:

* 'localProcessRunner' forks a worker executable.
* 'dockerRunner' runs a Docker container.
* 'functionRunner' is a documented placeholder for future serverless/FaaS
  execution.

The envelopes are designed to be language-agnostic so that worker processes
can be written in any language.
-}
module System.Agents.Session.Isolation (
    -- * Runner abstraction
    DeploymentRunner (..),
    IsolationError (..),

    -- * Envelope types
    IsolationEnvelope (..),
    IsolationResultEnvelope (..),
    IsolationResultStatus (..),

    -- * Envelope helpers
    mkIsolationEnvelope,
    mkIsolationSuccessEnvelope,
    mkIsolationErrorEnvelope,
    parseIsolationResultEnvelope,
    parseIsolationResultEnvelopeLBS,

    -- * Concrete runners
    localProcessRunner,
    dockerRunner,
    functionRunner,
) where

import Control.Exception (SomeException, evaluate, try)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import System.Exit (ExitCode (..))
import System.IO (hClose, hSetBinaryMode)
import System.Process (
    CreateProcess (..),
    StdStream (..),
    createProcess,
    proc,
    waitForProcess,
 )

import System.Agents.Session.Types (
    ContinuationToken (..),
    LlmToolCall (..),
    ToolCallDisposition (..),
    UserToolResponse (..),
 )
import System.Agents.Tools.Context (ToolExecutionContextSnapshot)

-------------------------------------------------------------------------------
-- Runner abstraction
-------------------------------------------------------------------------------

{- | Error produced by an isolated execution runner.
-}
newtype IsolationError = IsolationError Text
    deriving (Show, Eq)

{- | Runner for tool calls outside the current process.

The runner receives a stable 'IsolationEnvelope' containing the tool call,
a continuation token, a serialisable context snapshot, and the isolation
policy decision. It must return either an error or a 'UserToolResponse'.

This abstraction covers Docker, subprocess workers, and future serverless
targets.
-}
data DeploymentRunner = DeploymentRunner
    { drName :: Text
    -- ^ Human-readable runner name (e.g., "docker", "local-process")
    , drExecute :: IsolationEnvelope -> IO (Either IsolationError UserToolResponse)
    }

-------------------------------------------------------------------------------
-- Envelope types
-------------------------------------------------------------------------------

{- | Stable input envelope consumed by any external runner.

Serialised form:

@
{
  "token": "<uuid>",
  "toolCall": { "function": { "name": "..." }, "arguments": { ... } },
  "contextSnapshot": { ... },
  "policy": { "tag": "runIsolated", "spec": { "tag": "docker", "image": "..." } },
  "reason": "..."         -- optional, omitted when absent
}
@

The @policy@ field uses the same serialisation as 'ToolCallDisposition'
(see "System.Agents.Session.Types"). The exact shape of the @spec@ depends
on the 'IsolationSpec'.
-}
data IsolationEnvelope = IsolationEnvelope
    { ieToken :: ContinuationToken
    -- ^ Correlation token echoed back by the worker in the result envelope.
    , ieToolCall :: LlmToolCall
    -- ^ The LLM-issued tool call to execute.
    , ieContextSnapshot :: ToolExecutionContextSnapshot
    -- ^ Serialisable subset of the execution context.
    , ieDisposition :: ToolCallDisposition
    -- ^ The isolation policy decision (wrapped as @policy@ in JSON).
    , ieReason :: Maybe Text
    }
    deriving (Show, Eq)

instance Aeson.ToJSON IsolationEnvelope where
    toJSON envelope =
        Aeson.object $
            [ "token" Aeson..= envelope.ieToken
            , "toolCall" Aeson..= envelope.ieToolCall
            , "contextSnapshot" Aeson..= envelope.ieContextSnapshot
            , "policy" Aeson..= envelope.ieDisposition
            ]
                ++ ["reason" Aeson..= r | Just r <- [envelope.ieReason]]

instance Aeson.FromJSON IsolationEnvelope where
    parseJSON = Aeson.withObject "IsolationEnvelope" $ \v ->
        IsolationEnvelope
            <$> v Aeson..: "token"
            <*> v Aeson..: "toolCall"
            <*> v Aeson..: "contextSnapshot"
            <*> v Aeson..: "policy"
            <*> v Aeson..:? "reason"

-- | Build an input envelope for an isolated call.
mkIsolationEnvelope ::
    ContinuationToken ->
    LlmToolCall ->
    ToolExecutionContextSnapshot ->
    ToolCallDisposition ->
    Maybe Text ->
    IsolationEnvelope
mkIsolationEnvelope token call ctxSnap disp mReason =
    IsolationEnvelope
        { ieToken = token
        , ieToolCall = call
        , ieContextSnapshot = ctxSnap
        , ieDisposition = disp
        , ieReason = mReason
        }

{- | Lifecycle status of an isolated execution result.
-}
data IsolationResultStatus
    = IsolationSuccess
    | IsolationFailure
    deriving (Show, Eq)

instance Aeson.ToJSON IsolationResultStatus where
    toJSON IsolationSuccess = Aeson.String "success"
    toJSON IsolationFailure = Aeson.String "error"

instance Aeson.FromJSON IsolationResultStatus where
    parseJSON = Aeson.withText "IsolationResultStatus" $ \case
        "success" -> pure IsolationSuccess
        "error" -> pure IsolationFailure
        other -> fail $ "Unknown IsolationResultStatus: " ++ Text.unpack other

{- | Result envelope produced by an external runner.

Success form:

@
{ "token": "<uuid>", "status": "success", "result": { ... } }
@

Failure form:

@
{ "token": "<uuid>", "status": "error", "error": "..." }
@
-}
data IsolationResultEnvelope = IsolationResultEnvelope
    { ireToken :: ContinuationToken
    , ireStatus :: IsolationResultStatus
    , ireResult :: Maybe UserToolResponse
    , ireError :: Maybe Text
    }
    deriving (Show, Eq)

instance Aeson.ToJSON IsolationResultEnvelope where
    toJSON envelope =
        Aeson.object $
            ["token" Aeson..= envelope.ireToken, "status" Aeson..= envelope.ireStatus]
                ++ ["result" Aeson..= r | Just r <- [envelope.ireResult]]
                ++ ["error" Aeson..= e | Just e <- [envelope.ireError]]

instance Aeson.FromJSON IsolationResultEnvelope where
    parseJSON = Aeson.withObject "IsolationResultEnvelope" $ \v ->
        IsolationResultEnvelope
            <$> v Aeson..: "token"
            <*> v Aeson..: "status"
            <*> v Aeson..:? "result"
            <*> v Aeson..:? "error"

-- | Build a success result envelope.
mkIsolationSuccessEnvelope :: ContinuationToken -> UserToolResponse -> IsolationResultEnvelope
mkIsolationSuccessEnvelope token result =
    IsolationResultEnvelope
        { ireToken = token
        , ireStatus = IsolationSuccess
        , ireResult = Just result
        , ireError = Nothing
        }

-- | Build a failure result envelope.
mkIsolationErrorEnvelope :: ContinuationToken -> Text -> IsolationResultEnvelope
mkIsolationErrorEnvelope token err =
    IsolationResultEnvelope
        { ireToken = token
        , ireStatus = IsolationFailure
        , ireResult = Nothing
        , ireError = Just err
        }

-- | Parse a result envelope from a raw JSON value.
parseIsolationResultEnvelope :: Aeson.Value -> Either IsolationError IsolationResultEnvelope
parseIsolationResultEnvelope val =
    case Aeson.Types.parseEither Aeson.parseJSON val of
        Left err -> Left $ IsolationError $ Text.pack $ "invalid result envelope: " ++ err
        Right envelope -> Right envelope

-- | Parse a result envelope from a lazy 'LBS.ByteString'.
parseIsolationResultEnvelopeLBS :: LBS.ByteString -> Either IsolationError IsolationResultEnvelope
parseIsolationResultEnvelopeLBS bs =
    case Aeson.eitherDecode bs of
        Left err -> Left $ IsolationError $ Text.pack $ "invalid result envelope JSON: " ++ err
        Right envelope -> Right envelope

-------------------------------------------------------------------------------
-- Concrete runners
-------------------------------------------------------------------------------

{- | Fork a local worker process to execute an isolated tool call.

The envelope is serialised to compact JSON and written to the worker's
stdin. The worker must print a single valid 'IsolationResultEnvelope' to
stdout and exit with code 0 on success.

Errors are returned as 'IsolationError' for:

* Failure to start the process.
* Non-zero exit code.
* Unparseable stdout.
* A result envelope whose token does not match the input token.
-}
localProcessRunner :: FilePath -> DeploymentRunner
localProcessRunner workerPath =
    DeploymentRunner
        { drName = "local-process"
        , drExecute = runExternalProcess (proc workerPath []) "local process"
        }

{- | Run a Docker container to execute an isolated tool call.

The envelope is written to the container's stdin via @docker run --rm -i@.
The container image is expected to contain a worker that reads the envelope
from stdin and writes a result envelope to stdout.

If Docker is not installed or cannot run the container, the call returns
an 'IsolationError'.
-}
dockerRunner :: Text -> DeploymentRunner
dockerRunner image =
    DeploymentRunner
        { drName = "docker"
        , drExecute = runExternalProcess (proc "docker" ["run", "--rm", "-i", Text.unpack image]) "docker"
        }

{- | Future placeholder for serverless / FaaS execution.

This runner is intentionally unimplemented. Use it as a documented stub
when designing function-as-a-service integrations; attempting to execute
a call through it always returns an 'IsolationError'.
-}
functionRunner :: DeploymentRunner
functionRunner =
    DeploymentRunner
        { drName = "function-runner"
        , drExecute = const $
                pure $
                    Left $
                        IsolationError "functionRunner is a future placeholder for serverless/FaaS execution and is not yet implemented"
        }

-------------------------------------------------------------------------------
-- Shared external-process implementation
-------------------------------------------------------------------------------

-- | Run a 'CreateProcess' with the envelope on stdin and parse the result.
runExternalProcess ::
    CreateProcess ->
    Text ->
    IsolationEnvelope ->
    IO (Either IsolationError UserToolResponse)
runExternalProcess baseCp label envelope = do
    let inputJson = LBS.toStrict $ Aeson.encode envelope
    result <- try $ do
        (mStdin, mStdout, mStderr, ph) <-
            createProcess
                baseCp
                    { std_in = CreatePipe
                    , std_out = CreatePipe
                    , std_err = CreatePipe
                    }
        case (mStdin, mStdout, mStderr) of
            (Just hin, Just hout, Just herr) -> do
                hSetBinaryMode hin True
                hSetBinaryMode hout True
                hSetBinaryMode herr True
                LBS.hPut hin $ LBS.fromStrict inputJson
                hClose hin
                out <- LBS.hGetContents hout
                err <- LBS.hGetContents herr
                -- Force evaluation of handles before waiting for the process.
                _ <- evaluateLBS out
                _ <- evaluateLBS err
                exitCode <- waitForProcess ph
                pure (out, err, exitCode)
            _ ->
                ioError $ userError "failed to create process pipes"
    case result of
        Left (e :: SomeException) ->
            pure $ Left $ IsolationError $ "failed to start " <> label <> ": " <> Text.pack (show e)
        Right (out, err, exitCode) -> do
            let errTxt = Text.strip $ TextEnc.decodeUtf8 $ LBS.toStrict err
            case exitCode of
                ExitFailure code ->
                    pure $
                        Left $
                            IsolationError $
                                label <> " exited with code " <> Text.pack (show code)
                                    <> (if Text.null errTxt then "" else ": " <> errTxt)
                ExitSuccess ->
                    case parseIsolationResultEnvelopeLBS out of
                        Left parseErr -> pure $ Left parseErr
                        Right resultEnv -> validateResult envelope.ieToken resultEnv
  where
    validateResult inputToken resultEnv
        | resultEnv.ireToken /= inputToken =
            pure $ Left $ IsolationError "result envelope token does not match input token"
        | otherwise =
            case resultEnv.ireStatus of
                IsolationFailure ->
                    pure $ Left $ IsolationError $ maybe "unknown error" id resultEnv.ireError
                IsolationSuccess ->
                    case resultEnv.ireResult of
                        Nothing -> pure $ Left $ IsolationError "success envelope missing result field"
                        Just response -> pure $ Right response

-- | Fully evaluate a lazy bytestring so handles can be closed safely.
evaluateLBS :: LBS.ByteString -> IO ()
evaluateLBS bs = evaluate (LBS.length bs) >> pure ()
