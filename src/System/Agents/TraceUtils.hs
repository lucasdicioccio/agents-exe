module System.Agents.TraceUtils where

import Prod.Tracer (Tracer (..))

import System.Agents.Runtime.Trace (Trace (..), ConversationTrace (..))
import qualified System.Agents.LLMs.OpenAI as OpenAI

-------------------------------------------------------------------------------
traceWaitingOpenAIRateLimits ::
    OpenAI.ApiLimits ->
    (OpenAI.WaitAction -> IO ()) ->
    Tracer IO Trace
traceWaitingOpenAIRateLimits lims onWait = Tracer f
  where
    f (AgentTrace_Conversation _ _ _ (LLMTrace _ tr)) =
        runTracer (OpenAI.waitRateLimit lims onWait) tr
    f _ = pure ()

