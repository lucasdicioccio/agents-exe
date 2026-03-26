module System.Agents.TraceUtils where

import Prod.Tracer (Tracer (..))

import qualified System.Agents.LLMs.OpenAI as OpenAI
import System.Agents.Runtime.Trace (ConversationTrace (..), Trace (..))

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
