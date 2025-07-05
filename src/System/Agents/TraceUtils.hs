module System.Agents.TraceUtils where

import Prod.Tracer (Tracer (..))

import System.Agents.AgentTree (Trace (..))
import qualified System.Agents.LLMs.OpenAI as OpenAI
import qualified System.Agents.Runtime as Runtime

-------------------------------------------------------------------------------
traceWaitingOpenAIRateLimits ::
    OpenAI.ApiLimits ->
    (OpenAI.WaitAction -> IO ()) ->
    Tracer IO Trace
traceWaitingOpenAIRateLimits lims onWait = Tracer f
  where
    f (AgentTrace (Runtime.AgentTrace_Conversation _ _ _ (Runtime.LLMTrace _ tr))) =
        runTracer (OpenAI.waitRateLimit lims onWait) tr
    f _ = pure ()
