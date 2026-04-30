{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Progressive Disclosure combinators for agent tool management.

This module provides combinators for dynamically managing tool visibility
based on session state. The primary function 'agentEvaluateActiveTools'
wraps an agent to filter tools based on activation state, implementing
a "progressive disclosure" pattern where tools are revealed on demand.
-}
module System.Agents.Combinators.ProgressiveDisclosure (
    Trace (..),

    -- * Progressive Disclosure
    agentEvaluateActiveTools,
) where

import Control.Concurrent.STM (TVar, readTVarIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Prod.Tracer (Tracer (..), contramap)

import System.Agents.Session.Base
import qualified System.Agents.Session.Compat as SessionCompat
import qualified System.Agents.ToolPortal as ToolPortal
import System.Agents.ToolRegistration (ToolRegistration (..))
import qualified System.Agents.ToolRegistration as ToolRegistration
import System.Agents.ToolSchema (ParamProperty (..), ParamType (..), ToolDescription (..), ToolName (..))
import System.Agents.Tools.Activation (Activation (..), ToolgroupName)
import System.Agents.Tools.Activation.Session (
    ToolboxSessionState (..),
    extractToolgroups,
    foldSession,
    isToolgroupActive,
    makeActivateTool,
    makeDeactivateTool,
    makeDiscoverTools,
 )
import System.Agents.Tools.ExecuteToolCall (executeLlmToolCall)

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)

{- | Trace type re-exported from System.Agents.OneShot for compatibility.
This module shares the same trace types as OneShot for simplicity.
-}
data Trace
    = ToolRegistrationTrace !ToolRegistration.Trace
    | ToolPortalTrace !ToolPortal.Trace
    deriving (Show)

-- | A nil UUID for creating initial empty session references.
nilUUID :: UUID
nilUUID = UUID.fromWords 0 0 0 0

{- | Wrap an agent to dynamically evaluate and filter active tools based on session history.

This decorator:
1. Takes a TVar containing the available ToolRegistrations
2. Creates an IORef to track the current session state
3. Updates the IORef after each step (like onProgress)
4. Reads from the TVar and filters ToolRegistrations based on session activation state
5. Maps the filtered ToolRegistrations to SystemTools
6. Adds meta tools (meta_activate_tool, meta_deactivate_tool, meta_discover_tools) conditionally:
   - meta_activate_tool only if there are inactive toolgroups
   - meta_deactivate_tool only if there are active toolgroups
   - meta_discover_tools always added when any toolgroups exist

Tools are filtered based on 'meta_activate_tool' and 'meta_deactivate_tool' calls
in the session history. The list of ToolRegistrations is read fresh from the TVar
on each access, allowing runtime changes to the available tools.

Tools with 'OnDemandActivated' activation are only visible when their toolgroup
is active. Tools with 'AlwaysActivated' or no activation are always visible.

Example usage:

@
import System.Agents.Combinators.ProgressiveDisclosure (agentEvaluateActiveTools)

agent <- nodeToAgent store mPath convId tracer loadedApiKeys node
dynamicAgent <- agentEvaluateActiveTools (osNodeTools node) agent
@
-}
agentEvaluateActiveTools :: forall r. Tracer IO Trace -> TVar [ToolRegistration] -> Agent r -> IO (Agent r)
agentEvaluateActiveTools tracer toolsTVar agent = do
    -- Create an IORef to track the current session
    -- Use nil UUIDs for initial empty session
    let emptySessionId = SessionId nilUUID
    let emptyTurnId = TurnId nilUUID
    let emptySession = Session [] emptySessionId Nothing emptyTurnId (Just 1) Nothing
    sessionRef <- newIORef emptySession

    let rTools = filterTools sessionRef toolsTVar
    -- Create the decorated agent
    pure $
        agent
            { step = decorateStep sessionRef agent.step
            , sysTools = fmap (map toolRegistrationToSystemTool) rTools
            , toolCall =
                executeLlmToolCall
                    (contramap ToolRegistrationTrace tracer)
                    rTools
                    (SessionCompat.parseToolCallFromLlmToolCall, SessionCompat.callResultToUserToolResponse)
            }
  where
    -- \| Decorates the step function to update the sessionRef after each step
    decorateStep :: IORef Session -> (Session -> IO (Action r)) -> (Session -> IO (Action r))
    decorateStep sessionRef stepFn = \sess -> do
        -- Update the session reference with the current session
        writeIORef sessionRef sess
        -- Call the original step function
        stepFn sess

    -- \| Filters tools based on the current session activation state
    filterTools :: IORef Session -> TVar [ToolRegistration] -> IO [ToolRegistration]
    filterTools sessionRef tvar = do
        -- Get the current session state
        currentSession <- readIORef sessionRef

        -- Compute the activation state from session history
        let activationState = foldSession currentSession

        -- Read the ToolRegistrations from the TVar
        allToolRegs <- readTVarIO tvar

        -- Extract all unique toolgroups from the registrations (returns a Set)
        let allToolgroups :: Set ToolgroupName
            allToolgroups = extractToolgroups allToolRegs

        -- Determine which toolgroups are active vs inactive
        let activeToolgroups :: Set ToolgroupName
            activeToolgroups = Set.filter (isToolgroupActive activationState) allToolgroups
        let inactiveToolgroups :: Set ToolgroupName
            inactiveToolgroups = allToolgroups Set.\\ activeToolgroups

        -- Build meta tools conditionally based on active/inactive toolgroups
        let metaTools =
                concat
                    [ [makeActivateTool inactiveToolgroups | not (Set.null inactiveToolgroups)]
                    , [makeDeactivateTool activeToolgroups | not (Set.null activeToolgroups)]
                    , [makeDiscoverTools allToolgroups | not (Set.null allToolgroups)]
                    ]

        -- Filter ToolRegistrations based on activation state
        let activeToolRegs = filter (isToolRegActive activationState) allToolRegs

        -- Combine meta tools with active tool registrations
        pure $ (metaTools ++ activeToolRegs)

    -- \| Check if a ToolRegistration is active based on the activation state
    isToolRegActive :: ToolboxSessionState -> ToolRegistration -> Bool
    isToolRegActive activationState toolReg =
        case toolReg.toolActivation of
            Nothing -> True -- No activation control = always visible
            Just AlwaysActivated -> True -- Always activated
            Just (OnDemandActivated toolgroup) ->
                -- Check if the toolgroup is active in the session state
                isToolgroupActive activationState toolgroup

-- | Convert a ToolRegistration to a SystemTool for the Session agent.
toolRegistrationToSystemTool :: ToolRegistration -> SystemTool
toolRegistrationToSystemTool reg =
    let llmTool = reg.declareTool
        toolDefv1 =
            SystemToolDefinitionV1
                { name = llmTool.toolDescriptionName.getToolName
                , llmName = llmTool.toolDescriptionName.getToolName
                , description = llmTool.toolDescriptionText
                , properties = llmTool.toolDescriptionParamProperties
                , raw =
                    Aeson.object
                        [ "type" .= ("function" :: Text)
                        , "function"
                            .= Aeson.object
                                [ "name" .= llmTool.toolDescriptionName.getToolName
                                , "description" .= llmTool.toolDescriptionText
                                , "parameters" .= toolParamsToJson llmTool.toolDescriptionParamProperties
                                ]
                        ]
                }
     in SystemTool $ V1 toolDefv1

{- | Convert tool parameters to JSON schema.

Only properties with 'propertyRequired = True' are included in the 'required' array.
-}
toolParamsToJson :: [ParamProperty] -> Aeson.Value
toolParamsToJson props =
    Aeson.object
        [ "type" .= ("object" :: Text)
        , "properties" .= KeyMap.fromList (map paramPropertyToJson props)
        , "required" .= map propertyKey (filter propertyRequired props)
        , "additionalProperties" .= False
        ]
  where
    paramPropertyToJson :: ParamProperty -> (Aeson.Key, Aeson.Value)
    paramPropertyToJson p = (AesonKey.fromText p.propertyKey, paramTypeToJson p)
    paramTypeToJson :: ParamProperty -> Aeson.Value
    paramTypeToJson p =
        Aeson.object $
            [ "type" .= paramTypeToString p.propertyType
            , "description" .= p.propertyDescription
            ]
                ++ case p.propertyType of
                    EnumParamType values -> ["enum" .= values]
                    _ -> []

    paramTypeToString :: ParamType -> Text
    paramTypeToString NullParamType = "null"
    paramTypeToString StringParamType = "string"
    paramTypeToString BoolParamType = "boolean"
    paramTypeToString NumberParamType = "number"
    paramTypeToString (EnumParamType _) = "string"
    paramTypeToString (OpaqueParamType t) = t
    paramTypeToString (MultipleParamType t) = t
    paramTypeToString (ObjectParamType _) = "object"
