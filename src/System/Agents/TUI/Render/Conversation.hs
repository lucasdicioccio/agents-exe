{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Conversation rendering functions including tree nesting, session view, and turn navigation.
module System.Agents.TUI.Render.Conversation where

import Brick
import Brick.Focus (focusGetCurrent)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.List (listElements, listSelectedAttr, listSelectedElement)
import Control.Lens ((^.))
import Data.List (intersperse)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import System.Agents.Base (ConversationId (..))
import System.Agents.LLMs.OpenAI (TokenUsage (..))
import System.Agents.Session.Base hiding (Agent)
import System.Agents.Session.Signals (calculateTrajectorySignals)
import System.Agents.Session.Types (
    ExecutionSignals (..),
    InteractionSignals (..),
    StepByteUsage (..),
    TrajectorySignals (..),
    sessionTotalBytes,
 )
import System.Agents.TUI.Render.Attributes
import System.Agents.TUI.Render.Utils (borderWithFocus)
import System.Agents.TUI.Types

-- | Tree structure for nested conversations.
data ConversationTree = ConversationTree
    { treeConversation :: Conversation
    , treeChildren :: [ConversationTree]
    }

{- | Build a forest of conversation trees from a flat list.

This function handles "orphaned" conversations - children whose parents
are not in the list. This can happen due to async event ordering where
a child subcall event arrives before its parent is fully registered.
Such orphaned conversations are treated as temporary roots to ensure
they remain visible in the TUI.
-}
buildConversationForest :: [Conversation] -> [ConversationTree]
buildConversationForest convs =
    let
        -- Build a set of all conversation IDs for quick lookup
        convIds = Set.fromList $ map conversationId convs

        -- Find root conversations:
        -- 1. Conversations with no parent (parentId == Nothing), OR
        -- 2. "Orphaned" conversations whose parent is not in the list
        --    (this handles race conditions in async subcall creation)
        isRoot c =
            case conversationParentId c of
                Nothing -> True
                Just parentId -> not (Set.member parentId convIds)

        roots = filter isRoot convs

        -- Build tree recursively
        buildTree conv =
            ConversationTree
                { treeConversation = conv
                , treeChildren = map buildTree (findChildren conv)
                }

        -- Find all children of a given parent conversation
        findChildren parent =
            filter (\c -> conversationParentId c == Just (conversationId parent)) convs
     in
        map buildTree roots

-- | Sort conversations to ensure proper nesting order.
sortConversationsForNesting :: [Conversation] -> [Conversation]
sortConversationsForNesting convs =
    let forest = buildConversationForest convs
        -- Flatten maintaining tree order (pre-order traversal)
        go [] = []
        go (ConversationTree conv children : rest) =
            conv : go children ++ go rest
     in go forest

{- | Make prefix for a conversation at a specific depth with tree branches.
The Bool list indicates for each ancestor level (from root to immediate parent)
whether that ancestor is a last child (True) or not (False).
-}
makePrefix :: [Bool] -> Bool -> Text
makePrefix ancestorIsLasts isLast
    | null ancestorIsLasts = if isLast then "└─" else "├─"
    | otherwise =
        let
            -- Build the continuation part from ancestors
            -- For each ancestor: if it was the last child, use spaces ("  "),
            -- otherwise use a vertical bar ("│ ") to show continuation
            continuation = mconcat $ map (\isLastAncestor -> if isLastAncestor then "  " else "│ ") ancestorIsLasts
         in
            continuation <> (if isLast then "└─" else "├─")

-- | Render the conversation list with nesting.
render_conversationList :: TuiState -> Widget N
render_conversationList st =
    let convs = Vector.toList (listElements (st ^. tuiUI . conversationList))
        -- Create a custom rendering of the nested list
        hasFocus = focusGetCurrent (st ^. tuiUI . uiFocusRing) == Just ConversationListWidget
        selectedId = case listSelectedElement (st ^. tuiUI . conversationList) of
            Just (_, conv) -> Just (conversationId conv)
            Nothing -> Nothing
        forest = buildConversationForest convs
     in borderWithFocus
            st
            ConversationListWidget
            "Conversations"
            ( viewport ConversationListWidget Both $
                vBox $
                    renderConversationForest st selectedId hasFocus forest
            )

renderConversationForest :: TuiState -> Maybe ConversationId -> Bool -> [ConversationTree] -> [Widget N]
renderConversationForest st selectedId hasFocus trees =
    concatMap (\(idx, tree) -> renderTreeNode st selectedId hasFocus [] (idx == length trees - 1) tree) (zip [0 ..] trees)

{- | Render a tree node recursively.
The Bool list tracks for each ancestor whether it is a last child.
-}
renderTreeNode :: TuiState -> Maybe ConversationId -> Bool -> [Bool] -> Bool -> ConversationTree -> [Widget N]
renderTreeNode st selectedId hasFocus ancestorIsLasts isLast (ConversationTree conv children) =
    let isSelected = selectedId == Just (conversationId conv)
        nodeWidget = renderNestedConversationItem st hasFocus isSelected ancestorIsLasts isLast conv
        childWidgets = concatMap (\(idx, child) -> renderTreeNode st selectedId hasFocus (ancestorIsLasts ++ [isLast]) (idx == length children - 1) child) (zip [0 ..] children)
     in nodeWidget : childWidgets

-- | Render a nested conversation item with tree branches.
renderNestedConversationItem :: TuiState -> Bool -> Bool -> [Bool] -> Bool -> Conversation -> Widget N
renderNestedConversationItem st hasFocus isSelected ancestorIsLasts isLast conv =
    let indicator = case conversationStatus conv of
            ConversationStatus_Active -> "⟳ "
            ConversationStatus_WaitingForInput ->
                if isUnread then "● " else "○ "
            ConversationStatus_Paused -> "⏸ "
        -- Tree branch prefix based on ancestor status
        branchPrefix = makePrefix ancestorIsLasts isLast
        -- Selection marker
        selectionMarker = if isSelected && hasFocus then "▶ " else "  "
        baseText = branchPrefix <> indicator <> Text.take 20 (conversationName conv)
        -- Add turn count
        turnCount = case conversationSession conv of
            Nothing -> 0
            Just session -> length session.turns
        turnSuffix = if turnCount > 0 then " (" <> Text.pack (show turnCount) <> ")" else ""
        -- Add attachment count
        attachmentCount = getAttachmentCount st conv
        attachmentSuffix = if attachmentCount > 0 then " [📎" <> Text.pack (show attachmentCount) <> "]" else ""
        -- Add queued message count
        queueCount = getQueuedMessageCount st conv
        queueSuffix = if queueCount > 0 then " [" <> Text.pack (show queueCount) <> " queued]" else ""
        fullText = baseText <> turnSuffix <> attachmentSuffix <> queueSuffix
        -- Determine the appropriate attribute
        attr =
            if isSelected && hasFocus
                then subcallSelectedAttr
                else
                    if isSelected
                        then listSelectedAttr
                        else
                            if null ancestorIsLasts
                                then rootConversationAttr
                                else subcallAttr
     in withAttr attr $ txt $ selectionMarker <> fullText
  where
    isUnread = Set.member (conversationId conv) (st ^. tuiUI . unreadConversations)

-- | Get the number of queued messages for a conversation.
getQueuedMessageCount :: TuiState -> Conversation -> Int
getQueuedMessageCount st conv =
    let buffered = st ^. tuiUI . uiBufferedMessages
     in case Map.lookup (conversationId conv) buffered of
            Nothing -> 0
            Just msgs -> length msgs

-- | Get the number of attachments for a conversation.
getAttachmentCount :: TuiState -> Conversation -> Int
getAttachmentCount st conv =
    let attachments = st ^. tuiUI . attachedFiles
     in case Map.lookup (conversationId conv) attachments of
            Nothing -> 0
            Just atts -> length atts

-- | Render the conversation history view.
render_conversationView :: TuiState -> Widget N
render_conversationView st =
    content
  where
    content =
        case listSelectedElement (st ^. tuiUI . conversationList) of
            Nothing -> txt "No conversation selected"
            Just (_, conv) ->
                let mNavState = st ^. tuiUI . turnNavigation
                 in render_session
                        st
                        ConversationViewWidget
                        (conversationSession conv)
                        mNavState

-- | Render the session history view.
render_sessionView :: TuiState -> Widget N
render_sessionView st =
    content
  where
    content =
        case listSelectedElement (st ^. tuiUI . sessionList) of
            Nothing -> txt "No session selected"
            Just (_, session) ->
                let mNavState = st ^. tuiUI . turnNavigation
                 in render_session st SessionViewWidget (Just session) mNavState

-- | Render a session's turns.
render_session :: TuiState -> WidgetName -> Maybe Session -> Maybe TurnNavigationState -> Widget N
render_session _ _ Nothing _ =
    vBox [txt "session not started yet"]
render_session st w (Just session) mNavState =
    case mNavState of
        Nothing ->
            borderWithFocus st w "Session" $
                viewport w Both $
                    vBox $
                        [render_session_usage session]
                            ++ map render_turn (Prelude.reverse (zip [(0 :: Int) ..] $ Prelude.reverse session.turns))
        Just navState ->
            render_turn_navigation session navState

-- | Render session in turn navigation mode.
render_turn_navigation :: Session -> TurnNavigationState -> Widget N
render_turn_navigation session navState =
    let selectedIdx = navState ^. navSelectedTurnIndex
        totalTurns = navState ^. navTotalTurns
        headerText = "Turn Navigation (" <> Text.pack (show (selectedIdx + 1)) <> "/" <> Text.pack (show totalTurns) <> ") [Enter:exit F:fork]"
        turnsWithIndices = zip [0 ..] session.turns
        shownTurns = drop selectedIdx turnsWithIndices
     in borderWithLabel (txt headerText) $
            viewport TurnNavigationWidget Both $
                vBox $
                    [ txt "Up/Down: navigate  Enter: exit  F: fork from here"
                    , txt ""
                    ]
                        ++ map (render_navigable_turn selectedIdx) shownTurns

-- | Render a single turn with selection indicator.
render_navigable_turn :: Int -> (Int, Turn) -> Widget N
render_navigable_turn selectedIdx (idx, turn) =
    let isSelected = idx == selectedIdx
        selectionMarker = if isSelected then "▶ " else "  "
        turnWidget = render_turn (idx, turn)
     in if isSelected
            then withAttr selectedTurnAttr $ hBox [txt selectionMarker, turnWidget]
            else hBox [txt selectionMarker, turnWidget]

-- | Render total session usage (tokens if available, else bytes), turn count, and signal metrics.
render_session_usage :: Session -> Widget N
render_session_usage session =
    let turnCount = length session.turns
        mTokenStats = aggregateSessionTokenUsage session
        mSignals = Just (calculateTrajectorySignals session)
     in if turnCount == 0
            then emptyWidget
            else
                vBox
                    [ case mTokenStats of
                        Just tokenStats ->
                            withAttr tokenUsageAttr $
                                txt $
                                    "Session total: " <> formatTokenStats tokenStats <> "  (" <> Text.pack (show turnCount) <> " turns)  "
                        Nothing ->
                            let totalBytes = sessionTotalBytes session
                             in if totalBytes == 0
                                    then emptyWidget
                                    else
                                        withAttr byteUsageAttr $
                                            txt $
                                                "Session total: " <> formatBytes totalBytes <> "  (" <> Text.pack (show turnCount) <> " turns)  "
                    , renderSignalSummary mSignals
                    ]

-- | Render compact signal summary for TUI header.
renderSignalSummary :: Maybe TrajectorySignals -> Widget N
renderSignalSummary Nothing = emptyWidget
renderSignalSummary (Just signals) =
    let score = trajInformativenessScore signals
        is = trajInteraction signals
        es = trajExecution signals
        indicators =
            concat
                [ if sigDisengagementDetected is then ["⚠️ disengage"] else []
                , if sigLoopDetected es then ["🔴 loop"] else []
                , if sigFailureCount es > 0 then ["⚡" <> Text.pack (show (sigFailureCount es)) <> " fails"] else []
                , if sigMisalignmentCount is > 0 then ["🔄" <> Text.pack (show (sigMisalignmentCount is)) <> " misalign"] else []
                , if sigStagnationCount is > 0 then ["⏸️ " <> Text.pack (show (sigStagnationCount is)) <> " stagn"] else []
                ]
        scoreText = "Signals: " <> Text.pack (show score) <> "/100"
        indicatorsText = if null indicators then "" else " | " <> Text.intercalate " | " indicators
     in withAttr signalMetricsAttr $ txt $ scoreText <> indicatorsText

-- | Token usage statistics aggregated across a session.
data TokenUsageStats = TokenUsageStats
    { statPromptTokens :: Int
    , statCompletionTokens :: Int
    , statTotalTokens :: Int
    , statCachedTokens :: Maybe Int
    , statThinkingTokens :: Maybe Int
    }

-- | Aggregate token usage across all turns in a session.
aggregateSessionTokenUsage :: Session -> Maybe TokenUsageStats
aggregateSessionTokenUsage session =
    let allTokenUsages = collectTokenUsages session.turns
     in if null allTokenUsages
            then Nothing
            else Just $ aggregateTokenUsages allTokenUsages
  where
    collectTokenUsages :: [Turn] -> [TokenUsage]
    collectTokenUsages turnList =
        [ usage
        | turn <- turnList
        , usage <- extractUsageFromTurn turn
        ]

extractUsageFromTurn :: Turn -> [TokenUsage]
extractUsageFromTurn turn =
    let mUsage = case turn of
            UserTurn _ mStepUsage -> mStepUsage
            LlmTurn _ mStepUsage -> mStepUsage
            PartialUserTurn _ mStepUsage -> mStepUsage
     in case mUsage of
            Just stepUsage -> maybeToList (stepTokenUsage stepUsage)
            Nothing -> []
  where
    maybeToList :: Maybe a -> [a]
    maybeToList Nothing = []
    maybeToList (Just x) = [x]

aggregateTokenUsages :: [TokenUsage] -> TokenUsageStats
aggregateTokenUsages usages =
    TokenUsageStats
        { statPromptTokens = sum $ map tokenPromptTokens usages
        , statCompletionTokens = sum $ map tokenCompletionTokens usages
        , statTotalTokens = sum $ map tokenTotalTokens usages
        , statCachedTokens = sumMaybe $ map tokenCachedTokens usages
        , statThinkingTokens = sumMaybe $ map tokenThinkingTokens usages
        }

sumMaybe :: [Maybe Int] -> Maybe Int
sumMaybe ms =
    let vs = [v | Just v <- ms]
     in if null vs then Nothing else Just (sum vs)

-- | Format token stats for display.
formatTokenStats :: TokenUsageStats -> Text
formatTokenStats stats =
    let baseText = formatTokenCount (statTotalTokens stats) <> " tokens"
        details =
            [ "in: " <> formatTokenCount (statPromptTokens stats)
            , "out: " <> formatTokenCount (statCompletionTokens stats)
            ]
        withCached = case statCachedTokens stats of
            Just n | n > 0 -> details ++ ["cached: " <> formatTokenCount n]
            _ -> details
        withThinking = case statThinkingTokens stats of
            Just n | n > 0 -> withCached ++ ["think: " <> formatTokenCount n]
            _ -> withCached
     in baseText <> " (" <> Text.intercalate ", " withThinking <> ")"

-- | Format token count with thousands separator.
formatTokenCount :: Int -> Text
formatTokenCount n =
    let numText = Text.pack (show n)
     in addThousandSeparators numText

-- | Add thousand separators to a numeric text.
addThousandSeparators :: Text -> Text
addThousandSeparators numText =
    let digits = Text.unpack numText
        grouped = reverse $ group3 (reverse digits)
     in Text.pack $ concat (intersperse "," grouped)
  where
    group3 :: String -> [String]
    group3 [] = []
    group3 s = take 3 s : group3 (drop 3 s)

-- | Format bytes in human-readable form.
formatBytes :: Int -> Text
formatBytes n
    | n >= 1024 * 1024 * 1024 = Text.pack (show (n `div` (1024 * 1024 * 1024))) <> " GiB"
    | n >= 1024 * 1024 = Text.pack (show (n `div` (1024 * 1024))) <> " MiB"
    | n >= 1024 = Text.pack (show (n `div` 1024)) <> " KiB"
    | otherwise = Text.pack (show n) <> " B"

-- | Render a single turn with usage info (tokens or bytes fallback).
render_turn :: (Int, Turn) -> Widget N
render_turn (_k, turn) =
    case turn of
        UserTurn userTurn mUsage ->
            withAttr userMessageAttr $
                vBox
                    [ txt $
                        "> " <> case userTurn.userQuery of
                            Just (UserQuery q _) -> q
                            Nothing -> "(no query)"
                    , render_usage mUsage
                    , txt " "
                    ]
        LlmTurn llmTurn mUsage ->
            withAttr llmMessageAttr $
                vBox
                    [ txt $
                        "AI: " <> case llmTurn.llmResponse.responseText of
                            Just txt0 -> txt0
                            Nothing -> "(no response)"
                    , case llmTurn.llmResponse.responseThinking of
                        Just thinking -> withAttr thinkingAttr $ txt $ "Thinking: " <> thinking
                        Nothing -> emptyWidget
                    , render_usage mUsage
                    , txt " "
                    ]
        PartialUserTurn partial mUsage ->
            withAttr userMessageAttr $
                vBox
                    [ txt $
                        "[Partial] > " <> case partial.pUserQuery of
                            Just (UserQuery q _) -> q
                            Nothing -> "(no query)"
                    , render_usage mUsage
                    , txt " "
                    ]

-- | Render usage information for a turn (tokens or bytes).
render_usage :: Maybe StepByteUsage -> Widget N
render_usage Nothing = emptyWidget
render_usage (Just usage) =
    case usage.stepTokenUsage of
        Just tokens ->
            withAttr byteUsageAttr $
                txt $
                    "  [Tokens: " <> Text.pack (show $ tokenTotalTokens tokens) <> "]"
        Nothing ->
            if usage.stepTotalBytes > 0
                then
                    withAttr byteUsageAttr $
                        txt $
                            "  [" <> formatBytes usage.stepTotalBytes <> "]"
                else emptyWidget

