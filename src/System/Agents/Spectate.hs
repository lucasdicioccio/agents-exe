{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The model behind @agents-exe spectate@: a read-only view of what a
runner is doing, folded from its event stream.

A spectator is one more client of a runner ("System.Agents.Host.Client"):
it subscribes to 'System.Agents.Protocol.Event's and sends nothing. This
module is the pure half -- 'applyEvent' folds one event into a 'Spectate',
and the @render*@ functions turn it into lines of text -- so it has no
terminal dependency and is tested without one. The Brick program that
paints it lives in the @agents-tui@ library.

Three things are tracked, for someone watching a long run:

* the tree of sessions and sub-agent calls ('treeRows');
* tool calls, running and recently finished ('toolRows');
* the text each session's model produced ('sessionText').

Events carry a sequence number but no timestamp, so every duration shown
is measured by the spectator, from the time it received the event: the
caller passes that time to 'applyEvent'.
-}
module System.Agents.Spectate (
    -- * Model
    Spectate (..),
    Node (..),
    NodeState (..),
    ToolRow (..),
    ToolOutcome (..),
    emptySpectate,
    seedSessions,
    applyEvent,

    -- * Limits
    maxTextChars,
    maxFinishedTools,
    maxQuietRoots,

    -- * Views
    TreeRow (..),
    treeRows,
    toolRows,
    sessionText,
    followTarget,
    isActive,
    nodeLabel,

    -- * Text rendering
    renderTreeRow,
    renderToolRow,
    formatElapsed,
    shortSessionId,
    wrapText,
) where

import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.Ord (Down (..))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime)
import qualified Data.UUID as UUID

import System.Agents.OS.Events (ToolCallActivity (..))
import qualified System.Agents.OS.Events as Activity
import System.Agents.Protocol (Event (..), EventBody (..), EventSeq)
import System.Agents.Session.Base (
    LlmResponse (..),
    LlmTurnContent (..),
    PartialUserTurnContent (..),
    SessionId (..),
    SessionStatus (..),
    ToolCallId,
    Turn (..),
    UserQuery (..),
    UserTurnContent (..),
    sessionStatusText,
 )
import System.Agents.SessionStore (SessionMeta (..))
import System.Agents.TUI.ToolCallActivity (summarizeProgress)

-------------------------------------------------------------------------------
-- Model
-------------------------------------------------------------------------------

-- | What a session, or an in-tool sub-agent call, is doing.
data NodeState
    = -- | A run is active.
      NodeRunning
    | -- | No run is active; the status the session stopped in, or was
      -- last stored with.
      NodeStopped SessionStatus
    | -- | A sub-agent call returned to its caller.
      NodeReturned
    | -- | The run, or the sub-agent call, failed.
      NodeFailed Text
    deriving (Show, Eq)

-- | One session, or one in-tool sub-agent call.
data Node = Node
    { nSession :: SessionId
    , nAgent :: Maybe Text
    -- ^ The agent's slug, when an event said it.
    , nParent :: Maybe SessionId
    , nState :: NodeState
    , nSince :: UTCTime
    -- ^ When the spectator saw 'nState' begin.
    , nOrder :: Int
    -- ^ Order of first appearance, which the tree is sorted by.
    , nText :: Text
    -- ^ The tail of what the model wrote, and of the user queries it answered.
    , nStreaming :: Bool
    -- ^ @text.delta@ pieces were appended since the last stored LLM turn.
    }
    deriving (Show, Eq)

data ToolOutcome
    = ToolRunning
    | ToolSucceeded
    | ToolFailed (Maybe Text)
    | ToolCancelled
    deriving (Show, Eq)

-- | One tool call.
data ToolRow = ToolRow
    { trCall :: ToolCallId
    , trSession :: SessionId
    , trName :: Text
    , trOutcome :: ToolOutcome
    , trStartedAt :: UTCTime
    -- ^ When the spectator first heard of the call.
    , trEndedAt :: Maybe UTCTime
    , trProgress :: Maybe Text
    -- ^ The latest progress a background call reported, summarized.
    }
    deriving (Show, Eq)

data Spectate = Spectate
    { spNodes :: Map SessionId Node
    , spTools :: Map ToolCallId ToolRow
    , spLastText :: Maybe SessionId
    -- ^ The session that produced text most recently.
    , spLastSeq :: Maybe EventSeq
    , spEvents :: Int
    -- ^ Events folded so far.
    , spNextOrder :: Int
    }
    deriving (Show, Eq)

emptySpectate :: Spectate
emptySpectate = Spectate Map.empty Map.empty Nothing Nothing 0 0

-- | Characters of text kept per session.
maxTextChars :: Int
maxTextChars = 20000

-- | Finished tool calls kept, most recent first.
maxFinishedTools :: Int
maxFinishedTools = 100

-- | Root sessions with nothing active under them that are kept, most
-- recently changed first.
maxQuietRoots :: Int
maxQuietRoots = 50

-------------------------------------------------------------------------------
-- Folding events
-------------------------------------------------------------------------------

{- | Sessions already known when the spectator attaches (the events that
announced them are gone), so that a run in progress shows its tree at once.
-}
seedSessions :: UTCTime -> [SessionMeta] -> Spectate -> Spectate
seedSessions now metas sp = foldl (flip (fromMeta now)) sp (sortOn (.smCreatedAt) metas)

-- | Fold one event, received at the given time.
applyEvent :: UTCTime -> Event -> Spectate -> Spectate
applyEvent now ev sp0 = prune (body sp)
  where
    sp = sp0{spLastSeq = Just ev.evSeq, spEvents = sp0.spEvents + 1}
    onSession f = maybe id f ev.evSession
    body = case ev.evBody of
        SessionCreated meta -> fromMeta now meta
        SessionUpdated meta headTurn -> maybe id (headTurnText meta.smSessionId) headTurn . fromMeta now meta
        SessionDeleted sid -> deleteSession sid
        RunStarted _ -> onSession (setState now NodeRunning)
        RunStopped status -> onSession (setState now (NodeStopped status))
        SessionFailed msg -> onSession (setState now (NodeFailed msg))
        TextDelta piece -> onSession (appendDelta now piece)
        SubcallStarted parent child slug _depth -> subcallStarted now parent child slug
        SubcallCompleted child _result -> subcallEnded now child NodeReturned
        SubcallFailed child msg -> subcallEnded now child (NodeFailed msg)
        ToolCallStarted callId name -> onSession (toolStarted now callId name)
        ToolCallCompleted callId name ok ->
            onSession (toolEnded now callId name (if ok then ToolSucceeded else ToolFailed Nothing))
        ToolCallProgressed activity -> toolActivity now activity
        -- Deferred calls and hook failures have panels of their own, later.
        CallsDeferred _ -> id
        HookFailed _ -> id

-- | Make sure a session has a node, and change it.
withNode :: UTCTime -> SessionId -> (Node -> Node) -> Spectate -> Spectate
withNode now sid f sp = case Map.lookup sid sp.spNodes of
    Just node -> sp{spNodes = Map.insert sid (f node) sp.spNodes}
    Nothing ->
        let node =
                Node
                    { nSession = sid
                    , nAgent = Nothing
                    , nParent = Nothing
                    , nState = NodeStopped StatusReady
                    , nSince = now
                    , nOrder = sp.spNextOrder
                    , nText = ""
                    , nStreaming = False
                    }
         in sp{spNodes = Map.insert sid (f node) sp.spNodes, spNextOrder = sp.spNextOrder + 1}

setState :: UTCTime -> NodeState -> SessionId -> Spectate -> Spectate
setState now st sid = withNode now sid (stateTo now st)

-- | Change a node's state, keeping 'nSince' when the state is unchanged.
stateTo :: UTCTime -> NodeState -> Node -> Node
stateTo now st node
    | node.nState == st = node
    | otherwise = node{nState = st, nSince = now}

{- | What stored metadata says about a session. @session.updated@ arrives on
every stored step of a run, with the status stored at that step, so it only
moves the state when it brings news: a session not seen before, or one
that failed.
-}
fromMeta :: UTCTime -> SessionMeta -> Spectate -> Spectate
fromMeta now meta sp = withNode now meta.smSessionId update sp
  where
    known = Map.member meta.smSessionId sp.spNodes
    update node =
        restate
            node
                { nAgent = maybe node.nAgent Just meta.smAgent
                , nParent = maybe node.nParent Just meta.smParent
                }
    restate node = case meta.smStatus of
        StatusFailed -> stateTo now (NodeFailed (fromMaybe "failed" meta.smStatusDetail)) node
        StatusRunning -> stateTo now NodeRunning node
        status
            | not known -> stateTo now (NodeStopped status) node
            | otherwise -> node

deleteSession :: SessionId -> Spectate -> Spectate
deleteSession sid sp =
    sp
        { spNodes = Map.delete sid sp.spNodes
        , spTools = Map.filter ((/= sid) . (.trSession)) sp.spTools
        , spLastText = if sp.spLastText == Just sid then Nothing else sp.spLastText
        }

subcallStarted :: UTCTime -> SessionId -> SessionId -> Text -> Spectate -> Spectate
subcallStarted now parent child slug sp =
    withNode now child update (withNode now parent id sp)
  where
    fresh = not (Map.member child sp.spNodes)
    update node =
        (if fresh then stateTo now NodeRunning else id)
            node{nAgent = Just slug, nParent = Just parent}

{- | A sub-agent call ended. When the child is a session of its own, its
@run.stopped@\/@session.failed@ already said so, and only a still-running
node (an in-tool call, which has no such events) changes.
-}
subcallEnded :: UTCTime -> SessionId -> NodeState -> Spectate -> Spectate
subcallEnded now child st = withNode now child $ \node -> case (node.nState, st) of
    (NodeRunning, _) -> stateTo now st node
    (NodeFailed _, _) -> node
    (_, NodeFailed _) -> stateTo now st node
    _ -> node

appendDelta :: UTCTime -> Text -> SessionId -> Spectate -> Spectate
appendDelta now piece sid sp =
    (withNode now sid (\node -> node{nText = keepTail (node.nText <> piece), nStreaming = True}) sp)
        { spLastText = Just sid
        }

{- | The head turn of a stored version. A user query is quoted; an LLM
answer is appended unless @text.delta@ already streamed it (a host that
does not stream tokens only ever shows text here).
-}
headTurnText :: SessionId -> Turn -> Spectate -> Spectate
headTurnText sid turn sp = case turn of
    LlmTurn content _ -> case Map.lookup sid sp.spNodes of
        Nothing -> sp
        Just node ->
            let answer = fromMaybe "" content.llmResponse.responseText
                text
                    | node.nStreaming = paragraph node.nText
                    | Text.null (Text.strip answer) = node.nText
                    | otherwise = paragraph (node.nText <> answer)
             in sp
                    { spNodes = Map.insert sid node{nText = keepTail text, nStreaming = False} sp.spNodes
                    , spLastText = if text /= node.nText then Just sid else sp.spLastText
                    }
    UserTurn content _ -> quote content.userQuery
    PartialUserTurn content _ -> quote content.pUserQuery
  where
    quote :: Maybe UserQuery -> Spectate
    quote Nothing = sp
    quote (Just query) = case Map.lookup sid sp.spNodes of
        Nothing -> sp
        Just node ->
            let line = "> " <> Text.intercalate "\n> " (Text.lines query.queryText)
             in if Text.null (Text.strip query.queryText) || (line <> "\n\n") `Text.isSuffixOf` node.nText
                    then sp
                    else sp{spNodes = Map.insert sid node{nText = keepTail (paragraph (node.nText <> line))} sp.spNodes}

-- | End a paragraph: exactly one blank line after non-empty text.
paragraph :: Text -> Text
paragraph t
    | Text.null stripped = ""
    | otherwise = stripped <> "\n\n"
  where
    stripped = Text.stripEnd t

keepTail :: Text -> Text
keepTail = Text.takeEnd maxTextChars

toolStarted :: UTCTime -> ToolCallId -> Text -> SessionId -> Spectate -> Spectate
toolStarted now callId name sid sp
    | Map.member callId sp.spTools = sp
    | otherwise =
        (withNode now sid id sp)
            { spTools = Map.insert callId (ToolRow callId sid name ToolRunning now Nothing Nothing) sp.spTools
            }

-- | A call reached a final state; the first final state reported wins.
toolEnded :: UTCTime -> ToolCallId -> Text -> ToolOutcome -> SessionId -> Spectate -> Spectate
toolEnded now callId name outcome sid sp0 =
    sp{spTools = Map.adjust end callId sp.spTools}
  where
    sp = toolStarted now callId name sid sp0
    end row
        | isJust row.trEndedAt = row
        | otherwise = row{trOutcome = outcome, trEndedAt = Just now}

toolActivity :: UTCTime -> ToolCallActivity -> Spectate -> Spectate
toolActivity now act = case act.tcaPhase of
    Activity.ToolCallStarted -> toolStarted now callId name sid
    Activity.ToolCallProgressed payload -> \sp ->
        let started = toolStarted now callId name sid sp
         in started{spTools = Map.adjust (\row -> row{trProgress = Just (summarizeProgress payload)}) callId started.spTools}
    Activity.ToolCallCompleted -> toolEnded now callId name ToolSucceeded sid
    Activity.ToolCallFailed err -> toolEnded now callId name (ToolFailed (Just err)) sid
    Activity.ToolCallCancelled -> toolEnded now callId name ToolCancelled sid
  where
    callId = act.tcaToolCallId
    name = act.tcaToolName
    sid = act.tcaSessionId

{- | Keep the model bounded over a long watch: drop the oldest finished
tool calls, and the oldest root sessions that have nothing active left
under them, with their subtree.
-}
prune :: Spectate -> Spectate
prune = pruneRoots . pruneTools

pruneTools :: Spectate -> Spectate
pruneTools sp
    | length finished <= maxFinishedTools = sp
    | otherwise = sp{spTools = foldr (Map.delete . (.trCall)) sp.spTools (drop maxFinishedTools finished)}
  where
    finished = sortOn (Down . (.trEndedAt)) (filter (isJust . (.trEndedAt)) (Map.elems sp.spTools))

pruneRoots :: Spectate -> Spectate
pruneRoots sp
    | length quiet <= maxQuietRoots = sp
    | otherwise =
        sp
            { spNodes = Map.withoutKeys sp.spNodes dropped
            , spTools = Map.filter ((`Set.notMember` dropped) . (.trSession)) sp.spTools
            , spLastText = if maybe False (`Set.member` dropped) sp.spLastText then Nothing else sp.spLastText
            }
  where
    children = childrenOf sp
    subtree node = node : concatMap subtree (Map.findWithDefault [] node.nSession children)
    busy = Set.fromList (map (.trSession) (filter (isNothing . (.trEndedAt)) (Map.elems sp.spTools)))
    quietTree root = [members | let members = subtree root, not (any (\n -> isActive n || Set.member n.nSession busy) members)]
    quiet =
        sortOn
            (Down . maximum . map (.nSince))
            (concatMap quietTree (rootsOf sp))
    dropped = Set.fromList (map (.nSession) (concat (drop maxQuietRoots quiet)))

-------------------------------------------------------------------------------
-- Views
-------------------------------------------------------------------------------

isActive :: Node -> Bool
isActive node = node.nState == NodeRunning

-- | Nodes whose parent is unknown (or gone), in order of appearance.
rootsOf :: Spectate -> [Node]
rootsOf sp =
    sortOn (.nOrder) $
        filter (\n -> maybe True (`Map.notMember` sp.spNodes) n.nParent) (Map.elems sp.spNodes)

childrenOf :: Spectate -> Map SessionId [Node]
childrenOf sp =
    Map.map (sortOn (.nOrder)) $
        Map.fromListWith (<>) [(parent, [n]) | n <- Map.elems sp.spNodes, Just parent <- [n.nParent]]

-- | One line of the tree: a node and how deep it sits under its root.
data TreeRow = TreeRow
    { rowDepth :: Int
    , rowNode :: Node
    }
    deriving (Show, Eq)

-- | The tree, flattened depth first; each node appears once.
treeRows :: Spectate -> [TreeRow]
treeRows sp = concatMap (go 0) (rootsOf sp)
  where
    children = childrenOf sp
    go depth node =
        TreeRow depth node : concatMap (go (depth + 1)) (Map.findWithDefault [] node.nSession children)

-- | Running calls, oldest first, then finished ones, most recent first.
toolRows :: Spectate -> [ToolRow]
toolRows sp = sortOn (.trStartedAt) running <> sortOn (Down . (.trEndedAt)) finished
  where
    rows = Map.elems sp.spTools
    running = filter (isNothing . (.trEndedAt)) rows
    finished = filter (isJust . (.trEndedAt)) rows

sessionText :: SessionId -> Spectate -> Text
sessionText sid sp = maybe "" (.nText) (Map.lookup sid sp.spNodes)

{- | The session a spectator who chose none looks at: the one that wrote
text last, else the running one that appeared last, else the last one.
-}
followTarget :: Spectate -> Maybe SessionId
followTarget sp = case sp.spLastText of
    Just sid | Map.member sid sp.spNodes -> Just sid
    _ -> case sortOn (Down . (.nOrder)) (Map.elems sp.spNodes) of
        [] -> Nothing
        nodes@(latest : _) -> Just (maybe latest id (firstJust isActive nodes)).nSession
  where
    firstJust p = foldr (\n acc -> if p n then Just n else acc) Nothing

-- | The agent's slug, or a short session id when no event named it.
nodeLabel :: Node -> Text
nodeLabel node = fromMaybe ("session " <> shortSessionId node.nSession) node.nAgent

-------------------------------------------------------------------------------
-- Text rendering
-------------------------------------------------------------------------------

-- | The first eight characters of a session id.
shortSessionId :: SessionId -> Text
shortSessionId (SessionId uuid) = Text.take 8 (UUID.toText uuid)

-- | @42s@, @3m07s@, @2h05m@.
formatElapsed :: NominalDiffTime -> Text
formatElapsed dt
    | secs < 60 = showT secs <> "s"
    | secs < 3600 = showT (secs `div` 60) <> "m" <> pad2 (secs `mod` 60) <> "s"
    | otherwise = showT (secs `div` 3600) <> "h" <> pad2 ((secs `mod` 3600) `div` 60) <> "m"
  where
    secs = max 0 (floor dt) :: Int
    showT = Text.pack . show
    pad2 n = Text.justifyRight 2 '0' (showT n)

-- | @  * helper-slug  1a2b3c4d  running 12s@, indented by depth.
renderTreeRow :: UTCTime -> TreeRow -> Text
renderTreeRow now row =
    Text.replicate (2 * row.rowDepth) " "
        <> marker
        <> " "
        <> nodeLabel node
        <> (if isJust node.nAgent then "  " <> shortSessionId node.nSession else "")
        <> "  "
        <> state
  where
    node = row.rowNode
    elapsed = formatElapsed (diffUTCTime now node.nSince)
    (marker, state) = case node.nState of
        NodeRunning -> ("*", "running " <> elapsed)
        NodeStopped status -> ("-", sessionStatusText status <> " " <> elapsed)
        NodeReturned -> ("-", "returned " <> elapsed)
        NodeFailed msg -> ("!", "failed " <> elapsed <> ": " <> firstLine msg)

-- | @* bash  helper-slug  12s  compiling...@
renderToolRow :: UTCTime -> Spectate -> ToolRow -> Text
renderToolRow now sp row =
    Text.intercalate "  " $
        [marker <> " " <> row.trName, owner, timing]
            <> mapMaybe id [detail]
  where
    owner = maybe (shortSessionId row.trSession) nodeLabel (Map.lookup row.trSession sp.spNodes)
    took = formatElapsed (diffUTCTime (fromMaybe now row.trEndedAt) row.trStartedAt)
    (marker, timing, detail) = case row.trOutcome of
        ToolRunning -> ("*", took, row.trProgress)
        ToolSucceeded -> ("-", "ok in " <> took, Nothing)
        ToolFailed err -> ("!", "failed in " <> took, firstLine <$> err)
        ToolCancelled -> ("-", "cancelled after " <> took, Nothing)

firstLine :: Text -> Text
firstLine = Text.strip . Text.takeWhile (/= '\n') . Text.stripStart

{- | Break text into lines no wider than the given width, at spaces when a
line has one; newlines are kept. A width under one is treated as one.
-}
wrapText :: Int -> Text -> [Text]
wrapText width = concatMap wrapLine . Text.splitOn "\n"
  where
    w = max 1 width
    wrapLine line
        | Text.length line <= w = [line]
        | otherwise =
            let (candidate, rest) = Text.splitAt w line
                (before, lastWord) = Text.breakOnEnd " " candidate
             in if Text.null (Text.strip before) || " " `Text.isPrefixOf` rest
                    then Text.stripEnd candidate : wrapLine (Text.stripStart rest)
                    else Text.stripEnd before : wrapLine (lastWord <> rest)
