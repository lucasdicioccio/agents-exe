{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The layout of the @agents-exe spectate@ screen: which panels show,
where, how large, and how often the screen refreshes.

A layout is columns, left to right, each a stack of panels, top to
bottom. It has one textual form, which is what @--panels@ takes, what the
help overlay shows, and what a saved layout file holds:

> tree:60+tools:40/45,text/55

Columns are separated by @,@ and the panels of a column by @+@. A panel
may carry a height (@:N@) and a column a width (@/N@); both are shares,
relative to their neighbours, and default to 'defaultShare'. A panel that
is not named is hidden.

This module is pure -- parsing, rendering and the changes the keys make --
so that it is tested without a terminal, like "System.Agents.Spectate".
-}
module System.Agents.Spectate.Layout (
    -- * Layout
    Panel (..),
    Slot (..),
    Column (..),
    Layout (..),
    defaultLayout,
    defaultColumns,
    defaultShare,
    visiblePanels,
    panelName,
    panelTitle,

    -- * Panels, as text
    parsePanels,
    renderPanels,

    -- * Refresh interval
    defaultRefreshMs,
    minRefreshMs,
    maxRefreshMs,
    parseRefresh,
    renderRefresh,
    slowerRefresh,
    fasterRefresh,

    -- * Changes
    togglePanel,
    movePanel,
    restackPanel,
    resizeHeight,
    resizeWidth,
    nextPanel,

    -- * Sizes on screen
    shares,

    -- * Saved layouts
    renderLayoutFile,
    parseLayoutFile,

    -- * Equivalent command line
    layoutFlags,
) where

import Data.Char (isDigit)
import Data.List (elemIndex, find)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

-------------------------------------------------------------------------------
-- Layout
-------------------------------------------------------------------------------

-- | What the screen can show.
data Panel
    = -- | The tree of sessions and sub-agent calls.
      PanelTree
    | -- | Tool calls, running and recently finished.
      PanelTools
    | -- | The text of one session.
      PanelText
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | A panel in a column, with its share of the column's height.
data Slot = Slot
    { slotPanel :: Panel
    , slotHeight :: Int
    }
    deriving (Show, Eq)

-- | A stack of panels, with its share of the screen's width.
data Column = Column
    { colSlots :: [Slot]
    , colWidth :: Int
    }
    deriving (Show, Eq)

{- | Columns hold every visible panel once, there is at least one, and no
column is empty: 'parsePanels' and the changes below keep it so.
-}
data Layout = Layout
    { layColumns :: [Column]
    , layRefreshMs :: Int
    -- ^ Milliseconds between two refreshes of the screen.
    }
    deriving (Show, Eq)

-- | The share of a panel or a column that gives none.
defaultShare :: Int
defaultShare = 50

minShare, maxShare, shareStep :: Int
minShare = 5
maxShare = 95
shareStep = 5

-- | The fixed screen @spectate@ started with: tree over tools, text beside.
defaultColumns :: [Column]
defaultColumns =
    [ Column [Slot PanelTree 60, Slot PanelTools 40] 45
    , Column [Slot PanelText defaultShare] 55
    ]

defaultLayout :: Layout
defaultLayout = Layout defaultColumns defaultRefreshMs

-- | The visible panels, in reading order: column by column, top to bottom.
visiblePanels :: Layout -> [Panel]
visiblePanels layout = [slot.slotPanel | col <- layout.layColumns, slot <- col.colSlots]

-- | The name a panel has in @--panels@.
panelName :: Panel -> Text
panelName = \case
    PanelTree -> "tree"
    PanelTools -> "tools"
    PanelText -> "text"

-- | The name a panel has on screen.
panelTitle :: Panel -> Text
panelTitle = \case
    PanelTree -> "agents"
    PanelTools -> "tool calls"
    PanelText -> "text"

panelFromName :: Text -> Maybe Panel
panelFromName name = case Text.toLower name of
    "tree" -> Just PanelTree
    "agents" -> Just PanelTree
    "tools" -> Just PanelTools
    "text" -> Just PanelText
    _ -> Nothing

-------------------------------------------------------------------------------
-- Panels, as text
-------------------------------------------------------------------------------

-- | Parse the value of @--panels@.
parsePanels :: Text -> Either Text [Column]
parsePanels input = do
    cols <- traverse parseColumn (Text.splitOn "," (Text.filter (/= ' ') input))
    let panels = [slot.slotPanel | col <- cols, slot <- col.colSlots]
    case find (\p -> length (filter (== p) panels) > 1) panels of
        Just p -> Left ("panel " <> panelName p <> " is named more than once")
        Nothing -> Right cols
  where
    parseColumn col = case Text.splitOn "/" col of
        [slots] -> Column <$> parseSlots slots <*> pure defaultShare
        [slots, width] -> Column <$> parseSlots slots <*> parseShare "width" width
        _ -> Left ("more than one width in " <> quoted col)
    parseSlots slots = traverse parseSlot (Text.splitOn "+" slots)
    parseSlot slot = case Text.splitOn ":" slot of
        [name] -> Slot <$> parseName name <*> pure defaultShare
        [name, height] -> Slot <$> parseName name <*> parseShare "height" height
        _ -> Left ("more than one height in " <> quoted slot)
    parseName name
        | Text.null name = Left "a panel name is missing (expected tree, tools or text)"
        | otherwise = maybe (Left ("unknown panel " <> quoted name <> " (expected tree, tools or text)")) Right (panelFromName name)
    parseShare what t
        | not (Text.null t) && Text.all isDigit t && Text.length t <= 3
        , n <- read (Text.unpack t)
        , n >= 1 && n <= 100 =
            Right n
        | otherwise = Left ("the " <> what <> " " <> quoted t <> " is not a number between 1 and 100")
    quoted t = "'" <> t <> "'"

{- | The text 'parsePanels' reads back to the same columns. Shares equal to
'defaultShare' are left out.
-}
renderPanels :: [Column] -> Text
renderPanels = Text.intercalate "," . map column
  where
    column :: Column -> Text
    column col = Text.intercalate "+" (map slot col.colSlots) <> share "/" col.colWidth
    slot :: Slot -> Text
    slot s = panelName s.slotPanel <> share ":" s.slotHeight
    share sep n
        | n == defaultShare = ""
        | otherwise = sep <> Text.pack (show n)

-------------------------------------------------------------------------------
-- Refresh interval
-------------------------------------------------------------------------------

defaultRefreshMs, minRefreshMs, maxRefreshMs :: Int
defaultRefreshMs = 1000
minRefreshMs = 100
maxRefreshMs = 60000

-- | Parse the value of @--refresh@: seconds, with up to three decimals.
parseRefresh :: Text -> Either Text Int
parseRefresh input = case Text.splitOn "." (Text.strip input) of
    [whole] | digits whole -> check (number whole * 1000)
    [whole, frac]
        | (digits whole || Text.null whole)
        , digits frac
        , Text.length frac <= 3 ->
            check (number whole * 1000 + number (Text.justifyLeft 3 '0' frac))
    _ -> Left ("'" <> input <> "' is not a number of seconds")
  where
    digits t = not (Text.null t) && Text.all isDigit t && Text.length t <= 6
    number t = if Text.null t then 0 else read (Text.unpack t) :: Int
    check ms
        | ms < minRefreshMs || ms > maxRefreshMs =
            Left ("the refresh interval is between " <> renderRefresh minRefreshMs <> " and " <> renderRefresh maxRefreshMs <> " seconds")
        | otherwise = Right ms

-- | Seconds, as 'parseRefresh' reads them: @1@, @0.5@, @0.25@.
renderRefresh :: Int -> Text
renderRefresh ms
    | frac == 0 = whole
    | otherwise = whole <> "." <> Text.dropWhileEnd (== '0') (Text.justifyRight 3 '0' (Text.pack (show frac)))
  where
    whole = Text.pack (show (ms `div` 1000))
    frac = ms `mod` 1000

-- | The intervals the keys step through.
refreshSteps :: [Int]
refreshSteps = [100, 250, 500, 1000, 2000, 5000, 10000, 30000, 60000]

-- | The next longer interval of 'refreshSteps'.
slowerRefresh :: Int -> Int
slowerRefresh ms = maybe maxRefreshMs id (find (> ms) refreshSteps)

-- | The next shorter interval of 'refreshSteps'.
fasterRefresh :: Int -> Int
fasterRefresh ms = maybe minRefreshMs id (find (< ms) (reverse refreshSteps))

-------------------------------------------------------------------------------
-- Changes
-------------------------------------------------------------------------------

{- | Hide a visible panel, or show a hidden one as a new last column. The
last visible panel stays.
-}
togglePanel :: Panel -> Layout -> Layout
togglePanel panel layout
    | panel `notElem` visible = layout{layColumns = layout.layColumns <> [Column [Slot panel defaultShare] defaultShare]}
    | length visible <= 1 = layout
    | otherwise = layout{layColumns = removePanel panel layout.layColumns}
  where
    visible = visiblePanels layout

removePanel :: Panel -> [Column] -> [Column]
removePanel panel cols =
    [ col{colSlots = slots}
    | col <- cols
    , let slots = filter ((/= panel) . (.slotPanel)) col.colSlots
    , not (null slots)
    ]

{- | Exchange a panel with the one that many places away in reading order.
Places keep their sizes: only the panels move.
-}
movePanel :: Int -> Panel -> Layout -> Layout
movePanel delta panel layout = case elemIndex panel visible of
    Just i
        | j <- i + delta
        , j >= 0 && j < length visible ->
            let other = visible !! j
                swap p
                    | p == panel = other
                    | p == other = panel
                    | otherwise = p
             in layout{layColumns = [col{colSlots = [slot{slotPanel = swap slot.slotPanel} | slot <- col.colSlots]} | col <- layout.layColumns]}
    _ -> layout
  where
    visible = visiblePanels layout

{- | Change how a panel is stacked: one that shares its column gets a column
of its own, just after; one alone in its column joins the bottom of the
column before (the first column has none before, and stays).
-}
restackPanel :: Panel -> Layout -> Layout
restackPanel panel layout = layout{layColumns = go layout.layColumns}
  where
    holds :: Column -> Bool
    holds col = any ((== panel) . (.slotPanel)) col.colSlots
    go (prev : col : rest)
        | holds col && length col.colSlots == 1 = prev{colSlots = prev.colSlots <> col.colSlots} : rest
    go (col : rest)
        | holds col && length col.colSlots > 1 =
            col{colSlots = filter ((/= panel) . (.slotPanel)) col.colSlots}
                : Column (filter ((== panel) . (.slotPanel)) col.colSlots) defaultShare
                : rest
        | otherwise = col : go rest
    go [] = []

-- | Change a panel's share of its column's height, by that many steps.
resizeHeight :: Int -> Panel -> Layout -> Layout
resizeHeight steps panel layout =
    layout{layColumns = [col{colSlots = map slot col.colSlots} | col <- layout.layColumns]}
  where
    slot :: Slot -> Slot
    slot s
        | s.slotPanel == panel = s{slotHeight = stepShare steps s.slotHeight}
        | otherwise = s

-- | Change the share of the screen's width of a panel's column.
resizeWidth :: Int -> Panel -> Layout -> Layout
resizeWidth steps panel layout = layout{layColumns = map column layout.layColumns}
  where
    column :: Column -> Column
    column col
        | any ((== panel) . (.slotPanel)) col.colSlots = col{colWidth = stepShare steps col.colWidth}
        | otherwise = col

stepShare :: Int -> Int -> Int
stepShare steps share = max minShare (min maxShare (share + steps * shareStep))

{- | The visible panel that many places after a panel, wrapping around; the
first one when the panel is not visible.
-}
nextPanel :: Int -> Panel -> Layout -> Panel
nextPanel delta panel layout = case (visible, elemIndex panel visible) of
    ([], _) -> panel
    (first : _, Nothing) -> first
    (_, Just i) -> visible !! ((i + delta) `mod` length visible)
  where
    visible = visiblePanels layout

-------------------------------------------------------------------------------
-- Sizes on screen
-------------------------------------------------------------------------------

{- | Split a number of cells between shares: each gets its proportion,
rounded down, and the last one what is left, so that the sizes add up.
-}
shares :: Int -> [Int] -> [Int]
shares total weights = go (max 0 total) weights
  where
    whole = max 1 (sum weights)
    go _ [] = []
    go left [_] = [left]
    go left (w : ws) =
        let size = min left (max 0 total * w `div` whole)
         in size : go (left - size) ws

-------------------------------------------------------------------------------
-- Saved layouts
-------------------------------------------------------------------------------

{- | A layout as a file: one @panels@ line and one @refresh@ line, holding
what the flags of the same names take.
-}
renderLayoutFile :: Layout -> Text
renderLayoutFile layout =
    Text.unlines
        [ "# agents-exe spectate layout; --panels and --refresh override it"
        , "panels " <> renderPanels layout.layColumns
        , "refresh " <> renderRefresh layout.layRefreshMs
        ]

{- | Read a saved layout over a layout: a line the file does not have
leaves that part as it was. Blank lines and @#@ comments are skipped.
-}
parseLayoutFile :: Layout -> Text -> Either Text Layout
parseLayoutFile base content = foldl step (Right base) (mapMaybe keep (zip [1 :: Int ..] (Text.lines content)))
  where
    keep (n, line)
        | Text.null stripped || "#" `Text.isPrefixOf` stripped = Nothing
        | otherwise = Just (n, stripped)
      where
        stripped = Text.strip line
    step acc (n, line) = do
        layout <- acc
        let (key, value) = Text.break (== ' ') line
            located = either (\err -> Left ("line " <> Text.pack (show n) <> ": " <> err)) Right
        case key of
            "panels" -> located ((\cols -> layout{layColumns = cols}) <$> parsePanels value)
            "refresh" -> located ((\ms -> layout{layRefreshMs = ms}) <$> parseRefresh value)
            _ -> located (Left ("unknown setting '" <> key <> "' (expected panels or refresh)"))

-- | The flags that start @spectate@ with this layout.
layoutFlags :: Layout -> Text
layoutFlags layout =
    "--panels " <> renderPanels layout.layColumns <> " --refresh " <> renderRefresh layout.layRefreshMs
