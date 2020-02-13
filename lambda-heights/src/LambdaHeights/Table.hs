module LambdaHeights.Table where

import qualified Control.Monad.IO.Class as M
import Data.Matrix
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as V
import LambdaHeights.Render
import LambdaHeights.Types.Table
import Linear.V2
import qualified Linear.V2.Utils as V2
import Linear.V4
import qualified SDL
import qualified SDL.Font as SDLF

type CellStyler = Table -> Matrix CellStyle

type CellSizer = Table -> Matrix Size

type CellPositioner = Table -> Matrix Position

type TextPositioner = Table -> Matrix Position

type Generator a = Table -> (Int, Int) -> a

type Selector = Generator Bool

type StyleGen = Generator CellStyle

type SizeGen = Generator Size

type PositionGen = Generator Position

type TextPositionGen = Generator Position

type TableRenderer m = TableView -> m ()

type CellRenderer m = CellView -> m ()

type UpdateTable = [SDL.Event] -> Table -> Table

type ConvertEvent e = SDL.Event -> Maybe e

type ApplyEvent e = Table -> e -> Table

type LimitSelection = Table -> Table

-- Basics

tableSize :: TableView -> V2 Int
tableSize view =
  let minPositions = fmap viewPos view
      sizes = fmap viewSize view
      maxPositions = mapPos (\(r, c) pos -> pos + getElem r c sizes) minPositions
      minList = toList minPositions
      maxList = toList maxPositions
      minX = minimum $ map V2.getX minList
      maxX = maximum $ map V2.getX maxList
      minY = minimum $ map V2.getY minList
      maxY = maximum $ map V2.getY maxList
   in V2 (maxX - minX) (maxY - minY)

translate :: Position -> TableView -> TableView
translate pos =
  fmap $ \view -> view {viewPos = viewPos view + pos, viewTextPos = viewTextPos view + pos}

positionCenter :: Size -> Size -> Position
positionCenter (V2 pw ph) (V2 cw ch) =
  let half x = round (realToFrac x / 2 :: Float) in V2 (half pw - half cw) (half ph - half ch)

merge ::
  Matrix String ->
  Matrix CellStyle ->
  Matrix Size ->
  Matrix Position ->
  Matrix Position ->
  TableView
merge contents styles sizes positions textPositions =
  let f (r, c) =
        CellView
          (getElem r c contents)
          (getElem r c styles)
          (getElem r c sizes)
          (getElem r c positions)
          (getElem r c textPositions)
   in matrix (nrows contents) (ncols contents) f

-- Selectors

ifSelector :: Selector -> Generator a -> Generator a -> Generator a
ifSelector s lhs rhs table pos = if s table pos then lhs table pos else rhs table pos

selectedRow :: Selector
selectedRow table = row (V2.getX $ selected table) table

row :: Int -> Selector
row x table (r, c) = x == V2.getX (cellLocation $ getElem r c $ content table)

-- Style combinators

styleWith :: StyleGen -> CellStyler
styleWith g table = let V2 r c = dimension table in matrix r c $ g table

always :: CellStyle -> StyleGen
always x _ _ = x

-- Size combinators

loadFontSizes :: (M.MonadIO m) => SDLF.Font -> Matrix String -> m (Matrix Size)
loadFontSizes font cm = do
  sm <- mapM (SDLF.size font . T.pack) cm
  return $ fmap (uncurry V2) sm

alignWidths :: CellSizer -> CellSizer
alignWidths parent table =
  let sm = parent table in mapPos (\(_, c) (V2 _ h) -> V2 (maxColumnWidth c sm) h) sm

sizeWith :: SizeGen -> CellSizer
sizeWith generator table = let V2 r c = dimension table in matrix r c $ generator table

fixedSize :: Size -> SizeGen
fixedSize x _ _ = x

copy :: Matrix Size -> SizeGen
copy sm _ (r, c) = getElem r c sm

extend :: Size -> SizeGen -> SizeGen
extend x parent table (r, c) = x + parent table (r, c)

maxColumnWidth :: Int -> Matrix Size -> Int
maxColumnWidth c sm =
  let cs = getCol c sm
      maxWidth w1 (V2 w2 _) = max w1 w2
   in V.foldl maxWidth 0 cs

-- Position combinators

locateWith :: PositionGen -> CellPositioner
locateWith g table = let V2 r c = dimension table in matrix r c $ g table

grid :: Matrix Size -> PositionGen
grid sm _ (r, c) =
  let cs = [1 .. c - 1]
      rs = [1 .. r - 1]
      ws = map ((\(V2 w _) -> w) . (\c' -> getElem r c' sm)) cs
      hs = map ((\(V2 _ h) -> h) . (\r' -> getElem r' c sm)) rs
   in V2 (sum ws) (sum hs)

addGaps :: Size -> PositionGen -> PositionGen
addGaps (V2 xGap yGap) g table (r, c) =
  let V2 x y = g table (r, c)
      x' = x + xGap * (c - 1)
      y' = y + yGap * (r - 1)
   in V2 x' y'

indent :: Selector -> V2 Int -> PositionGen -> PositionGen
indent s (V2 ix iy) g table (r, c) =
  let V2 x y = g table (r, c) in if s table (r, c) then V2 (x + ix) (y + iy) else V2 x y

move :: V2 Int -> PositionGen -> PositionGen
move pos g table loc = g table loc + pos

-- Text position combinators

locateTextWith :: TextPositionGen -> TextPositioner
locateTextWith g table = let V2 r c = dimension table in matrix r c $ g table

centerText :: Matrix Size -> Matrix Position -> Matrix Size -> TextPositionGen
centerText sm pm fsm _ (r, c) =
  let V2 fw fh = getElem r c fsm :: V2 Int
      V2 w h = getElem r c sm :: V2 Int
      V2 x y = getElem r c pm
      x' = x + round (realToFrac w / 2 :: Float) - round (realToFrac fw / 2 :: Float)
      y' = y + round (realToFrac h / 2 :: Float) - round (realToFrac fh / 2 :: Float)
   in V2 x' y'

-- Templates

newMenuView :: (M.MonadIO m) => SDLF.Font -> Table -> m TableView
newMenuView f t = do
  let contents = cellText <$> content t
  fontSizes <- loadFontSizes f contents
  let selectedStyle = always $ CellStyle f (V4 30 30 30 255) (V4 0 191 255 255)
  let bodyStyle = always $ CellStyle f (V4 30 30 30 255) (V4 255 255 255 255)
  let styles = (styleWith $ ifSelector selectedRow selectedStyle bodyStyle) t
  let sizes = (alignWidths $ sizeWith $ extend (V2 20 20) $ copy fontSizes) t
  let positions = (locateWith $ indent selectedRow (V2 10 0) $ addGaps (V2 0 20) $ grid sizes) t
  let textPositions = (locateTextWith $ centerText sizes positions fontSizes) t
  return $ merge contents styles sizes positions textPositions

newTableView :: (M.MonadIO m) => SDLF.Font -> Table -> m TableView
newTableView f t = do
  let contents = cellText <$> content t
  fontSizes <- loadFontSizes f contents
  let headStyle = always $ CellStyle f (V4 0 191 255 255) (V4 30 30 30 255)
  let selectedStyle = always $ CellStyle f (V4 30 30 30 255) (V4 0 191 255 255)
  let bodyStyle = always $ CellStyle f (V4 30 30 30 255) (V4 255 255 255 255)
  let selectedOrBodyStyle = ifSelector selectedRow selectedStyle bodyStyle
  let styles = (styleWith $ ifSelector (row 1) headStyle selectedOrBodyStyle) t
  let sizes = (alignWidths $ sizeWith $ extend (V2 20 20) $ copy fontSizes) t
  let positions = (locateWith $ indent selectedRow (V2 10 0) $ addGaps (V2 20 20) $ grid sizes) t
  let textPositions = (locateTextWith $ centerText sizes positions fontSizes) t
  return $ merge contents styles sizes positions textPositions

-- Rendering

renderTable :: (M.MonadIO m) => CellRenderer m -> TableRenderer m
renderTable = mapM_

renderCell :: (M.MonadIO m) => SDL.Renderer -> CellRenderer m
renderCell renderer cell = do
  let text = viewText cell
  let style = viewStyle cell
  let size = V2.convert $ viewSize cell
  let pos = V2.convert $ viewPos cell
  let textPos = V2.convert $ viewTextPos cell
  SDL.rendererDrawColor renderer SDL.$= cellBg style
  SDL.fillRect renderer $ Just $ SDL.Rectangle (SDL.P pos) size
  renderText renderer (cellFont style) (cellFg style) textPos text

-- Updating

with :: ConvertEvent e -> ApplyEvent e -> UpdateTable
with convert apply events table = foldl apply table $ mapMaybe convert events

convertKeycode :: ConvertEvent SDL.Keycode
convertKeycode event = case SDL.eventPayload event of
  SDL.KeyboardEvent keyEvent ->
    let code = SDL.keysymKeycode (SDL.keyboardEventKeysym keyEvent)
        motion = SDL.keyboardEventKeyMotion keyEvent
        fromKey x SDL.Pressed = Just x
        fromKey _ _ = Nothing
     in fromKey code motion
  _ -> Nothing

applyKeycode :: LimitSelection -> ApplyEvent SDL.Keycode
applyKeycode limit table event =
  let V2 r c = selected table
   in limit $ case event of
        SDL.KeycodeLeft -> table {selected = V2 r (c - 1)}
        SDL.KeycodeUp -> table {selected = V2 (r - 1) c}
        SDL.KeycodeRight -> table {selected = V2 r (c + 1)}
        SDL.KeycodeDown -> table {selected = V2 (r + 1) c}
        _ -> table

limitNotFirstRow :: LimitSelection -> LimitSelection
limitNotFirstRow parent table =
  let V2 r c = selected $ parent table
      r' = if r < 2 then 2 else r
   in table {selected = V2 r' c}

limitFirstColumn :: LimitSelection -> LimitSelection
limitFirstColumn parent table =
  let V2 r _ = selected $ parent table in table {selected = V2 r 1}

limitAll :: LimitSelection
limitAll table =
  let V2 rm cm = dimension table
      V2 r c = selected table
      r' = bound (1, rm) r
      c' = bound (1, cm) c
   in table {selected = V2 r' c'}

bound :: (Ord a) => (a, a) -> a -> a
bound (f, l) x
  | x < f = f
  | x > l = l
  | otherwise = x

-- Viewport

updatePageViewport :: Table -> TableViewport -> TableViewport
updatePageViewport table viewport =
  let V2 fr fc = viewportFrom viewport
      V2 tr tc = viewportTo viewport
      range = tr - fr + 1
      V2 sr _ = selected table
      dr
        | sr > tr = range
        | sr < fr = - range
        | otherwise = 0
   in viewport {viewportFrom = V2 (fr + dr) fc, viewportTo = V2 (tr + dr) tc}

viewportTable :: TableViewport -> Table -> Table
viewportTable viewport table =
  let V2 fr fc = viewportFrom viewport
      V2 tr tc = viewportTo viewport
      tr' = min tr $ nrows $ content table
      content' = submatrix fr tr' fc tc $ content table
   in table {content = content'}
