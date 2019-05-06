module SDL.GUI.Table.RenderCombinators where

import           Data.Matrix
import qualified Data.Text           as T
import qualified Data.Vector         as V
import           Linear.V2
import qualified SDL
import qualified SDL.Font            as SDLF
import           SDL.GUI.Basics
import           SDL.GUI.Table.Types

type Position = V2 Int
type Size = V2 Int

type PositionMatrix = Matrix Position
type SizeMatrix = Matrix Size
type StyleMatrix a = Matrix a

type StyleCells a = Table -> IO (StyleMatrix a)
type SizeCells a = Table -> StyleMatrix a -> IO SizeMatrix
type LocateCells a = Table -> SizeMatrix -> IO PositionMatrix
type LocateText = SizeMatrix -> PositionMatrix -> Position -> Position
type RenderCell a = Table -> StyleMatrix a -> SizeMatrix -> PositionMatrix -> Position -> IO ()
type RenderTable = Table -> IO ()

type StyleGenerator a = Table -> (Int, Int) -> a
type SizeGenerator a = Table -> (Int, Int) -> Size
type PositionGenerator a = Table -> SizeMatrix -> (Int, Int) -> Position

cellPositions :: Table -> (V2 Int, [Position])
cellPositions table =
  let (rCount, cCount) = tableSize table
  in  (V2 rCount cCount, [ V2 r c | r <- [1 .. rCount], c <- [1 .. cCount] ])

renderWith :: StyleCells a -> SizeCells a -> LocateCells a -> RenderCell a -> RenderTable
renderWith styleCells sizeCells locateCells renderCell table = do
  styles    <- styleCells table
  sizes     <- sizeCells table styles
  positions <- locateCells table sizes
  let (_, ps) = cellPositions table
  mapM_ (renderCell table styles sizes positions) ps

-- StyleCells

styleCellsWith :: StyleGenerator a -> StyleCells a
styleCellsWith generator table = do
  let (rCount, cCount) = tableSize table
  return $ matrix rCount cCount $ generator table

colorsSelectedBody :: (a, a) -> StyleGenerator a
colorsSelectedBody (selectedStyle, defaultStyle) table (r, _) =
  let V2 sr _ = selected table in if sr == r then selectedStyle else defaultStyle

colorsHeadSelectedBody :: (a, a, a) -> StyleGenerator a
colorsHeadSelectedBody (headStyle, _, _) _ (1, _) = headStyle
colorsHeadSelectedBody (_, selectedStyle, defaultStyle) table (r, c) =
  colorsSelectedBody (selectedStyle, defaultStyle) table (r, c)

-- SizeCells

sizeCellsWith :: SizeGenerator a -> SizeCells a
sizeCellsWith generator table _ = do
  let (rCount, cCount) = tableSize table
  return $ matrix rCount cCount $ generator table

alignWidths :: SizeCells a -> SizeCells a
alignWidths parent table styles = do
  sizes <- parent table styles
  return $ mapPos (\(_, c) (V2 _ h) -> V2 (maxColumnWidth c sizes) h) sizes

maxColumnWidth :: Int -> SizeMatrix -> Int
maxColumnWidth c sizes =
  let cs = getCol c sizes
      maxWidth w1 (V2 w2 _) = max w1 w2
  in  V.foldl maxWidth 0 cs

fixedSize :: Size -> SizeGenerator a
fixedSize size _ _ = size

fontSize :: SizeMatrix -> SizeGenerator a
fontSize fontSizes _ (r, c) = getElem r c fontSizes

extend :: Size -> SizeGenerator a -> SizeGenerator a
extend size parent table (r, c) = size + parent table (r, c)

loadFontSizes :: SDLF.Font -> DataMatrix -> IO SizeMatrix
loadFontSizes font m = do
  sm <- mapM (SDLF.size font . T.pack) m
  return $ fmap (uncurry V2) sm

-- LocateCells

locateCellsWith :: PositionGenerator a -> LocateCells a
locateCellsWith generator table sizes = do
  let (V2 rCount cCount, _) = cellPositions table
  return $ matrix rCount cCount $ generator table sizes

grid :: PositionGenerator a
grid _ sizes (r, c) =
  let cs = [1 .. c - 1]
      rs = [1 .. r - 1]
      ws = map ((\(V2 w _) -> w) . (\c' -> getElem r c' sizes)) cs
      hs = map ((\(V2 _ h) -> h) . (\r' -> getElem r' c sizes)) rs
  in  V2 (sum ws) (sum hs)

addGaps :: Size -> PositionGenerator a -> PositionGenerator a
addGaps (V2 rGap cGap) parent table sizes (r, c) =
  let V2 x y = parent table sizes (r, c)
      x'     = x + cGap * (c - 1)
      y'     = y + rGap * (r - 1)
  in  V2 x' y'

indentSelected :: Int -> PositionGenerator a -> PositionGenerator a
indentSelected i parent table sizes (r, c) =
  let V2 x  y = parent table sizes (r, c)
      V2 sr _ = selected table
  in  if sr == r then V2 (x + i) y else V2 x y

  -- RenderCell

renderSimpleCell :: SDL.Renderer -> LocateText -> RenderCell CellStyle
renderSimpleCell renderer locateText table styles sizes positions (V2 r c) = do
  let text     = getElem r c $ content table
  let style    = getElem r c styles
  let V2 w h   = convertV2 $ getElem r c sizes
  let V2 x y   = convertV2 $ getElem r c positions
  let V2 tx ty = locateText sizes positions (V2 r c)
  SDL.rendererDrawColor renderer SDL.$= cellBg style
  SDL.fillRect renderer $ Just $ SDL.Rectangle (SDL.P $ V2 x y) (V2 w h)
  renderText renderer (cellFont style) (convertV2 $ V2 tx ty) (cellFg style) text
  return ()

locateTextWithIndent :: V2 Int -> LocateText
locateTextWithIndent (V2 ix iy) _ positions (V2 r c) =
  let V2 x y = convertV2 $ getElem r c positions in V2 (x + ix) (y + iy)

locateTextCentered :: SizeMatrix -> LocateText
locateTextCentered fontSizes sizes positions (V2 r c) =
  let V2 fw fh = convertV2 $ getElem r c fontSizes
      V2 w  h  = convertV2 $ getElem r c sizes
      V2 x  y  = convertV2 $ getElem r c positions
      x'       = x + round (realToFrac w / 2 :: Float) - round (realToFrac fw / 2 :: Float)
      y'       = y + round (realToFrac h / 2 :: Float) - round (realToFrac fh / 2 :: Float)
  in  V2 x' y'

convertV2 :: (Integral a, Integral b) => V2 a -> V2 b
convertV2 (V2 x y) = V2 (fromIntegral x) (fromIntegral y)






