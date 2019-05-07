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
type LocationGenerator a = Table -> SizeMatrix -> (Int, Int) -> Position

cellPositions :: Table -> [Position]
cellPositions table =
  let (rCount, cCount) = tableDimension table
  in  [ V2 r c | r <- [1 .. rCount], c <- [1 .. cCount] ]

applyGenerators
  :: StyleCells a
  -> SizeCells a
  -> LocateCells a
  -> Table
  -> IO (StyleMatrix a, SizeMatrix, PositionMatrix)
applyGenerators styleCells sizeCells locateCells table = do
  styles    <- styleCells table
  sizes     <- sizeCells table styles
  locations <- locateCells table sizes
  return (styles, sizes, locations)

tableSize :: PositionMatrix -> SizeMatrix -> V2 Int
tableSize positions sizes =
  let maxPositions = mapPos (\(r, c) pos -> pos + getElem r c sizes) positions
      minList      = toList positions
      maxList      = toList maxPositions
      getX (V2 x _) = x
      getY (V2 _ y) = y
      minX = minimum $ map getX minList
      maxX = maximum $ map getX maxList
      minY = minimum $ map getY minList
      maxY = maximum $ map getY maxList
  in  V2 (maxX - minX) (maxY - minY)

renderWith :: StyleCells a -> SizeCells a -> LocateCells a -> RenderCell a -> RenderTable
renderWith styleCells sizeCells locateCells renderCell table = do
  (styles, sizes, locations) <- applyGenerators styleCells sizeCells locateCells table
  mapM_ (renderCell table styles sizes locations) $ cellPositions table

-- StyleCells

styleCellsWith :: StyleGenerator a -> StyleCells a
styleCellsWith generator table = do
  let (rCount, cCount) = tableDimension table
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
  let (rCount, cCount) = tableDimension table
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

sizeFromFontSize :: SizeMatrix -> SizeGenerator a
sizeFromFontSize fontSizes _ (r, c) = getElem r c fontSizes

extendSize :: Size -> SizeGenerator a -> SizeGenerator a
extendSize size parent table (r, c) = size + parent table (r, c)

loadFontSizes :: SDLF.Font -> DataMatrix -> IO SizeMatrix
loadFontSizes font m = do
  sm <- mapM (SDLF.size font . T.pack) m
  return $ fmap (uncurry V2) sm

-- LocateCells

locateCellsWith :: LocationGenerator a -> LocateCells a
locateCellsWith generator table sizes = do
  let (rCount, cCount) = tableDimension table
  return $ matrix rCount cCount $ generator table sizes

grid :: LocationGenerator a
grid _ sizes (r, c) =
  let cs = [1 .. c - 1]
      rs = [1 .. r - 1]
      ws = map ((\(V2 w _) -> w) . (\c' -> getElem r c' sizes)) cs
      hs = map ((\(V2 _ h) -> h) . (\r' -> getElem r' c sizes)) rs
  in  V2 (sum ws) (sum hs)

addGaps :: Size -> LocationGenerator a -> LocationGenerator a
addGaps (V2 xGap yGap) parent table sizes (r, c) =
  let V2 x y = parent table sizes (r, c)
      x'     = x + xGap * (c - 1)
      y'     = y + yGap * (r - 1)
  in  V2 x' y'

indentSelected :: Int -> LocationGenerator a -> LocationGenerator a
indentSelected i parent table sizes (r, c) =
  let V2 x  y = parent table sizes (r, c)
      V2 sr _ = selected table
  in  if sr == r then V2 (x + i) y else V2 x y

move :: V2 Int -> LocationGenerator a -> LocationGenerator a
move pos parent table sizes loc = parent table sizes loc + pos

  -- RenderCell

renderSimpleCell :: SDL.Renderer -> V2 Int -> LocateText -> RenderCell CellStyle
renderSimpleCell renderer pos locateText table styles sizes positions (V2 r c) = do
  let text    = getElem r c $ content table
  let style   = getElem r c styles
  let size    = convertV2 $ getElem r c sizes
  let cellPos = getElem r c positions
  let textPos = locateText sizes positions (V2 r c)
  SDL.rendererDrawColor renderer SDL.$= cellBg style
  SDL.fillRect renderer $ Just $ SDL.Rectangle (SDL.P $ convertV2 $ pos + cellPos) size
  renderText renderer (cellFont style) (convertV2 $ pos + textPos) (cellFg style) text

locateTextWithIndent :: V2 Int -> LocateText
locateTextWithIndent (V2 ix iy) _ positions (V2 r c) =
  let V2 x y = convertV2 $ getElem r c positions in V2 (x + ix) (y + iy)

locateTextCentered :: SizeMatrix -> LocateText
locateTextCentered fontSizes sizes positions (V2 r c) =
  let V2 fw fh = convertV2 $ getElem r c fontSizes :: V2 Int
      V2 w  h  = convertV2 $ getElem r c sizes :: V2 Int
      V2 x  y  = convertV2 $ getElem r c positions
      x'       = x + round (realToFrac w / 2 :: Float) - round (realToFrac fw / 2 :: Float)
      y'       = y + round (realToFrac h / 2 :: Float) - round (realToFrac fh / 2 :: Float)
  in  V2 x' y'

convertV2 :: (Integral a, Integral b) => V2 a -> V2 b
convertV2 (V2 x y) = V2 (fromIntegral x) (fromIntegral y)






