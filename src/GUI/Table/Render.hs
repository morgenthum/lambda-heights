module GUI.Table.Render where

import           Data.Matrix
import qualified Data.Map as M
import qualified Data.Vector as V
import           GUI.Table.Types
import           Linear.V2

type Size = V2 Int

type PositionMatrix = Matrix Position
type SizeMatrix = Matrix Size
type StyleMatrix a = Matrix a

type StyleCells a = Table -> IO (StyleMatrix a)
type SizeCells a = Table -> StyleMatrix a -> IO SizeMatrix
type LocateCells a = Table -> SizeMatrix -> IO PositionMatrix
type RenderCell a = Table -> StyleMatrix a -> SizeMatrix -> PositionMatrix -> Position -> IO ()

type StyleGenerator a = Table -> (Int, Int) -> a
type SizeGenerator a = Table -> (Int, Int) -> Size
type PositionGenerator a = Table -> SizeMatrix -> (Int, Int) -> Position

renderWith :: StyleCells a -> SizeCells a -> LocateCells a -> RenderCell a -> Table -> IO ()
renderWith styleCells sizeCells locateCells renderCell table = do
  styles    <- styleCells table
  sizes     <- sizeCells table styles
  positions <- locateCells table sizes
  let (_, ps) = cellPositions table
  mapM_ (renderCell table styles sizes positions) ps

styleCellsWith :: StyleGenerator a -> StyleCells a
styleCellsWith generator table = do
  let rCount = nrows $ content table
  let cCount = ncols $ content table
  return $ matrix rCount cCount $ generator table

sizeCellsWith :: SizeGenerator a -> SizeCells a
sizeCellsWith generator table _ = do
  let (V2 rCount cCount, _) = cellPositions table
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

fontSize :: Size -> SizeMatrix -> SizeGenerator a
fontSize margin fontSizeMatrix _ (r, c) = margin + getElem r c fontSizeMatrix

locateCellsWith :: PositionGenerator a -> LocateCells a
locateCellsWith generator table sizes = do
  let (V2 rCount cCount, _) = cellPositions table
  return $ matrix rCount cCount $ generator table sizes

gaps :: Size -> PositionGenerator a
gaps (V2 rGap cGap) _ sizes (r, c) =
  let cs = [1 .. c - 1]
      rs = [1 .. r - 1]
      ws = map ((\(V2 w _) -> w) . (\c' -> getElem r c' sizes)) cs
      hs = map ((\(V2 _ h) -> h) . (\r' -> getElem r' c sizes)) rs
      x  = sum ws + cGap * length cs
      y  = sum hs + rGap * length rs
  in  V2 x y
