module SDL.GUI.Table.TableRenderer where

import           Linear.V2
import qualified SDL
import           SDL.GUI.Table.Types

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

calcTablePos :: SDL.Window -> SizeMatrix -> PositionMatrix -> IO (V2 Int)
calcTablePos window sizes positions = do
  V2 wW wH <- SDL.get $ SDL.windowSize window
  let V2 tW tH = tableSize positions sizes
  return $ V2 (half wW - half tW) (half wH - half tH)

half :: (Integral a, Integral b) => a -> b
half x = round (realToFrac x / 2 :: Float)

renderWith :: StyleCells a -> SizeCells a -> LocateCells a -> RenderCell a -> RenderTable
renderWith styleCells sizeCells locateCells renderCell table = do
  (styles, sizes, locations) <- applyGenerators styleCells sizeCells locateCells table
  mapM_ (renderCell table styles sizes locations) $ cellPositions table
