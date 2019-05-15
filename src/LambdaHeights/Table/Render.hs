module LambdaHeights.Table.Render where

import           Data.Matrix
import           LambdaHeights.Render
import           LambdaHeights.Types.Table
import           Linear.V2
import           Linear.V2.Utils
import qualified SDL

renderTable :: CellRenderer -> TableRenderer
renderTable r table view = mapM_ (r table view) $ cellLocations table

cellLocations :: Table -> [Position]
cellLocations table =
  let V2 rCount cCount = tableDimension table
  in  [ V2 r c | r <- [1 .. rCount], c <- [1 .. cCount] ]

renderRectCell :: SDL.Renderer -> Position -> CellRenderer
renderRectCell renderer pos table view (V2 r c) = do
  let text    = getElem r c $ content table
  let style   = getElem r c $ styles view
  let size    = convertV2 $ getElem r c $ sizes view
  let cellPos = getElem r c $ positions view
  let textPos = getElem r c $ textPositions view
  SDL.rendererDrawColor renderer SDL.$= cellBg style
  SDL.fillRect renderer $ Just $ SDL.Rectangle (SDL.P $ convertV2 $ pos + cellPos) size
  renderText renderer (cellFont style) (cellFg style) (convertV2 $ pos + textPos) text

calcCenterPosition :: SDL.Window -> Size -> IO Position
calcCenterPosition window (V2 w h) = do
  let half x = round (realToFrac x / 2 :: Float)
  V2 wW wH <- SDL.get $ SDL.windowSize window
  return $ V2 (half wW - half w) (half wH - half h)
