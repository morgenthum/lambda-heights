module LambdaHeights.Table.Render where

import           Data.Matrix
import           LambdaHeights.Render
import           LambdaHeights.Types.Table
import           Linear.V2
import           Linear.V2.Utils
import qualified SDL

renderTable :: CellRenderer -> TableRenderer
renderTable r table view = mapM_ (r table view) $ cellLocations table

renderRectCell :: SDL.Renderer -> Position -> CellRenderer
renderRectCell renderer pos table view (V2 r c) = do
  let text    = getElem r c $ content table
  let style   = getElem r c $ styles view
  let size    = convertV2 $ getElem r c $ sizes view
  let cellPos = getElem r c $ positions view
  let textPos = getElem r c $ textPositions view
  SDL.rendererDrawColor renderer SDL.$= cellBg style
  SDL.fillRect renderer $ Just $ SDL.Rectangle (SDL.P $ convertV2 $ pos + cellPos) size
  renderText renderer (cellFont style) (convertV2 $ pos + textPos) (cellFg style) text
