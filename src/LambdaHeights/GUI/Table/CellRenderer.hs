module LambdaHeights.GUI.Table.CellRenderer where

import           Data.Matrix
import           LambdaHeights.GUI.Basics
import           LambdaHeights.GUI.Table.Types
import           Linear.V2
import           Linear.V2.Utils
import qualified SDL

renderRectCell :: SDL.Renderer -> Position -> CellRenderer CellStyle
renderRectCell renderer pos table view (V2 r c) = do
  let text    = getElem r c $ content table
  let style   = getElem r c $ styles view
  let size    = convertV2 $ getElem r c $ sizes view
  let cellPos = getElem r c $ positions view
  let textPos = getElem r c $ textPositions view
  SDL.rendererDrawColor renderer SDL.$= cellBg style
  SDL.fillRect renderer $ Just $ SDL.Rectangle (SDL.P $ convertV2 $ pos + cellPos) size
  renderText renderer (cellFont style) (convertV2 $ pos + textPos) (cellFg style) text
