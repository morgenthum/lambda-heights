module Graphics.UI.Table.Render where

import           Graphics.UI.Render
import           Graphics.UI.Types
import           Graphics.UI.Types.Table
import           Linear.V2
import           Linear.V2.Utils
import qualified SDL

renderTable :: CellRenderer -> TableRenderer
renderTable = mapM_

renderRectCell :: SDL.Renderer -> Position -> CellRenderer
renderRectCell renderer tablePos cell = do
  let text'   = text cell
  let style'  = style cell
  let size'   = convertV2 $ size cell
  let cellPos = position cell
  let textPos = textPosition cell
  SDL.rendererDrawColor renderer SDL.$= cellBg style'
  SDL.fillRect renderer $ Just $ SDL.Rectangle (SDL.P $ convertV2 $ tablePos + cellPos) size'
  renderText renderer (cellFont style') (cellFg style') (convertV2 $ tablePos + textPos) text'

calcCenterPosition :: SDL.Window -> Size -> IO Position
calcCenterPosition window (V2 w h) = do
  let half x = round (realToFrac x / 2 :: Float)
  V2 wW wH <- SDL.get $ SDL.windowSize window
  return $ V2 (half wW - half w) (half wH - half h)
