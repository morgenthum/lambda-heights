module LambdaHeights.GUI.Table.CellRenderer where

import           Data.Matrix
import           LambdaHeights.GUI.Basics
import           LambdaHeights.GUI.Table.Types
import           Linear.V2
import qualified SDL

renderSimpleCell :: SDL.Renderer -> V2 Int -> RenderCell CellStyle
renderSimpleCell renderer pos table view (V2 r c) = do
  let text    = getElem r c $ content table
  let style   = getElem r c $ styles view
  let size    = convertV2 $ getElem r c $ sizes view
  let cellPos = getElem r c $ locations view
  let textPos = getElem r c $ textLocations view
  SDL.rendererDrawColor renderer SDL.$= cellBg style
  SDL.fillRect renderer $ Just $ SDL.Rectangle (SDL.P $ convertV2 $ pos + cellPos) size
  renderText renderer (cellFont style) (convertV2 $ pos + textPos) (cellFg style) text

convertV2 :: (Integral a, Integral b) => V2 a -> V2 b
convertV2 (V2 x y) = V2 (fromIntegral x) (fromIntegral y)
