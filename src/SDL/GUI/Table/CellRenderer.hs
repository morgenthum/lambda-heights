module SDL.GUI.Table.CellRenderer where

import           Data.Matrix
import           Linear.V2
import qualified SDL
import           SDL.GUI.Basics
import           SDL.GUI.Table.Types

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
