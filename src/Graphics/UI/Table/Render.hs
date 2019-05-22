module Graphics.UI.Table.Render where

import           Graphics.UI.Render
import           Graphics.UI.Types
import           Graphics.UI.Types.Table
import           Linear.V2
import           Linear.V2.Utils
import qualified SDL

renderTable :: CellRenderer a -> TableRenderer a
renderTable = mapM_

calcCenterPosition :: SDL.Window -> Size -> IO Position
calcCenterPosition window (V2 w h) = do
  let half x = round (realToFrac x / 2 :: Float)
  V2 wW wH <- SDL.get $ SDL.windowSize window
  return $ V2 (half wW - half w) (half wH - half h)
