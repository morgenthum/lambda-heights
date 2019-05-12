module LambdaHeights.GUI.Table.TableRenderer where

import           LambdaHeights.GUI.Table.Types
import           Linear.V2
import qualified SDL

generateView :: TableViewGenerators a -> Table -> TableView a
generateView generators table =
  let styles'        = styleCells generators table
      sizes'         = sizeCells generators table styles'
      locations'     = positionCells generators table sizes'
      textLocations' = positionTexts generators table sizes' locations'
  in  TableView
        { styles        = styles'
        , sizes         = sizes'
        , positions     = locations'
        , textPositions = textLocations'
        }

calcTablePos :: SDL.Window -> Size -> IO Position
calcTablePos window (V2 w h) = do
  V2 wW wH <- SDL.get $ SDL.windowSize window
  return $ V2 (half wW - half w) (half wH - half h)

half :: (Integral a, Integral b) => a -> b
half x = round (realToFrac x / 2 :: Float)

renderTable :: CellRenderer a -> TableRenderer a
renderTable r table view = mapM_ (r table view) $ cellLocations table
