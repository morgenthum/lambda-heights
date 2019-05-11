module LambdaHeights.GUI.Table.TableRenderer where

import           LambdaHeights.GUI.Table.Types
import           Linear.V2
import qualified SDL

generateView :: TableViewGenerators a -> Table -> TableView a
generateView generators table =
  let styles'        = styleCells generators table
      sizes'         = sizeCells generators table styles'
      locations'     = locateCells generators table sizes'
      textLocations' = locateText generators table sizes' locations'
  in  TableView
        { styles        = styles'
        , sizes         = sizes'
        , locations     = locations'
        , textLocations = textLocations'
        }

calcTablePos :: SDL.Window -> TableView a -> IO (V2 Int)
calcTablePos window view = do
  V2 wW wH <- SDL.get $ SDL.windowSize window
  let V2 tW tH = tableSize view
  return $ V2 (half wW - half tW) (half wH - half tH)

half :: (Integral a, Integral b) => a -> b
half x = round (realToFrac x / 2 :: Float)

renderTable :: RenderCell a -> RenderTable a
renderTable r table view = mapM_ (r table view) $ cellLocations table
