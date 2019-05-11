module LambdaHeights.GUI.Table.TextLocators where

import           Data.Matrix
import qualified LambdaHeights.GUI.Table.CellLocators  as Locate
import           LambdaHeights.GUI.Table.CellRenderer
import qualified LambdaHeights.GUI.Table.CellSizers    as Size
import qualified LambdaHeights.GUI.Table.CellStyler    as Style
import           LambdaHeights.GUI.Table.TableRenderer
import qualified LambdaHeights.GUI.Table.TableUpdater  as Update
import           LambdaHeights.GUI.Table.Types
import           LambdaHeights.RenderContext
import           Linear.V2
import           Linear.V4
import qualified SDL
import qualified SDL.Font                              as SDLF

type TextLocationGen a = Matrix Size -> Matrix Location -> (Int, Int) -> Location

with :: TextLocationGen a -> TextLocator
with g table sizes locations = let V2 r c = tableDimension table in matrix r c $ g sizes locations

indent :: V2 Int -> TextLocationGen a
indent (V2 ix iy) sizes locations (r, c) =
  let V2 x y = convertV2 $ getElem r c locations in V2 (x + ix) (y + iy)

center :: Matrix Size -> TextLocationGen a
center fontSizes sizes locations (r, c) =
  let V2 fw fh = convertV2 $ getElem r c fontSizes :: V2 Int
      V2 w  h  = convertV2 $ getElem r c sizes :: V2 Int
      V2 x  y  = convertV2 $ getElem r c locations
      x'       = x + round (realToFrac w / 2 :: Float) - round (realToFrac fw / 2 :: Float)
      y'       = y + round (realToFrac h / 2 :: Float) - round (realToFrac fh / 2 :: Float)
  in  V2 x' y'
