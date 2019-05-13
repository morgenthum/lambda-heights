module LambdaHeights.GUI.Table.TextLocators where

import           Data.Matrix
import           LambdaHeights.GUI.Table.Types
import           Linear.V2
import           Linear.V2.Utils

type TextLocationGen a = Matrix Size -> Matrix Position -> (Int, Int) -> Position

with :: TextLocationGen a -> TextPositioner
with g table sizes locations = let V2 r c = tableDimension table in matrix r c $ g sizes locations

indent :: V2 Int -> TextLocationGen a
indent (V2 ix iy) _ locations (r, c) =
  let V2 x y = convertV2 $ getElem r c locations in V2 (x + ix) (y + iy)

center :: Matrix Size -> TextLocationGen a
center fontSizes sizes locations (r, c) =
  let V2 fw fh = convertV2 $ getElem r c fontSizes :: V2 Int
      V2 w  h  = convertV2 $ getElem r c sizes :: V2 Int
      V2 x  y  = convertV2 $ getElem r c locations
      x'       = x + round (realToFrac w / 2 :: Float) - round (realToFrac fw / 2 :: Float)
      y'       = y + round (realToFrac h / 2 :: Float) - round (realToFrac fh / 2 :: Float)
  in  V2 x' y'
