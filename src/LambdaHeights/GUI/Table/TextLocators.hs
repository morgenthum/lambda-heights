module LambdaHeights.GUI.Table.TextLocators where

import           Data.Matrix
import           LambdaHeights.GUI.Table.Types
import           Linear.V2

type TextLocationGen = (Int, Int) -> Position

with :: TextLocationGen -> TextPositioner
with g table = let V2 r c = tableDimension table in matrix r c g

indent :: Matrix Position -> V2 Int -> TextLocationGen
indent pm (V2 ix iy) (r, c) =
  let V2 x y = getElem r c pm in V2 (x + ix) (y + iy)

center :: Matrix Size -> Matrix Position -> Matrix Size -> TextLocationGen
center sm pm fontSizes (r, c) =
  let V2 fw fh = getElem r c fontSizes :: V2 Int
      V2 w  h  = getElem r c sm :: V2 Int
      V2 x  y  = getElem r c pm
      x'       = x + round (realToFrac w / 2 :: Float) - round (realToFrac fw / 2 :: Float)
      y'       = y + round (realToFrac h / 2 :: Float) - round (realToFrac fh / 2 :: Float)
  in  V2 x' y'
