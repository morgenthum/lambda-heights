module LambdaHeights.GUI.Table.CellLocators where

import           Data.Matrix
import           LambdaHeights.GUI.Table.Types
import           Linear.V2

type LocationGen a = Table -> (Int, Int) -> Position

with :: LocationGen a -> CellPositioner
with g table = let V2 r c = tableDimension table in matrix r c $ g table

grid :: Matrix Size -> LocationGen a
grid sm _ (r, c) =
  let cs = [1 .. c - 1]
      rs = [1 .. r - 1]
      ws = map ((\(V2 w _) -> w) . (\c' -> getElem r c' sm)) cs
      hs = map ((\(V2 _ h) -> h) . (\r' -> getElem r' c sm)) rs
  in  V2 (sum ws) (sum hs)

addGaps :: Size -> LocationGen a -> LocationGen a
addGaps (V2 xGap yGap) g table (r, c) =
  let V2 x y = g table (r, c)
      x'     = x + xGap * (c - 1)
      y'     = y + yGap * (r - 1)
  in  V2 x' y'

indentSelected :: Int -> LocationGen a -> LocationGen a
indentSelected i g table (r, c) =
  let V2 x  y = g table (r, c)
      V2 sr _ = selected table
  in  if sr == r then V2 (x + i) y else V2 x y

move :: V2 Int -> LocationGen a -> LocationGen a
move pos g table loc = g table loc + pos
