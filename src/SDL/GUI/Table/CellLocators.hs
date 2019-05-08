module SDL.GUI.Table.CellLocators where

import           Data.Matrix
import           Linear.V2
import           SDL.GUI.Table.Types

with :: LocationGenerator a -> LocateCells a
with generator table sizes = do
  let (rCount, cCount) = tableDimension table
  return $ matrix rCount cCount $ generator table sizes

grid :: LocationGenerator a
grid _ sizes (r, c) =
  let cs = [1 .. c - 1]
      rs = [1 .. r - 1]
      ws = map ((\(V2 w _) -> w) . (\c' -> getElem r c' sizes)) cs
      hs = map ((\(V2 _ h) -> h) . (\r' -> getElem r' c sizes)) rs
  in  V2 (sum ws) (sum hs)

addGaps :: Size -> LocationGenerator a -> LocationGenerator a
addGaps (V2 xGap yGap) parent table sizes (r, c) =
  let V2 x y = parent table sizes (r, c)
      x'     = x + xGap * (c - 1)
      y'     = y + yGap * (r - 1)
  in  V2 x' y'

indentSelected :: Int -> LocationGenerator a -> LocationGenerator a
indentSelected i parent table sizes (r, c) =
  let V2 x  y = parent table sizes (r, c)
      V2 sr _ = selected table
  in  if sr == r then V2 (x + i) y else V2 x y

move :: V2 Int -> LocationGenerator a -> LocationGenerator a
move pos parent table sizes loc = parent table sizes loc + pos
