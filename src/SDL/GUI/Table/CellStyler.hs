module SDL.GUI.Table.CellStyler where

import           Data.Matrix
import           Data.Maybe
import           Linear.V2
import           SDL.GUI.Table.Types

with :: StyleGenerator a -> StyleCells a
with generator table = do
  let (rCount, cCount) = tableDimension table
  return $ matrix rCount cCount $ generator table

prefer :: StyleGenerator (Maybe a) -> StyleGenerator a -> StyleGenerator a
prefer x y table pos = fromMaybe (y table pos) (x table pos)

header :: a -> StyleGenerator (Maybe a)
header headStyle _ (1, _) = Just headStyle
header _         _ _      = Nothing

selectedAndBody :: a -> a -> StyleGenerator a
selectedAndBody selectedStyle bodyStyle table (r, _) =
  let V2 sr _ = selected table in if sr == r then selectedStyle else bodyStyle
