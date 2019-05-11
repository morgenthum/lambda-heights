module LambdaHeights.GUI.Table.CellStyler where

import           Data.Matrix
import           Data.Maybe
import           LambdaHeights.GUI.Table.Types
import           Linear.V2

type StyleGenerator a = Table -> (Int, Int) -> a

with :: StyleGenerator a -> CellStyler a
with generator table = let V2 r c = tableDimension table in matrix r c $ generator table

prefer :: StyleGenerator (Maybe a) -> StyleGenerator a -> StyleGenerator a
prefer x y table pos = fromMaybe (y table pos) (x table pos)

header :: a -> StyleGenerator (Maybe a)
header headStyle _ (1, _) = Just headStyle
header _         _ _      = Nothing

selectedAndBody :: a -> a -> StyleGenerator a
selectedAndBody selectedStyle bodyStyle table (r, _) =
  let V2 sr _ = selected table in if sr == r then selectedStyle else bodyStyle
