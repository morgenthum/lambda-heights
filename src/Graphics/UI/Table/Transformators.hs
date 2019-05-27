module Graphics.UI.Table.Transformators where

import           Data.Matrix
import           Graphics.UI.Types
import           Graphics.UI.Types.Table
import           Linear.V2

merge
  :: Table -> Matrix a -> Matrix Size -> Matrix Position -> Matrix Position -> Matrix (CellView a)
merge table styles sizes positions textPositions =
  let V2 rCount cCount = tableDimension table
      f (r, c) = CellView (getElem r c $ content table)
                          (getElem r c styles)
                          (getElem r c sizes)
                          (getElem r c positions)
                          (getElem r c textPositions)
  in  matrix rCount cCount f

translateTable :: Position -> TableView a -> TableView a
translateTable pos = fmap (translate pos)

translate :: Position -> CellView a -> CellView a
translate pos view =
  view { position = position view + pos, textPosition = textPosition view + pos }
