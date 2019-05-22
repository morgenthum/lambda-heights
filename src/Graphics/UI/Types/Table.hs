module Graphics.UI.Types.Table where

import           Data.Matrix
import           Graphics.UI.Types
import           Linear.V2
import qualified SDL

type CellStyler a = Table -> Matrix a
type CellSizer = Table -> Matrix Size
type CellPositioner = Table -> Matrix Position
type TextPositioner = Table -> Matrix Position

type TableRenderer a = TableView a -> IO ()
type CellRenderer a = CellView a -> IO ()
type UpdateTable = [SDL.Event] -> Table -> Table

data Table = Table {
  content  :: Matrix String,
  selected :: V2 Int
}

type TableView a = Matrix (CellView a)

data CellView a = CellView {
  text         :: String,
  style        :: a,
  size         :: Size,
  position     :: Position,
  textPosition :: Position
}

selectedValue :: Table -> String
selectedValue table = let V2 r c = selected table in getElem r c $ content table

tableDimension :: Table -> V2 Int
tableDimension table = let m = content table in V2 (nrows m) (ncols m)

tableSize :: TableView a -> V2 Int
tableSize view =
  let minPositions = fmap position view
      sizes        = fmap size view
      maxPositions = mapPos (\(r, c) pos -> pos + getElem r c sizes) minPositions
      minList      = toList minPositions
      maxList      = toList maxPositions
      getX (V2 x _) = x
      getY (V2 _ y) = y
      minX = minimum $ map getX minList
      maxX = maximum $ map getX maxList
      minY = minimum $ map getY minList
      maxY = maximum $ map getY maxList
  in  V2 (maxX - minX) (maxY - minY)
