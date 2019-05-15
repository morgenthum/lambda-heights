module LambdaHeights.Types.Table where

import           Data.Matrix
import           Data.Word
import           Linear.V2
import           Linear.V4
import qualified SDL
import qualified SDL.Font    as SDLF

type Position = V2 Int
type Size = V2 Int

type CellStyler = Table -> Matrix CellStyle
type CellSizer = Table -> Matrix Size
type CellPositioner = Table -> Matrix Position
type TextPositioner = Table -> Matrix Position

type TableRenderer = Table -> TableView -> IO ()
type CellRenderer = Table -> TableView -> Position -> IO ()
type UpdateTable = [SDL.Event] -> Table -> Table

data Table = Table {
  content  :: Matrix String,
  selected :: V2 Int
}

data TableView = TableView {
  styles        :: Matrix CellStyle,
  sizes         :: Matrix Size,
  positions     :: Matrix Position,
  textPositions :: Matrix Position
}

data CellStyle = CellStyle {
  cellFont :: SDLF.Font,
  cellBg   :: V4 Word8,
  cellFg   :: V4 Word8
}

selectedValue :: Table -> String
selectedValue table = let V2 r c = selected table in getElem r c $ content table

tableDimension :: Table -> V2 Int
tableDimension table = let m = content table in V2 (nrows m) (ncols m)

tableSize :: TableView -> V2 Int
tableSize view =
  let locations'   = positions view
      sizes'       = sizes view
      maxPositions = mapPos (\(r, c) pos -> pos + getElem r c sizes') locations'
      minList      = toList locations'
      maxList      = toList maxPositions
      getX (V2 x _) = x
      getY (V2 _ y) = y
      minX = minimum $ map getX minList
      maxX = maximum $ map getX maxList
      minY = minimum $ map getY minList
      maxY = maximum $ map getY maxList
  in  V2 (maxX - minX) (maxY - minY)

