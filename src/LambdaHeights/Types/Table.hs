module LambdaHeights.Types.Table where

import           Data.Matrix
import           Data.Word
import           Linear.V2
import           Linear.V4
import qualified SDL.Font    as SDLF

type Location = V2 Int
type Position = V2 Int
type Size = V2 Int

data Table = Table {
  content  :: Matrix Cell,
  selected :: Location
}

data Cell = Cell {
  cellLocation :: Location,
  cellText     :: String
}

data TableViewport = TableViewport {
  viewportFrom :: Location,
  viewportTo   :: Location
}

type TableView = Matrix CellView

data CellView = CellView {
  viewText    :: String,
  viewStyle   :: CellStyle,
  viewSize    :: Size,
  viewPos     :: Position,
  viewTextPos :: Position
}

data CellStyle = CellStyle {
  cellFont :: SDLF.Font,
  cellBg   :: V4 Word8,
  cellFg   :: V4 Word8
}

newTable :: [[String]] -> Location -> Table
newTable texts selected =
  let toCell (r, c) = Cell (V2 r c)
      cells = mapPos toCell $ fromLists texts
  in  Table cells selected

selectedValue :: Table -> Cell
selectedValue t = let V2 r c = selected t in getElem r c $ content t

dimension :: Table -> V2 Int
dimension t = let m = content t in V2 (nrows m) (ncols m)
