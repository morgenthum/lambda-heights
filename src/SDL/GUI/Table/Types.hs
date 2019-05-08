module SDL.GUI.Table.Types where

import           Data.Matrix
import           Data.Word
import           Linear.V2
import           Linear.V4
import qualified SDL
import qualified SDL.Font    as SDLF

type Color = V4 Word8

type Position = V2 Int
type Size = V2 Int

type DataMatrix = Matrix String
type PositionMatrix = Matrix Position
type SizeMatrix = Matrix Size
type StyleMatrix a = Matrix a

type SizeGenerator a = Table -> (Int, Int) -> Size
type SizeCells a = Table -> StyleMatrix a -> IO SizeMatrix

type LocationGenerator a = Table -> SizeMatrix -> (Int, Int) -> Position
type LocateCells a = Table -> SizeMatrix -> IO PositionMatrix

type StyleGenerator a = Table -> (Int, Int) -> a
type StyleCells a = Table -> IO (StyleMatrix a)

type RenderCell a = Table -> StyleMatrix a -> SizeMatrix -> PositionMatrix -> Position -> IO ()
type LocateText = SizeMatrix -> PositionMatrix -> Position -> Position

type RenderTable = Table -> IO ()

type ConvertEvent e = SDL.Event -> Maybe e
type ApplyEvent e = Table -> e -> Table
type LimitSelection = Table -> Table
type UpdateTable = [SDL.Event] -> Table -> Table

data Table = Table {
  content  :: DataMatrix,
  selected :: V2 Int
}

data CellStyle = CellStyle {
  cellFont :: SDLF.Font,
  cellBg   :: Color,
  cellFg   :: Color
}

tableDimension :: Table -> (Int, Int)
tableDimension table = let m = content table in (nrows m, ncols m)

tableSize :: PositionMatrix -> SizeMatrix -> V2 Int
tableSize positions sizes =
  let maxPositions = mapPos (\(r, c) pos -> pos + getElem r c sizes) positions
      minList      = toList positions
      maxList      = toList maxPositions
      getX (V2 x _) = x
      getY (V2 _ y) = y
      minX = minimum $ map getX minList
      maxX = maximum $ map getX maxList
      minY = minimum $ map getY minList
      maxY = maximum $ map getY maxList
  in  V2 (maxX - minX) (maxY - minY)
