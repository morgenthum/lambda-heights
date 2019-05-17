module Graphics.UI.Types.Table where

import           Data.Matrix
import           Data.Word
import           Linear.V2
import           Linear.V4
import Graphics.UI.Types
import Graphics.UI.Classes
import qualified SDL
import qualified SDL.Font    as SDLF

type CellStyler = Table -> Matrix CellStyle
type CellSizer = Table -> Matrix Size
type CellPositioner = Table -> Matrix Position
type TextPositioner = Table -> Matrix Position

type TableRenderer = TableView -> IO ()
type CellRenderer = CellView -> IO ()
type UpdateTable = [SDL.Event] -> Table -> Table

data Table = Table {
  content  :: Matrix String,
  selected :: V2 Int
}

newtype TableView = TableView (Matrix CellView)

instance HasSize TableView where
  calcSize (TableView m) =
     let minPositions = fmap position m
         sizes        = fmap size m
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

data CellView = CellView {
  text         :: String,
  style        :: CellStyle,
  size         :: Size,
  position     :: Position,
  textPosition :: Position
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
