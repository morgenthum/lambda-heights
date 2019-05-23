module LambdaHeights.Table where

import           Data.Word
import           Graphics.UI.Render
import           Graphics.UI.Table.Combinators
import           Graphics.UI.Table.Transformators
import           Graphics.UI.Types.Table
import           Linear.V2
import           Linear.V2.Utils
import           Linear.V4
import qualified SDL
import qualified SDL.Font                         as SDLF

data CellStyle = CellStyle {
  cellFont       :: SDLF.Font,
  cellBackground :: V4 Word8,
  cellForeground :: V4 Word8
}

newMenuView :: SDLF.Font -> Table -> IO (TableView CellStyle)
newMenuView font table = do
  fontSizes <- loadFontSizes font $ content table
  let selectedStyle = CellStyle font (V4 30 30 30 255) (V4 0 191 255 255)
  let bodyStyle     = CellStyle font (V4 30 30 30 255) (V4 255 255 255 255)
  let styles        = (styleWith $ selectedAndBody selectedStyle bodyStyle) table
  let sizes         = (alignWidths $ sizeWith $ extend (V2 20 20) $ copy fontSizes) table
  let positions =
        (positionWith $ indent selectedRow (V2 10 0) $ addGaps (V2 0 20) $ grid sizes) table
  let textPositions = (positionTextWith $ centerText sizes positions fontSizes) table
  return $ merge table styles sizes positions textPositions

newTableView :: SDLF.Font -> Table -> IO (TableView CellStyle)
newTableView font table = do
  fontSizes <- loadFontSizes font $ content table
  let headStyle     = CellStyle font (V4 0 191 255 255) (V4 30 30 30 255)
  let selectedStyle = CellStyle font (V4 30 30 30 255) (V4 0 191 255 255)
  let bodyStyle     = CellStyle font (V4 30 30 30 255) (V4 255 255 255 255)
  let styles =
        (styleWith $ prefer (header headStyle) $ selectedAndBody selectedStyle bodyStyle) table
  let sizes = (alignWidths $ sizeWith $ extend (V2 20 20) $ copy fontSizes) table
  let positions =
        (positionWith $ indent selectedRow (V2 10 0) $ addGaps (V2 20 20) $ grid sizes) table
  let textPositions = (positionTextWith $ centerText sizes positions fontSizes) table
  return $ merge table styles sizes positions textPositions

renderRectCell :: SDL.Renderer -> CellRenderer CellStyle
renderRectCell renderer cell = do
  let text'   = text cell
  let style'  = style cell
  let size'   = convertV2 $ size cell
  let pos = position cell
  let textPos = textPosition cell
  SDL.rendererDrawColor renderer SDL.$= cellBackground style'
  SDL.fillRect renderer $ Just $ SDL.Rectangle (SDL.P $ convertV2 pos) size'
  renderText renderer (cellFont style') (cellForeground style') (convertV2 textPos) text'
