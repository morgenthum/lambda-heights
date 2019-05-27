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
  font       :: SDLF.Font,
  background :: V4 Word8,
  foreground :: V4 Word8
}

newMenuView :: SDLF.Font -> Table -> IO (TableView CellStyle)
newMenuView f t = do
  fontSizes <- loadFontSizes f $ content t
  let selectedStyle = CellStyle f (V4 30 30 30 255) (V4 0 191 255 255)
  let bodyStyle     = CellStyle f (V4 30 30 30 255) (V4 255 255 255 255)
  let styles        = (styleWith $ selectedAndBody selectedStyle bodyStyle) t
  let sizes         = (alignWidths $ sizeWith $ extend (V2 20 20) $ copy fontSizes) t
  let positions     = (locateWith $ indent selectedRow (V2 10 0) $ addGaps (V2 0 20) $ grid sizes) t
  let textPositions = (locateTextWith $ centerText sizes positions fontSizes) t
  return $ merge t styles sizes positions textPositions

newTableView :: SDLF.Font -> Table -> IO (TableView CellStyle)
newTableView f t = do
  fontSizes <- loadFontSizes f $ content t
  let headStyle     = CellStyle f (V4 0 191 255 255) (V4 30 30 30 255)
  let selectedStyle = CellStyle f (V4 30 30 30 255) (V4 0 191 255 255)
  let bodyStyle     = CellStyle f (V4 30 30 30 255) (V4 255 255 255 255)
  let styles = (styleWith $ prefer (header headStyle) $ selectedAndBody selectedStyle bodyStyle) t
  let sizes         = (alignWidths $ sizeWith $ extend (V2 20 20) $ copy fontSizes) t
  let positions = (locateWith $ indent selectedRow (V2 10 0) $ addGaps (V2 20 20) $ grid sizes) t
  let textPositions = (locateTextWith $ centerText sizes positions fontSizes) t
  return $ merge t styles sizes positions textPositions

renderRectCell :: SDL.Renderer -> CellRenderer CellStyle
renderRectCell renderer cell = do
  let text'   = text cell
  let style'  = style cell
  let size'   = convertV2 $ size cell
  let pos     = position cell
  let textPos = textPosition cell
  SDL.rendererDrawColor renderer SDL.$= background style'
  SDL.fillRect renderer $ Just $ SDL.Rectangle (SDL.P $ convertV2 pos) size'
  renderText renderer (font style') (foreground style') (convertV2 textPos) text'
