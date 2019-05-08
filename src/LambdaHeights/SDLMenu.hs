module LambdaHeights.SDLMenu where

import           LambdaHeights.RenderContext
import           Linear.V2
import           Linear.V4
import qualified SDL.Font                    as SDLF
import qualified SDL.GUI.Table.CellLocators  as Locate
import           SDL.GUI.Table.CellRenderer
import qualified SDL.GUI.Table.CellSizers    as Size
import qualified SDL.GUI.Table.CellStyler    as Style
import           SDL.GUI.Table.TableRenderer
import           SDL.GUI.Table.Types

renderTableBuilder :: RenderContext -> SDLF.Font -> RenderTable
renderTableBuilder (window, renderer) font table = do
  fontSizes <- Size.loadFontSizes font $ content table
  let selectedStyle = CellStyle font (V4 0 0 0 255) (V4 0 191 255 255)
  let bodyStyle     = CellStyle font (V4 0 0 0 255) (V4 255 255 255 255)
  let styleCells    = Style.with $ Style.selectedAndBody selectedStyle bodyStyle
  let sizeCells = Size.alignWidths $ Size.with $ Size.extend (V2 20 20) $ Size.deriveFrom fontSizes
  let locateCells   = Locate.with $ Locate.indentSelected 5 Locate.grid
  (_, sizes, positions) <- applyGenerators styleCells sizeCells locateCells table
  tablePos              <- calcTablePos window sizes positions
  let renderCell = renderSimpleCell renderer tablePos $ locateTextCentered fontSizes
  renderWith styleCells sizeCells locateCells renderCell table
