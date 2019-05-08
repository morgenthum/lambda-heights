module SDL.GUI.Table.Example where

import           Data.Matrix
import           LambdaHeights.RenderContext
import           Linear.V2
import           Linear.V4
import qualified SDL
import qualified SDL.Font                    as SDLF
import qualified SDL.GUI.Table.CellLocators  as Locate
import           SDL.GUI.Table.CellRenderer
import qualified SDL.GUI.Table.CellSizers    as Size
import qualified SDL.GUI.Table.CellStyler    as Style
import           SDL.GUI.Table.TableRenderer
import qualified SDL.GUI.Table.TableUpdater  as Update
import           SDL.GUI.Table.Types

testContent :: [[String]]
testContent =
  [ ["header1", "header2"]
  , ["value11", "value12"]
  , ["value21", "value22"]
  , ["value31", "value32"]
  , ["value41", "value42"]
  , ["value51", "value52"]
  ]

newTable :: [[String]] -> Table
newTable xs = Table {content = fromLists xs, selected = V2 1 1}

selectedStyle :: SDLF.Font -> CellStyle
selectedStyle f = CellStyle f (V4 60 60 60 255) (V4 255 255 255 30)

bodyStyle :: SDLF.Font -> CellStyle
bodyStyle f = CellStyle f (V4 255 255 255 255) (V4 0 0 0 30)

renderTableBuilder :: SDL.Window -> SDL.Renderer -> SDLF.Font -> RenderTable
renderTableBuilder window renderer font table = do
  fontSizes <- Size.loadFontSizes font $ content table
  let styleCells  = Style.with $ Style.selectedAndBody (selectedStyle font) (bodyStyle font)
  let sizeCells = Size.alignWidths $ Size.with $ Size.extend (V2 20 20) $ Size.deriveFrom fontSizes
  let locateCells = Locate.with $ Locate.indentSelected 5 Locate.grid
  (_, sizes, positions) <- applyGenerators styleCells sizeCells locateCells table
  tablePos              <- calcTablePos window sizes positions
  let renderCell = renderSimpleCell renderer tablePos $ locateTextCentered fontSizes
  renderWith styleCells sizeCells locateCells renderCell table

start :: IO ()
start = do
  (window, renderer) <- newContext "SDL.GUI"
  font               <- SDLF.load "retro_gaming.ttf" 11
  let table  = newTable testContent
  let render = renderTableBuilder window renderer font
  let update = Update.with Update.toSelectEvent $ Update.applySelectEvent Update.limitAll
  loop renderer render update table
  deleteContext (window, renderer)

loop :: SDL.Renderer -> RenderTable -> UpdateTable -> Table -> IO ()
loop renderer render update table = do
  SDL.rendererDrawColor renderer SDL.$= V4 0 0 0 255
  SDL.clear renderer
  render table
  SDL.present renderer
  events <- SDL.pollEvents
  loop renderer render update $ update events table
