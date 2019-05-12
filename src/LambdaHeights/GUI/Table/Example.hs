module LambdaHeights.GUI.Table.Example where

import           Data.Matrix
import qualified LambdaHeights.GUI.Table.CellLocators  as Locate
import           LambdaHeights.GUI.Table.CellRenderer
import qualified LambdaHeights.GUI.Table.CellSizers    as Size
import qualified LambdaHeights.GUI.Table.CellStyler    as Style
import           LambdaHeights.GUI.Table.TableRenderer
import qualified LambdaHeights.GUI.Table.TableUpdater  as Update
import qualified LambdaHeights.GUI.Table.TextLocators  as TextLocate
import           LambdaHeights.GUI.Table.Types
import           LambdaHeights.RenderContext
import           Linear.V2
import           Linear.V4
import qualified SDL
import qualified SDL.Font                              as SDLF

testContent :: [[String]]
testContent =
  [ ["header1", "header2"]
  , ["value11", "value12"]
  , ["value21", "value22"]
  , ["value31", "value32"]
  , ["value41", "value42"]
  , ["value51", "value52"]
  ]

selectedStyle :: SDLF.Font -> CellStyle
selectedStyle f = CellStyle f (V4 60 60 60 255) (V4 255 255 255 30)

bodyStyle :: SDLF.Font -> CellStyle
bodyStyle f = CellStyle f (V4 255 255 255 255) (V4 0 0 0 30)

newTable :: [[String]] -> Table
newTable xs = Table {content = fromLists xs, selected = V2 1 1}

newGenerators :: SDLF.Font -> Table -> IO (TableViewGenerators CellStyle)
newGenerators font table = do
  fontSizes <- Size.loadFontSizes font $ content table
  return $ TableViewGenerators
    { styleCells    = Style.with $ Style.selectedAndBody (selectedStyle font) (bodyStyle font)
    , sizeCells     = Size.alignWidths $ Size.with $ Size.extend (V2 20 20) $ Size.copy fontSizes
    , positionCells = Locate.with $ Locate.indentSelected 5 Locate.grid
    , positionTexts = TextLocate.with $ TextLocate.center fontSizes
    }

buildTableRenderer :: RenderContext -> TableRenderer CellStyle
buildTableRenderer (window, renderer) table view = do
  tablePos <- calcTablePos window $ tableSize view
  renderTable (renderRectCell renderer tablePos) table view

start :: IO ()
start = do
  (window, renderer) <- newContext "SDL.GUI"
  font               <- SDLF.load "retro_gaming.ttf" 11
  let table = newTable testContent
  generators <- newGenerators font table
  let update = Update.with Update.toSelectEvent $ Update.applySelectEvent Update.limitAll
  loop (window, renderer) update table generators
  deleteContext (window, renderer)

loop :: RenderContext -> UpdateTable -> Table -> TableViewGenerators CellStyle -> IO ()
loop (window, renderer) update table generators = do
  SDL.rendererDrawColor renderer SDL.$= V4 0 0 0 255
  SDL.clear renderer
  let view   = generateView generators table
  let render = buildTableRenderer (window, renderer)
  render table view
  SDL.present renderer
  events <- SDL.pollEvents
  loop (window, renderer) update (update events table) generators
