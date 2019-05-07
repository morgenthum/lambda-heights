module SDL.GUI.Table.Example where

import           Data.Matrix
import           LambdaHeights.RenderContext
import           Linear.V2
import           Linear.V4
import qualified SDL
import qualified SDL.Font                        as SDLF
import           SDL.GUI.Table.RenderCombinators
import           SDL.GUI.Table.Types
import           SDL.GUI.Table.UpdateCombinators

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

headerStyle :: SDLF.Font -> CellStyle
headerStyle f = CellStyle f (V4 0 255 0 255) (V4 0 0 0 30)

selectedStyle :: SDLF.Font -> CellStyle
selectedStyle f = CellStyle f (V4 60 60 60 255) (V4 255 255 255 30)

defaultStyle :: SDLF.Font -> CellStyle
defaultStyle f = CellStyle f (V4 255 255 255 255) (V4 0 0 0 30)

renderExampleTable :: SDL.Window -> SDL.Renderer -> SDLF.Font -> RenderTable
renderExampleTable window renderer font table = do
  V2 wW wH <- SDL.get $ SDL.windowSize window
  let widthMid  = round $ realToFrac wW / 2
  let heightMid = round $ realToFrac wH / 2
  fontSizes <- loadFontSizes font $ content table
  let styleCells = styleCellsWith $ colorsSelectedBody (selectedStyle font, defaultStyle font)
  let sizeCells = alignWidths $ sizeCellsWith $ extendSize (V2 20 20) $ sizeFromFontSize fontSizes
  let locateCells = locateCellsWith $ indentSelected 5 grid
  (_, sizes, positions) <- applyGenerators styleCells sizeCells locateCells table
  let V2 w h     = tableSize positions sizes
  let tablePos = V2 (widthMid - round (realToFrac w / 2)) (heightMid - round (realToFrac h / 2))
  let renderCell = renderSimpleCell renderer tablePos $ locateTextCentered fontSizes
  renderWith styleCells sizeCells locateCells renderCell table

start :: IO ()
start = do
  (window, renderer) <- newContext "SDL.GUI"
  font               <- SDLF.load "retro_gaming.ttf" 11
  let table  = newTable testContent
  let render = renderExampleTable window renderer font
  let update = updateWith convertToSelectEvent $ applySelectEvent limitAll
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
