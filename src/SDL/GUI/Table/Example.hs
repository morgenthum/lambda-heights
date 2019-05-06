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
  [ ["header1", "headerheader2", "header3"]
  , ["value11", "value12", "value13"]
  , ["value21", "value22", "value23"]
  , ["value31", "value32", "value33"]
  , ["value41", "value42", "value43"]
  , ["value51", "value52", "value53"]
  ]

newTable :: [[String]] -> Table
newTable xs = Table {content = fromLists xs, selected = V2 2 1}

headerStyle :: SDLF.Font -> CellStyle
headerStyle f = CellStyle f (V4 0 255 0 255) (V4 0 0 0 255)

selectedStyle :: SDLF.Font -> CellStyle
selectedStyle f = CellStyle f (V4 60 60 60 255) (V4 255 255 255 255)

defaultStyle :: SDLF.Font -> CellStyle
defaultStyle f = CellStyle f (V4 255 255 255 255) (V4 0 0 0 255)

renderExampleTable :: SDL.Renderer -> SDLF.Font -> RenderTable
renderExampleTable renderer font table = do
  fontSizes <- loadFontSizes font $ content table
  let styles = (headerStyle font, selectedStyle font, defaultStyle font)
  renderWith (styleCellsWith $ colorsHeadSelectedBody styles)
             (alignWidths $ sizeCellsWith $ extend (V2 20 20) $ fontSize fontSizes)
             (locateCellsWith $ indentSelected 10 $ addGaps (V2 5 5) grid)
             (renderSimpleCell renderer $ locateTextCentered fontSizes)
             table

start :: IO ()
start = do
  (window, renderer) <- newContext "SDL.GUI"
  font               <- SDLF.load "retro_gaming.ttf" 11
  let table  = newTable testContent
  let render = renderExampleTable renderer font
  let update = updateWith convertToSelectEvent $ applySelectEvent $ limitNotFirst limitAll
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
