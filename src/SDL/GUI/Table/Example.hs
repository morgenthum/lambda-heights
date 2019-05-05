module SDL.GUI.Table.Example where

import           Control.Monad
import           Data.Matrix
import           SDL.GUI.Table.Combinators
import           SDL.GUI.Table.Types
import           LambdaHeights.RenderContext
import           Linear.V2
import           Linear.V4
import qualified SDL
import qualified SDL.Font                    as SDLF

testContent :: [[String]]
testContent =
  [ ["header1", "headerheader2", "header3"]
  , ["value11", "value12", "value13"]
  , ["value21", "value22", "value23"]
  , ["value31", "value32", "value33"]
  ]

newTable :: [[String]] -> Table
newTable xs = Table {content = fromLists xs, selected = V2 2 1}

headerStyle :: SDLF.Font -> CellStyle
headerStyle f = CellStyle f (V4 0 255 0 255) (V4 0 0 0 255)

selectedStyle :: SDLF.Font -> CellStyle
selectedStyle f = CellStyle f (V4 60 60 60 255) (V4 255 255 255 255)

defaultStyle :: SDLF.Font -> CellStyle
defaultStyle f = CellStyle f (V4 255 255 255 255) (V4 0 0 0 255)

newRenderer :: SDL.Renderer -> SDLF.Font -> Table -> IO ()
newRenderer renderer font table = do
  fontSizes <- loadFontSizes font $ content table
  let styles = (headerStyle font, selectedStyle font, defaultStyle font)
  renderWith (styleCellsWith $ colorsHeadSelectedBody styles)
             (alignWidths $ sizeCellsWith $ extend (V2 20 20) $ fontSize fontSizes)
             (locateCellsWith $ indentSelected 10 $ addGaps (V2 5 5) grid)
             (renderSimpleCell renderer)
             table

start :: IO ()
start = do
  (window, renderer) <- newContext "SDL.GUI"
  font               <- SDLF.load "retro_gaming.ttf" 11
  let table  = newTable testContent
  let render = newRenderer renderer font
  _ <- forever $ do
    SDL.rendererDrawColor renderer SDL.$= V4 0 0 0 255
    SDL.clear renderer
    render table
    SDL.present renderer
  deleteContext (window, renderer)
