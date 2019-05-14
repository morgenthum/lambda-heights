module LambdaHeights.Menu where

import           Data.Matrix
import           LambdaHeights.RenderContext
import qualified LambdaHeights.Table.Combinators as Table
import qualified LambdaHeights.Table.Render      as GUI
import qualified LambdaHeights.Table.Update      as Update
import qualified LambdaHeights.Types.Table       as GUI
import qualified LambdaHeights.Types.Timer       as Timer
import           Linear.V2
import           Linear.V4
import qualified SDL
import qualified SDL.Font                        as SDLF

type ToResult a = String -> a

newtype RenderConfig = RenderConfig {
  font :: SDLF.Font
}

keyInput :: IO [SDL.Event]
keyInput = SDL.pollEvents

updateDefault :: ToResult a -> Timer.LoopTimer -> [SDL.Event] -> GUI.Table -> Either a GUI.Table
updateDefault = update $ Update.with Update.toSelectEvent $ Update.applySelectEvent Update.limitAll

update
  :: GUI.UpdateTable
  -> ToResult a
  -> Timer.LoopTimer
  -> [SDL.Event]
  -> GUI.Table
  -> Either a GUI.Table
update updater toResult _ events table =
  let table' = updater events table
  in  if pressedKey SDL.KeycodeReturn events
        then Left $ toResult $ GUI.selectedValue table'
        else if pressedKey SDL.KeycodeEscape events then Left $ toResult "" else Right table'

pressedKey :: SDL.Keycode -> [SDL.Event] -> Bool
pressedKey code = any (isKeycode code)

isKeycode :: SDL.Keycode -> SDL.Event -> Bool
isKeycode code event = case SDL.eventPayload event of
  SDL.KeyboardEvent keyEvent ->
    let actualCode = SDL.keysymKeycode (SDL.keyboardEventKeysym keyEvent)
        motion     = SDL.keyboardEventKeyMotion keyEvent
    in  code == actualCode && motion == SDL.Pressed
  _ -> False

defaultView :: RenderConfig -> GUI.Table -> IO GUI.TableView
defaultView config table = do
  fontSizes <- Table.loadFontSizes (font config) $ GUI.content table
  let styles        = newStyler (font config) table
  let sizes         = newSizer fontSizes table
  let positions     = newPositioner sizes table
  let textPositions = newTextPositioner sizes positions fontSizes table
  return $ GUI.TableView styles sizes positions textPositions

render :: RenderContext -> Timer.LoopTimer -> GUI.Table -> GUI.TableView -> IO ()
render (window, renderer) _ table view = do
  tablePos <- GUI.calcTablePos window $ GUI.tableSize view
  GUI.renderTable (GUI.renderRectCell renderer tablePos) table view

newStyler :: SDLF.Font -> GUI.CellStyler
newStyler f =
  let selectedStyle = GUI.CellStyle f (V4 30 30 30 255) (V4 0 191 255 255)
      bodyStyle     = GUI.CellStyle f (V4 30 30 30 255) (V4 255 255 255 255)
  in  Table.styleWith $ Table.selectedAndBody selectedStyle bodyStyle

newSizer :: Matrix GUI.Size -> GUI.CellSizer
newSizer fontSizes =
  Table.alignWidths $ Table.sizeWith $ Table.extend (V2 20 20) $ Table.copy fontSizes

newPositioner :: Matrix GUI.Size -> GUI.CellPositioner
newPositioner sm =
  Table.positionWith $ Table.indentSelected 10 $ Table.addGaps (V2 20 20) $ Table.grid sm

newTextPositioner :: Matrix GUI.Size -> Matrix GUI.Position -> Matrix GUI.Size -> GUI.TextPositioner
newTextPositioner sm pm fontSizes = Table.positionTextWith $ Table.centerText sm pm fontSizes

createConfig :: IO RenderConfig
createConfig = RenderConfig <$> SDLF.load "HighSchoolUSASans.ttf" 28

deleteConfig :: RenderConfig -> IO ()
deleteConfig = SDLF.free . font
