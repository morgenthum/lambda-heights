module LambdaHeights.Menu where

import Data.Matrix
import qualified LambdaHeights.GUI.Table.CellLocators  as Locate
import qualified LambdaHeights.GUI.Table.CellRenderer  as GUI
import qualified LambdaHeights.GUI.Table.CellSizers    as Size
import qualified LambdaHeights.GUI.Table.CellStyler    as Style
import qualified LambdaHeights.GUI.Table.TableRenderer as GUI
import qualified LambdaHeights.GUI.Table.TableUpdater  as Update
import qualified LambdaHeights.GUI.Table.TextLocators  as TextLocate
import qualified LambdaHeights.GUI.Table.Types         as GUI
import           LambdaHeights.RenderContext
import qualified LambdaHeights.Types.Timer             as Timer
import           Linear.V2
import           Linear.V4
import qualified SDL
import qualified SDL.Font                              as SDLF

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

defaultView :: RenderConfig -> GUI.Table -> IO (GUI.TableView GUI.CellStyle)
defaultView config table = do
  fontSizes <- Size.loadFontSizes (font config) $ GUI.content table
  let styles = newStyler (font config) table
  let sizes = newSizer fontSizes table
  let positions = newPositioner sizes table
  let textPositions = newTextPositioner sizes positions fontSizes table
  return $ GUI.TableView styles sizes positions textPositions

render :: RenderContext -> Timer.LoopTimer -> GUI.Table -> GUI.TableView GUI.CellStyle -> IO ()
render (window, renderer) _ table view = do
  tablePos <- GUI.calcTablePos window $ GUI.tableSize view
  GUI.renderTable (GUI.renderRectCell renderer tablePos) table view

newStyler :: SDLF.Font -> GUI.CellStyler GUI.CellStyle
newStyler f =
  let selectedStyle = GUI.CellStyle f (V4 30 30 30 255) (V4 0 191 255 255)
      bodyStyle     = GUI.CellStyle f (V4 30 30 30 255) (V4 255 255 255 255)
  in Style.with $ Style.selectedAndBody selectedStyle bodyStyle

newSizer :: Matrix GUI.Size -> GUI.CellSizer
newSizer fontSizes = Size.alignWidths $ Size.with $ Size.extend (V2 20 20) $ Size.copy fontSizes

newPositioner :: Matrix GUI.Size -> GUI.CellPositioner
newPositioner sm = Locate.with
              $ Locate.indentSelected 10
              $ Locate.addGaps (V2 20 20)
              $ Locate.grid sm

newTextPositioner :: Matrix GUI.Size -> Matrix GUI.Position -> Matrix GUI.Size -> GUI.TextPositioner
newTextPositioner sm pm fontSizes = TextLocate.with $ TextLocate.center sm pm fontSizes

createConfig :: GUI.Table -> IO RenderConfig
createConfig table = RenderConfig <$> SDLF.load "HighSchoolUSASans.ttf" 28

deleteConfig :: RenderConfig -> IO ()
deleteConfig = SDLF.free . font
