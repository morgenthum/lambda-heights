module LambdaHeights.Menu where

import qualified Graphics.UI.Table.Render         as GUI
import qualified Graphics.UI.Table.Transformators as GUI
import qualified Graphics.UI.Table.Update         as GUI
import qualified Graphics.UI.Types.Table          as GUI
import           LambdaHeights.RenderContext
import qualified LambdaHeights.Table              as Table
import qualified SDL
import qualified SDL.Font                         as SDLF

type ToResult a = Maybe String -> a

newtype RenderConfig = RenderConfig {
  font :: SDLF.Font
}

createConfig :: IO RenderConfig
createConfig = RenderConfig <$> SDLF.load "HighSchoolUSASans.ttf" 28

deleteConfig :: RenderConfig -> IO ()
deleteConfig = SDLF.free . font

keyInput :: IO [SDL.Event]
keyInput = SDL.pollEvents

updateDefault :: ToResult a -> [SDL.Event] -> GUI.Table -> Either a GUI.Table
updateDefault = update $ GUI.with GUI.toSelectEvent $ GUI.applySelectEvent GUI.limitAll

update :: GUI.UpdateTable -> ToResult a -> [SDL.Event] -> GUI.Table -> Either a GUI.Table
update updater toResult events table =
  let table' = updater events table
  in  if pressedKey SDL.KeycodeReturn events
        then Left $ toResult $ Just $ GUI.selectedValue table'
        else if pressedKey SDL.KeycodeEscape events then Left $ toResult Nothing else Right table'

pressedKey :: SDL.Keycode -> [SDL.Event] -> Bool
pressedKey code = any (isPressedKeycode code)

isPressedKeycode :: SDL.Keycode -> SDL.Event -> Bool
isPressedKeycode code event = case SDL.eventPayload event of
  SDL.KeyboardEvent keyEvent ->
    let actualCode = SDL.keysymKeycode $ SDL.keyboardEventKeysym keyEvent
        motion     = SDL.keyboardEventKeyMotion keyEvent
    in  code == actualCode && motion == SDL.Pressed
  _ -> False

render :: RenderContext -> GUI.TableView Table.CellStyle -> IO ()
render (window, renderer) view = do
  pos <- GUI.calcCenterPosition window $ GUI.tableSize view
  let view' = GUI.translateTable pos view
  GUI.renderTable (Table.renderRectCell renderer) view'
