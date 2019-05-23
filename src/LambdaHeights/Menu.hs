module LambdaHeights.Menu where

import           Graphics.UI.Table.Render
import           Graphics.UI.Table.Transformators
import           Graphics.UI.Table.Update
import qualified Graphics.UI.Types.Table          as T
import           LambdaHeights.RenderContext
import qualified LambdaHeights.Table              as T
import           LambdaHeights.Types.Timer
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

updateDefault :: ToResult a -> [SDL.Event] -> T.Table -> Either a T.Table
updateDefault = update $ with toSelectEvent $ applySelectEvent limitAll

update :: T.UpdateTable -> ToResult a -> [SDL.Event] -> T.Table -> Either a T.Table
update updater toResult events table =
  let table' = updater events table
  in  if pressedKey SDL.KeycodeReturn events
        then Left $ toResult $ Just $ T.selectedValue table'
        else if pressedKey SDL.KeycodeEscape events then Left $ toResult Nothing else Right table'

pressedKey :: SDL.Keycode -> [SDL.Event] -> Bool
pressedKey code = any (isKeycode code)

isKeycode :: SDL.Keycode -> SDL.Event -> Bool
isKeycode code event = case SDL.eventPayload event of
  SDL.KeyboardEvent keyEvent ->
    let actualCode = SDL.keysymKeycode (SDL.keyboardEventKeysym keyEvent)
        motion     = SDL.keyboardEventKeyMotion keyEvent
    in  code == actualCode && motion == SDL.Pressed
  _ -> False

render :: RenderContext -> T.TableView T.CellStyle -> IO ()
render (window, renderer) view = do
  pos <- calcCenterPosition window $ T.tableSize view
  let view' = fmap (translate pos) view
  renderTable (T.renderRectCell renderer) view'
