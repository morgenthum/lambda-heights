module LambdaHeights.Menu where

import           LambdaHeights.RenderContext
import qualified LambdaHeights.Table         as Table
import qualified LambdaHeights.Types.Table   as Table
import           Linear.V2.Utils
import qualified SDL
import qualified SDL.Font                    as SDLF

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

updateDefault :: ToResult a -> [SDL.Event] -> Table.Table -> Either a Table.Table
updateDefault = update $ Table.with Table.toKeycode $ Table.applyKeycode Table.limitAll

update :: Table.UpdateTable -> ToResult a -> [SDL.Event] -> Table.Table -> Either a Table.Table
update updater toResult events table =
  let table' = updater events table
  in  if pressedKey SDL.KeycodeReturn events
        then Left $ toResult $ Just $ Table.cellText $ Table.selectedValue table'
        else if pressedKey SDL.KeycodeEscape events then Left $ toResult Nothing else Right table'

pressedKey :: SDL.Keycode -> [SDL.Event] -> Bool
pressedKey code = any $ isPressedKeycode code

isPressedKeycode :: SDL.Keycode -> SDL.Event -> Bool
isPressedKeycode code event = case SDL.eventPayload event of
  SDL.KeyboardEvent keyEvent ->
    let actualCode = SDL.keysymKeycode $ SDL.keyboardEventKeysym keyEvent
        motion     = SDL.keyboardEventKeyMotion keyEvent
    in  code == actualCode && motion == SDL.Pressed
  _ -> False

render :: RenderContext -> Table.TableView -> IO ()
render (window, renderer) view = do
  parentSize <- convert <$> SDL.get (SDL.windowSize window)
  let childSize = Table.tableSize view
  let pos       = Table.positionCenter parentSize childSize
  let view'     = Table.translate pos view
  Table.renderTable (Table.renderCell renderer) view'
