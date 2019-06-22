module LambdaHeights.Menu where

import qualified Control.Monad.IO.Class      as M
import qualified Data.Text                   as T
import           LambdaHeights.Render
import           LambdaHeights.RenderContext
import           LambdaHeights.Resources
import qualified LambdaHeights.Table         as Table
import qualified LambdaHeights.Types.Table   as Table
import           LambdaHeights.Version
import           Linear.V2
import           Linear.V2.Utils
import           Linear.V4
import qualified SDL
import qualified SDL.Font                    as SDLF

type ToResult a = Maybe String -> a

data RenderConfig = RenderConfig {
  font        :: SDLF.Font,
  versionFont :: SDLF.Font
}

createConfig :: (M.MonadIO m) => m RenderConfig
createConfig = RenderConfig <$> highSchoolUSASansFont 28 <*> retroGamingFont 11

deleteConfig :: (M.MonadIO m) => RenderConfig -> m ()
deleteConfig config = do
  SDLF.free $ font config
  SDLF.free $ versionFont config

keyInput :: (M.MonadIO m) => m [SDL.Event]
keyInput = SDL.pollEvents

updateDefault :: ToResult a -> [SDL.Event] -> Table.Table -> Either a Table.Table
updateDefault = update $ Table.with Table.convertKeycode $ Table.applyKeycode Table.limitAll

update :: Table.UpdateTable -> ToResult a -> [SDL.Event] -> Table.Table -> Either a Table.Table
update updater toResult events table =
  let table' = updater events table
  in  if pressedKey SDL.KeycodeReturn events || pressedKey SDL.KeycodeSpace events
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

render :: (M.MonadIO m) => RenderContext -> RenderConfig -> Table.TableView -> m ()
render (window, renderer) config view = do
  parentSize <- convert <$> SDL.get (SDL.windowSize window)
  let childSize = Table.tableSize view
  let pos       = Table.positionCenter parentSize childSize
  let view'     = Table.translate pos view
  Table.renderTable (Table.renderCell renderer) view'
  renderVersion (window, renderer) config

renderVersion :: (M.MonadIO m) => RenderContext -> RenderConfig -> m ()
renderVersion (window, renderer) config = do
  let version = "version " ++ show currentVersion
  V2 _ h  <- SDL.get $ SDL.windowSize window
  (_, th) <- SDLF.size (versionFont config) $ T.pack version
  let y = h - fromIntegral th - 10
  renderText renderer (versionFont config) (V4 90 90 90 255) (V2 10 y) version
