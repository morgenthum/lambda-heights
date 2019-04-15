module LambdaHeights.Pause where

import           Data.Word
import qualified LambdaHeights.Menu             as Menu
import           LambdaHeights.RenderContext
import qualified LambdaHeights.Types.KeyEvents  as Events
import qualified LambdaHeights.Types.Label      as Label
import qualified LambdaHeights.Types.PauseState as Pause
import qualified LambdaHeights.Types.Timer      as Timer
import qualified SDL

-- Update the menu.

update
  :: Timer.LoopTimer
  -> [Events.KeyEvent]
  -> Pause.State a
  -> Either Pause.ExitReason (Pause.State a)
update timer events state =
  let updated = Menu.update stateFromItem timer events $ Pause.menu state
  in  case updated of
        Left  result -> Left result
        Right menu   -> Right $ state { Pause.menu = menu }

stateFromItem :: Label.Label -> Pause.ExitReason
stateFromItem item = case Label.text item of
  "exit" -> Pause.Exit
  _      -> Pause.Resume


-- Render the 'real' state using the proxy-renderer
-- and render the pause overlay on top.

type ProxyRenderer a = Timer.LoopTimer -> a -> IO ()

data RenderConfig = RenderConfig {
  menuConfig   :: Menu.RenderConfig,
  overlayColor :: SDL.V4 Word8
}

defaultConfig :: IO RenderConfig
defaultConfig = do
  config <- Menu.defaultConfig
  return $ RenderConfig {menuConfig = config, overlayColor = SDL.V4 0 0 0 128}

deleteConfig :: RenderConfig -> IO ()
deleteConfig config = Menu.deleteConfig $ menuConfig config

render
  :: RenderContext -> RenderConfig -> ProxyRenderer a -> Timer.LoopTimer -> Pause.State a -> IO ()
render ctx config proxyRenderer timer state = do
  proxyRenderer timer $ Pause.state state
  renderOverlay ctx config
  Menu.renderNoClear ctx (menuConfig config) timer $ Pause.menu state

renderOverlay :: RenderContext -> RenderConfig -> IO ()
renderOverlay (window, renderer) config = do
  windowSize <- SDL.get $ SDL.windowSize window
  SDL.rendererDrawColor renderer SDL.$= overlayColor config
  SDL.fillRect renderer $ Just $ SDL.Rectangle (SDL.P $ SDL.V2 0 0) windowSize
