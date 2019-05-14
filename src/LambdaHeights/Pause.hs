module LambdaHeights.Pause where

import           Data.Word
import qualified LambdaHeights.Menu             as Menu
import           LambdaHeights.RenderContext
import qualified LambdaHeights.Types.PauseState as Pause
import qualified LambdaHeights.Types.Timer      as Timer
import qualified SDL

update :: Timer.LoopTimer -> [SDL.Event] -> Pause.State a -> Either Pause.ExitReason (Pause.State a)
update timer events state =
  let updated = Menu.updateDefault stateFromItem timer events $ Pause.menu state
      stateFromItem "exit" = Pause.Exit
      stateFromItem _      = Pause.Resume
  in  case updated of
        Left  result -> Left result
        Right menu   -> Right $ state { Pause.menu = menu }

type ProxyRenderer a = Timer.LoopTimer -> a -> IO ()

data RenderConfig = RenderConfig {
  menuConfig   :: Menu.RenderConfig,
  overlayColor :: SDL.V4 Word8
}

defaultConfig :: IO RenderConfig
defaultConfig = do
  config <- Menu.createConfig
  return $ RenderConfig {menuConfig = config, overlayColor = SDL.V4 0 0 0 128}

deleteConfig :: RenderConfig -> IO ()
deleteConfig config = Menu.deleteConfig $ menuConfig config

render
  :: RenderContext -> RenderConfig -> ProxyRenderer a -> Timer.LoopTimer -> Pause.State a -> IO ()
render (window, renderer) config proxyRenderer timer state = do
  proxyRenderer timer $ Pause.menuState state
  renderOverlay (window, renderer) config
  let table = Pause.menu state
  view <- Menu.defaultView (menuConfig config) table
  Menu.render (window, renderer) timer table view
  SDL.present renderer

renderOverlay :: RenderContext -> RenderConfig -> IO ()
renderOverlay (window, renderer) config = do
  windowSize <- SDL.get $ SDL.windowSize window
  SDL.rendererDrawColor renderer SDL.$= overlayColor config
  SDL.fillRect renderer $ Just $ SDL.Rectangle (SDL.P $ SDL.V2 0 0) windowSize
