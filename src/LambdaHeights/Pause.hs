module LambdaHeights.Pause where

import           Data.Word
import qualified LambdaHeights.Menu             as Menu
import           LambdaHeights.Render
import           LambdaHeights.RenderContext
import qualified LambdaHeights.Table            as Table
import qualified LambdaHeights.Types.PauseState as Pause
import qualified LambdaHeights.Types.Timer      as Timer
import           Linear.V2
import           Linear.V4
import qualified SDL

type ProxyRenderer a = Timer.LoopTimer -> a -> IO ()

data RenderConfig = RenderConfig {
  menuConfig   :: Menu.RenderConfig,
  overlayColor :: V4 Word8
}

createConfig :: IO RenderConfig
createConfig = do
  menuConfig <- Menu.createConfig
  return $ RenderConfig menuConfig $ V4 0 0 0 100

deleteConfig :: RenderConfig -> IO ()
deleteConfig = Menu.deleteConfig . menuConfig

update :: Timer.LoopTimer -> [SDL.Event] -> Pause.State a -> Either Pause.ExitReason (Pause.State a)
update _ events state =
  let updated = Menu.updateDefault toState events $ Pause.menu state
  in  case updated of
        Left  result -> Left result
        Right menu   -> Right $ state { Pause.menu = menu }

toState :: Maybe String -> Pause.ExitReason
toState (Just "exit") = Pause.Exit
toState _             = Pause.Resume

render
  :: RenderContext -> RenderConfig -> ProxyRenderer a -> Timer.LoopTimer -> Pause.State a -> IO ()
render (window, renderer) config proxyRenderer timer state =
  renderFrame renderer (V4 0 0 0 255) $ do
    proxyRenderer timer $ Pause.pausedState state
    renderOverlay (window, renderer) config
    renderMenu (window, renderer) config state

renderOverlay :: RenderContext -> RenderConfig -> IO ()
renderOverlay (window, renderer) config = do
  windowSize <- SDL.get $ SDL.windowSize window
  SDL.rendererDrawColor renderer SDL.$= overlayColor config
  SDL.fillRect renderer $ Just $ SDL.Rectangle (SDL.P $ V2 0 0) windowSize

renderMenu :: RenderContext -> RenderConfig -> Pause.State a -> IO ()
renderMenu ctx config state = do
  let font = Menu.font $ menuConfig config
  Table.newMenuView font (Pause.menu state) >>= Menu.render ctx (menuConfig config)
