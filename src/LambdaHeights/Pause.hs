module LambdaHeights.Pause where

import qualified LambdaHeights.Menu             as Menu
import           LambdaHeights.Render
import           LambdaHeights.RenderContext
import qualified LambdaHeights.Table            as Table
import qualified LambdaHeights.Types.PauseState as Pause
import qualified LambdaHeights.Types.Timer      as Timer
import qualified SDL

type ProxyRenderer a = Timer.LoopTimer -> a -> IO ()

newtype RenderConfig = RenderConfig {
  menuConfig   :: Menu.RenderConfig
}

createConfig :: IO RenderConfig
createConfig = RenderConfig <$> Menu.createConfig

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
render (window, renderer) config proxyRenderer timer state = do
  proxyRenderer timer $ Pause.pausedState state
  renderOverlay (window, renderer)
  renderMenu (window, renderer) config state

renderMenu :: RenderContext -> RenderConfig -> Pause.State a -> IO ()
renderMenu ctx config state = do
  let font = Menu.font $ menuConfig config
  Table.newMenuView font (Pause.menu state) >>= Menu.render ctx (menuConfig config)
