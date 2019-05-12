module LambdaHeights.MainMenu
  ( update
  , render
  )
where

import qualified LambdaHeights.Menu                as Menu
import           LambdaHeights.RenderContext
import qualified LambdaHeights.Types.GameState     as Game
import qualified LambdaHeights.Types.MainMenuState as MainMenu
import qualified LambdaHeights.Types.Timer         as Timer
import           Linear.V4
import qualified SDL

-- Update the menu.

update :: Timer.LoopTimer -> [SDL.Event] -> MainMenu.State -> Either Game.State MainMenu.State
update timer events state =
  let updated = Menu.update stateFromItem timer events $ MainMenu.menu state
  in  case updated of
        Left  result -> Left result
        Right menu   -> Right $ state { MainMenu.menu = menu }

stateFromItem :: String -> Game.State
stateFromItem "play"   = Game.Play
stateFromItem "replay" = Game.Replay
stateFromItem "exit"   = Game.Exit
stateFromItem _        = Game.Menu


-- Render the menu.

render :: RenderContext -> Menu.RenderConfig -> Timer.LoopTimer -> MainMenu.State -> IO ()
render (window, renderer) config timer state = do
  SDL.rendererDrawColor renderer SDL.$= V4 0 0 0 255
  SDL.clear renderer
  Menu.render (window, renderer) config timer $ MainMenu.menu state
  SDL.present renderer
