module LambdaHeights.MainMenu
  ( update
  , render
  )
where

import qualified LambdaHeights.Menu                as Menu
import           LambdaHeights.RenderContext
import qualified LambdaHeights.Types.GameState     as Game
import           LambdaHeights.Types.KeyEvents
import qualified LambdaHeights.Types.MainMenuState as MainMenu
import qualified LambdaHeights.Types.MenuItem      as MenuItem
import qualified LambdaHeights.Types.Timer         as Timer

-- Update the menu.

update :: Timer.LoopTimer -> [KeyEvent] -> MainMenu.State -> Either Game.State MainMenu.State
update timer events state =
  let updated = Menu.update stateFromItem timer events $ MainMenu.menu state
  in  case updated of
        Left  result -> Left result
        Right menu   -> Right $ state { MainMenu.menu = menu }

stateFromItem :: MenuItem.MenuItem -> Game.State
stateFromItem item = case MenuItem.text item of
  "play"   -> Game.Play
  "replay" -> Game.Replay
  "exit"   -> Game.Exit
  _        -> Game.Menu


-- Render the menu.

render :: RenderContext -> Menu.RenderConfig -> Timer.LoopTimer -> MainMenu.State -> IO ()
render ctx config timer state = Menu.render ctx config timer $ MainMenu.menu state
