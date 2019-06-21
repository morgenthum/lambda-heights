module LambdaHeights.MainMenu
  ( update
  , render
  )
where

import qualified LambdaHeights.Menu                as Menu
import           LambdaHeights.RenderContext
import qualified LambdaHeights.Table               as Table
import qualified LambdaHeights.Types.GameState     as Game
import qualified LambdaHeights.Types.MainMenuState as MainMenu
import qualified LambdaHeights.Types.Timer         as Timer
import qualified LambdaHeights.Types.Loop as Loop
import qualified SDL

update :: Loop.Update MainMenu.State Game.State [SDL.Event]
update timer events state =
  let updated = Menu.updateDefault toState events $ MainMenu.menu state
  in  case updated of
        Left  result -> (timer, Left result)
        Right menu   -> (timer, Right $ state { MainMenu.menu = menu })

toState :: Maybe String -> Game.State
toState (Just "play"  ) = Game.Play
toState (Just "replay") = Game.Replay
toState (Just "exit"  ) = Game.Exit
toState _               = Game.Menu

render :: RenderContext -> Menu.RenderConfig -> Timer.LoopTimer -> MainMenu.State -> IO ()
render (window, renderer) config _ state = do
  view <- Table.newMenuView (Menu.font config) $ MainMenu.menu state
  Menu.render (window, renderer) config view
