module LambdaHeights.MainMenu
  ( update
  , render
  )
where

import qualified Control.Monad.Reader              as M
import qualified LambdaHeights.Menu                as Menu
import           LambdaHeights.RenderContext
import qualified LambdaHeights.Table               as Table
import qualified LambdaHeights.Types.GameState     as Game
import qualified LambdaHeights.Types.Loop          as Loop
import qualified LambdaHeights.Types.MainMenuState as MainMenu
import qualified SDL

update :: Loop.Update MainMenu.State Game.State [SDL.Event]
update events = do
  state <- Loop.getUpdateState
  case Menu.updateDefault toState events $ MainMenu.menu state of
    Left  result -> Loop.putUpdateResult result
    Right menu   -> Loop.putUpdateState $ state { MainMenu.menu = menu }

toState :: Maybe String -> Game.State
toState (Just "play"  ) = Game.Play
toState (Just "replay") = Game.Replay
toState (Just "exit"  ) = Game.Exit
toState _               = Game.Menu

render :: RenderContext -> Menu.RenderConfig -> Loop.Render IO MainMenu.State
render (window, renderer) config = do
  state <- Loop.askRenderState
  view  <- M.lift $ Table.newMenuView (Menu.font config) $ MainMenu.menu state
  M.lift $ Menu.render (window, renderer) config view
