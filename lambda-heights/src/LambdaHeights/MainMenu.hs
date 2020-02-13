module LambdaHeights.MainMenu
  ( update,
    render,
  )
where

import ComposeEngine.RenderContext
import qualified ComposeEngine.Types.Loop as Loop
import qualified Control.Monad.Reader as M
import qualified LambdaHeights.Menu as Menu
import qualified LambdaHeights.Table as Table
import qualified LambdaHeights.Types.GameState as Game
import qualified LambdaHeights.Types.MainMenuState as MainMenu
import LambdaHeights.Types.Widgets
import LambdaHeights.Vectors
import qualified SDL

update :: Loop.Update MainMenu.State Game.State [SDL.Event]
update events = do
  state <- Loop.getUpdateState
  case Menu.updateDefault toState events $ MainMenu.menu state of
    Left result -> Loop.putUpdateResult result
    Right menu -> Loop.putUpdateState $ state {MainMenu.menu = menu}

toState :: Maybe String -> Game.State
toState (Just "play") = Game.Play
toState (Just "replay") = Game.Replay
toState (Just "exit") = Game.Exit
toState _ = Game.Menu

render :: RenderContext -> Menu.RenderConfig -> Loop.Render IO MainMenu.State
render (window, renderer) config = do
  state <- Loop.askRenderState
  view <- M.lift $ Table.newMenuView (Menu.font config) (MainMenu.menu state)
  M.lift $ Menu.render (window, renderer) config view
--let colors = (SDL.V4 255 255 255 255, SDL.V4 255 0 0 255)
--let buttonRenderer = simpleButtonRenderer (Menu.font config) colors colors
--let button = Button "OK" False (SP (V2 100 100)) (SS (V2 250 75)) buttonRenderer
--M.lift $ buttonRenderer (window, renderer) button
