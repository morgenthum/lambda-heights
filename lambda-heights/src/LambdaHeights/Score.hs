module LambdaHeights.Score
  ( update,
    render,
  )
where

import ComposeEngine.RenderContext
import qualified ComposeEngine.Types.Loop as Loop
import qualified Control.Monad.IO.Class as M
import qualified LambdaHeights.Menu as Menu
import LambdaHeights.Render
import qualified LambdaHeights.Table as Table
import qualified LambdaHeights.Types.ScoreState as Score
import Linear.V4
import qualified SDL

update :: Loop.Update Score.State () [SDL.Event]
update events = do
  state <- Loop.getUpdateState
  case Menu.updateDefault (const ()) events (Score.menu state) of
    Left _ -> Loop.putUpdateResult ()
    Right menu -> Loop.putUpdateState $ state {Score.menu = menu}

render :: (M.MonadIO m) => RenderContext -> Menu.RenderConfig -> Loop.Render m Score.State
render ctx config = do
  state <- Loop.askRenderState
  renderOverlay ctx (V4 0 0 0 100)
  view <- Table.newMenuView (Menu.font config) (Score.menu state)
  Menu.render ctx config view
