module LambdaHeights.Score
  ( update
  , render
  )
where

import qualified LambdaHeights.Menu             as Menu
import           LambdaHeights.Render
import           LambdaHeights.RenderContext
import qualified LambdaHeights.Table            as Table
import qualified LambdaHeights.Types.Loop       as Loop
import qualified LambdaHeights.Types.ScoreState as Score
import qualified LambdaHeights.Types.Timer      as Timer
import           Linear.V4
import qualified SDL

update :: Loop.Update Score.State () [SDL.Event]
update events = do
  state <- Loop.getState
  case Menu.updateDefault (const ()) events $ Score.menu state of
    Left  _    -> Loop.putResult ()
    Right menu -> Loop.putState $ state { Score.menu = menu }

render :: RenderContext -> Menu.RenderConfig -> Timer.LoopTimer -> Score.State -> IO ()
render ctx config _ state = do
  renderOverlay ctx $ V4 0 0 0 100
  view <- Table.newMenuView (Menu.font config) $ Score.menu state
  Menu.render ctx config view
