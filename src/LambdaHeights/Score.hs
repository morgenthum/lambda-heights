module LambdaHeights.Score
  ( update
  , render
  )
where

import qualified LambdaHeights.Menu             as Menu
import           LambdaHeights.Render
import           LambdaHeights.RenderContext
import qualified LambdaHeights.Table            as Table
import qualified LambdaHeights.Types.ScoreState as Score
import qualified LambdaHeights.Types.Timer      as Timer
import qualified LambdaHeights.Types.Loop as Loop
import qualified SDL

update :: Loop.Update Score.State () [SDL.Event]
update timer events state =
  let updated = Menu.updateDefault (const ()) events $ Score.menu state
  in  case updated of
        Left  _    -> (timer, Left ())
        Right menu -> (timer, Right $ state { Score.menu = menu })

render :: RenderContext -> Menu.RenderConfig -> Timer.LoopTimer -> Score.State -> IO ()
render ctx config _ state = do
  renderOverlay ctx
  view <- Table.newMenuView (Menu.font config) $ Score.menu state
  Menu.render ctx config view
