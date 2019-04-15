module LambdaHeights.Score where

import qualified LambdaHeights.Menu             as Menu
import           LambdaHeights.RenderContext
import qualified LambdaHeights.Types.KeyEvents  as Events
import qualified LambdaHeights.Types.ScoreState as Score
import qualified LambdaHeights.Types.Timer      as Timer

update :: Timer.LoopTimer -> [Events.KeyEvent] -> Score.State -> Either () Score.State
update timer events state =
  let updated = Menu.update (const ()) timer events $ Score.menu state
  in  case updated of
        Left  _    -> Left ()
        Right menu -> Right $ state { Score.menu = menu }

render :: RenderContext -> Menu.RenderConfig -> Timer.LoopTimer -> Score.State -> IO ()
render ctx config timer state = Menu.render ctx config timer $ Score.menu state
