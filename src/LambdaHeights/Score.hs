module LambdaHeights.Score where

import           LambdaHeights.Graphics
import qualified LambdaHeights.Menu                      as Menu
import qualified LambdaHeights.Types.KeyEvents           as Events
import qualified LambdaHeights.Types.ScoreState          as Score
import qualified LambdaHeights.Types.Timer               as Timer

-- Refresh the menu.

update :: Timer.LoopTimer -> [Events.KeyEvent] -> Score.State -> Either () Score.State
update timer events state =
  let updated = Menu.update (const ()) timer events $ Score.menu state
  in  case updated of
        Left  _    -> Left ()
        Right menu -> Right $ state { Score.menu = menu }


-- Render the score screen after game over.

render :: RenderContext -> Menu.RenderConfig -> Timer.LoopTimer -> Score.State -> IO ()
render ctx config timer state = Menu.render ctx config timer $ Score.menu state
