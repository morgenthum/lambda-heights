module LambdaTower.Score.Update where

import qualified LambdaTower.Score.State       as Score
import qualified LambdaTower.Timing.Timer      as Timer
import qualified LambdaTower.Types.ButtonList  as ButtonList
import qualified LambdaTower.Types.KeyEvents   as Events

update :: Timer.LoopTimer -> [Events.KeyEvent] -> Score.State -> IO (Either () Score.State)
update _ events state = do
  let list = ButtonList.ensureValidIndex $ ButtonList.applyEvents (Score.buttonList state) events
  return $ if ButtonList.action list then Left () else Right state

wrap :: Score.State -> ButtonList.ButtonList -> Score.State
wrap state buttonList = state { Score.buttonList = buttonList }
