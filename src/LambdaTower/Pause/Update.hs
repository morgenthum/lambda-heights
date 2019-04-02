module LambdaTower.Pause.Update where

import qualified LambdaTower.Pause.State       as Pause
import qualified LambdaTower.Timing.Timer      as Timer
import qualified LambdaTower.Types.Button      as Button
import qualified LambdaTower.Types.ButtonList  as ButtonList
import qualified LambdaTower.Types.KeyEvents   as Events

update
  :: Timer.LoopTimer
  -> [Events.KeyEvent]
  -> Pause.State a
  -> IO (Either Pause.ExitReason (Pause.State a))
update _ events state = do
  let list = ButtonList.ensureValidIndex $ ButtonList.applyEvents (Pause.buttonList state) events
  let newState = state { Pause.buttonList = list }
  return $ if ButtonList.action list
    then Left $ stateByButton $ ButtonList.selectedButton list
    else Right newState

stateByButton :: Button.Button -> Pause.ExitReason
stateByButton button = case Button.text button of
  "exit" -> Pause.Exit
  _      -> Pause.Resume
