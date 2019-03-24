module LambdaTower.Menu.Update (
  update
) where

import LambdaTower.Loop
import LambdaTower.State

import qualified LambdaTower.Types.Button as Button
import qualified LambdaTower.Types.ButtonList as ButtonList
import qualified LambdaTower.Types.KeyEvents as Events
import qualified LambdaTower.Types.MenuState as State

update :: Updater IO State.MenuState State [Events.KeyEvent]
update _ events state = do
  let buttonList = ButtonList.ensureValidIndex $ Events.applyEvents (State.buttonList state) events
  return $
    if ButtonList.action buttonList
      then Left $ stateByButton $ ButtonList.selectedButton buttonList
      else Right $ state { State.buttonList = buttonList }

stateByButton :: Button.Button -> State
stateByButton button =
  case Button.id button of
    0 -> Ingame
    1 -> Replay
    2 -> Exit
    _ -> Menu