module LambdaTower.Pause.Update where

import LambdaTower.Loop

import qualified LambdaTower.Components.Button as B
import qualified LambdaTower.Components.ButtonList as BL
import qualified LambdaTower.Components.Events as E
import qualified LambdaTower.Pause.PauseState as PS

pauseUpdate :: Updater IO PS.PauseState PS.ExitReason [E.KeyEvent]
pauseUpdate _ events state = do
  let buttonList = BL.ensureValidIndex $ E.applyEvents (PS.buttonList state) events
  let newState = wrap state buttonList
  return $
    if BL.action buttonList
      then Left $ stateByButton $ BL.selectedButton buttonList
      else Right newState

wrap :: PS.PauseState -> BL.ButtonList -> PS.PauseState
wrap state buttonList = state { PS.buttonList = buttonList }

stateByButton :: B.Button -> PS.ExitReason
stateByButton button =
  case B.id button of
    0 -> PS.Resume
    1 -> PS.Exit