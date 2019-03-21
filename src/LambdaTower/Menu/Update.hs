module LambdaTower.Menu.Update (
  update
) where

import LambdaTower.Loop

import qualified LambdaTower.Components.Button as B
import qualified LambdaTower.Components.ButtonList as BL
import qualified LambdaTower.Components.Events as E
import qualified LambdaTower.Menu.MenuState as M
import qualified LambdaTower.State as S

update :: Updater IO M.MenuState S.State [E.KeyEvent]
update _ events state = do
  let buttonList = BL.ensureValidIndex $ E.applyEvents (M.buttonList state) events
  return $
    if BL.action buttonList
      then Left $ stateByButton $ BL.selectedButton buttonList
      else Right $ wrap state buttonList

wrap :: M.MenuState -> BL.ButtonList -> M.MenuState
wrap state buttonList = state { M.buttonList = buttonList }

stateByButton :: B.Button -> S.State
stateByButton button =
  case B.id button of
    0 -> S.Ingame
    1 -> S.Replay
    2 -> S.Exit
    _ -> S.Menu