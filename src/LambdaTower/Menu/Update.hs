module LambdaTower.Menu.Update where

import LambdaTower.Loop

import qualified LambdaTower.Menu.Events as E
import qualified LambdaTower.Menu.MenuState as M
import qualified LambdaTower.State as S

update :: Updater IO M.MenuState S.State [E.KeyEvent]
update events state = do
  let newState = applyEvents state events
  return $
    if M.action newState
      then Right $ stateByButton . selectedButton $ newState
      else Left newState

applyEvents :: M.MenuState -> [E.KeyEvent] -> M.MenuState
applyEvents = foldl applyEvent

applyEvent :: M.MenuState -> E.KeyEvent -> M.MenuState
applyEvent state E.Up = state { M.selected = selectedId' }
  where selectedId = M.selected state
        selectedId' = if selectedId > 0 then selectedId - 1 else selectedId
applyEvent state E.Down = state { M.selected = selectedId' }
  where selectedId = M.selected state
        buttonCount = length $ M.buttons state
        selectedId' = if selectedId < buttonCount - 1 then selectedId + 1 else selectedId
applyEvent state E.Enter = state { M.action = True}

selectedButton :: M.MenuState -> M.Button
selectedButton state = M.buttons state !! M.selected state

stateByButton :: M.Button -> S.State
stateByButton button =
  case M.id button of
    0 -> S.Ingame
    1 -> S.Replay
    2 -> S.Exit
    _ -> S.Menu