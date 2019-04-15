module LambdaHeights.Types.PauseState where

import           LambdaHeights.Types.Label
import qualified LambdaHeights.Types.MenuState as Menu
import           Linear.V2

data State a = State {
  state  :: a,
  menu   :: Menu.State,
  reason :: Maybe ExitReason
}

data ExitReason = Exit | Resume

newState :: a -> State a
newState s = State
  { state  = s
  , menu   = Menu.newMenu
    [Label 0 "resume" (V2 500 550) AlignCenter, Label 1 "exit" (V2 500 450) AlignCenter]
  , reason = Nothing
  }
