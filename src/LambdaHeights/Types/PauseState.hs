module LambdaHeights.Types.PauseState where

import qualified LambdaHeights.Types.Menu     as Menu
import           LambdaHeights.Types.MenuItem

data State a = State {
  state  :: a,
  menu   :: Menu.Menu,
  reason :: Maybe ExitReason
}

data ExitReason = Exit | Resume

newState :: a -> State a
newState s = State
  { state  = s
  , menu   = Menu.newMenu [MenuItem 0 "resume" (500, 550), MenuItem 1 "exit" (500, 450)]
  , reason = Nothing
  }
