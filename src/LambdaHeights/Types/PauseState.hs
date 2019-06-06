module LambdaHeights.Types.PauseState where

import           LambdaHeights.Types.Table
import           Linear.V2

data State a = State {
  pausedState :: a,
  menu        :: Table,
  reason      :: Maybe ExitReason
}

data ExitReason = Exit | Resume

newState :: a -> State a
newState s =
  State {pausedState = s, menu = newTable [["resume"], ["exit"]] (V2 1 1), reason = Nothing}
