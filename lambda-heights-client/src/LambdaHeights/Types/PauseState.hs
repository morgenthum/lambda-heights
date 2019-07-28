module LambdaHeights.Types.PauseState where

import           LambdaHeights.Types.Table
import           Linear.V2

data State s = State {
  pausedState :: s,
  menu        :: Table,
  reason      :: Maybe ExitReason
}

data ExitReason = Exit | Resume

newState :: s -> State s
newState state = State state (newTable [["resume"], ["exit"]] (V2 1 1)) Nothing
