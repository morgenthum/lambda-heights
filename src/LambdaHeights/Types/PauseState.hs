module LambdaHeights.Types.PauseState where

import           Data.Matrix
import           LambdaHeights.GUI.Table.Types
import           Linear.V2

data State a = State {
  state  :: a,
  menu   :: Table,
  reason :: Maybe ExitReason
}

data ExitReason = Exit | Resume

newState :: a -> State a
newState s =
  State {state = s, menu = Table (fromLists [["resume"], ["exit"]]) (V2 1 1), reason = Nothing}
