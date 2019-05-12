module LambdaHeights.Types.PauseState where

import           Data.Matrix
import           LambdaHeights.GUI.Table.Types
import           Linear.V2

data State a = State {
  menuState  :: a,
  menu   :: Table,
  reason :: Maybe ExitReason
}

data ExitReason = Exit | Resume

newState :: a -> State a
newState s =
  State {menuState = s, menu = Table (fromLists [["resume"], ["exit"]]) (V2 1 1), reason = Nothing}
