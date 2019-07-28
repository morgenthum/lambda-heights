module LambdaHeights.Types.ReplayMenuState where

import           LambdaHeights.Types.Table
import           Linear.V2

data State = State {
  table    :: Table,
  viewport :: TableViewport
}

newState :: Table -> State
newState t =
  let V2 _ cols = dimension t in State {table = t, viewport = TableViewport (V2 1 2) (V2 7 cols)}
