module LambdaHeights.Types.ReplayMenuState where

import           Data.Matrix
import           LambdaHeights.GUI.Table.Types
import           Linear.V2

data State = State {
  table  :: Table,
  action :: Bool
}

newState :: [[String]] -> State
newState xs = State {table = Table (fromLists xs) (V2 1 1), action = False}
