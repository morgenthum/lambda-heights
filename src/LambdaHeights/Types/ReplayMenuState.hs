module LambdaHeights.Types.ReplayMenuState where

import           Data.Matrix
import           Linear.V2
import           SDL.GUI.Table.Types

data State = State {
  table  :: Table,
  action :: Bool
}

newState :: [[String]] -> State
newState xs = State {table = Table (fromLists xs) (V2 1 1), action = False}
