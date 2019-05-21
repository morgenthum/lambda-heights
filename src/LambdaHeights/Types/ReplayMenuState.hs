module LambdaHeights.Types.ReplayMenuState where

import           Data.Matrix
import           Graphics.UI.Types.Table
import           Linear.V2

newtype State = State {
  table  :: Table
}

newState :: [[String]] -> State
newState xs = State {table = Table (fromLists xs) (V2 1 1) Nothing}
