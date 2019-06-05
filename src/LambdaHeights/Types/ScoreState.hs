module LambdaHeights.Types.ScoreState where

import           LambdaHeights.Types.Table
import           Linear.V2

type Score = Int

data State = State {
  score :: Score,
  menu  :: Table
}

newState :: Int -> State
newState s = State {score = s, menu = newTable [["score: " ++ show s]] (V2 1 1)}
