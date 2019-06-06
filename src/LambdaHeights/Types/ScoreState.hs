module LambdaHeights.Types.ScoreState where

import           LambdaHeights.Types.Table
import           Linear.V2

type Score = Int

data State = State {
  score :: Score,
  menu  :: Table
}

newState :: Score -> State
newState score = State {score = score, menu = newTable [["score: " ++ show score]] (V2 1 1)}
