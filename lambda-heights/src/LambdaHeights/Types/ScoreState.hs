module LambdaHeights.Types.ScoreState where

import LambdaHeights.Types.Score
import LambdaHeights.Types.Table
import Linear.V2

data State
  = State
      { score :: Score,
        menu :: Table
      }

newState :: Score -> State
newState (Score score) = State {score = Score score, menu = newTable [["score: " ++ show score]] (V2 1 1)}
