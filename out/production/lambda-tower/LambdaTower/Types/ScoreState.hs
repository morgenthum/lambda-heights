module LambdaTower.Types.ScoreState where

import LambdaTower.Types.Button
import LambdaTower.Types.ButtonList
import LambdaTower.Screen

data ScoreState = ScoreState {
  score :: Int,
  buttonList :: ButtonList
}

newScoreState :: Int -> ScoreState
newScoreState s = ScoreState {
  score = s,
  buttonList = newButtonList newScreen [
    Button 0 "continue" (500, 450)
  ]
}