module LambdaTower.Types.ScoreState where

import           LambdaTower.Types.Button
import           LambdaTower.Types.ButtonList
import           LambdaTower.Screen

type Score = Int

data ScoreState = ScoreState {
  score :: Score,
  buttonList :: ButtonList
}

newScoreState :: Int -> ScoreState
newScoreState s = ScoreState {score = s, buttonList = newButtonList newScreen [Button 0 "continue" (500, 450)]}
