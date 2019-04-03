module LambdaTower.Types.ScoreState where

import           LambdaTower.Screen
import           LambdaTower.UserInterface

type Score = Int

data ScoreState = ScoreState {
  score :: Score,
  buttonList :: ButtonList
}

newScoreState :: Int -> ScoreState
newScoreState s =
  ScoreState {score = s, buttonList = newButtonList newScreen [Button 0 "continue" (500, 450)]}
