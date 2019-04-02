module LambdaTower.Score.State where

import           LambdaTower.Types.Button
import           LambdaTower.Types.ButtonList
import           LambdaTower.Screen

type Score = Int

data State = State {
  score :: Score,
  buttonList :: ButtonList
}

newScoreState :: Int -> State
newScoreState s =
  State {score = s, buttonList = newButtonList newScreen [Button 0 "continue" (500, 450)]}
