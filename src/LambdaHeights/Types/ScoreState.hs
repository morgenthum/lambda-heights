module LambdaHeights.Types.ScoreState where

import           LambdaHeights.Screen
import           LambdaHeights.UserInterface

type Score = Int

data State = State {
  score :: Score,
  buttonList :: ButtonList
}

newState :: Int -> State
newState s =
  State {score = s, buttonList = newButtonList newScreen [Button 0 "continue" (500, 450)]}
