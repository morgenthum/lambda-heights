module LambdaHeights.Types.ScoreState where

import           LambdaHeights.Types.Label
import qualified LambdaHeights.Types.MenuState as Menu
import           Linear.V2

type Score = Int

data State = State {
  score :: Score,
  menu  :: Menu.State
}

newState :: Int -> State
newState s =
  State {score = s, menu = Menu.newMenu [Label 0 ("score: " ++ show s) (V2 500 500) AlignCenter]}
