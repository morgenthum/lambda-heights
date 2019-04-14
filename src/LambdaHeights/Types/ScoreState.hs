module LambdaHeights.Types.ScoreState where

import qualified LambdaHeights.Types.Menu                as Menu
import           LambdaHeights.Types.MenuItem

type Score = Int

data State = State {
  score :: Score,
  menu :: Menu.Menu
}

newState :: Int -> State
newState s = State {score = s, menu = Menu.newMenu [MenuItem 0 ("score: " ++ show s) (500, 500)]}
