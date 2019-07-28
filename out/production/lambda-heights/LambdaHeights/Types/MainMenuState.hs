module LambdaHeights.Types.MainMenuState where

import           LambdaHeights.Types.Table
import           Linear.V2

newtype State = State {
  menu :: Table
}

newState :: State
newState = State {menu = newTable [["play"], ["replay"], ["exit"]] (V2 1 1)}
