module LambdaHeights.Types.MainMenuState where

import qualified LambdaHeights.Types.Menu     as Menu
import           LambdaHeights.Types.MenuItem

newtype State = State {
  menu :: Menu.Menu
}

newState :: State
newState = State
  { menu = Menu.newMenu
    [MenuItem 0 "play" (500, 600), MenuItem 1 "replay" (500, 500), MenuItem 2 "exit" (500, 400)]
  }
