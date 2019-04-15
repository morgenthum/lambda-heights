module LambdaHeights.Types.MainMenuState where

import           LambdaHeights.Types.Label
import qualified LambdaHeights.Types.MenuState as Menu
import           Linear.V2

newtype State = State {
  menu :: Menu.State
}

newState :: State
newState = State
  { menu = Menu.newMenu
    [ Label 0 "play"   (V2 500 600) AlignCenter
    , Label 1 "replay" (V2 500 500) AlignCenter
    , Label 2 "exit"   (V2 500 400) AlignCenter
    ]
  }
