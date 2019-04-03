module LambdaTower.UserInterface where

import           LambdaTower.Screen
import           LambdaTower.Types
import           LambdaTower.Types.KeyEvents

data Button = Button {
  id :: Int,
  text :: String,
  position :: Position
}

data ButtonList = ButtonList {
  screen :: Screen,
  buttons :: [Button],
  selected :: Int,
  action :: Bool
}

newButtonList :: Screen -> [Button] -> ButtonList
newButtonList s bs = ButtonList {screen = s, buttons = bs, selected = 0, action = False}

up :: ButtonList -> ButtonList
up list = list { selected = selected list - 1 }

down :: ButtonList -> ButtonList
down list = list { selected = selected list + 1 }

activate :: ButtonList -> ButtonList
activate list = list { action = True }

ensureValidIndex :: ButtonList -> ButtonList
ensureValidIndex list =
  let index = selected list
      count = length $ buttons list
      go | index < 0         = list { selected = 0 }
         | index > count - 1 = list { selected = count - 1 }
         | otherwise         = list
  in  go

selectedButton :: ButtonList -> Button
selectedButton list = buttons list !! selected list

applyEvents :: ButtonList -> [KeyEvent] -> ButtonList
applyEvents = foldl applyEvent

applyEvent :: ButtonList -> KeyEvent -> ButtonList
applyEvent buttonList Up    = up buttonList
applyEvent buttonList Down  = down buttonList
applyEvent buttonList Enter = activate buttonList
