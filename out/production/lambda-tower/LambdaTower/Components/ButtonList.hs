module LambdaTower.Components.ButtonList where

import LambdaTower.Components.Button
import LambdaTower.Screen

data ButtonList = ButtonList {
  screen :: Screen,
  buttons :: [Button],
  selected :: Int,
  action :: Bool
}

newButtonList :: Screen -> [Button] -> ButtonList
newButtonList initScreen initButtons = ButtonList {
  screen = initScreen,
  buttons = initButtons,
  selected = 0,
  action = False
}

up :: ButtonList -> ButtonList
up buttonList = buttonList { selected = selected buttonList - 1 }

down :: ButtonList -> ButtonList
down buttonList = buttonList { selected = selected buttonList + 1 }

activate :: ButtonList -> ButtonList
activate buttonList = buttonList { action = True }

ensureValidIndex :: ButtonList -> ButtonList
ensureValidIndex list
  | index < 0 = list { selected = 0 }
  | index > count - 1 = list { selected = count - 1 }
  | otherwise = list
  where index = selected list
        count = length $ buttons list

selectedButton :: ButtonList -> Button
selectedButton state = buttons state !! selected state