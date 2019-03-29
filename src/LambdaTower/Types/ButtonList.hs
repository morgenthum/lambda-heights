module LambdaTower.Types.ButtonList where

import           LambdaTower.Screen
import           LambdaTower.Types.Button

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
ensureValidIndex list | index < 0         = list { selected = 0 }
                      | index > count - 1 = list { selected = count - 1 }
                      | otherwise         = list
 where
  index = selected list
  count = length $ buttons list

selectedButton :: ButtonList -> Button
selectedButton list = buttons list !! selected list
