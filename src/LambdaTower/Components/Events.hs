module LambdaTower.Components.Events where

import LambdaTower.Components.ButtonList

data KeyEvent = Enter | Up | Down

applyEvents :: ButtonList -> [KeyEvent] -> ButtonList
applyEvents state = foldl applyEvent state

applyEvent :: ButtonList -> KeyEvent -> ButtonList
applyEvent buttonList Up = up buttonList
applyEvent buttonList Down = down buttonList
applyEvent buttonList Enter = activate buttonList