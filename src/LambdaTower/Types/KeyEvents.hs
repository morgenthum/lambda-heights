module LambdaTower.Types.KeyEvents where

import           LambdaTower.Types.ButtonList

data KeyEvent = Enter | Up | Down

applyEvents :: ButtonList -> [KeyEvent] -> ButtonList
applyEvents = foldl applyEvent

applyEvent :: ButtonList -> KeyEvent -> ButtonList
applyEvent buttonList Up    = up buttonList
applyEvent buttonList Down  = down buttonList
applyEvent buttonList Enter = activate buttonList
