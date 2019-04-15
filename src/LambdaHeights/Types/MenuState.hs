module LambdaHeights.Types.MenuState
  ( State(..)
  , newMenu
  , up
  , down
  , confirm
  , selectedItem
  , applyEvent
  , applyEvents
  )
where

import           LambdaHeights.Types.KeyEvents
import           LambdaHeights.Types.Label

data State = State {
  labels    :: [Label],
  selected  :: Int,
  confirmed :: Bool
}

newMenu :: [Label] -> State
newMenu is = State {labels = is, selected = 0, confirmed = False}

up :: State -> State
up menu = ensureValidIndex $ menu { selected = selected menu - 1 }

down :: State -> State
down menu = ensureValidIndex $ menu { selected = selected menu + 1 }

confirm :: State -> State
confirm menu = menu { confirmed = True }

ensureValidIndex :: State -> State
ensureValidIndex menu =
  let index = selected menu
      count = length $ labels menu
      go | index < 0         = menu { selected = 0 }
         | index > count - 1 = menu { selected = count - 1 }
         | otherwise         = menu
  in  go

selectedItem :: State -> Label
selectedItem menu = labels menu !! selected menu

applyEvents :: State -> [KeyEvent] -> State
applyEvents = foldl applyEvent

applyEvent :: State -> KeyEvent -> State
applyEvent menu Up    = up menu
applyEvent menu Down  = down menu
applyEvent menu Enter = confirm menu
