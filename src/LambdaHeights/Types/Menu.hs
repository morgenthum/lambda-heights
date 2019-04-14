module LambdaHeights.Types.Menu
  ( Menu(..)
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
import           LambdaHeights.Types.MenuItem

data Menu = Menu {
  items     :: [MenuItem],
  selected  :: Int,
  confirmed :: Bool
}

newMenu :: [MenuItem] -> Menu
newMenu is = Menu {items = is, selected = 0, confirmed = False}

up :: Menu -> Menu
up menu = ensureValidIndex $ menu { selected = selected menu - 1 }

down :: Menu -> Menu
down menu = ensureValidIndex $ menu { selected = selected menu + 1 }

confirm :: Menu -> Menu
confirm menu = menu { confirmed = True }

ensureValidIndex :: Menu -> Menu
ensureValidIndex menu =
  let index = selected menu
      count = length $ items menu
      go | index < 0         = menu { selected = 0 }
         | index > count - 1 = menu { selected = count - 1 }
         | otherwise         = menu
  in  go

selectedItem :: Menu -> MenuItem
selectedItem menu = items menu !! selected menu

applyEvents :: Menu -> [KeyEvent] -> Menu
applyEvents = foldl applyEvent

applyEvent :: Menu -> KeyEvent -> Menu
applyEvent menu Up    = up menu
applyEvent menu Down  = down menu
applyEvent menu Enter = confirm menu
