module LambdaHeights.Types.MenuItem where

import           LambdaHeights.Types

data MenuItem = MenuItem {
  id       :: Int,
  text     :: String,
  position :: Position
}
