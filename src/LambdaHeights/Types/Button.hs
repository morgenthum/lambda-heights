module LambdaHeights.Types.Button where

import           LambdaHeights.Types

data Button = Button {
  id :: Int,
  text :: String,
  position :: Position
}
