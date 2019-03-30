module LambdaTower.Ingame.Collision where

import           LambdaTower.Types

data Rect = Rect {
    position :: Position,
    size :: Size
}

inside :: Position -> Rect -> Bool
p `inside` r = px >= x && px <= x + w && py <= y && py >= y - h
 where
  (px, py)           = p
  Rect (x, y) (w, h) = r
