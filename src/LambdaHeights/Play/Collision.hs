module LambdaHeights.Play.Collision where

import           LambdaHeights.Types

type Point = Position

data Rect = Rect {
    position :: Position,
    size     :: Size
}

inside :: Point -> Rect -> Bool
p `inside` r =
  let (px, py)           = p
      Rect (x, y) (w, h) = r
  in  px >= x && px <= x + w && py <= y && py >= y - h
