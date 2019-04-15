module LambdaHeights.Play.Collision where

import           LambdaHeights.Types
import           Linear.V2

type Point = Position

data Rect = Rect {
    position :: Position,
    size     :: Size
}

inside :: Point -> Rect -> Bool
p `inside` r =
  let V2   px       py       = p
      Rect (V2 x y) (V2 w h) = r
  in  px >= x && px <= x + w && py <= y && py >= y - h
