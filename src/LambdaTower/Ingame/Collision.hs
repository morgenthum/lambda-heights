module LambdaTower.Ingame.Collision where

import           LambdaTower.Types

type Point = Position

data Rect = Rect {
    position :: Position,
    size :: Size
}

inside :: Point -> Rect -> Bool
point `inside` rect =
  let (px, py)           = point
      Rect (x, y) (w, h) = rect
  in  px >= x && px <= x + w && py <= y && py >= y - h
