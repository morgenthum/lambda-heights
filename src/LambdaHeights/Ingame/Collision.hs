module LambdaHeights.Ingame.Collision where

import           LambdaHeights.Types

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
