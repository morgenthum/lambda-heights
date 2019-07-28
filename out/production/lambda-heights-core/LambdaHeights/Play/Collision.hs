module LambdaHeights.Play.Collision where

import           LambdaHeights.Types
import           Linear.V2

type Point = Position
type Rect = (Position, Size)

inside :: Point -> Rect -> Bool
(V2 px py) `inside` (V2 x y, V2 w h) = px >= x && px <= x + w && py <= y && py >= y - h
