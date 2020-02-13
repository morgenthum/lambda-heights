module Linear.V2.Utils where

import Linear.V2

getX :: V2 a -> a
getX (V2 x _) = x

getY :: V2 a -> a
getY (V2 _ y) = y

convert :: (Integral a, Integral b) => V2 a -> V2 b
convert (V2 x y) = V2 (fromIntegral x) (fromIntegral y)
