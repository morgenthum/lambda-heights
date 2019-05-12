module Linear.V2.Utils where

import           Linear.V2

convertV2 :: (Integral a, Integral b) => V2 a -> V2 b
convertV2 (V2 x y) = V2 (fromIntegral x) (fromIntegral y)
