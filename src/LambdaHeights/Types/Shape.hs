module LambdaHeights.Types.Shape where

import           LambdaHeights.Types

data Shape = Shape {
  polygonXs :: [Float],
  polygonYs :: [Float]
}

size :: Shape -> Size
size shape = (maximum $ polygonXs shape, maximum $ polygonYs shape)
