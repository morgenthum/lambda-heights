module Graphics.UI.Classes where

import Graphics.UI.Types

class HasSize a where
  calcSize :: a -> Size
