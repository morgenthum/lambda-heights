module SDL.GUI.Types where

import           Foreign.C.Types

import qualified SDL

data Location = Location {
  position :: SDL.Point SDL.V2 CInt,
  size :: SDL.V2 CInt
}
