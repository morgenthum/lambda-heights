module SDL.GUI.Types where

import           Foreign.C.Types

import qualified SDL

data RenderContext = RenderContext {
  window :: SDL.Window,
  renderer :: SDL.Renderer
}

data Location = Location {
  position :: SDL.Point SDL.V2 CInt,
  size :: SDL.V2 CInt
}
