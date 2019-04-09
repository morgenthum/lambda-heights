module SDL.GUI.Renderable where

import           SDL

data RenderContext = RenderContext {
  window :: Window,
  renderer :: Renderer
}

class Renderable a where
  render :: RenderContext -> a -> IO ()
