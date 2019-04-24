module SDL.GUI.Basics where

import           Control.Monad
import           LambdaHeights.RenderContext
import           SDL.GUI.TableRenderer
import           SDL.GUI.Types.Table

start = do
  (window, renderer) <- newContext "SDL.GUI"
  _                  <- forever $ return ()
  deleteContext (window, renderer)
  return ()
