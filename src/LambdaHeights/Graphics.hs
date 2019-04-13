module LambdaHeights.Graphics
  ( RenderContext
  , newContext
  , deleteContext
  )
where

import qualified Data.Text                               as T
import qualified SDL
import qualified SDL.Font                                as SDLF

type RenderContext = (SDL.Window, SDL.Renderer)

windowSettings :: SDL.WindowConfig
windowSettings = SDL.defaultWindow { SDL.windowMode = SDL.FullscreenDesktop }

newContext :: String -> IO RenderContext
newContext windowTitle = do
  SDL.initializeAll
  SDLF.initialize
  window   <- SDL.createWindow (T.pack windowTitle) windowSettings
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  return (window, renderer)

deleteContext :: RenderContext -> IO ()
deleteContext (window, renderer) = do
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDLF.quit
  SDL.quit
