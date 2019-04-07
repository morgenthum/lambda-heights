module LambdaHeights.Graphics
  ( Graphics
  , newGraphics
  , deleteGraphics
  )
where

import qualified Data.Text                     as T

import qualified SDL
import qualified SDL.Font                      as SDLF

type Graphics = (SDL.Window, SDL.Renderer)

windowSettings :: SDL.WindowConfig
windowSettings = SDL.defaultWindow { SDL.windowMode = SDL.FullscreenDesktop }

newGraphics :: String -> IO Graphics
newGraphics windowTitle = do
  SDL.initializeAll
  SDLF.initialize
  window   <- SDL.createWindow (T.pack windowTitle) windowSettings
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  return (window, renderer)

deleteGraphics :: Graphics -> IO ()
deleteGraphics (window, renderer) = do
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDLF.quit
  SDL.quit
