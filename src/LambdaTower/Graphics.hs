module LambdaTower.Graphics (
  Graphics,
  newGraphics,
  deleteGraphics
) where

import Data.Text

import qualified SDL
import qualified SDL.Font as SDLF

type Graphics = (SDL.Window, SDL.Renderer, SDLF.Font)

windowSettings :: SDL.WindowConfig
windowSettings = SDL.defaultWindow {
  SDL.windowMode = SDL.FullscreenDesktop
}

newGraphics :: String -> String -> Int -> IO Graphics
newGraphics windowTitle fontName fontSize = do
  SDL.initializeAll
  SDLF.initialize

  window <- SDL.createWindow (pack windowTitle) windowSettings
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  font <- SDLF.load fontName fontSize

  return (window, renderer, font)


deleteGraphics :: Graphics -> IO ()
deleteGraphics (window, renderer, font) = do
  SDLF.free font

  SDL.destroyRenderer renderer
  SDL.destroyWindow window

  SDLF.quit
  SDL.quit