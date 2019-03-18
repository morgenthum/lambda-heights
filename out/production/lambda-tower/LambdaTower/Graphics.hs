module LambdaTower.Graphics (
  Graphics,
  newGraphics,
  deleteGraphics,
  renderText
) where

import qualified Data.Text as T

import Foreign.C.Types

import qualified SDL
import qualified SDL.Font as SDLF

type Graphics = (SDL.Window, SDL.Renderer)

windowSettings :: SDL.WindowConfig
windowSettings = SDL.defaultWindow {
  SDL.windowMode = SDL.FullscreenDesktop
}

newGraphics :: String -> IO Graphics
newGraphics windowTitle = do
  SDL.initializeAll
  SDLF.initialize

  window <- SDL.createWindow (T.pack windowTitle) windowSettings
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  return (window, renderer)

deleteGraphics :: Graphics -> IO ()
deleteGraphics (window, renderer) = do
  SDL.destroyRenderer renderer
  SDL.destroyWindow window

  SDLF.quit
  SDL.quit

renderText :: SDL.Renderer -> SDLF.Font -> SDL.V2 CInt -> SDLF.Color -> String -> IO ()
renderText renderer font position color text = do
  surface <- SDLF.blended font color (T.pack text)
  texture <- SDL.createTextureFromSurface renderer surface
  SDL.freeSurface surface

  textureInfo <- SDL.queryTexture texture
  let w = SDL.textureWidth textureInfo
  let h = SDL.textureHeight textureInfo

  SDL.copy renderer texture Nothing (Just $ SDL.Rectangle (SDL.P position) (SDL.V2 w h))
  SDL.destroyTexture texture