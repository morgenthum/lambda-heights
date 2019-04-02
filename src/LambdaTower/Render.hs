module LambdaTower.Render where

import           Foreign.C.Types

import           LambdaTower.Types

import qualified Data.Text                     as T

import qualified LambdaTower.Screen            as Screen
import qualified LambdaTower.Types.Button      as Button

import qualified SDL
import qualified SDL.Font                      as SDLF

renderButton
  :: SDL.Renderer
  -> WindowSize
  -> Screen.Screen
  -> SDLF.Font
  -> SDLF.Color
  -> Button.Button
  -> IO ()
renderButton renderer windowSize screen font color button = do
  (w, h) <- SDLF.size font $ T.pack $ Button.text button
  let SDL.V2 x y = Screen.toWindowPosition screen windowSize (Button.position button)
  let deltaX     = round (realToFrac w / 2 :: Float)
  let deltaY     = round (realToFrac h / 2 :: Float)
  renderText renderer font (SDL.V2 (x - deltaX) (y - deltaY)) color $ Button.text button

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
