module LambdaTower.Render where

import qualified Data.Text as T

import Foreign.C.Types

import qualified SDL
import qualified SDL.Font as SDLF

import LambdaTower.Types

import qualified LambdaTower.Types.Button as B
import qualified LambdaTower.Screen as S

renderButton :: SDL.Renderer -> WindowSize -> S.Screen -> SDLF.Font -> SDLF.Color -> B.Button -> IO ()
renderButton renderer windowSize screen font color button = do
  let SDL.V2 x y = S.toWindowPosition screen windowSize (B.position button)
  (w, h) <- SDLF.size font $ T.pack $ B.text button

  let deltaX = round (realToFrac w / 2 :: Float)
  let deltaY = round (realToFrac h / 2 :: Float)

  renderText renderer font (SDL.V2 (x - deltaX) (y - deltaY)) color $ B.text button

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