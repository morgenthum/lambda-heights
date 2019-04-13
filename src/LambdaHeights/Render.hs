module LambdaHeights.Render where

import qualified Data.Text                               as T
import           Foreign.C.Types
import qualified LambdaHeights.Scale                     as Scale
import qualified LambdaHeights.Types.Button              as UI
import qualified LambdaHeights.Types.Screen              as Screen
import qualified SDL
import qualified SDL.Font                                as SDLF

renderButton
  :: SDL.Renderer
  -> Scale.WindowSize
  -> Screen.Screen
  -> SDLF.Font
  -> SDLF.Color
  -> UI.Button
  -> IO ()
renderButton renderer windowSize screen font color button = do
  let position = Scale.toWindowPosition screen windowSize (UI.position button)
  renderCenteredText renderer font position color $ UI.text button

renderCenteredText :: SDL.Renderer -> SDLF.Font -> SDL.V2 CInt -> SDLF.Color -> String -> IO ()
renderCenteredText renderer font (SDL.V2 x y) color text = do
  (w, h) <- SDLF.size font $ T.pack text
  let deltaX   = round (realToFrac w / 2 :: Float)
  let deltaY   = round (realToFrac h / 2 :: Float)
  let position = SDL.V2 (x - deltaX) (y - deltaY)
  renderText renderer font position color text

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
