module LambdaHeights.Render where

import qualified Data.Text                   as T
import           Foreign.C.Types
import qualified LambdaHeights.Scale         as Scale
import qualified LambdaHeights.Types.Label   as Label
import qualified LambdaHeights.Types.Screen  as Screen
import           Linear.V2
import qualified SDL
import qualified SDL.Font                    as SDLF

renderLabel
  :: SDL.Renderer
  -> Scale.WindowSize
  -> Screen.Screen
  -> SDLF.Font
  -> SDLF.Color
  -> Label.Label
  -> IO ()
renderLabel renderer windowSize screen font color button = do
  let text     = Label.text button
  let (V2 x y) = Scale.toWindowPosition screen windowSize (Label.position button)
  case Label.alignment button of
    Label.AlignLeft   -> renderText renderer font (V2 x y) color text
    Label.AlignCenter -> do
      (w, h) <- SDLF.size font $ T.pack text
      let deltaX   = round (realToFrac w / 2 :: Float)
      let deltaY   = round (realToFrac h / 2 :: Float)
      let position = V2 (x - deltaX) (y - deltaY)
      renderText renderer font position color text

renderText :: SDL.Renderer -> SDLF.Font -> V2 CInt -> SDLF.Color -> String -> IO ()
renderText renderer font position color text = do
  surface <- SDLF.blended font color (T.pack text)
  texture <- SDL.createTextureFromSurface renderer surface
  SDL.freeSurface surface
  textureInfo <- SDL.queryTexture texture
  let w = SDL.textureWidth textureInfo
  let h = SDL.textureHeight textureInfo
  SDL.copy renderer texture Nothing (Just $ SDL.Rectangle (SDL.P position) (V2 w h))
  SDL.destroyTexture texture
