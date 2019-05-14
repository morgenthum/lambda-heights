module LambdaHeights.Render where

import qualified Data.Text       as T
import           Foreign.C.Types
import           Linear.V2
import qualified SDL
import qualified SDL.Font        as SDLF

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
