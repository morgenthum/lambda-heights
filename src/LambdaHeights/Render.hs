module LambdaHeights.Render where

import qualified Data.Text       as T
import           Data.Word
import           Foreign.C.Types
import           Linear.V2
import           Linear.V4
import qualified SDL
import qualified SDL.Font        as SDLF

renderFrame :: SDL.Renderer -> V4 Word8 -> IO () -> IO ()
renderFrame renderer color render = do
  SDL.rendererDrawColor renderer SDL.$= color
  SDL.clear renderer
  render
  SDL.present renderer

renderText :: SDL.Renderer -> SDLF.Font -> SDLF.Color -> V2 CInt -> String -> IO ()
renderText renderer font color position text = do
  surface <- SDLF.blended font color (T.pack text)
  texture <- SDL.createTextureFromSurface renderer surface
  SDL.freeSurface surface
  textureInfo <- SDL.queryTexture texture
  let w = SDL.textureWidth textureInfo
  let h = SDL.textureHeight textureInfo
  SDL.copy renderer texture Nothing (Just $ SDL.Rectangle (SDL.P position) (V2 w h))
  SDL.destroyTexture texture
