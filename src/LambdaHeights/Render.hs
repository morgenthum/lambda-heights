module LambdaHeights.Render where

import           Control.Monad.IO.Class
import qualified Data.Text                   as T
import           Data.Word
import           Foreign.C.Types
import           LambdaHeights.Loop
import           LambdaHeights.RenderContext
import           Linear.V2
import           Linear.V4
import qualified SDL
import qualified SDL.Font                    as SDLF

renderFrame :: (MonadIO m) => RenderContext -> V4 Word8 -> Render m s -> Render m s
renderFrame (_, renderer) color render timer state = do
  SDL.rendererDrawColor renderer SDL.$= color
  SDL.clear renderer
  render timer state
  SDL.present renderer

renderBoth :: (Monad m) => Render m s1 -> Render m s2 -> Render m (s1, s2)
renderBoth r1 r2 timer (s1, s2) = do
  r1 timer s1
  r2 timer s2

renderText :: (MonadIO m) => SDL.Renderer -> SDLF.Font -> SDLF.Color -> V2 CInt -> String -> m ()
renderText renderer font color position text = do
  surface <- SDLF.blended font color (T.pack text)
  texture <- SDL.createTextureFromSurface renderer surface
  SDL.freeSurface surface
  textureInfo <- SDL.queryTexture texture
  let w = SDL.textureWidth textureInfo
  let h = SDL.textureHeight textureInfo
  SDL.copy renderer texture Nothing (Just $ SDL.Rectangle (SDL.P position) (V2 w h))
  SDL.destroyTexture texture

renderOverlay :: RenderContext -> IO ()
renderOverlay (window, renderer) = do
  windowSize <- SDL.get $ SDL.windowSize window
  SDL.rendererDrawColor renderer SDL.$= V4 0 0 0 100
  SDL.fillRect renderer $ Just $ SDL.Rectangle (SDL.P $ V2 0 0) windowSize
