module LambdaHeights.Render where

import qualified Control.Monad.IO.Class      as M
import qualified Control.Monad.Reader        as M
import qualified Data.Text                   as T
import           Data.Word
import           Foreign.C.Types
import           LambdaHeights.Loop
import           LambdaHeights.RenderContext
import           LambdaHeights.Types.Loop
import           Linear.V2
import           Linear.V4
import qualified SDL
import qualified SDL.Font                    as SDLF

renderFrame :: (M.MonadIO m) => RenderContext -> V4 Word8 -> Render m s -> Render m s
renderFrame (_, renderer) color render = do
  timer <- askRenderTimer
  state <- askRenderState
  SDL.rendererDrawColor renderer SDL.$= color
  SDL.clear renderer
  M.lift $ M.runReaderT render (timer, state)
  SDL.present renderer

renderBoth :: (M.Monad m) => Render m s1 -> Render m s2 -> Render m (s1, s2)
renderBoth r1 r2 = do
  timer    <- askRenderTimer
  (s1, s2) <- askRenderState
  M.lift $ M.runReaderT r1 (timer, s1)
  M.lift $ M.runReaderT r2 (timer, s2)

renderText :: (M.MonadIO m) => SDL.Renderer -> SDLF.Font -> SDLF.Color -> V2 CInt -> String -> m ()
renderText renderer font color position text = do
  surface <- SDLF.blended font color (T.pack text)
  texture <- SDL.createTextureFromSurface renderer surface
  SDL.freeSurface surface
  textureInfo <- SDL.queryTexture texture
  let w = SDL.textureWidth textureInfo
  let h = SDL.textureHeight textureInfo
  SDL.copy renderer texture Nothing (Just $ SDL.Rectangle (SDL.P position) (V2 w h))
  SDL.destroyTexture texture

renderOverlay :: (M.MonadIO m) => RenderContext -> V4 Word8 -> m ()
renderOverlay (window, renderer) color = do
  windowSize <- SDL.get $ SDL.windowSize window
  SDL.rendererDrawColor renderer SDL.$= color
  SDL.fillRect renderer $ Just $ SDL.Rectangle (SDL.P $ V2 0 0) windowSize
