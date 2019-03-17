module LambdaTower.Menu.Render where

import Data.Word

import qualified Data.Text as T

import Foreign.C.Types

import qualified SDL
import qualified SDL.Font as SDLF

import LambdaTower.Graphics
import LambdaTower.Loop

import qualified LambdaTower.Menu.MenuState as M
import qualified LambdaTower.Screen as S

data RenderConfig = RenderConfig {
  font :: SDLF.Font,
  backgroundColor :: SDL.V4 Word8,
  textColor :: SDL.V4 Word8,
  selectedTextColor :: SDL.V4 Word8
}

defaultConfig :: IO RenderConfig
defaultConfig = do
  loadedFont <- SDLF.load "HighSchoolUSASans.ttf" 28
  return $ RenderConfig {
    font = loadedFont,
    backgroundColor = SDL.V4 30 30 30 255,
    textColor = SDL.V4 255 255 255 255,
    selectedTextColor = SDL.V4 0 191 255 255
  }

deleteConfig :: RenderConfig -> IO ()
deleteConfig = SDLF.free . font

render :: Graphics -> RenderConfig -> Renderer IO M.MenuState
render (window, renderer) config state = do
  SDL.rendererDrawColor renderer SDL.$= backgroundColor config
  SDL.clear renderer

  windowSize <- SDL.get $ SDL.windowSize window
  let view = M.view state
  let selectedId = M.selected state

  mapM_ (renderButton renderer config windowSize view selectedId) $ M.buttons state

  SDL.present renderer

renderButton :: SDL.Renderer -> RenderConfig -> SDL.V2 CInt -> S.View -> Int -> M.Button -> IO ()
renderButton renderer config windowSize view selectedId button = do
  let buttonFont = font config

  let SDL.V2 x y = S.translatePosition view windowSize (M.position button)
  (w, h) <- SDLF.size buttonFont $ T.pack . M.text $ button

  let deltaX = round (realToFrac w / 2 :: Float)
  let deltaY = round (realToFrac h / 2 :: Float)

  let color = if selectedId == M.id button then selectedTextColor config else textColor config

  renderText renderer buttonFont (SDL.V2 (x - deltaX) (y - deltaY)) color $ M.text button