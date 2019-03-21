module LambdaTower.Menu.Render where

import Data.Word

import qualified SDL
import qualified SDL.Font as SDLF

import LambdaTower.Graphics
import LambdaTower.Loop
import LambdaTower.Types

import qualified LambdaTower.Components.Button as B
import qualified LambdaTower.Components.ButtonList as BL
import qualified LambdaTower.Components.Render as R
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
  let buttonList = M.buttonList state
  let view = BL.screen buttonList
  let selectedId = BL.selected buttonList
  mapM_ (renderButton renderer config windowSize view selectedId) $ BL.buttons buttonList

  SDL.present renderer

renderButton :: SDL.Renderer -> RenderConfig -> WindowSize -> S.Screen -> Int -> B.Button -> IO ()
renderButton renderer config windowSize screen selectedId button = do
  let color = if selectedId == B.id button
              then selectedTextColor config
              else textColor config
  R.renderButton renderer windowSize screen (font config) color button