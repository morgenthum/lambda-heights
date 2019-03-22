module LambdaTower.Pause.Render where

import Data.Word

import qualified SDL
import qualified SDL.Font as SDLF

import LambdaTower.Graphics
import LambdaTower.Loop
import LambdaTower.Types

import qualified LambdaTower.Types.Button as Button
import qualified LambdaTower.Types.ButtonList as ButtonList
import qualified LambdaTower.Ingame.Render as Render
import qualified LambdaTower.Types.PauseState as State
import qualified LambdaTower.Render as Render
import qualified LambdaTower.Screen as Screen

data RenderConfig = RenderConfig {
  font :: SDLF.Font,
  overlayColor :: SDL.V4 Word8,
  textColor :: SDL.V4 Word8,
  selectedTextColor :: SDL.V4 Word8
}

defaultConfig :: IO RenderConfig
defaultConfig = do
  loadedFont <- SDLF.load "HighSchoolUSASans.ttf" 28
  return $ RenderConfig {
    font = loadedFont,
    overlayColor = SDL.V4 0 0 0 128,
    textColor = SDL.V4 255 255 255 255,
    selectedTextColor = SDL.V4 0 191 255 255
  }

deleteConfig :: RenderConfig -> IO ()
deleteConfig = SDLF.free . font

render :: Graphics -> RenderConfig -> Render.RenderConfig -> Renderer IO State.PauseState
render (window, renderer) pauseConfig ingameConfig state = do
  let pre = Render.clear renderer $ Render.bgColor ingameConfig
  let post = return ()

  Render.render pre post (window, renderer) ingameConfig (State.state state)

  windowSize <- SDL.get $ SDL.windowSize window
  SDL.rendererDrawColor renderer SDL.$= overlayColor pauseConfig
  SDL.fillRect renderer $ Just $ SDL.Rectangle (SDL.P $ SDL.V2 0 0) windowSize

  let buttonList = State.buttonList state
  let view = ButtonList.screen buttonList
  let selectedId = ButtonList.selected buttonList
  mapM_ (renderButton renderer pauseConfig windowSize view selectedId) $ ButtonList.buttons buttonList

  SDL.present renderer

renderButton :: SDL.Renderer -> RenderConfig -> WindowSize -> Screen.Screen -> Int -> Button.Button -> IO ()
renderButton renderer config windowSize screen selectedId button = do
  let color = if selectedId == Button.id button
              then selectedTextColor config
              else textColor config
  Render.renderButton renderer windowSize screen (font config) color button