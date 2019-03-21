module LambdaTower.Pause.Render where

import Data.Word

import qualified SDL
import qualified SDL.Font as SDLF

import LambdaTower.Graphics
import LambdaTower.Loop
import LambdaTower.Types

import qualified LambdaTower.Components.Button as B
import qualified LambdaTower.Components.ButtonList as BL
import qualified LambdaTower.Components.Render as R
import qualified LambdaTower.Ingame.Render as IR
import qualified LambdaTower.Pause.PauseState as PS
import qualified LambdaTower.Screen as S

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

renderPause :: Graphics -> RenderConfig -> IR.RenderConfig -> Renderer IO PS.PauseState
renderPause (window, renderer) pauseConfig ingameConfig state = do
  let clear = IR.clear renderer $ IR.bgColor ingameConfig
  IR.render clear void (window, renderer) ingameConfig (PS.state state)

  windowSize <- SDL.get $ SDL.windowSize window
  SDL.rendererDrawColor renderer SDL.$= overlayColor pauseConfig
  SDL.fillRect renderer $ Just $ SDL.Rectangle (SDL.P $ SDL.V2 0 0) windowSize

  let buttonList = PS.buttonList state
  let view = BL.screen buttonList
  let selectedId = BL.selected buttonList
  mapM_ (renderButton renderer pauseConfig windowSize view selectedId) $ BL.buttons buttonList

  SDL.present renderer

void :: IO ()
void = return ()

renderButton :: SDL.Renderer -> RenderConfig -> WindowSize -> S.Screen -> Int -> B.Button -> IO ()
renderButton renderer config windowSize screen selectedId button = do
  let color = if selectedId == B.id button
              then selectedTextColor config
              else textColor config
  R.renderButton renderer windowSize screen (font config) color button