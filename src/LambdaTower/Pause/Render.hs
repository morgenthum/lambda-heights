module LambdaTower.Pause.Render where

import           Data.Word

import           LambdaTower.Graphics
import           LambdaTower.Types

import qualified SDL
import qualified SDL.Font                      as SDLF

import qualified LambdaTower.Render            as Render
import qualified LambdaTower.Screen            as Screen
import qualified LambdaTower.Ingame.Render     as Ingame
import qualified LambdaTower.Pause.State       as Pause
import qualified LambdaTower.Timing.Timer      as Timer
import qualified LambdaTower.Types.Button      as Button
import qualified LambdaTower.Types.ButtonList  as ButtonList

data RenderConfig = RenderConfig {
  font :: SDLF.Font,
  overlayColor :: SDL.V4 Word8,
  textColor :: SDL.V4 Word8,
  selectedTextColor :: SDL.V4 Word8
}

defaultConfig :: IO RenderConfig
defaultConfig = do
  loadedFont <- SDLF.load "HighSchoolUSASans.ttf" 28
  return $ RenderConfig
    { font              = loadedFont
    , overlayColor      = SDL.V4 0 0 0 128
    , textColor         = SDL.V4 255 255 255 255
    , selectedTextColor = SDL.V4 0 191 255 255
    }

deleteConfig :: RenderConfig -> IO ()
deleteConfig = SDLF.free . font

render :: Graphics -> RenderConfig -> Ingame.RenderConfig -> Timer.LoopTimer -> Pause.State -> IO ()
render (window, renderer) pauseConfig ingameConfig timer state = do
  renderIngame (window, renderer) ingameConfig timer state
  renderOverlay (window, renderer) pauseConfig
  renderButtons (window, renderer) pauseConfig $ Pause.buttonList state
  SDL.present renderer

renderIngame :: Graphics -> Ingame.RenderConfig -> Timer.LoopTimer -> Pause.State -> IO ()
renderIngame (window, renderer) config timer state = do
  let pre = Ingame.clear renderer $ Ingame.bgColor config
  let post = return ()
  Ingame.render pre post (window, renderer) config timer $ Pause.state state

renderOverlay :: Graphics -> RenderConfig -> IO ()
renderOverlay (window, renderer) config = do
  windowSize <- SDL.get $ SDL.windowSize window
  SDL.rendererDrawColor renderer SDL.$= overlayColor config
  SDL.fillRect renderer $ Just $ SDL.Rectangle (SDL.P $ SDL.V2 0 0) windowSize

renderButtons :: Graphics -> RenderConfig -> ButtonList.ButtonList -> IO ()
renderButtons (window, renderer) config list = do
  let view       = ButtonList.screen list
  let selectedId = ButtonList.selected list
  windowSize <- SDL.get $ SDL.windowSize window
  mapM_ (renderButton renderer config windowSize view selectedId) $ ButtonList.buttons list

renderButton :: SDL.Renderer -> RenderConfig -> WindowSize -> Screen.Screen -> Int -> Button.Button -> IO ()
renderButton renderer config windowSize screen selectedId button = do
  let color = if selectedId == Button.id button then selectedTextColor config else textColor config
  Render.renderButton renderer windowSize screen (font config) color button
