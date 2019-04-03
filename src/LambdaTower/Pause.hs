module LambdaTower.Pause where

import           Data.Word

import           LambdaTower.Graphics
import           LambdaTower.Types
import           LambdaTower.Types.PauseState

import qualified SDL
import qualified SDL.Font                      as SDLF

import qualified LambdaTower.Render            as Render
import qualified LambdaTower.Screen            as Screen
import qualified LambdaTower.Timing.Timer      as Timer
import qualified LambdaTower.Types.KeyEvents   as Events
import qualified LambdaTower.UI.Button         as Button
import qualified LambdaTower.UI.ButtonList     as ButtonList


-- Update

update
  :: Timer.LoopTimer -> [Events.KeyEvent] -> PauseState a -> IO (Either ExitReason (PauseState a))
update _ events state = do
  let list = ButtonList.ensureValidIndex $ ButtonList.applyEvents (buttonList state) events
  let newState = state { buttonList = list }
  return $ if ButtonList.action list
    then Left $ stateByButton $ ButtonList.selectedButton list
    else Right newState

stateByButton :: Button.Button -> ExitReason
stateByButton button = case Button.text button of
  "exit" -> Exit
  _      -> Resume


-- Render

type ProxyRenderer a = Timer.LoopTimer -> a -> IO ()

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

render :: Graphics -> RenderConfig -> ProxyRenderer a -> Timer.LoopTimer -> PauseState a -> IO ()
render (window, renderer) pauseConfig proxyRenderer timer s = do
  proxyRenderer timer $ state s
  renderOverlay (window, renderer) pauseConfig
  renderButtons (window, renderer) pauseConfig $ buttonList s
  SDL.present renderer

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

renderButton
  :: SDL.Renderer -> RenderConfig -> WindowSize -> Screen.Screen -> Int -> Button.Button -> IO ()
renderButton renderer config windowSize screen selectedId button = do
  let color = if selectedId == Button.id button then selectedTextColor config else textColor config
  Render.renderButton renderer windowSize screen (font config) color button
