module LambdaTower.Pause where

import           Data.Word

import           LambdaTower.Graphics
import           LambdaTower.Types

import qualified SDL
import qualified SDL.Font                      as SDLF

import qualified LambdaTower.Types.PauseState  as Pause
import qualified LambdaTower.Render            as Render
import qualified LambdaTower.Screen            as Screen
import qualified LambdaTower.Timer             as Timer
import qualified LambdaTower.Types.KeyEvents   as Events
import qualified LambdaTower.UserInterface     as UI


-- Update

update
  :: Timer.LoopTimer -> [Events.KeyEvent] -> Pause.State a -> IO (Either Pause.ExitReason (Pause.State a))
update _ events state = do
  let list = UI.ensureValidIndex $ UI.applyEvents (Pause.buttonList state) events
  let newState = state { Pause.buttonList = list }
  return $ if UI.action list then Left $ stateByButton $ UI.selectedButton list else Right newState

stateByButton :: UI.Button -> Pause.ExitReason
stateByButton button = case UI.text button of
  "exit" -> Pause.Exit
  _      -> Pause.Resume


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
  return $ RenderConfig { font              = loadedFont
                        , overlayColor      = SDL.V4 0 0 0 128
                        , textColor         = SDL.V4 255 255 255 255
                        , selectedTextColor = SDL.V4 0 191 255 255
                        }

deleteConfig :: RenderConfig -> IO ()
deleteConfig = SDLF.free . font

render :: Graphics -> RenderConfig -> ProxyRenderer a -> Timer.LoopTimer -> Pause.State a -> IO ()
render (window, renderer) pauseConfig proxyRenderer timer s = do
  proxyRenderer timer $ Pause.state s
  renderOverlay (window, renderer) pauseConfig
  renderButtons (window, renderer) pauseConfig $ Pause.buttonList s
  SDL.present renderer

renderOverlay :: Graphics -> RenderConfig -> IO ()
renderOverlay (window, renderer) config = do
  windowSize <- SDL.get $ SDL.windowSize window
  SDL.rendererDrawColor renderer SDL.$= overlayColor config
  SDL.fillRect renderer $ Just $ SDL.Rectangle (SDL.P $ SDL.V2 0 0) windowSize

renderButtons :: Graphics -> RenderConfig -> UI.ButtonList -> IO ()
renderButtons (window, renderer) config list = do
  let view       = UI.screen list
  let selectedId = UI.selected list
  windowSize <- SDL.get $ SDL.windowSize window
  mapM_ (renderButton renderer config windowSize view selectedId) $ UI.buttons list

renderButton
  :: SDL.Renderer -> RenderConfig -> WindowSize -> Screen.Screen -> Int -> UI.Button -> IO ()
renderButton renderer config windowSize screen selectedId button = do
  let color = if selectedId == UI.id button then selectedTextColor config else textColor config
  Render.renderButton renderer windowSize screen (font config) color button
