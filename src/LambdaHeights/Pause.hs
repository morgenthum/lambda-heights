module LambdaHeights.Pause where

import           Data.Word
import           LambdaHeights.Graphics
import qualified LambdaHeights.Render                    as Render
import qualified LambdaHeights.Scale                     as Scale
import qualified LambdaHeights.Types.Menu                as UI
import qualified LambdaHeights.Types.MenuItem            as UI
import qualified LambdaHeights.Types.KeyEvents           as Events
import qualified LambdaHeights.Types.PauseState          as Pause
import qualified LambdaHeights.Types.Screen              as Screen
import qualified LambdaHeights.Types.Timer               as Timer
import qualified SDL
import qualified SDL.Font                                as SDLF

-- Update the menu.

update
  :: Timer.LoopTimer
  -> [Events.KeyEvent]
  -> Pause.State a
  -> Either Pause.ExitReason (Pause.State a)
update _ events state =
  let list     = UI.applyEvents (Pause.menu state) events
      newState = state { Pause.menu = list }
  in  if UI.confirmed list then Left $ stateByButton $ UI.selectedItem list else Right newState

stateByButton :: UI.MenuItem -> Pause.ExitReason
stateByButton button = case UI.text button of
  "exit" -> Pause.Exit
  _      -> Pause.Resume


-- Render the 'real' state using the proxy-renderer
-- and render the pause overlay on top.

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

render
  :: RenderContext -> RenderConfig -> ProxyRenderer a -> Timer.LoopTimer -> Pause.State a -> IO ()
render (window, renderer) pauseConfig proxyRenderer timer s = do
  proxyRenderer timer $ Pause.state s
  renderOverlay (window, renderer) pauseConfig
  renderButtons (window, renderer) pauseConfig $ Pause.menu s
  SDL.present renderer

renderOverlay :: RenderContext -> RenderConfig -> IO ()
renderOverlay (window, renderer) config = do
  windowSize <- SDL.get $ SDL.windowSize window
  SDL.rendererDrawColor renderer SDL.$= overlayColor config
  SDL.fillRect renderer $ Just $ SDL.Rectangle (SDL.P $ SDL.V2 0 0) windowSize

renderButtons :: RenderContext -> RenderConfig -> UI.Menu -> IO ()
renderButtons (window, renderer) config menu = do
  let screen     = Screen.newScreen
  let selectedId = UI.selected menu
  windowSize <- SDL.get $ SDL.windowSize window
  mapM_ (renderButton renderer config windowSize screen selectedId) $ UI.items menu

renderButton
  :: SDL.Renderer
  -> RenderConfig
  -> Scale.WindowSize
  -> Screen.Screen
  -> Int
  -> UI.MenuItem
  -> IO ()
renderButton renderer config windowSize screen selectedId button = do
  let color = if selectedId == UI.id button then selectedTextColor config else textColor config
  Render.renderButton renderer windowSize screen (font config) color button
