module LambdaTower.Score where

import           Data.Word

import qualified SDL
import qualified SDL.Font                      as SDLF

import           LambdaTower.Graphics
import           LambdaTower.Types
import           LambdaTower.Types.ScoreState

import qualified LambdaTower.Render            as Render
import qualified LambdaTower.Screen            as Screen
import qualified LambdaTower.Timer      as Timer
import qualified LambdaTower.Types.KeyEvents   as Events
import qualified LambdaTower.UserInterface     as UI


-- Update

update :: Timer.LoopTimer -> [Events.KeyEvent] -> ScoreState -> IO (Either () ScoreState)
update _ events state = do
  let list = UI.ensureValidIndex $ UI.applyEvents (buttonList state) events
  return $ if UI.action list then Left () else Right state

wrap :: ScoreState -> UI.ButtonList -> ScoreState
wrap state list = state { buttonList = list }


-- Render

data RenderConfig = RenderConfig {
  font :: SDLF.Font,
  backgroundColor :: SDL.V4 Word8,
  textColor :: SDL.V4 Word8,
  selectedTextColor :: SDL.V4 Word8
}

defaultConfig :: IO RenderConfig
defaultConfig = do
  loadedFont <- SDLF.load "HighSchoolUSASans.ttf" 28
  return $ RenderConfig
    { font              = loadedFont
    , backgroundColor   = SDL.V4 30 30 30 255
    , textColor         = SDL.V4 255 255 255 255
    , selectedTextColor = SDL.V4 0 191 255 255
    }

deleteConfig :: RenderConfig -> IO ()
deleteConfig = SDLF.free . font

render :: Graphics -> RenderConfig -> Timer.LoopTimer -> ScoreState -> IO ()
render (window, renderer) config _ state = do
  SDL.rendererDrawColor renderer SDL.$= backgroundColor config
  SDL.clear renderer

  let list       = buttonList state
  let view       = UI.screen list
  let selectedId = UI.selected list
  let text       = "score: " ++ show (score state)

  windowSize <- SDL.get $ SDL.windowSize window
  renderButton renderer config windowSize view (-1) $ UI.Button 0 text (500, 550)
  mapM_ (renderButton renderer config windowSize view selectedId) $ UI.buttons list

  SDL.present renderer

renderButton
  :: SDL.Renderer -> RenderConfig -> WindowSize -> Screen.Screen -> Int -> UI.Button -> IO ()
renderButton renderer config windowSize screen selectedId button = do
  let color = if selectedId == UI.id button then selectedTextColor config else textColor config
  Render.renderButton renderer windowSize screen (font config) color button

