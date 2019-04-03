module LambdaTower.Menu
  ( keyInput
  , update
  , defaultConfig
  , deleteConfig
  , render
  )
where

import           Data.Maybe
import           Data.Word

import           LambdaTower.Graphics
import           LambdaTower.State
import           LambdaTower.Types
import           LambdaTower.Types.KeyEvents
import           LambdaTower.Types.MenuState

import qualified SDL
import qualified SDL.Font                      as SDLF

import qualified LambdaTower.Render            as Render
import qualified LambdaTower.Screen            as Screen
import qualified LambdaTower.Timer             as Timer
import qualified LambdaTower.UserInterface     as UI


-- Input

keyInput :: IO [KeyEvent]
keyInput = mapMaybe eventToKeyEvent <$> SDL.pollEvents

eventToKeyEvent :: SDL.Event -> Maybe KeyEvent
eventToKeyEvent event = case SDL.eventPayload event of
  SDL.KeyboardEvent keyEvent ->
    let code   = SDL.keysymKeycode (SDL.keyboardEventKeysym keyEvent)
        motion = SDL.keyboardEventKeyMotion keyEvent
    in  keyToKeyEvent code motion
  _ -> Nothing

keyToKeyEvent :: SDL.Keycode -> SDL.InputMotion -> Maybe KeyEvent
keyToKeyEvent SDL.KeycodeReturn SDL.Pressed = Just Enter
keyToKeyEvent SDL.KeycodeW      SDL.Pressed = Just Up
keyToKeyEvent SDL.KeycodeUp     SDL.Pressed = Just Up
keyToKeyEvent SDL.KeycodeS      SDL.Pressed = Just Down
keyToKeyEvent SDL.KeycodeDown   SDL.Pressed = Just Down
keyToKeyEvent _                 _           = Nothing


-- Update

update :: Timer.LoopTimer -> [KeyEvent] -> MenuState -> IO (Either State MenuState)
update _ events state = do
  let list = UI.ensureValidIndex $ UI.applyEvents (buttonList state) events
  return $ if UI.action list
    then Left $ stateByButton $ UI.selectedButton list
    else Right $ state { buttonList = list }

stateByButton :: UI.Button -> State
stateByButton button = case UI.text button of
  "play"   -> Ingame
  "replay" -> Replay
  "exit"   -> Exit
  _        -> Menu


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
  return $ RenderConfig { font              = loadedFont
                        , backgroundColor   = SDL.V4 30 30 30 255
                        , textColor         = SDL.V4 255 255 255 255
                        , selectedTextColor = SDL.V4 0 191 255 255
                        }

deleteConfig :: RenderConfig -> IO ()
deleteConfig = SDLF.free . font

render :: Graphics -> RenderConfig -> Timer.LoopTimer -> MenuState -> IO ()
render (window, renderer) config _ state = do
  SDL.rendererDrawColor renderer SDL.$= backgroundColor config
  SDL.clear renderer
  let list       = buttonList state
  let view       = UI.screen list
  let selectedId = UI.selected list
  windowSize <- SDL.get $ SDL.windowSize window
  mapM_ (renderButton renderer config windowSize view selectedId) $ UI.buttons list
  SDL.present renderer

renderButton
  :: SDL.Renderer -> RenderConfig -> WindowSize -> Screen.Screen -> Int -> UI.Button -> IO ()
renderButton renderer config windowSize screen selectedId button = do
  let color = if selectedId == UI.id button then selectedTextColor config else textColor config
  Render.renderButton renderer windowSize screen (font config) color button
