module LambdaTower.Menu.Input where

import Data.Maybe

import qualified SDL

import LambdaTower.Menu.Events
import LambdaTower.Loop

handleKeyInput :: InputHandler IO [KeyEvent]
handleKeyInput = mapMaybe eventToKeyEvent <$> SDL.pollEvents

eventToKeyEvent :: SDL.Event -> Maybe KeyEvent
eventToKeyEvent event =
  case SDL.eventPayload event of
    SDL.KeyboardEvent keyEvent -> keyToKeyEvent code motion
      where code = SDL.keysymKeycode (SDL.keyboardEventKeysym keyEvent)
            motion = SDL.keyboardEventKeyMotion keyEvent
    _ -> Nothing

keyToKeyEvent :: SDL.Keycode -> SDL.InputMotion -> Maybe KeyEvent
keyToKeyEvent SDL.KeycodeReturn SDL.Pressed  = Just Enter
keyToKeyEvent SDL.KeycodeW      SDL.Pressed  = Just Up
keyToKeyEvent SDL.KeycodeUp     SDL.Pressed  = Just Up
keyToKeyEvent SDL.KeycodeS      SDL.Pressed  = Just Down
keyToKeyEvent SDL.KeycodeDown   SDL.Pressed  = Just Down
keyToKeyEvent _                 _            = Nothing