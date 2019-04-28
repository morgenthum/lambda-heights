module SDL.GUI.Events.Select where

import qualified SDL
import SDL.GUI.Events.Types

eventToSelectEvent :: SDL.Event -> Maybe SelectEvent
eventToSelectEvent event = case SDL.eventPayload event of
  SDL.KeyboardEvent keyEvent ->
    let code   = SDL.keysymKeycode $ SDL.keyboardEventKeysym keyEvent
        motion = SDL.keyboardEventKeyMotion keyEvent
    in  keyToSelectEvent code motion
  _ -> Nothing

keyToSelectEvent :: SDL.Keycode -> SDL.InputMotion -> Maybe SelectEvent
keyToSelectEvent SDL.KeycodeLeft   SDL.Pressed = Just SelectLeft
keyToSelectEvent SDL.KeycodeUp     SDL.Pressed = Just SelectUp
keyToSelectEvent SDL.KeycodeRight  SDL.Pressed = Just SelectRight
keyToSelectEvent SDL.KeycodeDown   SDL.Pressed = Just SelectDown
keyToSelectEvent _                 _           = Nothing
