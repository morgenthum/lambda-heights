module LambdaHeights.Ingame.Input
  ( keyInput
  )
where

import           Data.Maybe

import           LambdaHeights.Types.Events

import qualified SDL

keyInput :: IO Events
keyInput = do
  events <- SDL.pollEvents
  return $ Events (mapMaybe eventToControlEvent events) (mapMaybe eventToPlayerEvent events)

eventToControlEvent :: SDL.Event -> Maybe ControlEvent
eventToControlEvent event = case SDL.eventPayload event of
  SDL.QuitEvent -> Just Paused
  SDL.KeyboardEvent keyEvent ->
    let code   = SDL.keysymKeycode (SDL.keyboardEventKeysym keyEvent)
        motion = SDL.keyboardEventKeyMotion keyEvent
    in  keyToControlEvent code motion
  _ -> Nothing

keyToControlEvent :: SDL.Keycode -> SDL.InputMotion -> Maybe ControlEvent
keyToControlEvent SDL.KeycodeEscape SDL.Pressed = Just Paused
keyToControlEvent SDL.KeycodeP      SDL.Pressed = Just Paused
keyToControlEvent _                 _           = Nothing

eventToPlayerEvent :: SDL.Event -> Maybe PlayerEvent
eventToPlayerEvent event = case SDL.eventPayload event of
  SDL.KeyboardEvent keyEvent ->
    let code   = SDL.keysymKeycode (SDL.keyboardEventKeysym keyEvent)
        motion = SDL.keyboardEventKeyMotion keyEvent
    in  keyToPlayerEvent code motion
  _ -> Nothing

keyToPlayerEvent :: SDL.Keycode -> SDL.InputMotion -> Maybe PlayerEvent
keyToPlayerEvent SDL.KeycodeA     SDL.Pressed  = Just $ PlayerMoved MoveLeft True
keyToPlayerEvent SDL.KeycodeA     SDL.Released = Just $ PlayerMoved MoveLeft False
keyToPlayerEvent SDL.KeycodeLeft  SDL.Pressed  = Just $ PlayerMoved MoveLeft True
keyToPlayerEvent SDL.KeycodeLeft  SDL.Released = Just $ PlayerMoved MoveLeft False
keyToPlayerEvent SDL.KeycodeD     SDL.Pressed  = Just $ PlayerMoved MoveRight True
keyToPlayerEvent SDL.KeycodeD     SDL.Released = Just $ PlayerMoved MoveRight False
keyToPlayerEvent SDL.KeycodeRight SDL.Pressed  = Just $ PlayerMoved MoveRight True
keyToPlayerEvent SDL.KeycodeRight SDL.Released = Just $ PlayerMoved MoveRight False
keyToPlayerEvent SDL.KeycodeW     SDL.Pressed  = Just PlayerJumped
keyToPlayerEvent SDL.KeycodeUp    SDL.Pressed  = Just PlayerJumped
keyToPlayerEvent SDL.KeycodeSpace SDL.Pressed  = Just PlayerJumped
keyToPlayerEvent _                _            = Nothing
