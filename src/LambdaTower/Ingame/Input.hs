module LambdaTower.Ingame.Input
  ( keyInput
  )
where

import           Data.Maybe

import qualified SDL

import qualified LambdaTower.Ingame.GameEvents as Events

keyInput :: IO Events.GameEvents
keyInput = do
  events <- SDL.pollEvents
  let controlEvents = mapMaybe eventToControlEvent events
  let playerEvents  = mapMaybe eventToPlayerEvent events
  return $ Events.GameEvents controlEvents playerEvents

eventToControlEvent :: SDL.Event -> Maybe Events.ControlEvent
eventToControlEvent event = case SDL.eventPayload event of
  SDL.QuitEvent -> Just Events.Paused
  SDL.KeyboardEvent keyEvent ->
    let code   = SDL.keysymKeycode (SDL.keyboardEventKeysym keyEvent)
        motion = SDL.keyboardEventKeyMotion keyEvent
    in  keyToControlEvent code motion
  _ -> Nothing

keyToControlEvent :: SDL.Keycode -> SDL.InputMotion -> Maybe Events.ControlEvent
keyToControlEvent SDL.KeycodeEscape SDL.Pressed = Just Events.Paused
keyToControlEvent SDL.KeycodeP      SDL.Pressed = Just Events.Paused
keyToControlEvent _                 _           = Nothing

eventToPlayerEvent :: SDL.Event -> Maybe Events.PlayerEvent
eventToPlayerEvent event = case SDL.eventPayload event of
  SDL.KeyboardEvent keyEvent ->
    let code   = SDL.keysymKeycode (SDL.keyboardEventKeysym keyEvent)
        motion = SDL.keyboardEventKeyMotion keyEvent
    in  keyToPlayerEvent code motion
  _ -> Nothing

keyToPlayerEvent :: SDL.Keycode -> SDL.InputMotion -> Maybe Events.PlayerEvent
keyToPlayerEvent SDL.KeycodeA     SDL.Pressed  = Just $ Events.PlayerMoved Events.MoveLeft True
keyToPlayerEvent SDL.KeycodeA     SDL.Released = Just $ Events.PlayerMoved Events.MoveLeft False
keyToPlayerEvent SDL.KeycodeLeft  SDL.Pressed  = Just $ Events.PlayerMoved Events.MoveLeft True
keyToPlayerEvent SDL.KeycodeLeft  SDL.Released = Just $ Events.PlayerMoved Events.MoveLeft False
keyToPlayerEvent SDL.KeycodeD     SDL.Pressed  = Just $ Events.PlayerMoved Events.MoveRight True
keyToPlayerEvent SDL.KeycodeD     SDL.Released = Just $ Events.PlayerMoved Events.MoveRight False
keyToPlayerEvent SDL.KeycodeRight SDL.Pressed  = Just $ Events.PlayerMoved Events.MoveRight True
keyToPlayerEvent SDL.KeycodeRight SDL.Released = Just $ Events.PlayerMoved Events.MoveRight False
keyToPlayerEvent SDL.KeycodeSpace SDL.Pressed  = Just Events.PlayerJumped
keyToPlayerEvent _                _            = Nothing
