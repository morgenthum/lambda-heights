module LambdaTower.Ingame.Input
  ( keyInput
  )
where

import           Data.Maybe

import qualified SDL

import qualified LambdaTower.Ingame.Events     as Ingame

keyInput :: IO Ingame.Events
keyInput = do
  events <- SDL.pollEvents
  let controlEvents = mapMaybe eventToControlEvent events
  let playerEvents  = mapMaybe eventToPlayerEvent events
  return $ Ingame.Events controlEvents playerEvents

eventToControlEvent :: SDL.Event -> Maybe Ingame.ControlEvent
eventToControlEvent event = case SDL.eventPayload event of
  SDL.QuitEvent -> Just Ingame.Paused
  SDL.KeyboardEvent keyEvent ->
    let code   = SDL.keysymKeycode (SDL.keyboardEventKeysym keyEvent)
        motion = SDL.keyboardEventKeyMotion keyEvent
    in  keyToControlEvent code motion
  _ -> Nothing

keyToControlEvent :: SDL.Keycode -> SDL.InputMotion -> Maybe Ingame.ControlEvent
keyToControlEvent SDL.KeycodeEscape SDL.Pressed = Just Ingame.Paused
keyToControlEvent SDL.KeycodeP      SDL.Pressed = Just Ingame.Paused
keyToControlEvent _                 _           = Nothing

eventToPlayerEvent :: SDL.Event -> Maybe Ingame.PlayerEvent
eventToPlayerEvent event = case SDL.eventPayload event of
  SDL.KeyboardEvent keyEvent ->
    let code   = SDL.keysymKeycode (SDL.keyboardEventKeysym keyEvent)
        motion = SDL.keyboardEventKeyMotion keyEvent
    in  keyToPlayerEvent code motion
  _ -> Nothing

keyToPlayerEvent :: SDL.Keycode -> SDL.InputMotion -> Maybe Ingame.PlayerEvent
keyToPlayerEvent SDL.KeycodeA     SDL.Pressed  = Just $ Ingame.PlayerMoved Ingame.MoveLeft True
keyToPlayerEvent SDL.KeycodeA     SDL.Released = Just $ Ingame.PlayerMoved Ingame.MoveLeft False
keyToPlayerEvent SDL.KeycodeLeft  SDL.Pressed  = Just $ Ingame.PlayerMoved Ingame.MoveLeft True
keyToPlayerEvent SDL.KeycodeLeft  SDL.Released = Just $ Ingame.PlayerMoved Ingame.MoveLeft False
keyToPlayerEvent SDL.KeycodeD     SDL.Pressed  = Just $ Ingame.PlayerMoved Ingame.MoveRight True
keyToPlayerEvent SDL.KeycodeD     SDL.Released = Just $ Ingame.PlayerMoved Ingame.MoveRight False
keyToPlayerEvent SDL.KeycodeRight SDL.Pressed  = Just $ Ingame.PlayerMoved Ingame.MoveRight True
keyToPlayerEvent SDL.KeycodeRight SDL.Released = Just $ Ingame.PlayerMoved Ingame.MoveRight False
keyToPlayerEvent SDL.KeycodeSpace SDL.Pressed  = Just Ingame.PlayerJumped
keyToPlayerEvent _                _            = Nothing
