module LambdaHeights.Ingame.Update
  ( update
  , output
  )
where

import           Control.Monad.STM
import           Control.Concurrent.STM.TChan

import           Data.Function
import           Data.List


import qualified Control.Monad.State                     as M

import qualified LambdaHeights.Screen                    as Screen
import qualified LambdaHeights.Ingame.Collision          as Collision
import qualified LambdaHeights.Ingame.Pattern            as Pattern

import qualified LambdaHeights.Types.Events              as Events
import qualified LambdaHeights.Types.IngameState         as State
import qualified LambdaHeights.Types.Layer               as Layer
import qualified LambdaHeights.Types.Player              as Player
import qualified LambdaHeights.Types.Timer               as Timer

type Updater = Timer.LoopTimer -> Events.Events -> State.State -> Either State.Result State.State
type Output = Timer.LoopTimer -> Events.Events -> Either State.Result State.State -> IO ()


-- Update th world state.
-- 1. Applies occured events to the current world state.
-- 2. Updates the current world state using the update factor.

updateFactor :: Float
updateFactor = 1 / 128

update :: Updater
update timer events state =
  let playerEvents = Events.playerEvents events
      time         = State.time state
      screen       = State.screen state
      layers       = State.layers state
      player       = State.player state
      motion       = updateMotion playerEvents $ State.motion state
      state'       = State.State
        { State.time   = time + Timer.rate timer
        , State.screen = updateScreen player screen
        , State.motion = resetMotion motion
        , State.player = updatePlayer screen motion layers player
        , State.layers = updateLayers screen layers
        }
  in  updatedResult events state'

updatedResult :: Events.Events -> State.State -> Either State.Result State.State
updatedResult events state =
  let paused = elem Events.Paused $ Events.controlEvents events
  in  if dead (State.screen state) (State.player state)
        then Left $ State.Result State.Finished state
        else if paused then Left $ State.Result State.Pause state else Right state


-- Updating the screen involves two steps:
-- 1. Move the screen upwards over time.
-- 2. Ensure that the player is always visible within the screen.

updateScreen :: Player.Player -> Screen.Screen -> Screen.Screen
updateScreen player = scrollScreenToPlayer player . scrollScreenOverTime

scrollScreenOverTime :: Screen.Screen -> Screen.Screen
scrollScreenOverTime screen =
  let height = Screen.bottom screen
      factor = min 400 (100 + height / 100)
  in  if height == 0 then screen else scrollScreen (updateFactor * factor) screen

scrollScreenToPlayer :: Player.Player -> Screen.Screen -> Screen.Screen
scrollScreenToPlayer player screen =
  let (_, y)   = Player.position player
      distance = Screen.top screen - y
  in  if distance < 250 then scrollScreen (250 - distance) screen else screen

scrollScreen :: Float -> Screen.Screen -> Screen.Screen
scrollScreen delta screen =
  screen { Screen.top = Screen.top screen + delta, Screen.bottom = Screen.bottom screen + delta }


-- Apply the player events to the motion.

updateMotion :: [Events.PlayerEvent] -> State.Motion -> State.Motion
updateMotion events motion = applyPlayerEvents motion events

applyPlayerEvents :: State.Motion -> [Events.PlayerEvent] -> State.Motion
applyPlayerEvents = foldl applyPlayerEvent

applyPlayerEvent :: State.Motion -> Events.PlayerEvent -> State.Motion
applyPlayerEvent moveState (Events.PlayerMoved Events.MoveLeft b) =
  moveState { State.moveLeft = b }
applyPlayerEvent moveState (Events.PlayerMoved Events.MoveRight b) =
  moveState { State.moveRight = b }
applyPlayerEvent moveState Events.PlayerJumped = moveState { State.jump = True }

resetMotion :: State.Motion -> State.Motion
resetMotion motion = motion { State.jump = False }


-- Updating the player involves the following steps:
-- 1. Update the motion of the player.
-- 2. Apply collision detection and corrections.
-- 3. Update the score (highest reached layer).

updatePlayer :: Screen.Screen -> State.Motion -> [Layer.Layer] -> Player.Player -> Player.Player
updatePlayer screen motion layers =
  updateScore layers
    . (`collideWith` layers)
    . (`bounceFrom` screen)
    . updatePlayerMotion motion
    . updateMotionType layers

updateScore :: [Layer.Layer] -> Player.Player -> Player.Player
updateScore layers player = case find (player `onTop`) layers of
  Nothing    -> player
  Just layer -> player { Player.score = max (Layer.layerId layer) (Player.score player) }

updateMotionType :: [Layer.Layer] -> Player.Player -> Player.Player
updateMotionType layers player =
  let collided = not $ null $ find (player `inside`) layers
  in  player { Player.motionType = if collided then Player.Ground else Player.Air }


-- Update the motion of the player (acceleration, velocity, position).

updatePlayerMotion :: State.Motion -> Player.Player -> Player.Player
updatePlayerMotion motion player = updateAcc motion player & updateVel & updatePos

updateAcc :: State.Motion -> Player.Player -> Player.Player
updateAcc motion player =
  let vel = Player.velocity player
      acc = case Player.motionType player of
        Player.Ground -> calcJumpAcc motion vel $ calcGroundAcc motion
        Player.Air    -> calcAirAcc motion
  in  player { Player.acceleration = acc }

calcJumpAcc :: State.Motion -> Player.Velocity -> Player.Acceleration -> Player.Acceleration
calcJumpAcc motion vel acc =
  let jump         = State.jump motion
      (accX, _   ) = acc
      (velX, velY) = vel
      velLength    = sqrt $ (velX ** 2) + (velY ** 2)
      go | jump && velLength >= 1500 = (accX, 400000)
         | jump && velLength >= 750  = (accX, 320000)
         | jump                      = (accX, 160000)
         | otherwise                 = acc
  in  go

calcGroundAcc :: State.Motion -> Player.Acceleration
calcGroundAcc motion =
  let left  = State.moveLeft motion
      right = State.moveRight motion
      go | left && not right = (-8000, -4000)
         | right && not left = (8000, -4000)
         | otherwise         = (0, -4000)
  in  go

calcAirAcc :: State.Motion -> Player.Acceleration
calcAirAcc motion =
  let left  = State.moveLeft motion
      right = State.moveRight motion
      go | left && not right = (-4000, -4000)
         | right && not left = (4000, -4000)
         | otherwise         = (0, -4000)
  in  go

updateVel :: Player.Player -> Player.Player
updateVel player =
  let vel = decelerate (Player.motionType player) $ Player.velocity player
      acc = Player.acceleration player
  in  player { Player.velocity = applyWithFactor updateFactor vel acc }

decelerate :: Player.MotionType -> Player.Velocity -> Player.Velocity
decelerate Player.Ground (x, y) = (x * 0.925, y)
decelerate Player.Air    (x, y) = (x * 0.99, y)

updatePos :: Player.Player -> Player.Player
updatePos player =
  let pos = Player.position player
      vel = Player.velocity player
  in  player { Player.position = applyWithFactor updateFactor pos vel }

applyWithFactor :: Fractional a => a -> (a, a) -> (a, a) -> (a, a)
applyWithFactor factor (x, y) (x', y') = (x + x' * factor, y + y' * factor)


-- Correct the position and velocity if it is colliding with a layer
-- or the bounds of the screen.

bounceFrom :: Player.Player -> Screen.Screen -> Player.Player
player `bounceFrom` screen =
  let (posX, posY) = Player.position player
      (velX, velY) = Player.velocity player
      minX         = Screen.left screen
      maxX         = Screen.right screen
      outLeft      = posX < minX && velX < 0
      outRight     = posX > maxX && velX > 0
      go | outLeft   = player { Player.position = (minX, posY), Player.velocity = (-velX, velY) }
         | outRight  = player { Player.position = (maxX, posY), Player.velocity = (-velX, velY) }
         | otherwise = player
  in  go

collideWith :: Player.Player -> [Layer.Layer] -> Player.Player
player `collideWith` layers = if falling player
  then case find (player `onTop`) layers of
    Nothing    -> player
    Just layer -> settle $ player `onto` layer
  else player

onto :: Player.Player -> Layer.Layer -> Player.Player
player `onto` layer =
  let (posX, _) = Player.position player in player { Player.position = (posX, Layer.posY layer) }

settle :: Player.Player -> Player.Player
settle player = let (velX, _) = Player.velocity player in player { Player.velocity = (velX, 0) }

dead :: Screen.Screen -> Player.Player -> Bool
dead screen player = let (_, y) = Player.position player in y < Screen.bottom screen

falling :: Player.Player -> Bool
falling player = let (_, y) = Player.velocity player in y < 0

inside :: Player.Player -> Layer.Layer -> Bool
player `inside` layer =
  let playerPos = Player.position player
      layerPos  = Layer.position layer
      size      = Layer.size layer
  in  playerPos `Collision.inside` Collision.Rect layerPos size

onTop :: Player.Player -> Layer.Layer -> Bool
player `onTop` layer =
  let playerPos = Player.position player
      layerPos  = Layer.position layer
      (w, _)    = Layer.size layer
  in  playerPos `Collision.inside` Collision.Rect layerPos (w, 20)


-- Drop passed and generate new layers.

newPattern :: [Pattern.PatternEntry]
newPattern =
  Pattern.combine 1 [Pattern.leftRightPattern, Pattern.boostPattern, Pattern.stairsPattern]

updateLayers :: Screen.Screen -> [Layer.Layer] -> [Layer.Layer]
updateLayers screen = fillLayers screen . dropPassedLayers screen

fillLayers :: Screen.Screen -> [Layer.Layer] -> [Layer.Layer]
fillLayers screen []               = unfoldLayer screen Layer.ground
fillLayers screen (layer : layers) = unfoldLayer screen layer ++ layers

unfoldLayer :: Screen.Screen -> Layer.Layer -> [Layer.Layer]
unfoldLayer screen = reverse . unfoldr (generateLayer generator screen)
  where generator = nextLayerByPattern newPattern

nextLayerByPattern :: [Pattern.PatternEntry] -> Layer.Layer -> Layer.Layer
nextLayerByPattern [] layer = layer
nextLayerByPattern (p : ps) layer =
  let entryIds = map Pattern.entryId (p : ps)
  in  case elemIndex (Layer.entryId layer) entryIds of
        Nothing -> layer `deriveFrom` p
        Just i  -> case (p : ps ++ [p]) !! (i + 1) of
          p' -> layer `deriveFrom` p'

deriveFrom :: Layer.Layer -> Pattern.PatternEntry -> Layer.Layer
layer `deriveFrom` entry =
  let Pattern.PatternEntry entryId ((w, h), x) d = entry
      layerId = Layer.layerId layer + 1
      (_, y)  = Layer.position layer
  in  Layer.Layer layerId entryId (w, h) (x, y + d)

generateLayer
  :: (Layer.Layer -> Layer.Layer)
  -> Screen.Screen
  -> Layer.Layer
  -> Maybe (Layer.Layer, Layer.Layer)
generateLayer generator screen layer =
  if Screen.top screen < Layer.posY layer - 500 then Nothing else Just (layer, generator layer)

dropPassedLayers :: Screen.Screen -> [Layer.Layer] -> [Layer.Layer]
dropPassedLayers screen = filter (not . passed screen)

passed :: Screen.Screen -> Layer.Layer -> Bool
screen `passed` layer = Screen.bottom screen > Layer.posY layer


-- Broadcast occured events into a channel.

output :: TChan (Maybe [Events.PlayerEvent]) -> Output
output channel _ events eitherState = do
  M.liftIO $ atomically $ writeTChan channel $ Just $ Events.playerEvents events
  case eitherState of
    Left  _ -> M.liftIO $ atomically $ writeTChan channel Nothing
    Right _ -> return ()
