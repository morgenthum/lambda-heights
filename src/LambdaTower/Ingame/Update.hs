module LambdaTower.Ingame.Update (
  update,
  updateAndWrite
) where

import Control.Monad.STM
import Control.Concurrent.STM.TChan

import qualified Control.Lens as L
import qualified Control.Monad.State as M

import Data.List

import LambdaTower.Loop
import LambdaTower.Types

import qualified LambdaTower.Types.GameEvents as Events
import qualified LambdaTower.Types.GameState as State
import qualified LambdaTower.Types.Layer as Layer
import qualified LambdaTower.Types.Pattern as Pattern
import qualified LambdaTower.Types.Player as Player
import qualified LambdaTower.Types.Timer as Timer
import qualified LambdaTower.Screen as Screen

-- Factor for scrolling the screen and update the acceleration.

updateFactor :: Float
updateFactor = 1 / 128


-- Wrapper around one update to broadcast the occured events for serialization

updateAndWrite :: TChan (Maybe [Events.PlayerEvent]) -> Updater IO State.GameState State.GameResult Events.GameEvents
updateAndWrite channel timer events gameState = do
  result <- update timer events gameState
  M.liftIO $ atomically $ writeTChan channel $ Just $ Events.playerEvents events
  case result of
    Left _ -> M.liftIO $ atomically $ writeTChan channel Nothing
    Right _ -> return ()
  return result


-- Control flow over one update cycle.

type GameStateUpdate a = M.StateT State.GameState IO a

update :: Updater IO State.GameState State.GameResult Events.GameEvents
update timer events = M.evalStateT go
  where go = do updateTimeM timer
                updateScreenM
                updateMotionM (Events.playerEvents events)
                updateLayersM
                updatePlayerM
                resetMotionM
                returnStateM (Events.controlEvents events)

updateTimeM :: Timer.LoopTimer -> GameStateUpdate ()
updateTimeM timer = M.modify f
  where f s = s { State.time = State.time s + fromIntegral (L.view Timer.rate timer) }

updateScreenM :: GameStateUpdate ()
updateScreenM = M.modify f
  where f s = s { State.screen = updateScreen (State.player s) $ State.screen s }

updateMotionM :: [Events.PlayerEvent] -> GameStateUpdate ()
updateMotionM events = M.modify f
  where f s = s { State.motion = updateMotion (State.motion s) events }

updateLayersM :: GameStateUpdate ()
updateLayersM = M.modify f
  where f s = s { State.layers = updateLayers (State.screen s) $ State.layers s }

updatePlayerM :: GameStateUpdate ()
updatePlayerM = M.modify f
  where f s = s { State.player = updatePlayer (State.screen s) (State.motion s) (State.layers s) $ State.player s }

resetMotionM :: GameStateUpdate ()
resetMotionM = M.modify f
  where f s = s {
    State.motion = (State.motion s) {
      State.jump = False,
      State.air = playerInAir (State.layers s) $ State.player s
    }
  }

returnStateM :: [Events.ControlEvent] -> GameStateUpdate (Either State.GameResult State.GameState)
returnStateM events = do
  s <- M.get
  let dead = playerDead (State.screen s) (State.player s)
  let paused = elem Events.Paused events
  return $
    if dead then Left $ State.GameResult s State.Finished
    else if paused then Left $ State.GameResult s State.Pause
    else Right s


-- Updating the view involves two steps:
-- a) Move the view upwards over time.
-- b) Ensure that the player is always visible within the view.

updateScreen :: Player.Player -> Screen.Screen -> Screen.Screen
updateScreen player = scrollScreenToPlayer player . scrollScreenOverTime

scrollScreenOverTime :: Screen.Screen -> Screen.Screen
scrollScreenOverTime screen = if height == 0 then screen else scrollScreen (updateFactor * factor) screen
  where height = Screen.bottom screen
        factor = min 400 (100 + height / 100)

scrollScreenToPlayer :: Player.Player -> Screen.Screen -> Screen.Screen
scrollScreenToPlayer player screen = if space < 250 then scrollScreen (250 - space) screen else screen
  where (_, y) = Player.position player
        space = Screen.top screen - y

scrollScreen :: Float -> Screen.Screen -> Screen.Screen
scrollScreen delta screen = screen {
  Screen.top = Screen.top screen + delta,
  Screen.bottom = Screen.bottom screen + delta
}


-- Apply the player events to the motion.

updateMotion :: State.Motion -> [Events.PlayerEvent] -> State.Motion
updateMotion = foldl applyPlayerEvents

applyPlayerEvents :: State.Motion -> Events.PlayerEvent -> State.Motion
applyPlayerEvents moveState (Events.PlayerMoved Events.MoveLeft b) = moveState { State.moveLeft = b }
applyPlayerEvents moveState (Events.PlayerMoved Events.MoveRight b) = moveState { State.moveRight = b }
applyPlayerEvents moveState Events.PlayerJumped = moveState { State.jump = True }


-- Drop passed and generate new layers.

newPattern :: [Pattern.PatternEntry]
newPattern = Pattern.combine 1 [
    Pattern.leftRightPattern,
    Pattern.boostPattern,
    Pattern.stairsPattern
  ]

updateLayers :: Screen.Screen -> [Layer.Layer] -> [Layer.Layer]
updateLayers screen = fillLayers screen . dropPassedLayers screen

fillLayers :: Screen.Screen -> [Layer.Layer] -> [Layer.Layer]
fillLayers screen [] = unfoldLayers screen Layer.ground
fillLayers screen (layer:layers) = unfoldLayers screen layer ++ layers

unfoldLayers :: Screen.Screen -> Layer.Layer -> [Layer.Layer]
unfoldLayers screen = reverse . unfoldr (generateLayer generator screen)
  where generator = nextLayerByPattern newPattern

nextLayerByPattern :: [Pattern.PatternEntry] -> Layer.Layer -> Layer.Layer
nextLayerByPattern [] layer = layer
nextLayerByPattern (p:ps) layer =
  case elemIndex (Layer.entryId layer) entryIds of
    Nothing -> deriveLayer p layer
    Just i -> case (p:ps ++ [p]) !! (i + 1) of p' -> deriveLayer p' layer
  where entryIds = map Pattern.entryId (p:ps)

deriveLayer :: Pattern.PatternEntry -> Layer.Layer -> Layer.Layer
deriveLayer pattern layer = Layer.Layer layerId entryId (w, h) (x, y + d)
  where Pattern.PatternEntry entryId ((w, h), x) d = pattern
        layerId = Layer.id layer + 1
        (_, y) = Layer.position layer

generateLayer :: (Layer.Layer -> Layer.Layer) -> Screen.Screen -> Layer.Layer -> Maybe (Layer.Layer, Layer.Layer)
generateLayer generator screen layer =
  if Screen.top screen < Layer.posY layer - 500
  then Nothing
  else Just (layer, generator layer)

dropPassedLayers :: Screen.Screen -> [Layer.Layer] -> [Layer.Layer]
dropPassedLayers screen = filter $ not . layerPassed screen

layerPassed :: Screen.Screen -> Layer.Layer -> Bool
layerPassed screen layer = Screen.bottom screen > Layer.posY layer


-- Updating the player involves the following steps:
-- a) Update the motion of the player.
-- b) Apply collision detection and corrections.
-- c) Update the score (highest reached layer).

updatePlayer :: Screen.Screen -> State.Motion -> [Layer.Layer] -> Player.Player -> Player.Player
updatePlayer screen motion layers =
  updateScore layers
  . collidePlayerWithLayers layers
  . bouncePlayerFromBounds screen
  . updatePlayerMotion motion

updateScore :: [Layer.Layer] -> Player.Player -> Player.Player
updateScore layers player =
  case collidedLayer layers player of
    Nothing -> player
    Just layer -> player { Player.score = max (Layer.id layer) (Player.score player) }


-- Update the motion of the player (acceleration, velocity, position).

updatePlayerMotion :: State.Motion -> Player.Player -> Player.Player
updatePlayerMotion motion player = player { Player.position = pos, Player.velocity = vel, Player.acceleration = acc }
  where acc = updateAcceleration (Player.acceleration player) (Player.velocity player) motion
        vel = updateVelocity (Player.velocity player) motion acc
        pos = updatePosition (Player.position player) vel

updateAcceleration :: Player.Acceleration -> Player.Velocity -> State.Motion -> Player.Acceleration
updateAcceleration acc vel motion =
  if State.air motion
  then updateAirAcceleration motion
  else updateGroundAcceleration acc vel motion

updateGroundAcceleration :: Player.Acceleration -> Player.Velocity -> State.Motion -> Player.Acceleration
updateGroundAcceleration (accX, _) (velX, velY) motion
  | jump && fast = (accX, 320000)
  | jump = (accX, 160000)
  | left && not right = (-8000, -4000)
  | right && not left = (8000, -4000)
  | otherwise = (0, -4000)
  where left = State.moveLeft motion
        right = State.moveRight motion
        jump = State.jump motion
        vel = sqrt $ (velX ** 2) + (velY ** 2)
        fast = vel >= 750

updateAirAcceleration :: State.Motion -> Player.Acceleration
updateAirAcceleration motion
  | left && not right = (-4000, -4000)
  | right && not left = (4000, -4000)
  | otherwise = (0, -4000)
  where left = State.moveLeft motion
        right = State.moveRight motion

updateVelocity :: Player.Velocity -> State.Motion -> Player.Acceleration -> Player.Velocity
updateVelocity vel motion = applyAcceleration (decelerate motion vel)

applyAcceleration :: Player.Velocity -> Player.Acceleration -> Player.Velocity
applyAcceleration (x, y) (x', y') = (x+x'*updateFactor, y+y'*updateFactor)

decelerate :: State.Motion -> Player.Velocity -> Player.Velocity
decelerate motion (x, y) = if State.air motion then (x * 0.99, y) else (x * 0.925, y)

updatePosition :: Position -> Player.Velocity -> Position
updatePosition = applyAcceleration


-- Correct the position and velocity if it is colliding with a layer
-- or the bounds of the level.

bouncePlayerFromBounds :: Screen.Screen -> Player.Player -> Player.Player
bouncePlayerFromBounds screen player
  | outLeft = player { Player.position = (minX, posY), Player.velocity = (-velX, velY) }
  | outRight = player { Player.position = (maxX, posY), Player.velocity = (-velX, velY) }
  | otherwise = player
  where (posX, posY) = Player.position player
        (velX, velY) = Player.velocity player
        minX = Screen.left screen
        maxX = Screen.right screen
        outLeft = posX < minX && velX < 0
        outRight = posX > maxX && velX > 0

collidePlayerWithLayers :: [Layer.Layer] -> Player.Player -> Player.Player
collidePlayerWithLayers layers player =
  if playerFalling player then
    case collidedLayer layers player of
      Nothing -> player
      Just layer ->
        if shouldLift layer player
        then resetVelocityY $ liftPlayerOnLayer layer player
        else player
  else player

shouldLift :: Layer.Layer -> Player.Player -> Bool
shouldLift layer player = xInRect p (w, h) x && yInRect p (w, 20) y
  where p = Layer.position layer
        (w, h) = Layer.size layer
        (x, y) = Player.position player

playerDead :: Screen.Screen -> Player.Player -> Bool
playerDead screen player = let (_, y) = Player.position player in y < Screen.bottom screen

playerFalling :: Player.Player -> Bool
playerFalling player = let (_, y) = Player.velocity player in y < 0

collidedLayer :: [Layer.Layer] -> Player.Player -> Maybe Layer.Layer
collidedLayer layers player =
  case filter (positionInLayer $ Player.position player) layers of
    [] -> Nothing
    layer:_ -> Just layer

positionInLayer :: Position -> Layer.Layer -> Bool
positionInLayer (x, y) layer = xInRect p s x && yInRect p s y
  where p = Layer.position layer
        s = Layer.size layer

liftPlayerOnLayer :: Layer.Layer -> Player.Player -> Player.Player
liftPlayerOnLayer layer player = player { Player.position = (posX, Layer.posY layer) }
  where (posX, _) = Player.position player

resetVelocityY :: Player.Player -> Player.Player
resetVelocityY player = player { Player.velocity = (velX, 0) }
  where (velX, _) = Player.velocity player

playerInAir :: [Layer.Layer] -> Player.Player -> Bool
playerInAir layers = null . collidedLayer layers

xInRect :: Position -> Size -> Float -> Bool
xInRect (posX, _) (w, _) x = x >= posX && x <= posX + w

yInRect :: Position -> Size -> Float -> Bool
yInRect (_, posY) (_, h) y = y <= posY && y >= posY - h