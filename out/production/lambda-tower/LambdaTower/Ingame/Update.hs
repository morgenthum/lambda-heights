module LambdaTower.Ingame.Update (
  replayUpdate,
  updateAndWrite,
  update
) where

import Control.Monad.State
import Control.Monad.STM
import Control.Concurrent.STM.TChan

import Data.List

import LambdaTower.Loop

import qualified LambdaTower.Ingame.Events as E
import qualified LambdaTower.Ingame.GameState as G
import qualified LambdaTower.Ingame.Layer as L
import qualified LambdaTower.Ingame.Player as P
import qualified LambdaTower.Screen as G

type GameStateUpdate a = StateT G.GameState IO a

deltaTime :: Float
deltaTime = 1 / 128

-- Updating the replays need no calculations because we already have all states.
-- We need two steps:
-- a) Return the front state of the list and remove it for the next cycle.
-- b) Return the score if the replay is done.

replayUpdate :: Updater IO ([E.PlayerEvent], [[E.PlayerEvent]], G.GameState) P.Score ()
replayUpdate _ (_, [], s) = return $ Right $ P.score . G.player $ s
replayUpdate _ (_, e:es, s) = do
  eitherState <- update e s
  case eitherState of
    Left s' -> return . Left $ (e, es, s')
    Right score -> return . Right $ score


-- Wrapper around one update to broadcast the occured events for serialization

updateAndWrite :: TChan (Maybe [E.PlayerEvent]) -> Updater IO G.GameState P.Score [E.PlayerEvent]
updateAndWrite channel events gameState = do
  result <- update events gameState
  case result of
    Left _ -> liftIO . atomically . writeTChan channel $ Just events
    Right _ -> liftIO . atomically . writeTChan channel $ Nothing
  return result


-- Control flow over one update cycle.

update :: Updater IO G.GameState P.Score [E.PlayerEvent]
update events = evalStateT go
  where go = do updateViewM
                updateMotionM events
                updateLayersM
                updatePlayerM
                resetMotionM
                returnStateM

updateViewM :: GameStateUpdate ()
updateViewM = modify f
  where f s = s { G.view = updateView (G.player s) $ G.view s }

updateMotionM :: [E.PlayerEvent] -> GameStateUpdate ()
updateMotionM events = modify f
  where f s = s { G.motion = updateMotion (G.motion s) events }

updateLayersM :: GameStateUpdate ()
updateLayersM = modify f
  where f s = s { G.layers = updateLayers (G.view s) $ G.layers s }

updatePlayerM :: GameStateUpdate ()
updatePlayerM = modify f
  where f s = s { G.player = updatePlayer (G.view s) (G.motion s) (G.layers s) $ G.player s }

resetMotionM :: GameStateUpdate ()
resetMotionM = modify f
  where f s = s {
    G.motion = (G.motion s) {
      G.jump = False,
      G.air = playerInAir (G.player s) (G.layers s)
    }
  }

returnStateM :: GameStateUpdate (Either G.GameState P.Score)
returnStateM = do
  s <- get
  return $ if playerDead (G.view s) (G.player s)
    then Right $ P.score $ G.player s
    else Left s


-- Updating the view involves two steps:
-- a) Move the view upwards over time.
-- b) Ensure that the player is always visible within the view.

updateView :: P.Player -> G.Screen -> G.Screen
updateView player = scrollViewToPlayer player . scrollViewOverTime

scrollViewOverTime :: G.Screen -> G.Screen
scrollViewOverTime view = if height == 0 then view else scrollView (deltaTime*factor) view
  where height = G.bottom view
        factor = min 400 (100+height/100)

scrollViewToPlayer :: P.Player -> G.Screen -> G.Screen
scrollViewToPlayer player view = if distance < 250 then scrollView (250-distance) view else view
  where (_, y) = P.position player
        distance = G.top view - y

scrollView :: Float -> G.Screen -> G.Screen
scrollView delta view = view {
  G.top = G.top view + delta,
  G.bottom = G.bottom view + delta
}


-- Apply the player events to the motion.

updateMotion :: G.Motion -> [E.PlayerEvent] -> G.Motion
updateMotion = foldl applyPlayerEvents

applyPlayerEvents :: G.Motion -> E.PlayerEvent -> G.Motion
applyPlayerEvents moveState (E.PlayerMoved E.MoveLeft b) = moveState { G.moveLeft = b }
applyPlayerEvents moveState (E.PlayerMoved E.MoveRight b) = moveState { G.moveRight = b }
applyPlayerEvents moveState E.PlayerJumped = moveState { G.jump = True }


-- Drop passed and generate new layers.

updateLayers :: G.Screen -> [L.Layer] -> [L.Layer]
updateLayers view = fillLayers view . dropPassedLayers view

fillLayers :: G.Screen -> [L.Layer] -> [L.Layer]
fillLayers view [] = unfoldLayers view L.ground
fillLayers view (layer:layers) = unfoldLayers view layer ++ layers

unfoldLayers :: G.Screen -> L.Layer -> [L.Layer]
unfoldLayers view = reverse . unfoldr (generateLayer view)

generateLayer :: G.Screen -> L.Layer -> Maybe (L.Layer, L.Layer)
generateLayer view layer = if G.top view < L.posY layer then Nothing else Just (layer, nextLayer layer)

nextLayer :: L.Layer -> L.Layer
nextLayer layer =
  case (L.id layer, L.size layer, L.position layer) of
    (layerId, (1000, h), (_, y)) -> L.Layer (layerId+1) (500, h) (100, y+200)
    (layerId, (500, h), (100, y)) -> L.Layer (layerId+1) (500, h) (400, y+200)
    (layerId, (500, h), (400, y)) -> L.Layer (layerId+1) (500, h) (100, y+200)

dropPassedLayers :: G.Screen -> [L.Layer] -> [L.Layer]
dropPassedLayers view = filter $ not . layerPassed view

layerPassed :: G.Screen -> L.Layer -> Bool
layerPassed view layer = G.bottom view > L.posY layer


-- Updating the player involves the following steps:
-- a) Update the motion of the player.
-- b) Apply collision detection and corrections.
-- c) Update the score (highest reached layer).

updatePlayer :: G.Screen -> G.Motion -> [L.Layer] -> P.Player -> P.Player
updatePlayer view motion layers =
  updateScore layers
  . collidePlayerWithLayers layers
  . bouncePlayerFromBounds view
  . updatePlayerMotion motion

updateScore :: [L.Layer] -> P.Player -> P.Player
updateScore layers player =
  case layerCollidedWithPlayer player layers of
    Nothing -> player
    Just layer -> player { P.score = max (L.id layer) (P.score player) }


-- Update the motion of the player (acceleration, velocity, position).

updatePlayerMotion :: G.Motion -> P.Player -> P.Player
updatePlayerMotion motion player = player { P.position = pos, P.velocity = vel, P.acceleration = acc }
  where acc = updateAcceleration (P.acceleration player) (P.velocity player) motion
        vel = updateVelocity (P.velocity player) motion acc
        pos = updatePosition (P.position player) vel

updateAcceleration :: P.Acceleration -> P.Velocity -> G.Motion -> P.Acceleration
updateAcceleration acc vel motion =
  if G.air motion
  then updateAirAcceleration motion
  else updateGroundAcceleration acc vel motion

updateGroundAcceleration :: P.Acceleration -> P.Velocity -> G.Motion -> P.Acceleration
updateGroundAcceleration (accX, _) (velX, velY) motion
  | jump && (abs velX > 750 || abs velY > 750) = (accX, 250000)
  | jump = (accX, 125000)
  | left && right = (0, -2000)
  | left = (-7500, -2000)
  | right = (7500, -2000)
  | otherwise = (0, -2000)
  where left = G.moveLeft motion
        right = G.moveRight motion
        jump = G.jump motion

updateAirAcceleration :: G.Motion -> P.Acceleration
updateAirAcceleration motion
  | left && right = (0, -2000)
  | left = (-1500, -2000)
  | right = (1500, -2000)
  | otherwise = (0, -2000)
  where left = G.moveLeft motion
        right = G.moveRight motion

updateVelocity :: P.Velocity -> G.Motion -> P.Acceleration -> P.Velocity
updateVelocity vel motion = applyAcceleration (decelerate motion vel)

applyAcceleration :: P.Velocity -> P.Acceleration -> P.Velocity
applyAcceleration (x, y) (x', y') = (x+x'*deltaTime, y+y'*deltaTime)

decelerate :: G.Motion -> P.Velocity -> P.Velocity
decelerate motion (x, y) = if G.air motion then (x, y) else (x*0.925, y)

updatePosition :: P.Position -> P.Velocity -> P.Position
updatePosition = applyAcceleration


-- Correct the position and velocity if it is colliding with a layer
-- or the bounds of the level.

bouncePlayerFromBounds :: G.Screen -> P.Player -> P.Player
bouncePlayerFromBounds view player
  | outLeft = player { P.position = (minX, posY), P.velocity = ((-velX) * 0.75, velY) }
  | outRight = player { P.position = (maxX, posY), P.velocity = ((-velX) * 0.75, velY) }
  | otherwise = player
  where (posX, posY) = P.position player
        (velX, velY) = P.velocity player
        minX = G.left view
        maxX = G.right view
        outLeft = posX < minX && velX < 0
        outRight = posX > maxX && velX > 0

collidePlayerWithLayers :: [L.Layer] -> P.Player -> P.Player
collidePlayerWithLayers layers player =
  if playerFalling player then
    case layerCollidedWithPlayer player layers of
      Nothing -> player
      Just layer -> resetVelocityY . liftPlayerOnLayer layer $ player
  else player

playerDead :: G.Screen -> P.Player -> Bool
playerDead view player = let (_, y) = P.position player in y < G.bottom view

playerFalling :: P.Player -> Bool
playerFalling player = let (_, y) = P.velocity player in y < 0

layerCollidedWithPlayer :: P.Player -> [L.Layer] -> Maybe L.Layer
layerCollidedWithPlayer player layers =
  case filter (positionInLayer $ P.position player) layers of
    [] -> Nothing
    layer:_ -> Just layer

positionInLayer :: P.Position -> L.Layer -> Bool
positionInLayer (x, y) layer = x >= lx && x <= lx + lw && y <= ly && y >= ly - lh
  where (lw, lh) = L.size layer
        (lx, ly) = L.position layer

liftPlayerOnLayer :: L.Layer -> P.Player -> P.Player
liftPlayerOnLayer layer player = player { P.position = (posX, L.posY layer) }
  where (posX, _) = P.position player

resetVelocityY :: P.Player -> P.Player
resetVelocityY player = player { P.velocity = (velX, 0) }
  where (velX, _) = P.velocity player

playerInAir :: P.Player -> [L.Layer] -> Bool
playerInAir player = null . layerCollidedWithPlayer player