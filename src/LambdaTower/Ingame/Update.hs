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
import qualified LambdaTower.Screen as S

type GameStateUpdate a = StateT G.GameState IO a

deltaTime :: Float
deltaTime = 1 / 128

-- Updating the replays need no calculations because we already have all states.
-- We need two steps:
-- a) Return the front state of the list and remove it for the next cycle.
-- b) Return the score if the replay is done.

replayUpdate :: Updater IO ([[E.PlayerEvent]], G.GameState) P.Score ()
replayUpdate _ ([], gameState) = return $ Right $ P.score . G.player $ gameState
replayUpdate _ (events:eventsStore, gameState) = do
  eitherState <- update events gameState
  case eitherState of
    Left newState -> return . Left $ (eventsStore, newState)
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
  where go = do updateScreenM
                updateMotionM events
                updateLayersM
                updatePlayerM
                resetMotionM
                returnStateM

updateScreenM :: GameStateUpdate ()
updateScreenM = modify f
  where f s = s { G.screen = updateScreen (G.player s) $ G.screen s }

updateMotionM :: [E.PlayerEvent] -> GameStateUpdate ()
updateMotionM events = modify f
  where f s = s { G.motion = updateMotion (G.motion s) events }

updateLayersM :: GameStateUpdate ()
updateLayersM = modify f
  where f s = s { G.layers = updateLayers (G.screen s) $ G.layers s }

updatePlayerM :: GameStateUpdate ()
updatePlayerM = modify f
  where f s = s { G.player = updatePlayer (G.screen s) (G.motion s) (G.layers s) $ G.player s }

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
  return $ if playerDead (G.screen s) (G.player s)
    then Right $ P.score $ G.player s
    else Left s


-- Updating the view involves two steps:
-- a) Move the view upwards over time.
-- b) Ensure that the player is always visible within the view.

updateScreen :: P.Player -> S.Screen -> S.Screen
updateScreen player = scrollScreenToPlayer player . scrollScreenOverTime

scrollScreenOverTime :: S.Screen -> S.Screen
scrollScreenOverTime screen = if height == 0 then screen else scrollScreen (deltaTime*factor) screen
  where height = S.bottom screen
        factor = min 400 (100+height/100)

scrollScreenToPlayer :: P.Player -> S.Screen -> S.Screen
scrollScreenToPlayer player screen = if distance < 250 then scrollScreen (250-distance) screen else screen
  where (_, y) = P.position player
        distance = S.top screen - y

scrollScreen :: Float -> S.Screen -> S.Screen
scrollScreen delta screen = screen {
  S.top = S.top screen + delta,
  S.bottom = S.bottom screen + delta
}


-- Apply the player events to the motion.

updateMotion :: G.Motion -> [E.PlayerEvent] -> G.Motion
updateMotion = foldl applyPlayerEvents

applyPlayerEvents :: G.Motion -> E.PlayerEvent -> G.Motion
applyPlayerEvents moveState (E.PlayerMoved E.MoveLeft b) = moveState { G.moveLeft = b }
applyPlayerEvents moveState (E.PlayerMoved E.MoveRight b) = moveState { G.moveRight = b }
applyPlayerEvents moveState E.PlayerJumped = moveState { G.jump = True }


-- Drop passed and generate new layers.

updateLayers :: S.Screen -> [L.Layer] -> [L.Layer]
updateLayers screen = fillLayers screen . dropPassedLayers screen

fillLayers :: S.Screen -> [L.Layer] -> [L.Layer]
fillLayers screen [] = unfoldLayers screen L.ground
fillLayers screen (layer:layers) = unfoldLayers screen layer ++ layers

unfoldLayers :: S.Screen -> L.Layer -> [L.Layer]
unfoldLayers screen = reverse . unfoldr (generateLayer generator screen)
  where generator = patternLayerGenerator simplePattern (S.bottom screen)

simplePattern :: Float -> Float
simplePattern 200 = 400 -- right
simplePattern 400 = 600
simplePattern 600 = 150
simplePattern 150 = 800
simplePattern 800 = 550 -- left
simplePattern 550 = 300
simplePattern 300 = 100
simplePattern 100 = 200
simplePattern _   = 200 -- entry

generateLayer :: (L.Layer -> L.Layer) -> S.Screen -> L.Layer -> Maybe (L.Layer, L.Layer)
generateLayer f screen layer =
  if S.top screen < L.posY layer
  then Nothing
  else Just (layer, f layer)

patternLayerGenerator :: (Float -> Float) -> Float -> L.Layer -> L.Layer
patternLayerGenerator pattern height layer =
  case (L.id layer, L.size layer, L.origin layer) of
    (layerId, (_, h), (x, y)) -> L.Layer (layerId+1) (w, h) (translatedX, y+200) (originX, y+200)
      where w = layerWidthByHeight height
            originX = pattern x
            translatedX = originX - w / 2

layerWidthByHeight :: Float -> Float
layerWidthByHeight height
  | height < 1000 = 400
  | height < 5000 = 300
  | otherwise = 200

dropPassedLayers :: S.Screen -> [L.Layer] -> [L.Layer]
dropPassedLayers screen = filter $ not . layerPassed screen

layerPassed :: S.Screen -> L.Layer -> Bool
layerPassed screen layer = S.bottom screen > L.posY layer


-- Updating the player involves the following steps:
-- a) Update the motion of the player.
-- b) Apply collision detection and corrections.
-- c) Update the score (highest reached layer).

updatePlayer :: S.Screen -> G.Motion -> [L.Layer] -> P.Player -> P.Player
updatePlayer screen motion layers =
  updateScore layers
  . collidePlayerWithLayers layers
  . bouncePlayerFromBounds screen
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

bouncePlayerFromBounds :: S.Screen -> P.Player -> P.Player
bouncePlayerFromBounds screen player
  | outLeft = player { P.position = (minX, posY), P.velocity = ((-velX) * 0.75, velY) }
  | outRight = player { P.position = (maxX, posY), P.velocity = ((-velX) * 0.75, velY) }
  | otherwise = player
  where (posX, posY) = P.position player
        (velX, velY) = P.velocity player
        minX = S.left screen
        maxX = S.right screen
        outLeft = posX < minX && velX < 0
        outRight = posX > maxX && velX > 0

collidePlayerWithLayers :: [L.Layer] -> P.Player -> P.Player
collidePlayerWithLayers layers player =
  if playerFalling player then
    case layerCollidedWithPlayer player layers of
      Nothing -> player
      Just layer -> resetVelocityY . liftPlayerOnLayer layer $ player
  else player

playerDead :: S.Screen -> P.Player -> Bool
playerDead screen player = let (_, y) = P.position player in y < S.bottom screen

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