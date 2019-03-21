module LambdaTower.Ingame.Update (
  replayUpdate,
  updateAndWrite,
  update
) where

import Control.Monad.State
import Control.Monad.STM
import Control.Concurrent.STM.TChan

import Data.List
import Data.Word

import LambdaTower.Loop

import qualified LambdaTower.Components.Button as B
import qualified LambdaTower.Components.ButtonList as BL
import qualified LambdaTower.Components.Events as E
import qualified LambdaTower.Ingame.GameEvents as G
import qualified LambdaTower.Ingame.GameState as G
import qualified LambdaTower.Ingame.Layer as L
import qualified LambdaTower.Ingame.Player as P
import qualified LambdaTower.Screen as S

updateFactor :: Float
updateFactor = 1 / 128

-- Updating the replays need no calculations because we already have all states.
-- We need two steps:
-- a) Return the front state of the list and remove it for the next cycle.
-- b) Return the score if the replay is done.

type ReplayState = ([[G.PlayerEvent]], G.GameState)

replayUpdate :: Updater IO ReplayState ReplayState [G.ControlEvent]
replayUpdate _ _ ([], gameState) = return $ Left ([], gameState)
replayUpdate timer controlEvents (events:eventStore, gameState) = do
  eitherState <- update timer (G.GameEvents controlEvents events) gameState
  case eitherState of
    Left gameState' -> return $ Left (eventStore, G.state gameState')
    Right gameState' -> return $ Right (eventStore, gameState')


-- Wrapper around one update to broadcast the occured events for serialization

updateAndWrite :: TChan (Maybe [G.PlayerEvent]) -> Updater IO G.GameState G.GameResult G.GameEvents
updateAndWrite channel timer events gameState = do
  result <- update timer events gameState
  case result of
    Left _ -> liftIO $ atomically $ writeTChan channel Nothing
    Right _ -> liftIO $ atomically $ writeTChan channel $ Just $ G.playerEvents events
  return result


-- Control flow over one update cycle.

type GameStateUpdate a = StateT G.GameState IO a

update :: Updater IO G.GameState G.GameResult G.GameEvents
update timer events = evalStateT go
  where go = do updateTimeM timer
                updateScreenM
                updateMotionM (G.playerEvents events)
                updateLayersM
                updatePlayerM
                resetMotionM
                returnStateM (G.controlEvents events)

updateTimeM :: LoopTimer -> GameStateUpdate ()
updateTimeM timer = modify f
  where f s = s { G.time = G.time s + fromIntegral (rate timer) }

updateScreenM :: GameStateUpdate ()
updateScreenM = modify f
  where f s = s { G.screen = updateScreen (G.player s) $ G.screen s }

updateMotionM :: [G.PlayerEvent] -> GameStateUpdate ()
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

returnStateM :: [G.ControlEvent] -> GameStateUpdate (Either G.GameResult G.GameState)
returnStateM events = do
  s <- get
  return $
    if playerDead (G.screen s) (G.player s) then Left $ G.GameResult s G.Exit
    else if paused events then Left $ G.GameResult s G.Pause
    else Right s

paused :: [G.ControlEvent] -> Bool
paused = elem G.Paused


-- Updating the view involves two steps:
-- a) Move the view upwards over time.
-- b) Ensure that the player is always visible within the view.

updateScreen :: P.Player -> S.Screen -> S.Screen
updateScreen player = scrollScreenToPlayer player . scrollScreenOverTime

scrollScreenOverTime :: S.Screen -> S.Screen
scrollScreenOverTime screen = if height == 0 then screen else scrollScreen (updateFactor * factor) screen
  where height = S.bottom screen
        factor = min 400 (100 + height / 100)

scrollScreenToPlayer :: P.Player -> S.Screen -> S.Screen
scrollScreenToPlayer player screen = if space < 250 then scrollScreen (250 - space) screen else screen
  where (_, y) = P.position player
        space = S.top screen - y

scrollScreen :: Float -> S.Screen -> S.Screen
scrollScreen delta screen = screen {
  S.top = S.top screen + delta,
  S.bottom = S.bottom screen + delta
}


-- Apply the player events to the motion.

updateMotion :: G.Motion -> [G.PlayerEvent] -> G.Motion
updateMotion = foldl applyPlayerEvents

applyPlayerEvents :: G.Motion -> G.PlayerEvent -> G.Motion
applyPlayerEvents moveState (G.PlayerMoved G.MoveLeft b) = moveState { G.moveLeft = b }
applyPlayerEvents moveState (G.PlayerMoved G.MoveRight b) = moveState { G.moveRight = b }
applyPlayerEvents moveState G.PlayerJumped = moveState { G.jump = True }
applyPlayerEvents moveState _ = moveState


-- Drop passed and generate new layers.

data PatternEntry = PatternEntry {
  description :: (L.Size, Float),
  distance :: Float
}

updateLayers :: S.Screen -> [L.Layer] -> [L.Layer]
updateLayers screen = fillLayers screen . dropPassedLayers screen

fillLayers :: S.Screen -> [L.Layer] -> [L.Layer]
fillLayers screen [] = unfoldLayers screen L.ground
fillLayers screen (layer:layers) = unfoldLayers screen layer ++ layers

unfoldLayers :: S.Screen -> L.Layer -> [L.Layer]
unfoldLayers screen = reverse . unfoldr (generateLayer generator screen)
  where generator = nextLayerByPattern (easyPattern ++ stairsPattern)

easyPattern :: [PatternEntry]
easyPattern = [
    PatternEntry ((500, 50), 0) 150,
    PatternEntry ((500, 50), 500) 150
  ]

stairsPattern :: [PatternEntry]
stairsPattern = [
    PatternEntry ((300, 50), 0) 150,
    PatternEntry ((350, 50), 700) 0,
    PatternEntry ((350, 50), 100) 150,
    PatternEntry ((300, 50), 600) 0
  ]

nextLayerByPattern :: [PatternEntry] -> L.Layer -> L.Layer
nextLayerByPattern [] layer = layer
nextLayerByPattern (p:ps) layer =
  case elemIndex (L.size layer, L.posX layer) descriptions of
    Nothing -> let (PatternEntry ((w, h), x) d) = p in L.Layer layerId (w, h) (x, y + d)
    Just i ->
      case (p:ps ++ [p]) !! (i + 1) of
        PatternEntry ((w, h), x) d -> L.Layer layerId (w, h) (x, y + d)
  where descriptions = map description (p:ps)
        layerId = L.id layer + 1
        (_, y) = L.position layer

generateLayer :: (L.Layer -> L.Layer) -> S.Screen -> L.Layer -> Maybe (L.Layer, L.Layer)
generateLayer generator screen layer =
  if S.top screen < L.posY layer
  then Nothing
  else Just (layer, generator layer)

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
  | jump && (vel >= 750) = (accX, 320000)
  | jump = (accX, 160000)
  | left && right = (0, -4000)
  | left = (-8000, -4000)
  | right = (8000, -4000)
  | otherwise = (0, -4000)
  where left = G.moveLeft motion
        right = G.moveRight motion
        jump = G.jump motion
        vel = sqrt $ (velX ** 2) + (velY ** 2)

updateAirAcceleration :: G.Motion -> P.Acceleration
updateAirAcceleration motion
  | left && right = (0, -4000)
  | left = (-4000, -4000)
  | right = (4000, -4000)
  | otherwise = (0, -4000)
  where left = G.moveLeft motion
        right = G.moveRight motion

updateVelocity :: P.Velocity -> G.Motion -> P.Acceleration -> P.Velocity
updateVelocity vel motion = applyAcceleration (decelerate motion vel)

applyAcceleration :: P.Velocity -> P.Acceleration -> P.Velocity
applyAcceleration (x, y) (x', y') = (x+x'*updateFactor, y+y'*updateFactor)

decelerate :: G.Motion -> P.Velocity -> P.Velocity
decelerate motion (x, y) = if G.air motion then (x * 0.99, y) else (x * 0.925, y)

updatePosition :: P.Position -> P.Velocity -> P.Position
updatePosition = applyAcceleration


-- Correct the position and velocity if it is colliding with a layer
-- or the bounds of the level.

bouncePlayerFromBounds :: S.Screen -> P.Player -> P.Player
bouncePlayerFromBounds screen player
  | outLeft = player { P.position = (minX, posY), P.velocity = (-velX, velY) }
  | outRight = player { P.position = (maxX, posY), P.velocity = (-velX, velY) }
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
      Just layer -> resetVelocityY $ liftPlayerOnLayer layer player
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