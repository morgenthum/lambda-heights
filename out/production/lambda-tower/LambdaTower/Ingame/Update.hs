module LambdaTower.Ingame.Update (
  replay,
  updater
) where

import Control.Monad.STM
import Control.Concurrent.STM.TChan

import Data.List
import Data.Maybe

import LambdaTower.Loop

import qualified LambdaTower.Ingame.Events as E
import qualified LambdaTower.Ingame.Layer as L
import qualified LambdaTower.Ingame.Player as P
import qualified LambdaTower.Ingame.State as S

deltaTime :: Float
deltaTime = 1 / 128

replay :: Updater IO (S.State, [S.State]) P.Score ()
replay (state, []) _ = return $ Right $ P.score . S.player . S.gameState $ state
replay (_, state:states) _ = return $ Left (state, states)

updater :: TChan (Maybe S.State) -> Updater IO S.State P.Score [E.PlayerEvent]
updater channel state events = do
  atomically $ writeTChan channel $ Just state
  let gameState = S.gameState state
  let view = updateView (S.player gameState) $ S.view gameState
  let motion = updateMotion (S.motion gameState) events
  let layers = updateLayers view $ S.layers gameState
  let player = updatePlayer view motion layers $ S.player gameState
  if playerDead view player
  then do
    atomically $ writeTChan channel Nothing
    return $ Right (P.score player)
  else return $ Left
    state {
      S.gameState = gameState {
        S.view = view,
        S.motion = motion {
          S.jump = False,
          S.air = playerInAir player layers },
        S.player = player,
        S.layers = layers
      }
    }


playerDead :: S.View -> P.Player -> Bool
playerDead view player = let (_, y) = P.position player in y < S.bottom view


-- Updating the view involves two steps:
-- a) Ensure that the player is always visible within the view
-- b) Move the view upwards over time

updateView :: P.Player -> S.View -> S.View
updateView player = scrollViewToPlayer player . scrollViewOverTime

scrollView :: Float -> S.View -> S.View
scrollView delta view = view {
    S.top = S.top view + delta,
    S.bottom = S.bottom view + delta
  }

scrollViewOverTime :: S.View -> S.View
scrollViewOverTime view = if S.bottom view == 0 then view else scrollView (deltaTime*150) view

scrollViewToPlayer :: P.Player -> S.View -> S.View
scrollViewToPlayer player view = if distance < 250 then scrollView (250-distance) view else view
  where (_, y) = P.position player
        distance = S.top view - y


-- Apply the player events to the motion

updateMotion :: S.Motion -> [E.PlayerEvent] -> S.Motion
updateMotion = foldl applyPlayerEvents

applyPlayerEvents :: S.Motion -> E.PlayerEvent -> S.Motion
applyPlayerEvents moveState (E.PlayerMoved E.MoveLeft b) = moveState { S.moveLeft = b }
applyPlayerEvents moveState (E.PlayerMoved E.MoveRight b) = moveState { S.moveRight = b }
applyPlayerEvents moveState E.PlayerJumped = moveState { S.jump = True }


-- Drop passed and generate new layers.

updateLayers :: S.View -> [L.Layer] -> [L.Layer]
updateLayers view = fillLayers view . dropPassedLayers view

fillLayers :: S.View -> [L.Layer] -> [L.Layer]
fillLayers view [] = unfoldLayers view L.ground
fillLayers view (layer:layers) = unfoldLayers view layer ++ layers

unfoldLayers :: S.View -> L.Layer -> [L.Layer]
unfoldLayers view = reverse . unfoldr (generateLayer view)

generateLayer :: S.View -> L.Layer -> Maybe (L.Layer, L.Layer)
generateLayer view layer = if S.top view < L.posY layer then Nothing else Just (layer, nextLayer layer)

nextLayer :: L.Layer -> L.Layer
nextLayer layer =
  case (L.id layer, L.size layer, L.position layer) of
    (id, (1000, h), (x, y)) -> L.Layer (id+1) (500, h) (100, y+200)
    (id, (500, h), (100, y)) -> L.Layer (id+1) (500, h) (400, y+200)
    (id, (500, h), (400, y)) -> L.Layer (id+1) (500, h) (100, y+200)

dropPassedLayers :: S.View -> [L.Layer] -> [L.Layer]
dropPassedLayers view = filter $ not . layerPassed view

layerPassed :: S.View -> L.Layer -> Bool
layerPassed view layer = S.bottom view > L.posY layer


-- Updating the player involves two steps:
-- a) Update the motion of the player
-- b) Apply collision detection and corrections

updatePlayer :: S.View -> S.Motion -> [L.Layer] -> P.Player -> P.Player
updatePlayer view motion layers = correctPlayerPosition view layers . updatePlayerMotion motion


-- Update the motion of the player (acceleration, velocity, position).

updatePlayerMotion :: S.Motion -> P.Player -> P.Player
updatePlayerMotion motion player = player { P.position = pos, P.velocity = vel, P.acceleration = acc }
  where acc = updateAcceleration (P.acceleration player) (P.velocity player) motion
        vel = updateVelocity (P.velocity player) motion acc
        pos = updatePosition (P.position player) vel

updateAcceleration :: P.Acceleration -> P.Velocity -> S.Motion -> P.Acceleration
updateAcceleration acc vel motion =
  if S.air motion
  then updateAirAcceleration motion
  else updateGroundAcceleration acc vel motion

updateGroundAcceleration :: P.Acceleration -> P.Velocity -> S.Motion -> P.Acceleration
updateGroundAcceleration (accX, _) (velX, velY) motion
  | jump && (abs velX > 750 || abs velY > 750) = (accX, 250000)
  | jump = (accX, 125000)
  | left && right = (0, -2000)
  | left = (-7500, -2000)
  | right = (7500, -2000)
  | otherwise = (0, -2000)
  where left = S.moveLeft motion
        right = S.moveRight motion
        jump = S.jump motion

updateAirAcceleration :: S.Motion -> P.Acceleration
updateAirAcceleration motion
  | left && right = (0, -2000)
  | left = (-1500, -2000)
  | right = (1500, -2000)
  | otherwise = (0, -2000)
  where left = S.moveLeft motion
        right = S.moveRight motion

updateVelocity :: P.Velocity -> S.Motion -> P.Acceleration -> P.Velocity
updateVelocity vel motion = applyAcceleration (decelerate motion vel)

applyAcceleration :: P.Velocity -> P.Acceleration -> P.Velocity
applyAcceleration (x, y) (x', y') = (x+x'*deltaTime, y+y'*deltaTime)

decelerate :: S.Motion -> P.Velocity -> P.Velocity
decelerate motion (x, y) = if S.air motion then (x, y) else (x*0.925, y)

updatePosition :: P.Position -> P.Velocity -> P.Position
updatePosition = applyAcceleration


-- Correct the position and velocity if it is colliding with a layer
-- or the bounds of the level.

correctPlayerPosition :: S.View -> [L.Layer] -> P.Player -> P.Player
correctPlayerPosition view layers = correctPlayerPositionY layers . correctPlayerPositionX view

correctPlayerPositionX :: S.View -> P.Player -> P.Player
correctPlayerPositionX view player
  | outLeft = player { P.position = (minX, posY), P.velocity = ((-velX) * 0.75, velY) }
  | outRight = player { P.position = (maxX, posY), P.velocity = ((-velX) * 0.75, velY) }
  | otherwise = player
  where (posX, posY) = P.position player
        (velX, velY) = P.velocity player
        minX = S.left view
        maxX = S.right view
        outLeft = posX < minX && velX < 0
        outRight = posX > maxX && velX > 0

correctPlayerPositionY :: [L.Layer] -> P.Player -> P.Player
correctPlayerPositionY layers player
  | playerFalling player = case playerCollidedLayers player layers of
      [] -> player
      layer:_ -> resetVelocityY . updateScore layer . liftPlayerOnLayer layer $ player
  | otherwise = player

playerFalling :: P.Player -> Bool
playerFalling player = let (_, y) = P.velocity player in y < 0

playerCollidedLayers :: P.Player -> [L.Layer] -> [L.Layer]
playerCollidedLayers player = filter (positionInLayer $ P.position player)

positionInLayer :: P.Position -> L.Layer -> Bool
positionInLayer (x, y) layer = x >= lx && x <= lx + lw && y <= ly && y >= ly - lh
  where (lw, lh) = L.size layer
        (lx, ly) = L.position layer

liftPlayerOnLayer :: L.Layer -> P.Player -> P.Player
liftPlayerOnLayer layer player = player { P.position = (posX, L.posY layer) }
  where (posX, _) = P.position player

updateScore :: L.Layer -> P.Player -> P.Player
updateScore layer player = player { P.score = max (L.id layer) (P.score player) }

resetVelocityY :: P.Player -> P.Player
resetVelocityY player = player { P.velocity = (velX, 0) }
  where (velX, _) = P.velocity player

playerInAir :: P.Player -> [L.Layer] -> Bool
playerInAir player = null . playerCollidedLayers player