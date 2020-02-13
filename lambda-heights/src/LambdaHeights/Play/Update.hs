module LambdaHeights.Play.Update
  ( update,
  )
where

import qualified ComposeEngine.Types.Loop as Loop
import qualified ComposeEngine.Types.Timer as Timer
import Control.Monad.State
import Data.Function
import Data.List
import qualified LambdaHeights.Play.Pattern as Pattern
import qualified LambdaHeights.Types.Events as Events
import qualified LambdaHeights.Types.Layer as Layer
import qualified LambdaHeights.Types.PlayState as State
import qualified LambdaHeights.Types.Player as Player
import LambdaHeights.Types.Score
import qualified LambdaHeights.Types.Screen as Screen
import LambdaHeights.Vectors

-- Update the world state.
-- 1. Applies occured events to the current world state.
-- 2. Updates the current world state using the update factor.

updateFactor :: Float
updateFactor = 1 / 128

-- | Applies occured events and updates the game state.
update :: Loop.Update State.State State.Result Events.Events
update events = do
  timer <- Loop.getUpdateTimer
  state <- Loop.getUpdateState
  let time = State.duration state
  let screen = State.screen state
  let layers = State.layers state
  let player = State.player state
  let motion = updateMotion (Events.player events) $ State.motion state
  let state' =
        state
          { State.duration = time + Timer.rate timer,
            State.screen = updateScreen player screen,
            State.motion = resetMotion motion,
            State.player = updatePlayer screen motion layers player,
            State.layers = updateLayers screen layers
          }
  put (timer, updatedResult events state')

updatedResult :: Events.Events -> State.State -> Either State.Result State.State
updatedResult events state =
  let paused = elem Events.Paused $ Events.control events
      died = dead (State.screen state) (State.player state)
      go
        | died = Left $ State.Result State.Finished state
        | paused = Left $ State.Result State.Paused state
        | otherwise = Right state
   in go

-- Updating the screen involves two steps:
-- 1. Move the screen upwards over time.
-- 2. Ensure that the player is always visible within the screen.

updateScreen :: Player.Player -> Screen.Screen -> Screen.Screen
updateScreen player = scrollScreenToPlayer player . scrollScreenOverTime

scrollScreenOverTime :: Screen.Screen -> Screen.Screen
scrollScreenOverTime screen =
  let bottom = Screen.bottom screen
      factor = min 500 (100 + bottom / 125)
   in if bottom == 0 then screen else scrollScreen (updateFactor * factor) screen

scrollScreenToPlayer :: Player.Player -> Screen.Screen -> Screen.Screen
scrollScreenToPlayer player screen =
  let WP (V2 _ y) = Player.position player
      top = Screen.top screen
      distance = top - y
   in if distance < 250 then scrollScreen (250 - distance) screen else screen

scrollScreen :: Float -> Screen.Screen -> Screen.Screen
scrollScreen delta screen =
  let (Screen.SP (V2 x y)) = Screen.pos screen
   in screen {Screen.pos = Screen.SP (V2 x (y + delta))}

-- Apply the player events to the motion.

updateMotion :: [Events.PlayerEvent] -> State.Motion -> State.Motion
updateMotion events motion = applyPlayerEvents motion events

applyPlayerEvents :: State.Motion -> [Events.PlayerEvent] -> State.Motion
applyPlayerEvents = foldl applyPlayerEvent

applyPlayerEvent :: State.Motion -> Events.PlayerEvent -> State.Motion
applyPlayerEvent moveState (Events.PlayerMoved Events.MoveLeft b) =
  moveState {State.moveLeft = b}
applyPlayerEvent moveState (Events.PlayerMoved Events.MoveRight b) =
  moveState {State.moveRight = b}
applyPlayerEvent moveState Events.PlayerJumped = moveState {State.jump = True}

resetMotion :: State.Motion -> State.Motion
resetMotion motion = motion {State.jump = False}

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
  Nothing -> player
  Just layer -> player {Player.score = max (Score $ Layer.layerId layer) (Player.score player)}

updateMotionType :: [Layer.Layer] -> Player.Player -> Player.Player
updateMotionType layers player =
  let collided = not $ null $ find (player `inside`) layers
   in player {Player.motionType = if collided then Player.Ground else Player.Air}

-- Update the motion of the player (acceleration, velocity, position).

updatePlayerMotion :: State.Motion -> Player.Player -> Player.Player
updatePlayerMotion motion player = updateAcc motion player & updateVel & updatePos

updateAcc :: State.Motion -> Player.Player -> Player.Player
updateAcc motion player =
  let vel = Player.velocity player
      acc = case Player.motionType player of
        Player.Ground -> calcJumpAcc motion vel $ calcGroundAcc motion
        Player.Air -> calcAirAcc motion
   in player {Player.acceleration = acc}

calcJumpAcc :: State.Motion -> WorldVel -> WorldAcc -> WorldAcc
calcJumpAcc motion vel acc =
  let jump = State.jump motion
      WA (V2 accX _) = acc
      WV (V2 velX velY) = vel
      velLength = sqrt $ (velX ** 2) + (velY ** 2)
      go
        | jump && velLength >= 750 = WA (V2 accX 320000)
        | jump = WA (V2 accX 160000)
        | otherwise = acc
   in go

gravity :: Float
gravity = -4000

calcGroundAcc :: State.Motion -> WorldAcc
calcGroundAcc motion =
  let left = State.moveLeft motion
      right = State.moveRight motion
      acc = 8000
      go
        | left && not right = WA (V2 (- acc) gravity)
        | right && not left = WA (V2 acc gravity)
        | otherwise = WA (V2 0 gravity)
   in go

calcAirAcc :: State.Motion -> WorldAcc
calcAirAcc motion =
  let left = State.moveLeft motion
      right = State.moveRight motion
      acc = 4000
      go
        | left && not right = WA (V2 (- acc) gravity)
        | right && not left = WA (V2 acc gravity)
        | otherwise = WA (V2 0 gravity)
   in go

updateVel :: Player.Player -> Player.Player
updateVel player =
  let WV vel = decelerate (Player.motionType player) $ Player.velocity player
      WA acc = Player.acceleration player
   in player {Player.velocity = WV (applyWithFactor updateFactor vel acc)}

decelerate :: Player.MotionType -> WorldVel -> WorldVel
decelerate Player.Ground (WV (V2 x y)) = WV $ V2 (x * 0.925) y
decelerate Player.Air (WV (V2 x y)) = WV $ V2 (x * 0.99) y

updatePos :: Player.Player -> Player.Player
updatePos player =
  let WP pos = Player.position player
      WV vel = Player.velocity player
   in player {Player.position = WP (applyWithFactor updateFactor pos vel)}

applyWithFactor :: Fractional a => a -> V2 a -> V2 a -> V2 a
applyWithFactor factor (V2 x y) (V2 x' y') = V2 (x + x' * factor) (y + y' * factor)

-- Correct the position and velocity if it is colliding with a layer
-- or the bounds of the screen.

bounceFrom :: Player.Player -> Screen.Screen -> Player.Player
player `bounceFrom` screen =
  let WP (V2 posX posY) = Player.position player
      WV (V2 velX velY) = Player.velocity player
      Screen.SS (V2 w _) = Screen.size screen
      minX = 0
      maxX = w
      outLeft = posX < minX && velX < 0
      outRight = posX > maxX && velX > 0
      go
        | outLeft = player {Player.position = WP (V2 minX posY), Player.velocity = WV (V2 (- velX) velY)}
        | outRight = player {Player.position = WP (V2 maxX posY), Player.velocity = WV (V2 (- velX) velY)}
        | otherwise = player
   in go

collideWith :: Player.Player -> [Layer.Layer] -> Player.Player
player `collideWith` layers =
  if falling player
    then case find (player `onTop`) layers of
      Nothing -> player
      Just layer -> settle $ player `onto` layer
    else player

onto :: Player.Player -> Layer.Layer -> Player.Player
player `onto` layer =
  let WP (V2 posX _) = Player.position player in player {Player.position = WP (V2 posX (Layer.posY layer))}

settle :: Player.Player -> Player.Player
settle player = let WV (V2 velX _) = Player.velocity player in player {Player.velocity = WV (V2 velX 0)}

dead :: Screen.Screen -> Player.Player -> Bool
dead screen player =
  let WP (V2 _ y) = Player.position player
   in y < Screen.bottom screen

falling :: Player.Player -> Bool
falling player = let WV (V2 _ y) = Player.velocity player in y < 0

inside :: Player.Player -> Layer.Layer -> Bool
player `inside` layer =
  let playerPos = Player.position player
      layerPos = Layer.position layer
      size = Layer.size layer
   in playerPos `insidePlain` (layerPos, size)

onTop :: Player.Player -> Layer.Layer -> Bool
player `onTop` layer =
  let playerPos = Player.position player
      layerPos = Layer.position layer
      WS (V2 w _) = Layer.size layer
   in playerPos `insidePlain` (layerPos, WS (V2 w 20))

insidePlain :: WorldPos -> (WorldPos, WorldSize) -> Bool
(WP (V2 px py)) `insidePlain` (WP (V2 x y), WS (V2 w h)) = px >= x && px <= x + w && py <= y && py >= y - h

-- Drop passed and generate new layers.

newPattern :: [Pattern.PatternEntry]
newPattern =
  Pattern.combine
    1
    [ Pattern.leftRightPattern,
      Pattern.boostPattern,
      Pattern.stairsPattern,
      Pattern.highPattern
    ]

updateLayers :: Screen.Screen -> [Layer.Layer] -> [Layer.Layer]
updateLayers screen = fillLayers screen . dropPassedLayers screen

fillLayers :: Screen.Screen -> [Layer.Layer] -> [Layer.Layer]
fillLayers screen [] = unfoldLayer screen Layer.ground
fillLayers screen (layer : layers) = unfoldLayer screen layer ++ layers

unfoldLayer :: Screen.Screen -> Layer.Layer -> [Layer.Layer]
unfoldLayer screen =
  let generator = nextLayerByPattern newPattern
   in reverse . unfoldr (generateLayer generator screen)

nextLayerByPattern :: [Pattern.PatternEntry] -> Layer.Layer -> Layer.Layer
nextLayerByPattern [] layer = layer
nextLayerByPattern (p : ps) layer =
  let entryIds = map Pattern.entryId (p : ps)
   in case elemIndex (Layer.entryId layer) entryIds of
        Nothing -> layer `deriveFrom` p
        Just i -> case (p : ps ++ [p]) !! (i + 1) of
          p' -> layer `deriveFrom` p'

deriveFrom :: Layer.Layer -> Pattern.PatternEntry -> Layer.Layer
layer `deriveFrom` entry =
  let Pattern.PatternEntry entryId (WS (V2 w h)) (WP (V2 x y)) = entry
      layerId = Layer.layerId layer + 1
      WP (V2 _ py) = Layer.position layer
   in Layer.Layer layerId entryId (WS (V2 w h)) $ WP (V2 x (py + y))

generateLayer ::
  (Layer.Layer -> Layer.Layer) ->
  Screen.Screen ->
  Layer.Layer ->
  Maybe (Layer.Layer, Layer.Layer)
generateLayer generator screen layer =
  if Screen.top screen < Layer.posY layer - 500 then Nothing else Just (layer, generator layer)

dropPassedLayers :: Screen.Screen -> [Layer.Layer] -> [Layer.Layer]
dropPassedLayers screen = dropWhile (passed screen)

passed :: Screen.Screen -> Layer.Layer -> Bool
screen `passed` layer = Screen.bottom screen > Layer.posY layer
