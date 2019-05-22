module LambdaHeights.Play.Render
  ( RenderConfig(..)
  , createConfig
  , deleteConfig
  , renderDefault
  , renderPause
  , render
  , clear
  , present
  )
where

import qualified Data.Vector.Storable          as V
import           Data.Word
import           Foreign.C.Types
import qualified Graphics.UI.Render            as Render
import           LambdaHeights.RenderContext
import qualified LambdaHeights.Scale           as Scale
import           LambdaHeights.Types
import qualified LambdaHeights.Types.Layer     as Layer
import qualified LambdaHeights.Types.Player    as Player
import qualified LambdaHeights.Types.PlayState as State
import qualified LambdaHeights.Types.Screen    as Screen
import qualified LambdaHeights.Types.Timer     as Timer
import           Linear.V2
import qualified SDL
import qualified SDL.Font                      as SDLF
import qualified SDL.Primitive                 as SDLP

data RenderConfig = RenderConfig {
  font              :: SDLF.Font,
  headlineColor     :: SDL.V4 Word8,
  bgColor           :: SDL.V4 Word8,
  playerColor       :: SDL.V4 Word8,
  playerShadowColor :: SDL.V4 Word8,
  textColor         :: SDL.V4 Word8
}

createConfig :: IO RenderConfig
createConfig = do
  loadedFont <- SDLF.load "retro_gaming.ttf" 11
  return $ RenderConfig
    { font              = loadedFont
    , headlineColor     = SDL.V4 255 255 255 255
    , bgColor           = SDL.V4 30 30 30 255
    , playerColor       = SDL.V4 135 31 120 255
    , playerShadowColor = SDL.V4 255 255 255 255
    , textColor         = SDL.V4 0 191 255 255
    }

deleteConfig :: RenderConfig -> IO ()
deleteConfig = SDLF.free . font

data Shape = Shape {
  polygonXs :: [Float],
  polygonYs :: [Float]
}

shapeSize :: Shape -> Size
shapeSize shape = V2 (maximum $ polygonXs shape) (maximum $ polygonYs shape)

playerShape :: Shape
playerShape = Shape [0, 10, 40, 30, 20, 10, 0, 15] [80, 80, 0, 0, 25, 0, 0, 40]

playerSpeedShape :: Shape
playerSpeedShape = Shape [0, 10, 25, 10, 0, 15] [80, 80, 40, 0, 0, 40]

clear :: SDL.Renderer -> SDLP.Color -> IO ()
clear renderer color = do
  SDL.rendererDrawColor renderer SDL.$= color
  SDL.clear renderer

present :: SDL.Renderer -> IO ()
present = SDL.present

renderDefault :: RenderContext -> RenderConfig -> Timer.LoopTimer -> State.State -> IO ()
renderDefault (window, renderer) config =
  render (clear renderer $ bgColor config) (present renderer) (window, renderer) config

renderPause :: RenderContext -> RenderConfig -> Timer.LoopTimer -> State.State -> IO ()
renderPause (window, renderer) config =
  render (clear renderer $ bgColor config) (return ()) (window, renderer) config

render :: IO () -> IO () -> RenderContext -> RenderConfig -> Timer.LoopTimer -> State.State -> IO ()
render pre post (window, renderer) config timer state = do
  pre
  windowSize <- SDL.get $ SDL.windowSize window
  mapM_ (renderLayer renderer windowSize $ State.screen state) $ State.layers state
  renderPlayer renderer config windowSize (State.screen state) (State.player state)
  renderPlayerShadow renderer config windowSize (State.screen state) (State.player state)
  renderHud renderer config timer state
  post

renderHud :: SDL.Renderer -> RenderConfig -> Timer.LoopTimer -> State.State -> IO ()
renderHud renderer config timer state = do
  renderHudVelocity renderer config state
  renderHudTime renderer config state
  renderHudScore renderer config state
  renderHudFPS renderer config timer

renderHudVelocity :: SDL.Renderer -> RenderConfig -> State.State -> IO ()
renderHudVelocity renderer config state = do
  let textFont = font config
  let V2 x y   = Player.velocity . State.player $ state
  Render.renderText renderer textFont (headlineColor config) (V2 20 20) "VELOCITY"
  Render.renderText renderer textFont (textColor config) (V2 100 20) $ show (round x :: Int)
  Render.renderText renderer textFont (textColor config) (V2 150 20) $ show (round y :: Int)

renderHudTime :: SDL.Renderer -> RenderConfig -> State.State -> IO ()
renderHudTime renderer config state = do
  let textFont = font config
  let duration = State.duration state
  let seconds = truncate (realToFrac duration / 1000 :: Float) :: Integer
  let millis   = mod duration 1000
  Render.renderText renderer textFont (headlineColor config) (V2 20 40) "TIME"
  Render.renderText renderer textFont (textColor config) (V2 100 40) (show seconds)
  Render.renderText renderer textFont (textColor config) (V2 150 40) (show millis)

renderHudScore :: SDL.Renderer -> RenderConfig -> State.State -> IO ()
renderHudScore renderer config state = do
  let textFont = font config
  let score    = Player.score . State.player $ state
  Render.renderText renderer textFont (headlineColor config) (V2 20 60) "SCORE"
  Render.renderText renderer textFont (textColor config) (V2 100 60) (show score)

renderHudFPS :: SDL.Renderer -> RenderConfig -> Timer.LoopTimer -> IO ()
renderHudFPS renderer config timer = do
  let textFont = font config
  let fps      = Timer.fps . Timer.counter $ timer
  Render.renderText renderer textFont (headlineColor config) (V2 250 20) "FPS"
  Render.renderText renderer textFont (textColor config) (V2 320 20) (show fps)

renderPlayer :: SDL.Renderer -> RenderConfig -> V2 CInt -> Screen.Screen -> Player.Player -> IO ()
renderPlayer renderer config windowSize screen player = do
  let shape =
        centerBottom (Player.position player) $ flipByVel (Player.velocity player) playerShape
  renderShape renderer windowSize screen (playerColor config) shape

renderPlayerShadow
  :: SDL.Renderer -> RenderConfig -> V2 CInt -> Screen.Screen -> Player.Player -> IO ()
renderPlayerShadow renderer config windowSize screen player = do
  let V2 posX posY = Player.position player
  let V2 velX _    = Player.velocity player
  let offX         = if velX >= 0 then -20 else 20
  let shape =
        centerBottom (V2 (posX + offX) posY) $ flipByVel (Player.velocity player) playerSpeedShape
  let SDL.V4 r g b _ = playerShadowColor config
  let a = round $ if abs velX > 10000 then 255 else abs velX / 10000 * 255
  renderShape renderer windowSize screen (SDL.V4 r g b a) shape

renderShape :: SDL.Renderer -> Scale.WindowSize -> Screen.Screen -> SDLP.Color -> Shape -> IO ()
renderShape renderer (V2 w h) screen color shape = do
  let toVector   = foldl V.snoc V.empty
  let transformX = fromIntegral . Scale.translate screen w
  let transformY = fromIntegral . Scale.translateFlipped screen h
  let xs         = toVector . map transformX $ polygonXs shape
  let ys         = toVector . map transformY $ polygonYs shape
  SDLP.fillPolygon renderer xs ys color

renderLayer :: SDL.Renderer -> Scale.WindowSize -> Screen.Screen -> Layer.Layer -> IO ()
renderLayer renderer windowSize screen layer = do
  let size     = Scale.toWindowSize screen windowSize (Layer.size layer)
  let position = Scale.toWindowPosition screen windowSize (Layer.position layer)
  SDL.rendererDrawColor renderer SDL.$= layerColor layer
  SDL.fillRect renderer $ Just $ SDL.Rectangle (SDL.P position) size

flipByVel :: Player.Velocity -> Shape -> Shape
flipByVel (V2 velX _) shape = if velX >= 0 then shape else flipShape shape

flipShape :: Shape -> Shape
flipShape shape =
  let V2 w _ = shapeSize shape
      xs     = map (\x -> w - x) $ polygonXs shape
      ys     = polygonYs shape
  in  Shape xs ys

centerBottom :: Position -> Shape -> Shape
centerBottom (V2 posX posY) (Shape xs ys) =
  let V2 w _ = shapeSize (Shape xs ys)
      xs'    = map ((\x -> x - w / 2) . (+ posX)) xs
      ys'    = map (+ posY) ys
  in  Shape xs' ys'

layerColor :: Layer.Layer -> SDL.V4 Word8
layerColor layer | h >= 300  = SDL.V4 0 255 127 255
                 | w >= 1000 = SDL.V4 255 255 255 255
                 | w >= 500  = SDL.V4 75 0 130 255
                 | w >= 400  = SDL.V4 31 135 120 255
                 | w >= 300  = SDL.V4 255 127 80 255
                 | otherwise = SDL.V4 255 215 0 255
  where V2 w h = Layer.size layer
