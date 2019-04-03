module LambdaTower.Ingame.Render
  ( RenderConfig(..)
  , defaultConfig
  , deleteConfig
  , renderDefault
  , renderPause
  , render
  , clear
  , present
  )
where

import           Data.Word

import           Foreign.C.Types

import           LambdaTower.Types
import           LambdaTower.Graphics
import           LambdaTower.Types.IngameState

import qualified Data.Vector.Storable          as V

import qualified SDL
import qualified SDL.Font                      as SDLF
import qualified SDL.Primitive                 as SDLP

import qualified LambdaTower.Render            as Render
import qualified LambdaTower.Screen            as Screen
import qualified LambdaTower.Ingame.Layer      as Layer
import qualified LambdaTower.Ingame.Player     as Player
import qualified LambdaTower.Timing.Timer      as Timer
import qualified LambdaTower.Types.Shape       as Shape

data RenderConfig = RenderConfig {
  font :: SDLF.Font,
  headlineColor :: SDL.V4 Word8,
  bgColor :: SDL.V4 Word8,
  playerColor :: SDL.V4 Word8,
  playerShadowColor :: SDL.V4 Word8,
  textColor :: SDL.V4 Word8
}

defaultConfig :: IO RenderConfig
defaultConfig = do
  loadedFont <- SDLF.load "HighSchoolUSASans.ttf" 14
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

playerShape :: Shape.Shape
playerShape = Shape.Shape [0, 10, 40, 30, 20, 10, 0, 15] [80, 80, 0, 0, 25, 0, 0, 40]

playerShadowShape :: Shape.Shape
playerShadowShape = Shape.Shape [0, 10, 25, 10, 0, 15] [80, 80, 40, 0, 0, 40]

clear :: SDL.Renderer -> SDLP.Color -> IO ()
clear renderer color = do
  SDL.rendererDrawColor renderer SDL.$= color
  SDL.clear renderer

present :: SDL.Renderer -> IO ()
present = SDL.present

renderDefault :: Graphics -> RenderConfig -> Timer.LoopTimer -> IngameState -> IO ()
renderDefault (window, renderer) config =
  render (clear renderer $ bgColor config) (present renderer) (window, renderer) config

renderPause :: Graphics -> RenderConfig -> Timer.LoopTimer -> IngameState -> IO ()
renderPause (window, renderer) config =
  render (clear renderer $ bgColor config) (return ()) (window, renderer) config

render :: IO () -> IO () -> Graphics -> RenderConfig -> Timer.LoopTimer -> IngameState -> IO ()
render pre post (window, renderer) config timer state = do
  pre
  windowSize <- SDL.get $ SDL.windowSize window
  mapM_ (renderLayer renderer windowSize $ screen state) $ layers state
  renderPlayer renderer config windowSize (screen state) (player state)
  renderPlayerShadow renderer config windowSize (screen state) (player state)
  renderHud renderer config timer state
  post

renderHud :: SDL.Renderer -> RenderConfig -> Timer.LoopTimer -> IngameState -> IO ()
renderHud renderer config timer state = do
  renderHudVelocity renderer config state
  renderHudTime renderer config state
  renderHudScore renderer config state
  renderHudFPS renderer config timer

renderHudVelocity :: SDL.Renderer -> RenderConfig -> IngameState -> IO ()
renderHudVelocity renderer config state = do
  let textFont = font config
  let (x, y)   = Player.velocity . player $ state
  Render.renderText renderer textFont (SDL.V2 20 20) (headlineColor config) "velocity"
  Render.renderText renderer textFont (SDL.V2 100 20) (textColor config) $ show (round x :: Int)
  Render.renderText renderer textFont (SDL.V2 150 20) (textColor config) $ show (round y :: Int)

renderHudTime :: SDL.Renderer -> RenderConfig -> IngameState -> IO ()
renderHudTime renderer config state = do
  let textFont = font config
  let duration = time state
  let seconds = round (realToFrac duration / 1000 :: Float) :: Integer
  let millis   = mod duration 1000
  Render.renderText renderer textFont (SDL.V2 20 40) (headlineColor config) "time"
  Render.renderText renderer textFont (SDL.V2 100 40) (textColor config) (show seconds)
  Render.renderText renderer textFont (SDL.V2 150 40) (textColor config) (show millis)

renderHudScore :: SDL.Renderer -> RenderConfig -> IngameState -> IO ()
renderHudScore renderer config state = do
  let textFont = font config
  let score    = Player.score . player $ state
  Render.renderText renderer textFont (SDL.V2 20 60) (headlineColor config) "score"
  Render.renderText renderer textFont (SDL.V2 100 60) (textColor config) (show score)

renderHudFPS :: SDL.Renderer -> RenderConfig -> Timer.LoopTimer -> IO ()
renderHudFPS renderer config timer = do
  let textFont = font config
  let fps      = Timer.fps . Timer.counter $ timer
  Render.renderText renderer textFont (SDL.V2 250 20) (headlineColor config) "fps"
  Render.renderText renderer textFont (SDL.V2 320 20) (textColor config) (show fps)

renderPlayer
  :: SDL.Renderer -> RenderConfig -> SDL.V2 CInt -> Screen.Screen -> Player.Player -> IO ()
renderPlayer renderer config windowSize screen player = do
  let shape =
        centerBottom (Player.position player) $ flipByVel (Player.velocity player) playerShape
  renderShape renderer windowSize screen (playerColor config) shape

renderPlayerShadow
  :: SDL.Renderer -> RenderConfig -> SDL.V2 CInt -> Screen.Screen -> Player.Player -> IO ()
renderPlayerShadow renderer config windowSize screen player = do
  let (posX, posY) = Player.position player
  let (velX, _)    = Player.velocity player
  let offX         = if velX >= 0 then -20 else 20
  let shape =
        centerBottom (posX + offX, posY) $ flipByVel (Player.velocity player) playerShadowShape
  let SDL.V4 r g b _ = playerShadowColor config
  let a = round $ if abs velX > 10000 then 255 else abs velX / 10000 * 255
  renderShape renderer windowSize screen (SDL.V4 r g b a) shape

renderShape :: SDL.Renderer -> WindowSize -> Screen.Screen -> SDLP.Color -> Shape.Shape -> IO ()
renderShape renderer (SDL.V2 w h) screen color shape = do
  let toVector   = foldl V.snoc V.empty
  let transformX = fromIntegral . Screen.translate screen w
  let transformY = fromIntegral . Screen.translateFlipped screen h
  let xs         = toVector . map transformX $ Shape.polygonXs shape
  let ys         = toVector . map transformY $ Shape.polygonYs shape
  SDLP.fillPolygon renderer xs ys color

renderLayer :: SDL.Renderer -> WindowSize -> Screen.Screen -> Layer.Layer -> IO ()
renderLayer renderer windowSize screen layer = do
  let size     = Screen.toWindowSize screen windowSize (Layer.size layer)
  let position = Screen.toWindowPosition screen windowSize (Layer.position layer)
  SDL.rendererDrawColor renderer SDL.$= layerColor layer
  SDL.fillRect renderer $ Just $ SDL.Rectangle (SDL.P position) size

flipByVel :: Player.Velocity -> Shape.Shape -> Shape.Shape
flipByVel (velX, _) shape = if velX >= 0 then shape else flipShape shape

flipShape :: Shape.Shape -> Shape.Shape
flipShape shape =
  let (w, _) = Shape.size shape
      xs     = map (\x -> w - x) $ Shape.polygonXs shape
      ys     = Shape.polygonYs shape
  in  Shape.Shape xs ys

centerBottom :: Position -> Shape.Shape -> Shape.Shape
centerBottom (posX, posY) (Shape.Shape xs ys) =
  let (w, _) = Shape.size (Shape.Shape xs ys)
      xs'    = map ((\x -> x - w / 2) . (+ posX)) xs
      ys'    = map (+ posY) ys
  in  Shape.Shape xs' ys'

layerColor :: Layer.Layer -> SDL.V4 Word8
layerColor layer | h >= 300  = SDL.V4 0 255 127 255
                 | w >= 1000 = SDL.V4 255 255 255 255
                 | w >= 500  = SDL.V4 75 0 130 255
                 | w >= 400  = SDL.V4 31 135 120 255
                 | w >= 300  = SDL.V4 255 127 80 255
                 | otherwise = SDL.V4 255 215 0 255
  where (w, h) = Layer.size layer
