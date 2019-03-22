module LambdaTower.Ingame.Render (
  RenderConfig(..),
  defaultConfig,
  deleteConfig,
  defaultRender,
  render,
  clear,
  present
) where

import Data.Word

import qualified Data.Vector.Storable as V

import Foreign.C.Types

import qualified SDL
import qualified SDL.Font as SDLF
import qualified SDL.Primitive as SDLP

import LambdaTower.Graphics
import LambdaTower.Loop
import LambdaTower.Types

import qualified LambdaTower.Types.GameState as State
import qualified LambdaTower.Types.Layer as Layer
import qualified LambdaTower.Types.Player as Player
import qualified LambdaTower.Types.Shape as Shape
import qualified LambdaTower.Render as Render
import qualified LambdaTower.Screen as Screen

data RenderConfig = RenderConfig {
  font :: SDLF.Font,
  whiteColor :: SDL.V4 Word8,
  bgColor :: SDL.V4 Word8,
  playerColor :: SDL.V4 Word8,
  playerBurnerColor :: SDL.V4 Word8,
  layerColor :: SDL.V4 Word8,
  textColor :: SDL.V4 Word8
}

defaultConfig :: IO RenderConfig
defaultConfig = do
  loadedFont <- SDLF.load "HighSchoolUSASans.ttf" 14
  return $ RenderConfig {
    font = loadedFont,
    whiteColor = SDL.V4 255 255 255 255,
    bgColor = SDL.V4 30 30 30 255,
    playerColor = SDL.V4 135 31 120 255,
    playerBurnerColor = SDL.V4 255 255 255 255,
    layerColor = SDL.V4 31 135 120 255,
    textColor = SDL.V4 0 191 255 255
  }

deleteConfig :: RenderConfig -> IO ()
deleteConfig = SDLF.free . font


playerShape :: Shape.Shape
playerShape = Shape.Shape [0, 10, 40, 30, 20, 10, 0, 15] [80, 80, 0, 0, 25, 0, 0, 40]

playerBurnerShape :: Shape.Shape
playerBurnerShape = Shape.Shape [0, 10, 25, 10, 0, 15] [80, 80, 40, 0, 0, 40]

clear :: SDL.Renderer -> SDLP.Color -> IO ()
clear renderer color = do
  SDL.rendererDrawColor renderer SDL.$= color
  SDL.clear renderer

present :: SDL.Renderer -> IO ()
present = SDL.present

defaultRender :: Graphics -> RenderConfig -> Renderer IO State.GameState
defaultRender (window, renderer) config =
  render (clear renderer $ bgColor config) (present renderer) (window, renderer) config

render :: IO () -> IO () -> Graphics -> RenderConfig -> Renderer IO State.GameState
render pre post (window, renderer) config state = do
  pre
  windowSize <- SDL.get $ SDL.windowSize window

  mapM_ (renderLayer renderer config windowSize $ State.screen state) $ State.layers state
  renderPlayer renderer config windowSize (State.screen state) (State.player state)
  renderPlayerBurner renderer config windowSize (State.screen state) (State.player state)
  renderHUD renderer config state
  post

renderHUD :: SDL.Renderer -> RenderConfig -> State.GameState -> IO ()
renderHUD renderer config state = do
  let textFont = font config
  let (velX, velY) = Player.velocity . State.player $ state

  Render.renderText renderer textFont (SDL.V2 20 20) (whiteColor config) "velocity"
  Render.renderText renderer textFont (SDL.V2 100 20) (textColor config) $ show (round velX :: Int)
  Render.renderText renderer textFont (SDL.V2 150 20) (textColor config) $ show (round velY :: Int)

  let duration = State.time state
  let seconds = round (realToFrac duration / 1000 :: Double) :: Integer
  let millis = mod duration 1000

  Render.renderText renderer textFont (SDL.V2 20 40) (whiteColor config) "time"
  Render.renderText renderer textFont (SDL.V2 100 40) (textColor config) (show seconds)
  Render.renderText renderer textFont (SDL.V2 150 40) (textColor config) (show millis)

  Render.renderText renderer textFont (SDL.V2 20 60) (whiteColor config) "score"
  Render.renderText renderer textFont (SDL.V2 100 60) (textColor config) (show $ Player.score $ State.player state)

renderPlayer :: SDL.Renderer -> RenderConfig -> SDL.V2 CInt -> Screen.Screen -> Player.Player -> IO ()
renderPlayer renderer config windowSize screen player = do
  let shape = translateCenterBottom (Player.position player)
            . flipShapeByVelocity (Player.velocity player)
            $ playerShape

  renderShape renderer windowSize screen (playerColor config) shape

renderPlayerBurner :: SDL.Renderer -> RenderConfig -> SDL.V2 CInt -> Screen.Screen -> Player.Player -> IO ()
renderPlayerBurner renderer config windowSize screen player = do
  let (posX, posY) = Player.position player
  let (velX, _) = Player.velocity player
  let offX = if velX >= 0 then -20 else 20

  let shape = translateCenterBottom (posX+offX, posY)
            . flipShapeByVelocity (Player.velocity player)
            $ playerBurnerShape

  let SDL.V4 r g b _ = playerBurnerColor config
  let a = round $ if abs velX > 10000 then 255 else abs velX / 10000 * 255

  renderShape renderer windowSize screen (SDL.V4 r g b a) shape

flipShapeByVelocity :: Player.Velocity -> Shape.Shape -> Shape.Shape
flipShapeByVelocity (velX, _) shape = if velX >= 0 then shape else flipShape shape

flipShape :: Shape.Shape -> Shape.Shape
flipShape shape = Shape.Shape xs ys
  where (w, _) = Shape.size shape
        xs = map (\x -> w - x) $ Shape.polygonXs shape
        ys = Shape.polygonYs shape

translateCenterBottom :: Position -> Shape.Shape -> Shape.Shape
translateCenterBottom (posX, posY) (Shape.Shape xs ys) = Shape.Shape xs' ys'
  where (w, _) = Shape.size (Shape.Shape xs ys)
        xs' = map ((\x -> x - w / 2) . (+posX)) xs
        ys' = map (+posY) ys

renderShape :: SDL.Renderer -> WindowSize -> Screen.Screen -> SDLP.Color -> Shape.Shape -> IO ()
renderShape renderer (SDL.V2 w h) screen color shape = do
  let toVector = foldl V.snoc V.empty

  let transformX = fromIntegral . Screen.translate screen w
  let transformY = fromIntegral . Screen.translateFlipped screen h

  let xs = toVector . map transformX $ Shape.polygonXs shape
  let ys = toVector . map transformY $ Shape.polygonYs shape

  SDLP.fillPolygon renderer xs ys color

renderLayer :: SDL.Renderer -> RenderConfig -> WindowSize -> Screen.Screen -> Layer.Layer -> IO ()
renderLayer renderer config windowSize screen layer = do
  let size = Screen.toWindowSize screen windowSize (Layer.size layer)
  let position = Screen.toWindowPosition screen windowSize (Layer.position layer)

  SDL.rendererDrawColor renderer SDL.$= layerColor config
  SDL.fillRect renderer $ Just $ SDL.Rectangle (SDL.P position) size
