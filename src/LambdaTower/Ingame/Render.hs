module LambdaTower.Ingame.Render (
  RenderConfig(..),
  defaultConfig,
  deleteConfig,
  defaultRender,
  renderReplay,
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

import qualified LambdaTower.Components.Render as R
import qualified LambdaTower.Ingame.GameEvents as G
import qualified LambdaTower.Ingame.GameState as G
import qualified LambdaTower.Ingame.Layer as L
import qualified LambdaTower.Ingame.Player as P
import qualified LambdaTower.Screen as S

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

type Shape = ([Float], [Float])
type Size = (Float, Float)

shapeSize :: Shape -> Size
shapeSize shape = (maximum . shapeXs $ shape, maximum . shapeYs $ shape)

shapeXs :: Shape -> [Float]
shapeXs = fst

shapeYs :: Shape -> [Float]
shapeYs = snd

playerShape :: Shape
playerShape = ([0, 10, 40, 30, 20, 10, 0, 15], [80, 80, 0, 0, 25, 0, 0, 40])

playerBurnerShape :: Shape
playerBurnerShape = ([0, 10, 25, 10, 0, 15], [80, 80, 40, 0, 0, 40])

renderReplay :: Graphics -> RenderConfig -> Renderer IO ([[G.PlayerEvent]], G.GameState)
renderReplay graphics config (_, state) = defaultRender graphics config state

clear :: SDL.Renderer -> SDLP.Color -> IO ()
clear renderer color = do
  SDL.rendererDrawColor renderer SDL.$= color
  SDL.clear renderer

present :: SDL.Renderer -> IO ()
present = SDL.present

defaultRender :: Graphics -> RenderConfig -> Renderer IO G.GameState
defaultRender (window, renderer) config =
  render (clear renderer $ bgColor config) (present renderer) (window, renderer) config

render :: IO () -> IO () -> Graphics -> RenderConfig -> Renderer IO G.GameState
render pre post (window, renderer) config state = do
  pre
  windowSize <- SDL.get $ SDL.windowSize window

  mapM_ (renderLayer renderer config windowSize $ G.screen state) $ G.layers state
  renderPlayer renderer config windowSize (G.screen state) (G.player state)
  renderPlayerBurner renderer config windowSize (G.screen state) (G.player state)
  renderHUD renderer config state
  post

renderHUD :: SDL.Renderer -> RenderConfig -> G.GameState -> IO ()
renderHUD renderer config state = do
  let textFont = font config
  let (velX, velY) = P.velocity . G.player $ state

  R.renderText renderer textFont (SDL.V2 20 20) (whiteColor config) "velocity"
  R.renderText renderer textFont (SDL.V2 100 20) (textColor config) $ show (round velX :: Int)
  R.renderText renderer textFont (SDL.V2 150 20) (textColor config) $ show (round velY :: Int)

  let duration = G.time state
  let seconds = round (realToFrac duration / 1000 :: Double) :: Integer
  let millis = mod duration 1000

  R.renderText renderer textFont (SDL.V2 20 40) (whiteColor config) "time"
  R.renderText renderer textFont (SDL.V2 100 40) (textColor config) (show seconds)
  R.renderText renderer textFont (SDL.V2 150 40) (textColor config) (show millis)

  R.renderText renderer textFont (SDL.V2 20 60) (whiteColor config) "score"
  R.renderText renderer textFont (SDL.V2 100 60) (textColor config) (show $ P.score $ G.player state)

renderPlayer :: SDL.Renderer -> RenderConfig -> SDL.V2 CInt -> S.Screen -> P.Player -> IO ()
renderPlayer renderer config windowSize screen player = do
  let shape = translateCenterBottom (P.position player)
            . flipShapeByVelocity (P.velocity player)
            $ playerShape

  renderShape renderer windowSize screen (playerColor config) shape

renderPlayerBurner :: SDL.Renderer -> RenderConfig -> SDL.V2 CInt -> S.Screen -> P.Player -> IO ()
renderPlayerBurner renderer config windowSize screen player = do
  let (posX, posY) = P.position player
  let (velX, _) = P.velocity player
  let offX = if velX >= 0 then -20 else 20

  let shape = translateCenterBottom (posX+offX, posY)
            . flipShapeByVelocity (P.velocity player)
            $ playerBurnerShape

  let SDL.V4 r g b _ = playerBurnerColor config
  let a = round $ if abs velX > 10000 then 255 else abs velX / 10000 * 255

  renderShape renderer windowSize screen (SDL.V4 r g b a) shape

flipShapeByVelocity :: P.Velocity -> Shape -> Shape
flipShapeByVelocity (velX, _) shape = if velX >= 0 then shape else flipShape shape

flipShape :: Shape -> Shape
flipShape shape = (map (\x -> w - x) $ shapeXs shape, shapeYs shape)
  where (w, _) = shapeSize shape

translateCenterBottom :: S.Position -> Shape -> Shape
translateCenterBottom (posX, posY) (xs, ys) = (xs', ys')
  where (w, _) = shapeSize (xs, ys)
        xs' = map ((\x -> x - w / 2) . (+posX)) xs
        ys' = map (+posY) ys

renderShape :: SDL.Renderer -> S.WindowSize -> S.Screen -> SDLP.Color -> Shape -> IO ()
renderShape renderer (SDL.V2 w h) screen color shape = do
  let toVector = foldl V.snoc V.empty

  let transformX = fromIntegral . S.translate screen w
  let transformY = fromIntegral . S.translateFlipped screen h

  let xs = toVector . map transformX $ shapeXs shape
  let ys = toVector . map transformY $ shapeYs shape

  SDLP.fillPolygon renderer xs ys color

renderLayer :: SDL.Renderer -> RenderConfig -> S.WindowSize -> S.Screen -> L.Layer -> IO ()
renderLayer renderer config windowSize screen layer = do
  let size = S.toWindowSize screen windowSize (L.size layer)
  let position = S.toWindowPosition screen windowSize (L.position layer)

  SDL.rendererDrawColor renderer SDL.$= layerColor config
  SDL.fillRect renderer $ Just $ SDL.Rectangle (SDL.P position) size
