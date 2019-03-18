module LambdaTower.Ingame.Render (
  defaultConfig,
  deleteConfig,
  render,
  renderReplay
) where

import Data.Word

import qualified Data.Vector.Storable as V

import Foreign.C.Types

import qualified SDL
import qualified SDL.Font as SDLF
import qualified SDL.Primitive as SDLP

import LambdaTower.Graphics
import LambdaTower.Loop

import qualified LambdaTower.Ingame.Events as E
import qualified LambdaTower.Ingame.Layer as L
import qualified LambdaTower.Ingame.GameState as G
import qualified LambdaTower.Ingame.Player as P
import qualified LambdaTower.Screen as S

data RenderConfig = RenderConfig {
  font :: SDLF.Font,
  whiteColor :: SDL.V4 Word8,
  bgColor :: SDL.V4 Word8,
  playerColor :: SDL.V4 Word8,
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

renderReplay :: Graphics -> RenderConfig -> Renderer IO ([E.PlayerEvent], [[E.PlayerEvent]], G.GameState)
renderReplay graphics config (_, _, state) = render graphics config state

render :: Graphics -> RenderConfig -> Renderer IO G.GameState
render (window, renderer) config state = do
  SDL.rendererDrawColor renderer SDL.$= bgColor config
  SDL.clear renderer

  windowSize <- SDL.get $ SDL.windowSize window

  mapM_ (renderLayer renderer config windowSize $ G.view state) $ G.layers state
  renderPlayer renderer config windowSize (G.view state) (G.player state)
  renderHUD renderer config state

  SDL.present renderer

renderHUD :: SDL.Renderer -> RenderConfig -> G.GameState -> IO ()
renderHUD renderer config state = do
  let textFont = font config
  let (velX, velY) = P.velocity . G.player $ state

  renderText renderer textFont (SDL.V2 20 20) (whiteColor config) "velocity"
  renderText renderer textFont (SDL.V2 100 20) (textColor config) $ show (round velX :: Int)
  renderText renderer textFont (SDL.V2 150 20) (textColor config) $ show (round velY :: Int)

  end <- SDL.ticks
  let duration = end - G.begin state
  let seconds = round (realToFrac duration / 1000 :: Float) :: Int
  let millis = mod duration 1000

  renderText renderer textFont (SDL.V2 20 40) (whiteColor config) "time"
  renderText renderer textFont (SDL.V2 100 40) (textColor config) (show seconds)
  renderText renderer textFont (SDL.V2 150 40) (textColor config) (show millis)

  renderText renderer textFont (SDL.V2 20 60) (whiteColor config) "score"
  renderText renderer textFont (SDL.V2 100 60) (textColor config) (show $ P.score . G.player $ state)

renderPlayer :: SDL.Renderer -> RenderConfig -> SDL.V2 CInt -> S.Screen -> P.Player -> IO ()
renderPlayer renderer config windowSize view player = do
  let shape = shapeByVelocity (P.velocity player) playerShape

  let (posX, posY) = P.position player
  let (w, _) = shapeSize shape

  let xs = map ((\x -> x - w / 2) . (+posX)) $ shapeXs shape
  let ys = map (+posY) $ shapeYs shape

  renderShape renderer windowSize view (playerColor config) (xs, ys)

shapeByVelocity :: P.Velocity -> Shape -> Shape
shapeByVelocity (velX, _) shape = if velX >= 0 then shape else flipShape shape

flipShape :: Shape -> Shape
flipShape shape = (map (\x -> w - x) $ shapeXs shape, shapeYs shape)
  where (w, _) = shapeSize shape

renderShape :: SDL.Renderer -> S.WindowSize -> S.Screen -> SDLP.Color -> Shape -> IO ()
renderShape renderer (SDL.V2 w h) view color shape = do
  let toVector = foldl V.snoc V.empty

  let transformX = fromIntegral . S.translateX view w
  let transformY = fromIntegral . S.translateY view h

  let xs = toVector . map transformX $ shapeXs shape
  let ys = toVector . map transformY $ shapeYs shape

  SDLP.fillPolygon renderer xs ys color

renderLayer :: SDL.Renderer -> RenderConfig -> S.WindowSize -> S.Screen -> L.Layer -> IO ()
renderLayer renderer config windowSize view layer = do
  let size = S.toWindowSize view windowSize (L.size layer)
  let position = S.toWindowPosition view windowSize (L.position layer)

  SDL.rendererDrawColor renderer SDL.$= layerColor config
  SDL.fillRect renderer $ Just $ SDL.Rectangle (SDL.P position) size
