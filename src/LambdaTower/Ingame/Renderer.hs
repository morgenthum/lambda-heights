module LambdaTower.Ingame.Renderer (
  renderer,
  replayRenderer
) where

import Data.Int

import Foreign.C.Types

import LambdaTower.Ingame.State
import LambdaTower.Graphics
import LambdaTower.Loop

import qualified Data.Vector.Storable as V

import qualified SDL

import qualified Data.Text as T

import qualified SDL.Font as SDLF
import qualified SDL.Primitive as SDLP

import qualified LambdaTower.Ingame.Layer as L
import qualified LambdaTower.Ingame.Player as P

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

fontName = "HighSchoolUSASans.ttf"
fontSize = 14

whiteColor  = SDL.V4 255 255 255 255
bgColor     = SDL.V4 30 30 30 255
playerColor = SDL.V4 135 31 120 255
layerColor  = SDL.V4 31 135 120 255
textColor   = SDL.V4 0 191 255 255

replayRenderer :: Graphics -> Renderer IO (GameState, [GameState])
replayRenderer graphics (state, _) = renderer graphics state

renderer :: Graphics -> Renderer IO GameState
renderer (window, renderer, font) state = do
  SDL.rendererDrawColor renderer SDL.$= bgColor
  SDL.clear renderer

  windowSize <- SDL.get $ SDL.windowSize window
  mapM_ (renderLayer renderer windowSize $ view state) $ layers state
  renderPlayer renderer windowSize (view state) (player state)
  renderHUD renderer font state

  SDL.present renderer

renderHUD :: SDL.Renderer -> SDLF.Font -> GameState -> IO ()
renderHUD renderer font state = do
  let (velX, velY) = P.velocity . player $ state

  renderText renderer font (SDL.V2 20 20) whiteColor "velocity"
  renderText renderer font (SDL.V2 100 20) textColor (show . round $ velX)
  renderText renderer font (SDL.V2 150 20) textColor (show . round $ velY)

  end <- SDL.ticks
  let duration = end - begin state
  let seconds = round $ realToFrac duration / 1000
  let millis = mod duration 1000

  renderText renderer font (SDL.V2 20 40) whiteColor "time"
  renderText renderer font (SDL.V2 100 40) textColor (show seconds)
  renderText renderer font (SDL.V2 150 40) textColor (show millis)

  renderText renderer font (SDL.V2 20 60) whiteColor "score"
  renderText renderer font (SDL.V2 100 60) textColor (show $ P.score . player $ state)

renderText :: SDL.Renderer -> SDLF.Font -> SDL.V2 CInt -> SDLF.Color -> String -> IO ()
renderText renderer font position color text = do
  surface <- SDLF.blended font color (T.pack text)
  texture <- SDL.createTextureFromSurface renderer surface
  SDL.freeSurface surface

  textureInfo <- SDL.queryTexture texture
  let w = SDL.textureWidth textureInfo
  let h = SDL.textureHeight textureInfo

  SDL.copy renderer texture Nothing (Just $ SDL.Rectangle (SDL.P position) (SDL.V2 w h))

  SDL.destroyTexture texture

renderPlayer :: SDL.Renderer -> SDL.V2 CInt -> View -> P.Player -> IO ()
renderPlayer renderer windowSize view player = do
  let shape = shapeByVelocity (P.velocity player) playerShape

  let (x, y) = P.position player
  let (w, h) = shapeSize shape

  let xs = map ((\x -> x - (w/2)) . (+x)) $ shapeXs shape
  let ys = map (+y) $ shapeYs shape

  renderShape renderer windowSize view playerColor (xs, ys)

shapeByVelocity :: P.Velocity -> Shape -> Shape
shapeByVelocity (velX, _) shape = if velX >= 0 then shape else flipShape shape

flipShape :: Shape -> Shape
flipShape shape = (map (\x -> w - x) $ shapeXs shape, shapeYs shape)
  where (w, _) = shapeSize shape

renderShape :: SDL.Renderer -> SDL.V2 CInt -> View -> SDLP.Color -> Shape -> IO ()
renderShape renderer (SDL.V2 winW winH) view color shape = do
  let toVector = foldl V.snoc V.empty

  let transformX = fromIntegral . translateX view winW
  let transformY = fromIntegral . translateY view winH

  let xs = toVector . map transformX $ shapeXs shape
  let ys = toVector . map transformY $ shapeYs shape

  SDLP.fillPolygon renderer xs ys color

renderLayer :: SDL.Renderer -> SDL.V2 CInt -> View -> L.Layer -> IO ()
renderLayer renderer windowSize view layer = do
  let size = translateSize view windowSize (L.size layer)
  let position = translatePosition view windowSize (L.position layer)

  SDL.rendererDrawColor renderer SDL.$= layerColor
  SDL.fillRect renderer $ Just $ SDL.Rectangle (SDL.P position) size

translateSize :: View -> SDL.V2 CInt -> (Float, Float) -> SDL.V2 CInt
translateSize view (SDL.V2 w h) (x, y) = SDL.V2 (round x') (round y')
  where x' = x * fromIntegral w / (right view - left view)
        y' = y * fromIntegral h / (top view - bottom view)

translatePosition :: View -> SDL.V2 CInt -> (Float, Float) -> SDL.V2 CInt
translatePosition view (SDL.V2 w h) (x, y) = SDL.V2 x' y'
  where x' = translateX view w x
        y' = translateY view h y

translateX :: (Integral a) => View -> a -> Float -> a
translateX view w = round . (* fromIntegral w) . normalize (left view, right view)

translateY :: (Integral a) => View -> a -> Float -> a
translateY view h = round . (* fromIntegral h) . flipRange . normalize (bottom view, top view)

normalize :: (Float, Float) -> Float -> Float
normalize (min, max) x = (x - min) / (max - min)

flipRange :: Float -> Float
flipRange x = 1 - x
