module LambdaTower.Ingame.Renderer (
  renderer
) where

import Data.Text
import Foreign.C.Types

import LambdaTower.Ingame.Events
import LambdaTower.Ingame.Input
import LambdaTower.Ingame.State
import LambdaTower.Ingame.Update
import LambdaTower.Graphics
import LambdaTower.Loop

import qualified SDL
import qualified SDL.Font as SDLF

import qualified LambdaTower.Ingame.Layer as L
import qualified LambdaTower.Ingame.Player as P

fontName = "HighSchoolUSASans.ttf"
fontSize = 14

whiteColor  = SDL.V4 255 255 255 255
bgColor     = SDL.V4 30 30 30 255
playerColor = SDL.V4 135 31 120 255
layerColor  = SDL.V4 31 135 120 255
textColor   = SDL.V4 0 191 255 255

renderer :: Graphics -> Renderer IO State
renderer (window, renderer, font) state = do

  SDL.rendererDrawColor renderer SDL.$= bgColor
  SDL.clear renderer

  windowSize <- SDL.get $ SDL.windowSize window
  mapM_ (renderLayer renderer windowSize $ view state) $ layers state
  renderPlayer renderer windowSize (view state) (player state)
  renderHUD renderer font state

  SDL.present renderer


renderHUD :: SDL.Renderer -> SDLF.Font -> State -> IO ()
renderHUD renderer font state = do

  let (SDL.V2 velX velY) = P.velocity . player $ state

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

    surface <- SDLF.blended font color (pack text)
    texture <- SDL.createTextureFromSurface renderer surface
    SDL.freeSurface surface

    textureInfo <- SDL.queryTexture texture
    let w = SDL.textureWidth textureInfo
    let h = SDL.textureHeight textureInfo

    SDL.copy renderer texture Nothing (Just $ SDL.Rectangle (SDL.P position) (SDL.V2 w h))

    SDL.destroyTexture texture


renderPlayer :: SDL.Renderer -> SDL.V2 CInt -> View -> P.Player -> IO ()
renderPlayer renderer windowSize view player = do

  let (SDL.V2 w h) = translateSize view windowSize (P.size player)
  let (SDL.V2 x y) = translatePosition view windowSize (P.position player)

  let position = SDL.P $ SDL.V2 (fromIntegral $ x - round (realToFrac w / 2)) (fromIntegral $ y - h)

  SDL.rendererDrawColor renderer SDL.$= playerColor
  SDL.fillRect renderer (Just $ SDL.Rectangle position (SDL.V2 w h))


renderLayer :: SDL.Renderer -> SDL.V2 CInt -> View -> L.Layer -> IO ()
renderLayer renderer windowSize view layer = do

  let size = translateSize view windowSize (L.size layer)
  let position = translatePosition view windowSize (L.position layer)

  SDL.rendererDrawColor renderer SDL.$= layerColor
  SDL.fillRect renderer (Just $ SDL.Rectangle (SDL.P position) size)


translateSize :: View -> SDL.V2 CInt -> SDL.V2 Float -> SDL.V2 CInt
translateSize view (SDL.V2 w h) (SDL.V2 x y) = SDL.V2 (round x') (round y')
  where x' = x * fromIntegral w / (right view - left view)
        y' = y * fromIntegral h / (top view - bottom view)

translatePosition :: View -> SDL.V2 CInt -> SDL.V2 Float -> SDL.V2 CInt
translatePosition view (SDL.V2 w h) (SDL.V2 x y) = SDL.V2 (round x') (round y')
  where x' = (* fromIntegral w) . normalize (left view, right view) $ x
        y' = (* fromIntegral h) . flipRange . normalize (bottom view, top view) $ y

normalize :: (Float, Float) -> Float -> Float
normalize (min, max) x = (x - min) / (max - min)

flipRange :: Float -> Float
flipRange x = 1 - x
