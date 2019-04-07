module LambdaHeights.Types where

import           Foreign.C.Types

import qualified SDL

type WindowPosition = SDL.V2 CInt
type WindowSize = SDL.V2 CInt

type Position = (Float, Float)
type Size = (Float, Float)

