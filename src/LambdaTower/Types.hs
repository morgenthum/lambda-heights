module LambdaTower.Types where

import Control.Concurrent.STM.TChan

import Foreign.C.Types

import qualified SDL

type WindowPosition = SDL.V2 CInt
type WindowSize = SDL.V2 CInt

type Position = (Float, Float)
type Size = (Float, Float)

type Channel a = TChan (Maybe [a])