module SDL.GUI.Clickable where

class Clickable a where
  click :: a -> b
