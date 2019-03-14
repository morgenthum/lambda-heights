module LambdaTower.Ingame.Events where

data Direction = MoveLeft
               | MoveRight
               deriving Show

data PlayerEvent = PlayerMoved Direction Bool
                 | PlayerJumped
                 deriving Show