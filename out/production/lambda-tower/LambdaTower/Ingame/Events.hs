module LambdaTower.Ingame.Events where

data Direction = MoveLeft
               | MoveRight

data PlayerEvent = PlayerMoved Direction Bool
                 | PlayerJumped