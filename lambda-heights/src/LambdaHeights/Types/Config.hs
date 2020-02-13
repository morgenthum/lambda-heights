module LambdaHeights.Types.Config where

import Control.Monad.Reader
import qualified SDL.Font as SDLF

type ConfigReader = ReaderT Config IO

data Config
  = Config
      { menuFont :: SDLF.Font,
        metaFont :: SDLF.Font
      }
