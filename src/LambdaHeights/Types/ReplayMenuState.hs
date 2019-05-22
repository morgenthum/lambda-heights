module LambdaHeights.Types.ReplayMenuState where

import           Graphics.UI.Types.Table

newtype State = State {
  table  :: Table
}

