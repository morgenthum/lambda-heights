module GUI.Table.Types where

import           Data.Matrix
import           Linear.V2

type DataMatrix = Matrix String
type Position = V2 Int

data Table = Table {
  content  :: DataMatrix,
  selected :: Position
}

cellPositions :: Table -> (V2 Int, [Position])
cellPositions table =
  let rCount = nrows $ content table
      cCount = ncols $ content table
  in  (V2 rCount cCount, [ V2 r c | r <- [1 .. rCount], c <- [1 .. cCount] ])
