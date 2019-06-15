module LambdaHeights.Play.Pattern
  ( PatternEntry(..)
  , combine
  , leftRightPattern
  , boostPattern
  , stairsPattern
  , highPattern
  )
where

import           LambdaHeights.Types
import           Linear.V2

-- | Represents a template for a layer.
data PatternEntry = PatternEntry {
  entryId       :: Int,
  entrySize     :: Size,
  entryPosition :: Position -- ^ x = delta from left side, y = delta y from previous layer
}

-- | Generates entries with increasing ids from given Int.
type PatternGenerator = Int -> [PatternEntry]

-- | Combines results of generators with contiguous ids.
combine :: Int -> [PatternGenerator] -> [PatternEntry]
combine _ [] = []
combine begin (f : fs) =
  let ps  = f begin
      end = begin + length ps
  in  ps ++ combine end fs

leftRightPattern :: PatternGenerator
leftRightPattern begin = repeatPattern
  10
  begin
  [ PatternEntry 0 (V2 400 50) (V2 0 150)
  , PatternEntry 0 (V2 500 50) (V2 500 150)
  , PatternEntry 0 (V2 500 50) (V2 0 150)
  , PatternEntry 0 (V2 400 50) (V2 600 150)
  ]

boostPattern :: PatternGenerator
boostPattern begin = repeatPattern
  1
  begin
  [ PatternEntry 0 (V2 1000 50) (V2 0 500)
  , PatternEntry 0 (V2 600 300) (V2 200 550)
  , PatternEntry 0 (V2 1000 50) (V2 0 1000)
  ]

stairsPattern :: PatternGenerator
stairsPattern begin = repeatPattern
  4
  begin
  [ PatternEntry 0 (V2 300 50) (V2 0 150)
  , PatternEntry 0 (V2 500 50) (V2 500 0)
  , PatternEntry 0 (V2 400 50) (V2 250 150)
  , PatternEntry 0 (V2 400 50) (V2 500 150)
  , PatternEntry 0 (V2 300 50) (V2 600 150)
  , PatternEntry 0 (V2 500 50) (V2 0 0)
  , PatternEntry 0 (V2 400 50) (V2 500 150)
  , PatternEntry 0 (V2 400 50) (V2 250 150)
  ]

highPattern :: PatternGenerator
highPattern begin = repeatPattern
  1
  begin
  [ PatternEntry 0 (V2 300 50) (V2 100 500)
  , PatternEntry 0 (V2 300 50) (V2 600 0)
  , PatternEntry 0 (V2 300 50) (V2 200 750)
  , PatternEntry 0 (V2 300 50) (V2 500 750)
  , PatternEntry 0 (V2 300 50) (V2 200 750)
  , PatternEntry 0 (V2 300 50) (V2 500 750)
  , PatternEntry 0 (V2 300 50) (V2 200 750)
  , PatternEntry 0 (V2 300 50) (V2 500 750)
  , PatternEntry 0 (V2 300 50) (V2 200 750)
  , PatternEntry 0 (V2 300 50) (V2 500 750)
  ]

repeatPattern :: Int -> Int -> [PatternEntry] -> [PatternEntry]
repeatPattern n begin = increasingIdsFrom begin . concat . replicate n

increasingIdsFrom :: Int -> [PatternEntry] -> [PatternEntry]
increasingIdsFrom _ []       = []
increasingIdsFrom i (p : ps) = p { entryId = i } : increasingIdsFrom (i + 1) ps
