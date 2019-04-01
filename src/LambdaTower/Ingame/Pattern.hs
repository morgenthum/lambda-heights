module LambdaTower.Ingame.Pattern where

import           LambdaTower.Types

data PatternEntry = PatternEntry {
  entryId :: Int,
  description :: (Size, Float),
  distance :: Float
}

repeatPattern :: Int -> Int -> [PatternEntry] -> [PatternEntry]
repeatPattern n begin = increasingIdsFrom begin . concat . replicate n

increasingIdsFrom :: Int -> [PatternEntry] -> [PatternEntry]
increasingIdsFrom _ []       = []
increasingIdsFrom i (p : ps) = p { entryId = i } : increasingIdsFrom (i + 1) ps

combine :: Int -> [Int -> [PatternEntry]] -> [PatternEntry]
combine _     []       = []
combine begin (f : fs) = ps ++ combine end fs
 where
  ps  = f begin
  end = begin + length ps

leftRightPattern :: Int -> [PatternEntry]
leftRightPattern begin = repeatPattern
  10
  begin
  [ PatternEntry 0 ((400, 50), 0)   150
  , PatternEntry 0 ((500, 50), 500) 150
  , PatternEntry 0 ((500, 50), 0)   150
  , PatternEntry 0 ((400, 50), 600) 150
  ]

stairsPattern :: Int -> [PatternEntry]
stairsPattern begin = repeatPattern
  8
  begin
  [ PatternEntry 0 ((300, 50), 0)   150
  , PatternEntry 0 ((500, 50), 500) 0
  , PatternEntry 0 ((400, 50), 250) 150
  , PatternEntry 0 ((400, 50), 500) 150
  , PatternEntry 0 ((300, 50), 600) 150
  , PatternEntry 0 ((500, 50), 0)   0
  , PatternEntry 0 ((400, 50), 500) 150
  , PatternEntry 0 ((400, 50), 250) 150
  ]


boostPattern :: Int -> [PatternEntry]
boostPattern begin = repeatPattern
  1
  begin
  [PatternEntry 0 ((1000, 50), 0) 400, PatternEntry 0 ((600, 300), 200) 650, PatternEntry 0 ((1000, 50), 0) 1000]
