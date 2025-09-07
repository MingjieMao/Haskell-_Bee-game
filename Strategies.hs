{-

A number of mostly bad strategies, all with type parameter d = Int, which
is used as a clock. None actually look at the core hive data to see what
might be appropriate.

-}

module Strategies
where

import Geometry
import Hive

-- never does anything
idle :: DecisionFn Int
idle h0 = (([], 0), 0)

-- does OK on map w1 (because it knows where the food is)
w1OK :: DecisionFn Int
w1OK (_,_,_,_,d)
  | d `mod` 5 == 0 =
      (([Expedition [((-2,0), Gather 5)]], 0), (d + 1) `mod` 5)
  | d `mod` 5 == 1 =
      (([Expedition [((5,0), Gather 1)]], 0), (d + 1) `mod` 5)
  | otherwise = (([], 0), (d + 1) `mod` 5)

-- does a search, breeds a larva, and gathers thereafter
-- dies off, mostly because the gathering is occasionally done by adults
-- with too little stored food, making the gathers insufficient
searchingStrat :: DecisionFn Int
searchingStrat (_,_,_,_,d)
  | d == 0 =
      (([Expedition [((-2,-2), Search)], NewLarva], 0), 1)
  | d == 1 =
      (([Expedition [((-2,-2), Gather 5)]], 1), 2)
  | otherwise = (([], 0), 1)

larvaThenIdle (_, _, _, _, d)
  | d == 15 = (([NewLarva], 0), 16)
  | otherwise = (([], 0), d + 1)

test :: DecisionFn Int
test h0 = (([Expedition [((-2,0), Gather 5), ((0,-4), Gather 1)]], 0), 0)