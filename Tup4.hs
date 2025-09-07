{-

Module      : Tup4
Description : This module defines functions for mapping, folding, updating, 
and selecting elements from 4-tuples used in quad tree, and four quadrants are 
(top-left, top-right, bottom-left, bottom-right).

-}

module Tup4
where

import Geometry

-- Tup4 representing a 4-tuple of identical types: (a,a,a,a)
type Tup4 a = (a,a,a,a)

-- | Apply the function to a 4-tuple of a tree.
-- >>> map4 (+1) (1, 2, 3, 4)
-- (2,3,4,5)
-- >>> map4 (*2) (1, 2, 3, 4)
-- (2,4,6,8)
map4 :: (a -> b) -> Tup4 a -> Tup4 b
map4 f (a1,a2,a3,a4) = (f a1, f a2, f a3, f a4)

-- | Fold over a 4-tuple from right to left.
-- Order: a1 -> a2 -> a3 -> a4 -> b
-- >>> fold4 (+) (1, 2, 3, 4) 0
-- 10
-- >>> fold4 (:) ("a", "b", "c", "d") []
-- ["a","b","c","d"]
fold4 :: (a -> b -> b) -> Tup4 a -> b -> b
fold4 f (a1,a2,a3,a4) b = f a1 (f a2 (f a3 (f a4 b)))

-- | Updates one element in a 4-tuple based on a given position.
-- >>> update4 ((0,0),2) (0,1) (++ "!") ("TL", "TR", "BL", "BR")
-- ("TL!","TR","BL","BR")
-- >>> update4 ((0,0),2) (1,1) (++ "!") ("TL", "TR", "BL", "BR")
-- ("TL","TR!","BL","BR")
-- >>> update4 ((1,1),4) (1,1) (*2) (1,2,3,4)
-- (1,2,6,4)
update4 :: Extent -> XYPosn -> (a -> a) -> Tup4 a -> Tup4 a
update4 ((x0,y0), d0) (x,y) f (a1,a2,a3,a4) = 
  case (x < midX, y < midY) of
    (True, False)  -> (f a1, a2, a3, a4) -- top-left
    (False, False) -> (a1, f a2, a3, a4) -- top-right
    (True, True)   -> (a1, a2, f a3, a4) -- bottom-left
    (False, True)  -> (a1, a2, a3, f a4) -- bottom-right
  where
    midX = x0 + d0 `div` 2
    midY = y0 + d0 `div` 2 

-- | Select the element in the 4-tuple corresponding to the quadrant.
-- that contains the given position.
-- >>> select4 ((0,0),2) (0,1) ("TL", "TR", "BL", "BR")
-- "TL"
-- >>> select4 ((0,0),2) (1,1) ("TL", "TR", "BL", "BR")
-- "TR"
-- >>> select4 ((0,0),2) (0,0) ("TL", "TR", "BL", "BR")
-- "BL"
-- >>> select4 ((0,0),2) (1,0) ("TL", "TR", "BL", "BR")
-- "BR"
select4 :: Extent -> XYPosn -> Tup4 a -> a
select4 ((x0,y0), d0) (x,y) (a1,a2,a3,a4) = 
  case (x < midX, y < midY) of
    (True, False)  -> a1 -- top-left
    (False, False) -> a2 -- top-right
    (True, True)   -> a3 -- bottom-left
    (False, True)  -> a4 -- bottom-right
  where
    midX = x0 + d0 `div` 2
    midY = y0 + d0 `div` 2
    