{-

Module      : Geometry
Author      : COMP1100, Mingjie Mao, u8173888
Date        : 17/5/2025
Description : This module defines Geometry data type and functions for 
working with grid coordinates.
Basic functions over "extents" and xy points in the Cartesian plane.

Coordinates are always integers, and distances are calculated
according to Manhattan distance, as if one can only travel is the
North-South and East-West directions, never diagonally.

An "extent" is a pair of an x-y point corresponding to the lower left
cell, and a number indicating the length of the sides of the square.
Thus: the extent ((1,3), 2) includes the points

   (1,4)   (2,4)
   (1,3)   (2,3)

The size integer is occasionally referred to as the "scale". An extent
will include scale^2 many points.

-}

module Geometry
where

-- | Position on a discrete Cartesian plane.
type XYPosn = (Int,Int)

-- | Square with lower left cell at XYPosn and side of length Int.
type Extent = (XYPosn, Int)

-- | List of places to go, coupled with things to do once there.
type Expedition a = [(XYPosn, a)]

-- | Manhattan distance between to XYPosns (bees can't fly diagonals).
-- >>> distance (0,0) (3,0)
-- 3
-- >>> distance (1,2) (-1,4)
-- 4
distance :: XYPosn -> XYPosn -> Int
distance (x1,y1) (x2,y2) = abs (x1 - x2) + abs (y1 - y2)

-- | Vector addition
-- >>> vplus (3,2) (-1,1)
-- (2,3)
vplus :: XYPosn -> XYPosn -> XYPosn
vplus (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)

-- | Vector difference
-- >>> vminus (3,2) (-1,1) 
-- (4,1)
vminus :: XYPosn -> XYPosn -> XYPosn
vminus (x1,y1) (x2,y2) = (x1 - x2, y1 - y2)

-- | Functions for extracting extent components.
lleft :: Extent -> XYPosn
lleft (xy,_) = xy

scale :: Extent -> Int
scale (_,d) = d

-- | points that make up an extent.
-- >>> extentPts ((0,0), 3)
-- [(0,0),(1,0),(2,0),(0,1),(1,1),(2,1),(0,2),(1,2),(2,2)]
extentPts :: Extent -> [XYPosn]
extentPts e = concat $ extentPtsInRows e

-- | Points that make up an extent, arranged in rows.
-- >>> extentPtsInRows ((0,3), 3)
-- [[(0,3),(1,3),(2,3)],[(0,4),(1,4),(2,4)],[(0,5),(1,5),(2,5)]]
-- >>> extentPtsInRows ((0, 0), 1)
-- [[(0,0)]]
-- >>> extentPtsInRows ((-1, -1), 2)
-- [[(-1,-1),(0,-1)],[(-1,0),(0,0)]]
extentPtsInRows :: Extent -> [[XYPosn]]
extentPtsInRows ((x0,y0), d) = 
  [[(x,y) | x <- [x0 .. x0 + d - 1]] | y <- [y0..y0 + d - 1]]

-- | True if first extent is completely contained within the second.
containedWithin :: Extent -> Extent -> Bool 
containedWithin ((x1,y1), d1) ((x2, y2), d2) = 
  x2 <= x1 && x1 + d1 <= x2 + d2 && 
  y2 <= y1 && y1 + d1 <= y2 + d2 

-- | True if the point appears within the extent.
-- >>> ptWithin (2,0) ((0,0),4)
-- True
-- >>> ptWithin (0,2) ((0,0),4)
-- True
-- >>> ptWithin (4,1) ((0,0),4)
-- False
-- >>> ptWithin (1,4) ((0,0),4)
-- False
ptWithin :: XYPosn -> Extent -> Bool
ptWithin (x1,y1) ((x2,y2),d) = 
  x1 >= x2 && x1 < x2 + d && 
  y1 >= y2 && y1 < y2 + d 

-- | Calculates the integer square root of the argument, rounding down
-- >>> isqrt 9
-- 3
-- >>> isqrt 26
-- 5
-- >>> isqrt 64
-- 8
isqrt :: Int -> Int 
isqrt i 
  | i <= 0 = 0 
  | i == 1 = 1
  | otherwise = let x = 2 * isqrt (i `div` 4)
                in
                  if i < (x + 1) ^ 2 then x else x + 1
