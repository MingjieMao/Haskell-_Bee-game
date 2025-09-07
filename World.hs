{-

Module      : World
Description : World is a 2D plane centered on the hive. Locations other 
than the hive's may contain food sources, which are pairs of ints : 
the current food level, and the max food level. Food levels tick up 
(1/tick) from current to max.

-}

module World
where

import Geometry
import QTree
import Tup4

-- amount of food that one bee can carry
carryingCapacity :: Int
carryingCapacity = 15

data WorldElem = Hive | Flower Int Int
  deriving (Show, Eq)

type World = QTree WorldElem


-- returns number of non-zero content flowers and the total food content
-- in the 3x3 extent whose bottom left cell is at the XYPosn.
-- If the extent falls partially or totally off
-- the edge of the world, then only those cells that are within the world are
-- consulted.
search :: World -> XYPosn -> (Int,Int)
search w xy =
  foldl f (0,0) $ extentPts (xy,3)
    where f acc@(nflow,tot) xy = case lookupQT w xy of
            Just (Flower cf _) -> if 0 < cf then (nflow + 1,tot+cf)
                                  else acc
            _ -> acc

-- takes in
--    (input world, amount of food already being carried by bee) and
--    extent to search for flowers
-- returns
--    1. updated world,
--    2. amount of food gathered,
--    3. the number of cells -- examined (= the cost to the bee)
gatherFood :: (World,Int) -> Extent -> (World, Int, Int)
gatherFood (w,carried) e =
  let posns0 = extentPts e
      posns = filter (\p -> p `ptWithin` extent w) posns0
  in
      gatherFromPosns posns (w,carried,0)

-- gathers food from the list of positions.
-- input is (World, amount-gathered-so-far, no. of cells looked at so far)
-- returns updated triple of above
gatherFromPosns :: [XYPosn] -> (World,Int,Int) -> (World,Int,Int)
gatherFromPosns posns x@(w,fd,numCellsSeen) = case posns of
  [] -> x
  xy : rest -> case lookupQT w xy of
    Just (Flower cf t) ->
      let d = min cf (carryingCapacity - fd)
          w' = insertQT xy (Flower (cf - d) t) w
          fd' = fd + d
          state = (w',fd',numCellsSeen+1)
      in
        if fd' == carryingCapacity then state
        else gatherFromPosns rest state
    _ -> gatherFromPosns rest (w,fd,numCellsSeen+1)

-- updates all of a world's flowers to get one bigger, up to their maximum
-- size
growFlowers :: World -> World
growFlowers = mapQT growFlower

-- | Grows flowers by 1 and not exceeding its max food level, 
-- other elements unchanged.
-- >>> growFlower (0,0) (Flower 2 5)
-- Flower 3 5
-- >>> growFlower (1,1) (Flower 5 5)
-- Flower 5 5
-- >>> growFlower (0,0) Hive
-- Hive
growFlower :: XYPosn -> WorldElem -> WorldElem
growFlower _ (Flower curr maxf) = Flower (min (curr + 1) maxf) maxf
growFlower _ other              = other

showWorld :: World -> String
showWorld w = concat $ foldr rowF [] $ reverse $ extentPtsInRows $ extent w
  where rowF xys acc = foldr (\xy acc -> elemStr xy : acc) ("\n" : acc) xys
        elemStr xy = case lookupQT w xy of
                           Just we -> showWElem we
                           Nothing -> "  .  "

showWElem :: WorldElem -> String
showWElem we = case we of
  Flower cf mx -> " " ++ show cf ++ "/" ++ show mx ++ " "
  Hive -> "  H  "