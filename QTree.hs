{-

Module      : QTree
Description : This module implements QTree structure to store and query data.

-}

module QTree
where

import Tup4
import Geometry

-- nodes store bottom left (x,y) and square size and then
-- either Nothing indicating no content for that area, or some subtrees which
-- divide the square into four: top left, top right, bottom left, bottom right

data QTree a = Node Extent (Maybe (Tup4 (QTree a))) | Leaf XYPosn (Maybe a)
  deriving (Show, Eq)


emptyQT :: Extent -> QTree a
emptyQT e
  | scale e <= 1 = Leaf (lleft e) Nothing
  | isPow2 (scale e) = Node e Nothing
  | otherwise = error "emptyQT passed non power of 2 for size"

isPow2 :: Int -> Bool
isPow2 i
  | i == 1 = True 
  | otherwise = i `mod` 2 == 0 && isPow2 (i `div` 2)

extent :: QTree a -> Extent
extent qt = case qt of
              Node e _ -> e
              Leaf p _ -> (p,1)

-- | Filter elements in the QTree that satisfy the predicate.
-- >>> let q1 = Leaf (0,1) (Just 1)
-- >>> let q2 = Leaf (1,1) (Just 2)
-- >>> let q3 = Leaf (0,0) (Just 3)
-- >>> let q4 = Leaf (1,0) (Just 4)
-- >>> let tree = Node ((0,0),2) (Just (q1,q2,q3,q4))
-- >>> filterQT even tree
-- [((1,1),2),((1,0),4)]
-- >>> filterQT odd tree
-- [((0,1),1),((0,0),3)]
-- >>> filterQT (> 2) tree
-- [((0,0),3),((1,0),4)]
filterQT :: (a -> Bool) -> QTree a -> [(XYPosn, a)]
filterQT p qt = 
    foldQT (\pos val acc -> if p val then (pos, val) : acc else acc) qt []  

-- | Insert a value at a given position in the QTree.
-- Replaces existing values if present.
-- Assumes the position lies within the tree's extent.
-- Note: subtrees ordered as (top-left, top-right, bottom-left, bottom-right)
-- >>> let t1 = Leaf (0,0) Nothing
-- >>> insertQT (0,0) "bee" t1
-- Leaf (0,0) (Just "bee")
-- >>> let t2 = emptyQT ((0,0),2)
-- >>> lookupQT t2 (0,0)
-- Nothing
-- >>> let t3 = insertQT (0,0) "bee" (emptyQT ((0,0),2))
-- >>> lookupQT t3 (0,0)
-- Just "bee"
-- >>> let t4 = insertQT (1,1) "a" $ insertQT (0,0) "b" $ emptyQT ((0,0),2)
-- >>> lookupQT t4 (0,0)
-- Just "b"
-- >>> let t5 = insertQT (1,1) "a" (emptyQT ((2,2),2)) 
-- >>> lookupQT t5 (1,1)
-- Nothing
-- >>> let t6 = insertQT (0,0) "1" $ insertQT (0,0) "2" $ emptyQT ((0,0) ,2)
-- >>> lookupQT t6 (0,0)
-- Just "1"
-- >>> let t7 = insertQT (4,4) "True" (emptyQT ((0,0),4))
-- >>> lookupQT t7 (4,4)
-- Nothing
-- >>> let t8 = insertQT (2,2) "False" (emptyQT ((2,2),2))
-- >>> lookupQT t8 (2,2)
-- Just "False"
insertQT :: XYPosn -> a -> QTree a -> QTree a
insertQT pos val (Leaf p _) 
  | pos == p  = Leaf pos (Just val)
  | otherwise = error "insertQT: position mismatch in Leaf"
insertQT pos val (Node ext Nothing) = 
  Node ext (Just updatedSubtrees)
  where 
    (tl, tr, bl, br) = splitExtent ext
    emptySubtrees = (emptyQT tl, emptyQT tr, emptyQT bl, emptyQT br)
    updatedSubtrees = update4 ext pos (insertQT pos val) emptySubtrees
insertQT pos val (Node ext (Just subtrees)) = 
  Node ext (Just updatedSubtrees)
  where
    updatedSubtrees = update4 ext pos (insertQT pos val) subtrees

-- | Split an extent into 4 quadrants.
-- >>> splitExtent ((0, 0), 4)
-- (((0,2),2),((2,2),2),((0,0),2),((2,0),2))
-- >>> splitExtent ((2, 3), 2)
-- (((2,4),1),((3,4),1),((2,3),1),((3,3),1))
splitExtent :: Extent -> (Extent, Extent, Extent, Extent)
splitExtent ((x0, y0), s) = 
  (((x0,     y0 + h), h),    -- top-left
   ((x0 + h, y0 + h), h),    -- top-right
   ((x0,     y0    ), h),    -- bottom-left
   ((x0 + h, y0    ), h))    -- bottom-right  
  where 
    h = s `div` 2

-- | Fold a QTree from bottom-right to top-left using the given function.
-- Fold order: bottom-right → bottom-left → top-right → top-left
-- >>> let q1 = Leaf (0,1) (Just "TL")
-- >>> let q2 = Leaf (1,1) (Just "TR")
-- >>> let q3 = Leaf (0,0) (Just "BL")
-- >>> let q4 = Leaf (1,0) (Just "BR")
-- >>> let tree = Node ((0,0),2) (Just (q1,q2,q3,q4))
-- >>> foldQT (\pos a acc -> (show pos ++ ":" ++ a) : acc) tree []
-- ["(0,1):TL","(1,1):TR","(0,0):BL","(1,0):BR"]
-- >>> foldQT (\pos a acc -> (pos, a) : acc) tree []
-- [((0,1),"TL"),((1,1),"TR"),((0,0),"BL"),((1,0),"BR")]
-- >>> foldQT (\_ a acc -> a : acc) (emptyQT ((0,0),2)) []
-- []
-- >>> foldQT (\_ a acc -> a : acc) (Leaf (0,0) (Just "X")) []
-- ["X"]
-- >>> foldQT (\_ a acc -> a : acc) (Leaf (1,1) (Just "2")) []
-- ["2"]
-- >>> foldQT (\_ a acc -> a : acc) (Leaf (0,0) Nothing) []
-- []
-- >>> foldQT (\_ _ acc -> acc + 1) tree 0
-- 4
-- >>> foldQT (\_ a acc -> a ++ acc) tree ""
-- "TLTRBLBR"
foldQT :: (XYPosn -> a -> b -> b) -> QTree a -> b -> b
foldQT f (Leaf pos Nothing) acc = acc
foldQT f (Leaf pos (Just a)) acc = f pos a acc
foldQT f (Node _ Nothing) acc = acc 
foldQT f (Node _ (Just (q1, q2, q3, q4))) acc = acc4
  where
    acc1 = foldQT f q4 acc  -- bottom-right 
    acc2 = foldQT f q3 acc1 -- bottom-left
    acc3 = foldQT f q2 acc2 -- top-right
    acc4 = foldQT f q1 acc3 -- top-left 

-- | Applies a function to all values in a QTree
-- >>> mapQT (\_ n -> n + 1) (Leaf (0,0) (Just 2))
-- Leaf (0,0) (Just 3)
-- >>> mapQT (\_ n -> n * 2) (Leaf (1,1) Nothing)
-- Leaf (1,1) Nothing
-- >>> let q1 = Leaf (0,0) (Just 1)
-- >>> let q2 = Leaf (1,0) (Just 2)
-- >>> let q3 = Leaf (0,1) (Just 3)
-- >>> let q4 = Leaf (1,1) (Just 4)
-- >>> let t' = Node ((0,0),2) Nothing
-- >>> mapQT (\(x,y) n -> show (x + y + n)) t'
-- Node ((0,0),2) Nothing
-- >>> let t = Leaf (0,0) (Just 5)
-- >>> mapQT (\_ n -> n * 2) t
-- Leaf (0,0) (Just 10)
mapQT :: (XYPosn -> a -> b) -> QTree a -> QTree b
mapQT f (Leaf pos Nothing)  = Leaf pos Nothing
mapQT f (Leaf pos (Just a)) = Leaf pos (Just (f pos a))
mapQT f (Node ext Nothing)  = Node ext Nothing 
mapQT f (Node ext (Just (q1,q2,q3,q4))) = 
  Node ext (Just (mapQT f q1, mapQT f q2, mapQT f q3, mapQT f q4))

-- | looks up the value at a given position in QTree.
-- Returns 'Just a' if present, or 'Nothing' otherwise.
-- >>> let q = Leaf (0,0) (Just "1")
-- >>> lookupQT q (0,0)
-- Just "1"
-- >>> lookupQT q (1,1)
-- Nothing
-- >>> let qt = emptyQT ((-8,-8), 16)
-- >>> let qt' = insertQT (0,0) "Hello" qt
-- >>> lookupQT qt' (0,0)
-- Just "Hello"
-- >>> lookupQT qt' (1,1)
-- Nothing
-- >>> let qt'' = insertQT (0,0) "Goodbye" qt'
-- >>> lookupQT qt'' (0,0)
-- Just "Goodbye"
-- >>> lookupQT qt'' (16,16)
-- Nothing
lookupQT :: QTree a -> XYPosn -> Maybe a 
lookupQT (Leaf pos val) queryPos = 
  if pos == queryPos then val else Nothing 
lookupQT (Node _ Nothing) queryPos = Nothing
lookupQT (Node ext (Just subtrees)) queryPos = 
  case ptWithin queryPos ext of
    False -> Nothing
    True  -> lookupQT (select4 ext queryPos subtrees) queryPos
      