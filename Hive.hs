{-

Module      : Hive
Description : This module defines internal representation and operations
of the hive, used in a grid bee world.
The Beehive.

The hive contains some number of Bees, which are either adults or
larvae. Adults are assigned to perform BeeActions, including the no-op
Idle action. Larvae are necessarily idle. Implicitly there is also a
Queen, but the Queen is also immortal and doesn't consume food.

Adults only die through starvation (oh to be a bee); Larvae turn into
Adults after ageToMaturity many ticks.

Life from day to day consumes 1 food;
Expeditions cost:
- the number of steps (out and back again) `div` 5
- 1 per search;
- 1 per cell examined in an unsuccessful gather

-}

module Hive
where

import Geometry
import World
import Data.List (intercalate)
import Data.Maybe (mapMaybe)

-- the bees
newtype Adult = Adult Int deriving (Show,Eq)
data Larva = Larva Int Int  -- ticks to adulthood, food level
  deriving (Show,Eq)

-- hive constants
ageToMaturity :: Int
ageToMaturity = 5

danceStackMaxDepth :: Int
danceStackMaxDepth = 5

initialAdultFood :: Int
initialAdultFood = 20

stepCostDivisor :: Int
stepCostDivisor = 6

{-
Core hive (Hive0 type) is represented as tuple containing components:
  1.  list of integers corresponding to the adults, with each integer being
      that adult's food level.  (Zero food = death by starvation).
  2.  list of pairs of integers.  First of each pair is (reversed)
      age of larva = number of ticks before larva become adult.
      When larva is born its "age" is set to ageToMaturity.
      Second component of each pair is that larva's food level.
      Again, if food hits zero, larva dies.
  3.  At most danceStackMaxDepth Waggle-dances from returned explorers,
      arranged in a stack so that most recent dances are on top of stack,
      if length gets too great, oldest dances are removed
  4.  Amount of food left over from previous distributions of gathered food
  5.  User data (type variable d)
-}

type Hive0 d = ([Adult], [Larva], WaggleDanceStack, Int, d)

-- the "full" hive is the data above coupled with the hive's position on the
-- map, and the strategy.  These last two components don't change.
type Hive d = (Hive0 d, DecisionFn d, XYPosn)

-- build a new hive with some number of adults, some strategy information and
-- its position in the world.
newHive :: Int -> (DecisionFn d, d) -> XYPosn -> Hive d
newHive numAdults (strat, d0) xy =
  ((replicate numAdults (Adult initialAdultFood), [], [], 0, d0), strat, xy)

-- hive decisions are made from turn to turn, with the decision function
-- being passed the data of the Hive0
type DecisionFn d = Hive0 d -> (Action, d)


-- First Int component is number of preserved waggle-dances to erase from head 
-- of danceStack; these have been processed by GatherActions
-- Second component is new value of data component
-- The BeeActions are allocated to adults in the order that adults are listed;
-- being Idle or getting a new larva costs 1 food.
type Action = ([BeeAction], Int)


data ExpeditionAction = Search | Gather Int
data BeeAction = Expedition (Expedition ExpeditionAction) | Idle | NewLarva



strategy :: Hive d -> DecisionFn d
strategy (_,df,_) = df

core :: Hive d -> Hive0 d
core (h0, _, _) = h0

adults :: Hive d -> [Adult]
adults ((as, _, _, _, _), _, _) = as

larvae :: Hive d -> [Larva]
larvae ((_, ls, _, _, _), _, _) = ls

type WaggleDanceStack = [WaggleDance]

-- position searched (all searches are of size 3);
-- the nonzero number of flowers found there
-- (bees don't bother reporting if they found nothing at all); and
-- the total food count there
type WaggleDance = (XYPosn, Int, Int)

eraseActions :: Int -> Hive d -> Hive d
eraseActions i ((as, ls, wds, fd, d), df, pos) =
  ((as, ls, drop i wds, fd, d), df,pos)

setAdults :: [Adult] -> Hive d -> Hive d
setAdults as ((_, ls, wds, fd, d), df, pos) = ((as,ls,wds,fd,d),df,pos)

setData :: d -> Hive d -> Hive d
setData d ((as, ls, wds, fd, _), df, pos) = ((as,ls,wds,fd,d),df,pos)

updAdults :: (Adult -> Adult) -> Hive d -> Hive d
updAdults f ((as,ls,wds,fd,d),df,pos) = ((map f as, ls, wds, fd, d), df, pos)

setLarvae :: [Larva] -> Hive d -> Hive d
setLarvae ls ((as, _, wds,fd,d), df,pos) = ((as,ls,wds,fd,d),df,pos)

updLarvae :: (Larva -> Larva) -> Hive d -> Hive d
updLarvae f ((as, ls, wds,fd,d), df,pos) = ((as,map f ls,wds,fd,d),df,pos)

updDances :: ([WaggleDance] -> [WaggleDance]) -> Hive d -> Hive d
updDances f ((as, ls, wds,fd,d),df,pos) = ((as, ls, f wds, fd,d), df, pos)

pushDance :: WaggleDance -> Hive d -> Hive d
pushDance d = updDances (d:)

dances :: Hive d -> [WaggleDance]
dances ((_, _, wds, _, _), _, _) = wds

leftovers :: Hive d -> Int
leftovers ((_,_,_,fd,_), _, _) = fd

setLeftovers :: Int -> Hive d -> Hive d
setLeftovers fd ((as,ls,wds,_,d),df,pos) = ((as,ls,wds,fd,d),df,pos)

feedLarva fd (Larva age f) = Larva age (f + fd)
feedAdult fd (Adult f) = Adult (f + fd)

hiveLocation :: Hive d -> XYPosn
hiveLocation (_, _, pos) = pos

distributeFood :: Int -> Hive d -> Hive d
distributeFood fc0 h0
  | popn == 0 = h0
  | otherwise =
    let fc = fc0 + leftovers h0
        share = fc `div` popn
    in
      updAdults (feedAdult share) $
      updLarvae (feedLarva share) $
      setLeftovers (fc - popn * share) h0
  where popn = population h0

population :: Hive d -> Int
population h = length (adults h) + length (larvae h)

dropExcessDances :: Hive d -> Hive d
dropExcessDances = updDances (take danceStackMaxDepth)

showAdults :: [Adult] -> String
showAdults as = "[" ++ intercalate "," (map (\ (Adult i) -> show i) as) ++ "]"

showLarvae :: [Larva] -> String
showLarvae ls = "[" ++ intercalate "," (map showf ls) ++ "]"
  where showf (Larva a f) = show a ++ "/" ++ show f

simpleHiveToString :: Hive d -> String
simpleHiveToString h =
  "Adults: " ++ showAdults (adults h) ++
  "; Larvae: " ++ showLarvae (larvae h) ++
  "; leftovers: " ++ show (leftovers h)

-- presents hive's stored dances as a somewhat compact string
dancesToString :: Hive d -> String
dancesToString h = "[" ++ intercalate ";" (map show $ dances h) ++ "]"

ageStarveAndKill :: Hive d -> Hive d
ageStarveAndKill h =
  setAdults (hungrierAdults ++ newadults) $ setLarvae olderLarvae $ h
  where (newadults, olderLarvae) = ageStarveAndMatureLarvae (larvae h)
        hungrierAdults = starveAdults (adults h)

-- | Ages larvae: They can starve, turn into adults, or simply consume some food
-- and get a little older.
-- >>> ageStarveAndMatureLarvae [Larva 10 0, Larva 6 4]
-- ([],[Larva 5 3])
-- >>> ageStarveAndMatureLarvae [Larva 0 6]
-- ([Adult 5],[])
-- >>> ageStarveAndMatureLarvae [Larva 0 0]
-- ([],[])
ageStarveAndMatureLarvae :: [Larva] -> ([Adult], [Larva])
ageStarveAndMatureLarvae [] = ([], [])
ageStarveAndMatureLarvae (Larva age food : xs)
  | food == 0 = ageStarveAndMatureLarvae xs
  | age == 0  = (Adult (food - 1) : adults, larvae)
  | otherwise = (adults, Larva (age - 1) (food - 1) : larvae)
  where
    (adults, larvae) = ageStarveAndMatureLarvae xs

starveAdults :: [Adult] -> [Adult]
starveAdults as = mapMaybe f as
  where f (Adult food)
         | food <= 0  = Nothing -- starved
         | otherwise  = Just (Adult (food - 1))

-- | Zip two lists together and also return the unmatched elements.
-- Output is a triple:
-- List of paired items from both inputs.
-- Unpaired elements from the first list.
-- Unpaired elements from the second list.
-- >>> zipWithRemainders [1,2,3,4] ["Hello", "world"]
-- ([(1,"Hello"),(2,"world")],[3,4],[])
-- >>> zipWithRemainders ["Hello", "world"] [1,2,3,4]
-- ([("Hello",1),("world",2)],[],[3,4])
-- >>> zipWithRemainders [] [1,2]
-- ([],[],[1,2])
-- >>> zipWithRemainders [1,2,3] []
-- ([],[1,2,3],[])
zipWithRemainders :: [a] -> [b] -> ([(a,b)], [a], [b])
zipWithRemainders [] bs = ([], [], bs)
zipWithRemainders as [] = ([], as, [])
zipWithRemainders (a:as) (b:bs) = ((a,b) : zs, as', bs')
  where (zs, as', bs') = zipWithRemainders as bs
