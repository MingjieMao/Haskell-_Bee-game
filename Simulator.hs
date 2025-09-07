{-

Simulator

Functions for simulating the state of the bee world.  Core of this is the
simulate1 function which does one clock-tick of this.

The simulate1 function determines the hive strategy and then figures
out what this means for the hive and the world. Once bees are
allocated to actions there is a complicated type called ProcessAction
that is used to track the effects of a single bee's actions. Below
this, there is a relatively complicated type called ProcessAction,
whicih tracks all the data relevant for that bee, including the food
expense of its actions, how much food it may have gathered etc.

-}

module Simulator
where

import Geometry
import QTree
import World
import Hive

-- first int is total expedition cost; second is total food gathered
-- these numbers are gathered for presentation to the user; they aren't
-- further required by the simulation
type OneStepResult d = (World, Hive d, Int, Int)

-- performs n steps of simulation with an IO action tied into the logic so
-- that
simulateNIO :: Int -> (Int -> OneStepResult d -> IO ()) -> (World, Hive d) ->
               IO (Int, (World, Hive d))
simulateNIO numSteps printer s0 = simulateA 0 s0
    where simulateA n s0@(w0,h0)
            | numSteps <= n || population h0 == 0 = return (n, s0)
            | otherwise =
                do
                  let (w,h,cost,gathered) = simulate1 s0
                  printer (n + 1) (w,h,cost,gathered)
                  simulateA (n + 1) (w,h)


-- perform one step of the simulation
simulate1 :: (World,Hive d) -> OneStepResult d
simulate1 (w0, h0) = (w, h, expCost, gathered)
  where ((beeActions, numErase), newData) = strategy h0 (core h0)
        beeAssignment = zipWithRemainders (adults h0) beeActions
        (w1,h1,expCost,gathered) =
          processActions beeAssignment w0 (eraseActions numErase h0)
        w = growFlowers w1
        h = ageStarveAndKill $ dropExcessDances $ setData newData h1



-- does a lot:
--   1. ignores beeactions that haven't been assigned to adults
--   2. performs expeditions (searches push results onto dance stack;
--      gathers bring back food that
--      is distributed evenly to all adults; newgrubs creates a new larva,
--      food of adult doing the action
--      is shared with new larva; search expeditions make bees hungrier,
--      those that gathered at all don't pay a food cost)
--   3. remembers which flowers have been harvested
--   4. drops old dances from hive-memory
-- Returns
--   1. updated world;
--   2. updated hive;
--   3. total expedition cost; and
--   4. total good gathered
processActions :: ([(Adult,BeeAction)], [Adult], [BeeAction]) ->
                  World -> Hive d -> OneStepResult d
processActions (abs, idle_ads, _) w0 h0 = (w1,h,expCost,foodGathered)
  where (w1,h1,expCost,foodGathered) =
          foldr processFullAction (w0,setAdults (reverse idle_ads) h0,0,0) abs
        h2 = setAdults (reverse $ adults $ h1) h1
        h = distributeFood foodGathered h2


processFullAction ::
  (Adult,BeeAction) -> (World,Hive d,Int,Int) -> (World,Hive d,Int,Int)
processFullAction (a@(Adult fd0),b) (w0,h0,cost0, allgathered0) =
  let (Adult fd,pa) = processAction (a,b) (mkPA w0 h0)
      w = paWorld pa
      h = paHive pa
      wds = paDances pa
      stepCount = nSteps pa
      searchCount = numSearches pa
      newFood = paGathered pa
      searchStepCost = searchCount + stepCount `div` stepCostDivisor
      fd' = fd - searchStepCost
  in
    -- have five new fields to consider:
    --  1. the returned adult
    --  2. the waggle dances from searches
    --  3. numsteps taken by action
    --  4. food gathered
    --  5. numsearches performed
    -- the adult might have to die if it has insufficient food to cover
    -- num searches + steps `div` 5
    if fd' <= 0 then -- didn't make it back
      (w, h, cost0, allgathered0)
    else
      let
        h' = setAdults (Adult fd' : adults h) $ foldr pushDance h wds
      in
        (w, h', cost0 + searchStepCost + (fd0 - fd), newFood + allgathered0)


processAction ::
  (Adult,BeeAction) -> ProcessAction d -> (Adult,ProcessAction d)
processAction (a@(Adult food), bee_action) pa =
  case bee_action of
    Idle -> (a, pa)
    NewLarva ->
      (Adult (food - food `div` 2), updHive (setLarvae newls) pa)
      where newls = Larva ageToMaturity (food `div` 2) : larvae (paHive pa)
    Expedition exp -> processExpedition a exp pa

processExpedition :: Adult -> Expedition ExpeditionAction ->
                     ProcessAction d -> (Adult, ProcessAction d)
processExpedition a path pa =
  processPath a (hiveLocation $ paHive pa) path pa

processPath :: Adult -> XYPosn -> Expedition ExpeditionAction ->
               ProcessAction d -> (Adult,ProcessAction d)
processPath a@(Adult food) xy0 path pa =
  case path of
    [] -> (a,incSteps (distance xy0 (hiveLocation (paHive pa))) pa)
    (dxdy, Gather gsz) : rest ->
      let xy = xy0 `vplus` dxdy
          paWithSteps = incSteps (distance xy0 xy) pa
          gathered = paGathered pa
      in
        if gathered >= carryingCapacity then
          processPath a xy rest paWithSteps
        else -- attempt to find flowers with food
          let sz = min (isqrt food) gsz
              (w1, gathered', cost) =
                gatherFood (paWorld pa,gathered) (xy, sz)
          in
            if gathered' > gathered then  -- got some food; no cost
              processPath a xy rest
                (paSetWorld w1 $ paSetGathered gathered' paWithSteps)
            else
              -- didn't get any food, world unchanged therefore;
              -- but make bee pay the cost
              processPath (Adult (food - cost)) xy rest paWithSteps
    (dxdy, Search) : rest ->
      let xy = xy0 `vplus` dxdy
          (numFlowers,totFood) = search (paWorld pa) xy
          wd = (xy `vminus` (hiveLocation $ paHive pa), numFlowers, totFood)
          pa' = if 0 < numFlowers then newWD wd pa else pa
      in
        processPath a xy rest (incSearches (incSteps (distance xy0 xy) pa'))

-- last three ints are numSteps, numSearches, foodGathered
type ProcessAction d = (World,Hive d,[WaggleDance],Int,Int,Int)

mkPA :: World -> Hive d -> ProcessAction d
mkPA w h = (w,h,[],0,0,0)

paHive :: ProcessAction d -> Hive d
paHive (_,h,_,_,_,_) = h

paWorld :: ProcessAction d -> World
paWorld (w,_,_,_,_,_) = w

paSetWorld :: World -> ProcessAction d -> ProcessAction d
paSetWorld w' (w,h,wds,nsteps,nsearches,food) =
  (w',h,wds,nsteps,nsearches,food)

paGathered :: ProcessAction d -> Int
paGathered (_,_,_,_,_,g) = g

paSetGathered :: Int -> ProcessAction d -> ProcessAction d
paSetGathered f' (w,h,wds,nsteps,nsearches,_) =
  (w,h,wds,nsteps,nsearches,f')

updHive :: (Hive d -> Hive d) -> ProcessAction d -> ProcessAction d
updHive f (w,h,wds,nsteps,nsearches,food) =
  (w,f h,wds,nsteps,nsearches,food)

nSteps :: ProcessAction d -> Int
nSteps (_,_,_,ns,_,_) = ns

numSearches :: ProcessAction d -> Int
numSearches (_,_,_,_,ns,_) = ns

incSteps :: Int -> ProcessAction d -> ProcessAction d
incSteps n (w,h,wds,nsteps,nsearches,food) =
  (w,h,wds,nsteps+n,nsearches,food)

incSearches :: ProcessAction d -> ProcessAction d
incSearches (w,h,wds,nsteps,nsearches,food) =
  (w,h,wds,nsteps,nsearches+1,food)

newWD :: WaggleDance -> ProcessAction d -> ProcessAction d
newWD wd (w,h,wds,nsteps,nsearches,food) =
  (w,h,wd:wds,nsteps,nsearches,food)

paDances :: ProcessAction d -> [WaggleDance]
paDances (w,h,wds,nsteps,nsearches,food) = wds
