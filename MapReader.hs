module MapReader
where

import Geometry
import QTree
import World
import Data.Char
import System.IO

stringToWRow :: String -> [Maybe WorldElem]
stringToWRow s =
  concat $ map categorise $ words s
  where categorise s = case s of
          'H' : _ -> [Just Hive]
          '.' : _ -> map (\ _ -> Nothing) s
          c : _ -> if isDigit c then
                     let i::Int = read s
                     in [Just (Flower i i)]
                   else error ("MapReader: Bad character " ++ [c])

stringsToWMatrix ss = map stringToWRow ss

type Inserter = (XYPosn -> WorldElem -> World -> World)

normMatrix :: Inserter -> [String] -> World
normMatrix insert ss =
  let
    m = stringsToWMatrix ss
    d = max (length m) (foldl max 0 $ map length m)
    half = d `div` 2
    ll = (-half, -half)
    rowfold r (i,qt) re = case re of
                            Nothing -> (i+1,qt)
                            Just e -> (i + 1, insert (i,r) e qt)
    fold (rnumber, qt) row =
      (rnumber+1, snd $ foldl (rowfold rnumber) (-half, qt) row)
    (_, qt0) = foldl fold (-half, emptyQT (ll,d)) $ reverse m
  in
    qt0

readStream :: Inserter -> Handle -> IO (QTree WorldElem)
readStream insert h =
  do
    ls <- getLines []
    return (normMatrix insert ls)
   where getLines a = do
           eofp <- hIsEOF h
           if eofp then return $ reverse a
           else
             do
               l1 <- hGetLine h
               getLines (l1:a)


readFromFile :: String -> IO (QTree WorldElem)
readFromFile s = readFromFileWithInserter insertQT s

readFromFileWithInserter :: Inserter -> String -> IO World
readFromFileWithInserter ins s = withFile s ReadMode (readStream ins)

-- | MapReader
--
-- >>> w <- readFromFile "w1.txt"
-- >>> tightSearch w (-6,5)
-- (2,2)
-- >>> looseSearch w (-7,-8)
-- 3
-- >>> looseSearch w (-7,5)
-- 3
-- >>> looseSearch w (6,6)
-- 2
-- >>> tightSearch w (6,6)
-- (2,14)