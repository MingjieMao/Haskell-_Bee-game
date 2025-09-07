{-

Main

Uses facilities students do not need to understand or alter.

Creates a command-line utility to run simulations on choice of world
file, with user able to control number of adults initially present in
hive.

Type

  runhaskell Main -h

to see options.

Alternatively, can build a faster version by using

  make

in the terminal, followed by

  ./beesimulate -h

In other words, runhaskell Main and ./beesimulate are equivalent, but
generating ./beesimulate takes an extra step (calling "make"), which you need
to do every time you change your source files.

-}


module Main
where

import MapReader
import Strategies
import Hive
import World
import QTree
import Tup4
import Geometry
import Simulator
import System.Environment
import Text.Read
import System.IO (hPutStrLn, stderr)
import System.Exit (die, exitSuccess)
import System.Console.GetOpt

simulateFile :: ClineOptions -> String -> IO (World, Hive Int)
simulateFile opts filename =
  let
      strat = if idleStrat opts then idle else test
      defaultPrint stepnum (w,h,cost,gathered) =
        putStr (stepStr ++ hiveStr ++ worldStr ++ statsStr ++
                " " ++ dancesStr ++ "\n\n")
        where
          stepStr = "Step " ++ show stepnum ++ ": "
          hiveStr = simpleHiveToString h ++ "\n"
          worldStr = case showWorlds opts of
                       Nothing -> ""
                       Just freq -> if stepnum `mod` freq == 0 then showWorld w
                                    else ""
          statsStr = "Exp+Mouths+Fd: -" ++ show cost ++ "-" ++
                     show mouths ++ "+" ++ show gathered ++ "=" ++
                     show (gathered-(mouths + cost))
          dancesStr = "Ds: " ++ dancesToString h
          mouths = length (adults h) + length (larvae h)
      p = if quiet opts then (\_ _ -> return ()) else defaultPrint
  in
    do
      w0 <- readFromFile filename
      hpos <- case filterQT (\we -> we == Hive) w0 of
                   [(xy,_)] ->
                     do
                       if not $ quiet opts 
                       then putStrLn ("Hive at " ++ show xy)
                       else return ()
                       return xy
                   [] -> die "No hive in loaded world"
                   _ -> die "Too many hives in loaded world"
      let h0 = newHive (numAdults opts) (strat,0) hpos
      (steps, (_,h)) <- simulateNIO (numSteps opts) p (w0, h0)
      if population h == 0 then
         putStrLn ("Hive died out after " ++ show steps ++ " steps")
      else
         putStrLn ("Hive still not dead after " ++ show steps ++ " steps")
      exitSuccess


processFlags :: [(a -> IO a)] -> a -> IO a
processFlags flgs a = case flgs of
  [] -> return a
  f:fs -> do
            a' <- f a
            processFlags fs a'

-- show's usage message, and then exits with failure / success depending on
-- value of flag
usageThen :: Bool -> IO a
usageThen b =
  do
    nm <- getProgName
    let msg = usageInfo (nm ++ " [options] worldfilename") clineFlags
    if b then do
      putStrLn msg
      exitSuccess
    else die msg

main =
  do
    clargs <- getArgs
    let (flags, args, errs) = getOpt RequireOrder clineFlags clargs
    case errs of
      [] -> doit flags args
      _ -> die (concat errs)
  where doit fs as =
         do
           options <- processFlags fs initialOptions
           if help options then usageThen True
           else
             case as of
               [filename] -> simulateFile options filename
               _ -> usageThen False


data ClineOptions = ClineOptions {
   numAdults :: Int,
   help :: Bool,
   showWorlds :: Maybe Int,
   idleStrat :: Bool,
   numSteps :: Int,
   quiet :: Bool
   }

initialOptions :: ClineOptions
initialOptions = ClineOptions {
  numAdults = 10,
  numSteps = 10,
  help = False,
  showWorlds = Nothing,
  idleStrat = False,
  quiet = False
}

clineFlags :: [OptDescr (ClineOptions -> IO ClineOptions)]
clineFlags =
  [ Option "h?" ["help"]
       (NoArg (\opts -> return $ opts {help = True}))
       "Show help message (only)"
  , Option "a" ["numAdults"]
       (ReqArg (\nstr opts ->
                       case readMaybe nstr of
                         Nothing -> die ("Bad number for -a option: " ++ nstr)
                         Just n -> return $ opts {numAdults = n})
         "adult-count"
       )
       "No. starting adults (dflt = 10)"
  , Option "i" [] (NoArg (\opts -> return $ opts {idleStrat = True}))
       "Use idle strategy"
  , Option "n" []
       (ReqArg (\nstr opts ->
                       case readMaybe nstr of
                         Nothing -> die ("Bad number for -n option: " ++ nstr)
                         Just n -> return $ opts {numSteps = n})
         "step-count"
       )
       "No. simulation steps (dflt = 10)"
  , Option "q" ["quiet"]
       (NoArg (\opts -> return $ opts {quiet = True}))
       "Quiet sim.; only shows verdict"
  , Option "w" ["showWorlds"]
       (OptArg
         (\stropt opts ->
             case stropt of
               Nothing -> return $ opts {showWorlds = Just 1}
               Just str -> case readMaybe str of
                   Nothing -> die ("Bad number for -w option: " ++ str)
                   Just i -> if i <= 0 then
                               die ("Bad number for -w option: " ++ str)
                             else
                               return $ opts {showWorlds = Just i})
         "Print-freq.")
       "Show worlds while printing"
  ]