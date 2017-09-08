{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedWildCards #-}
module TORCS.Examples.Learning.Optimizing where

import FRP.Yampa hiding (derivative)

import TORCS.Types
import TORCS.Connect
import Control.Concurrent
import TORCS.Examples.Learning.Simple
import TORCS.Examples.Learning.SGD
import Debug.Trace

import System.Process
import System.IO.Unsafe

import System.Random.Shuffle
import System.Random

import Control.Lens

optimize :: Thetas
optimize = 
  multiVarSGD 
    [speed, turning]
    2 0.1 0.001 
    (Thetas {_speed=160.0,_turning=14}) (Thetas {_speed=0,_turning=0}) evalDriver

evalDriver :: Thetas -> IO Double
evalDriver t@Thetas{..} = do
  print $ "Attemping with params: "++(show t)
  endState <- startTORCS >> (startDriver $ threeLapDriver t)
  print $ "Cost = "++(show $ calcCost endState)++" with params: "++(show t)
  runCommand $ "echo "++(show _speed)++","++(show $ calcCost endState)++">> test.csv"
  return $ calcCost endState

-- | What was the 'cost' of this driver based on its final states
calcCost :: (CarState,DriveState) -> Double
calcCost (c, d) =
  (damage c) + (sum $ lapTimes c)

-- | have to wait for torcs to start before we try to connect
--   TODO use some shell stuff to get the status of torcs (or the port it opens)
startTORCS = 
  spawnProcess "torcs" ["-r /home/mark/.torcs/config/raceman/practice.xml"] >> --NB torcs requires full path
  threadDelay 100000  

threeLapDriver :: Thetas -> Driver
threeLapDriver t = proc e -> do
  CarState{..} <- arr getE -< e
  driveState <- myDriver t -< e
  m <- arr restarting -< (lapTimes,curLapTime)
  returnA -< driveState {meta = m}

-- restart after 3 laps or if one lap times out (200 sec here) 
restarting :: ([Double],Double) -> Int
restarting (lapTs,ct) = 
    if (length lapTs > 3) || ct > timeout then 1 else 0
  where
    timeout = 200
