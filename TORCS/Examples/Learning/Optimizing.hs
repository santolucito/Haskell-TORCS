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
import Debug.Trace

import System.Process
import System.IO.Unsafe

import Control.Lens

optimize :: Thetas
optimize = 
  multiVarGD 
    0.1 0.001 
    (Thetas {_speed=160.0,_turning=14}) (Thetas {_speed=0,_turning=0}) evalDriver

allLenses = [speed, turning]

-- | Use data type Theta for params to be passed to driver
multiVarGD :: Double -> Double -> Thetas -> Thetas -> (Thetas -> IO Double) -> Thetas
multiVarGD goal learnRate t t_prev f =
  let
    --TODO better convergance fxn
    converged = (abs((_speed t)-(_speed t_prev)) <= goal)
    continueGD = multiVarGD goal learnRate updatedThetas t f
    -- NB use a fixed theta for takeStep, but update a seperate copy of theta in the fold
    updatedThetas = foldr (takeStep learnRate t f) t allLenses
  in
    if not converged
    then continueGD
    else t

-- TODO get rid of unsafe here? (actually its fine here, but would be a nice exercise to fix)
takeStep :: Double -> Thetas -> (Thetas -> IO Double) -> _Lens -> Thetas -> Thetas
takeStep learnRate t f part updatedTheta = 
  over part (\x -> x - learnRate * (unsafePerformIO $ partialDerivative f part t)) updatedTheta

-- TODO there must be a better way
-- but for now just move in one direction and interpolate 
partialDerivative :: (Thetas -> IO Double) -> _Lens -> Thetas -> IO Double
partialDerivative f part t = do
  x1 <- f t
  x2 <- f (over part (\x->x*1.01) t)
  return $ ((x2 - x1) / 0.01)

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
  spawnProcess "torcs" ["-r /home/mark/.torcs/config/raceman/practice.xml"] >> 
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
