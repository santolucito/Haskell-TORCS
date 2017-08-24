{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
module TORCS.Examples.Optimizing where

import FRP.Yampa hiding (derivative)

import TORCS.Types
import TORCS.Connect
import Control.Concurrent
import TORCS.Examples.SimpleExample
import Debug.Trace

import System.Process
import System.IO.Unsafe



optimize :: Double
optimize = gradientDescent 0.1 0.001 160.0 (-1.0) evalDriver

gradientDescent :: Double -> Double -> Double -> Double -> (Double -> IO Double) -> Double
gradientDescent goal learnRate x x_prev f
  | ((traceMe $ abs(x-x_prev)) > goal) = gradientDescent goal learnRate (x - learnRate * (unsafePerformIO $ derivative f x)) x f
  | otherwise              = x

-- TODO there must be a better way
-- but for now just move in one direction and interpolate 
derivative :: (Double -> IO Double) -> Double -> IO Double
derivative f init = do
  x1 <- f init
  x2 <- f (init + (0.01 * init))
  return $ ((x2 - x1) / 0.01)
  

evalDriver :: Double -> IO Double
evalDriver speed = do
  print $ "Attemping with speed = "++(show speed)
  endState <- startTORCS >> (startDriver $ threeLapDriver speed)
  print $ "Cost at speed = "++(show speed)++" is cost = "++(show $ calcCost endState)
  runCommand $ "echo "++(show speed)++","++(show $ calcCost endState)++">> test.csv"
  return $ calcCost endState

-- What was the 'cost' of this driver based on its final states
calcCost :: (CarState,DriveState) -> Double
calcCost (c, d) =
  (damage c) + (sum $ lapTimes c)

{-optimizer :: IO ()
optimizer =  do
  let ds = map threeLapDriver [150,160..220]
  costs <- mapM (\d-> startTORCS >> startDriver d) ds
  print costs
  return () -}

--have to wait for torcs to start before we try to connect
--TODO use some shell stuff to get the status of torcs (or the port it opens)
startTORCS = 
  spawnProcess "torcs" ["-r /home/mark/.torcs/config/raceman/practice.xml"] >> 
  threadDelay 50000  

threeLapDriver :: Double -> Driver
threeLapDriver targetSpeed = proc e -> do
  CarState{..} <- arr getE -< e
  driveState <- myDriver targetSpeed -< e
  m <- arr (restarting targetSpeed) -< (lapTimes,curLapTime)
  returnA -< driveState {meta = m}

-- restart after 3 laps or if one lap times out (200 sec here) 
restarting :: Double -> ([Double],Double) -> Int
restarting s (lapTs,ct) = 
    if (length lapTs > 3) || ct > timeout then 1 else 0
  where
    timeout = 200
