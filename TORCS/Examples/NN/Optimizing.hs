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
import TORCS.Examples.NN.Simple
import Debug.Trace


import System.Process


optimize :: Thetas
optimize = 
  multiVarSGD 
    [speed, turning]
    2 0.1 0.001 
    (Thetas {_speed=160.0,_turning=14}) (Thetas {_speed=0,_turning=0}) evalDriver

evalDriver :: Thetas -> IO Double
evalDriver t@Thetas{..} = do
  print $ "Attemping with params: "++(show t)
  endState <- (startDriver $ threeLapDriver t)
  print $ "Cost = "++(show $ calcCost endState)++" with params: "++(show t)
  runCommand $ "echo "++(show _speed)++","++(show $ calcCost endState)++">> test.csv"
  return $ calcCost endState

-- | What was the 'cost' of this driver based on its final states
calcCost :: (CarState,DriveState) -> Double
calcCost (c, d) =
  (damage c) + (curLapTime c)

dragRacer:: Thetas -> Driver
dragRacer t = proc e -> do
  CarState{..} <- arr getE -< e
  driveState <- myDriver t -< e
  m <- arr restarting -< (distRaced,curLapTime)
  returnA -< driveState {meta = m}

-- restart after 250m or time out
restarting :: (Double,Double) -> Int
restarting (dist,ct) = 
    if (dist>= 250) || ct > timeout then 1 else 0
  where
    timeout = 20
