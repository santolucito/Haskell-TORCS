{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
module TORCS.Example where

import FRP.Yampa

import TORCS.Types
import TORCS.Connect

import Debug.Trace

main :: IO ()
main = startDriver myDriver

myDriver :: Driver
myDriver = proc e -> do
    CarState{..} <- arr getE -< e
    rec
       oldG <- iPre 0 -< traceShow g g
       g <- arr shifting -< (rpm,oldG)
       s <- arr steering -< (angle,trackPos)
       a <- arr gas -< (speedX,s)
    returnA -< defaultDriveState {accel = a, gear = g, steer = s}

targetSpeed = 150

shifting :: (Double,Int) -> Int
shifting (rpm,g) = if 
  | rpm > 6000 -> min 6 (g+1)
  | rpm < 2000 -> max 1 (g-1)
  | otherwise  -> g
 
steering :: (Double,Double) -> Double
steering (spd,trackPos) = let
  turns = spd*17 / pi
  centering = turns - ((trackPos)*0.3)
  clip x = max (-1) $ min x 1
 in
  clip centering
  
gas :: (Double,Double) -> Double
gas (speed,steer) = 
  if speed < (targetSpeed-(steer*280)) then 0.8 else 0
  
getE :: Event CarState -> CarState
getE  e = case e of
  NoEvent -> defaultCarState -- if no data, default
  Event i -> i
