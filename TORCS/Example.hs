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
    g <- arr shifting -< speedX
    s <- arr steering -< (angle,trackPos)
    a <- arr gas -< (speedX,s)
    returnA -< defaultDriveState {accel = a, gear = g, steer = s}

targetSpeed = 100

shifting :: Double -> Int
shifting s = if 
  | s > 170 -> 6
  | s > 140 -> 5
  | s > 110 -> 4
  | s > 80 -> 3
  | s > 50 -> 2
  | s <= 50 -> 1
 
steering :: (Double,Double) -> Double
steering (spd,trackPos) = let
  turns = spd*14 / pi
  centering = turns - (trackPos*0.1)
  clip x = max (-1) $ min x 1
 in
  clip centering
  
gas :: (Double,Double) -> Double
gas (speed,steer) = 
  if speed < (targetSpeed-(steer*50)) then 0.5 else 0
  
getE :: Event CarState -> CarState
getE  e = case e of
  NoEvent -> defaultCarState -- if no data, default
  Event i -> i
