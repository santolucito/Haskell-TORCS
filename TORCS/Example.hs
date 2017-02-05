{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
module TORCS.Example (soloDrive, twoPlayer) where

import FRP.Yampa

import TORCS.Types
import TORCS.Connect

import Debug.Trace

soloDrive :: IO ()
soloDrive = startDriver myDriver

twoPlayer = startDrivers [myDriver,myDriver]

myDriver :: Driver
myDriver = proc e -> do
    CarState{..} <- arr getE -< e
    rec
       oldG <- iPre 0 -< (traceShow communications g)
       g <- arr shifting -< (rpm,oldG)
       s <- arr steering -< (angle,trackPos)
       a <- arr gas -< (speedX,s)
       msg <- arr (\g->if g>4 then "im fast" else "im starting") -< g
    returnA -< defaultDriveState {accel = a, gear = g, steer = s, broadcast = msg}

targetSpeed = 150

shifting :: (Double,Int) -> Int
shifting (rpm,g) = if 
  | rpm > 6000 -> min 6 (g+1)
  | rpm < 2500 -> max 1 (g-1)
  | otherwise  -> g
 
steering :: (Double,Double) -> Double
steering (angle,trackPos) = let
  turns = angle*14 / pi
  centering = turns - ((trackPos)*0.2)
  clip x = max (-1) $ min x 1
 in
  clip centering
  
gas :: (Double,Double) -> Double
gas (speed,steer) = 
  if speed < (targetSpeed-((abs steer)*800)) then 0.8 else 0
  
getE :: Event CarState -> CarState
getE  e = case e of
  NoEvent -> defaultCarState -- if no data, default
  Event i -> i
