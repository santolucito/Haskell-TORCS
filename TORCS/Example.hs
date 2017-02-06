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

twoPlayer = startDrivers [myDriver,myDriver2]

myDriver2 :: Driver
myDriver2 = proc e -> do
    CarState{..} <- arr getE -< e
    rec
       g <- arr shifting -< (rpm,gear')
       s <- arr (steering 0) -< (angle,trackPos)
       a <- arr (gas 210) -< (speedX,s)
       msg <- arr (\g->if g>4 then "im fast" else "im starting") -< g
    returnA -< defaultDriveState {accel = a, gear = g, steer = s, broadcast = msg}

myDriver :: Driver
myDriver = proc e -> do
    CarState{..} <- arr getE -< e
    rec
       g <- arr shifting -< (rpm,gear')
       s <- arr (steering 0.1) -< (angle,trackPos)
       a <- arr (gas 210) -< (speedX,s)
       msg <- arr (\g->if g>4 then "im fast" else "im starting") -< g
    returnA -< defaultDriveState {accel = a, gear = g, steer = s, broadcast = msg}


shifting :: (Double,Int) -> Int
shifting (rpm,g) = if 
  | rpm > 5500 -> min 6 (g+1)
  | rpm < 3000 -> max 1 (g-1)
  | otherwise  -> g
 
steering :: Double -> (Double,Double) -> Double
steering offset (angle,trackPos) = let
  turns = angle*14 / pi
  centering = turns - ((trackPos+offset)*0.2)
  clip x = max (-1) $ min x 1
 in
  clip centering
  
gas :: Double -> (Double,Double) -> Double
gas targetSpeed (speed,steer) = 
  if speed < (targetSpeed-((abs steer)*1800)) then 0.8 else 0
  
getE :: Event CarState -> CarState
getE  e = case e of
  NoEvent -> defaultCarState -- if no data, default
  Event i -> i
