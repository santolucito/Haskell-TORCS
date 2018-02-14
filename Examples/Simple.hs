{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import FRP.Yampa

import TORCS

import Debug.Trace

main :: IO ()
main = (startGUIDriver $ myDriver 100) >>= print 

myDriver :: Double -> Driver
myDriver targetSpeed = proc CarState{..} -> do
    g <- arr shifting -< (rpm,gear')
    s <- arr steering -< (angle,trackPos)
    a <- arr (gas targetSpeed) -< (speedX,s)
    m <- arr endRace -< (lapTimes,curLapTime)
    returnA -< defaultDriveState {accel = a, gear = g, steer = s, meta = m}

shifting :: (Double,Int) -> Int
shifting (rpm,g) = if 
  | rpm > 7000 -> min 6 (g+1)
  | rpm < 3000 -> max 1 (g-1)
  | otherwise  -> g

steering :: (Double,Double) -> Double
steering (spd,trackPos) = let
  turns = spd*14 / pi
  centering = turns - (trackPos*0.1)
  clip x = max (-1) $ min x 1
 in
  clip centering
  
gas :: Double -> (Double,Double) -> Double
gas targetSpeed (speed,steer) = 
  if speed < (targetSpeed-(steer*50)) then 0.5 else 0

-- | Run 3 laps then stop the simulation
endRace :: ([Double],Double) -> Int
endRace (lapTs,ct) = 
    if (length lapTs >= 2) || ct > timeout then 1 else 0
  where
    timeout = 200
