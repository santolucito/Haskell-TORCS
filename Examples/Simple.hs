{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import FRP.Yampa

import TORCS.Types
import TORCS.Connect

main :: IO ()
main = startDriver_ $ myDriver 100

myDriver :: Double -> Driver
myDriver targetSpeed = proc e -> do
    CarState{..} <- arr getE -< e
    g <- arr shifting -< (rpm,gear')
    s <- arr steering -< (angle,trackPos)
    a <- arr (gas targetSpeed) -< (speedX,s)
    returnA -< defaultDriveState {accel = a, gear = g, steer = s}

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
  
getE :: Event CarState -> CarState
getE  e = case e of
  NoEvent -> defaultCarState -- if no data, default
  Event i -> i
