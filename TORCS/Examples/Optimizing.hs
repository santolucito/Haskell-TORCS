{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
module TORCS.Examples.Optimizing where

import FRP.Yampa

import TORCS.Types
import TORCS.Connect
import Control.Concurrent

import Debug.Trace

optimizer :: IO ()
optimizer =  do
  let ds = map myDriver [100,105..200]
  mapM (\d-> threadDelay 1000000 >> startDriver d) ds
  return ()


myDriver :: Double -> Driver
myDriver targetSpeed = proc e -> do
    CarState{..} <- arr getE -< e
    g <- arr shifting -< speedX
    s <- arr steering -< (angle,trackPos)
    a <- arr (gas targetSpeed) -< (speedX,s)
    rec 
      lapTimes <- arr countLaps -< (lapTimes',lastLapTime)
      lapTimes' <- iPre [] -< lapTimes
    m <- arr (restarting targetSpeed) -< (lapTimes,curLapTime)
    returnA -< defaultDriveState {accel = a, gear = g, steer = s, meta = m}

--A small wrapper to keep track of how long each lap took
--TODO move this to core library?
countLaps :: ([Double],Double) -> [Double]
countLaps (lapTs,lastT) = if 
  | lastT == 0 -> []
  | lastT>0 && (length lapTs == 0 || lastT /= head lapTs) -> lastT : lapTs
  | otherwise -> lapTs

restarting :: Double -> ([Double],Double) -> Int
restarting s (lapTs,ct) = 
  if (length lapTs > 3) || ct > 200 then trace ((show $ sum lapTs)++" - "++show s) 1 else 0
  --TODO replace debug with actual output

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
  
gas :: Double -> (Double,Double) -> Double
gas targetSpeed (speed,steer) = 
  if speed < (targetSpeed-(steer*50)) then 0.5 else 0
  
getE :: Event CarState -> CarState
getE  e = case e of
  NoEvent -> defaultCarState -- if no data, default
  Event i -> i
