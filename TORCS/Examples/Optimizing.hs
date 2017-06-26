{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
module TORCS.Examples.Optimizing where

import FRP.Yampa

import TORCS.Types
import TORCS.Connect
import Control.Concurrent
import TORCS.Examples.SimpleExample
import Debug.Trace

optimizer :: IO ()
optimizer =  do
  let ds = map optimizingDriver [180,185..220]
  mapM (\d-> threadDelay 1000000 >> startDriver d) ds
  return ()


optimizingDriver :: Double -> Driver
optimizingDriver targetSpeed = proc e -> do
  CarState{..} <- arr getE -< e
  driveState <- myDriver targetSpeed -< e
  rec 
     lapTimes <- arr countLaps -< (lapTimes',lastLapTime)
     lapTimes' <- iPre [] -< lapTimes
  m <- arr (restarting targetSpeed) -< (lapTimes,curLapTime)
  returnA -< driveState {meta = m}
  
   
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
