{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
module TORCS.Examples.ComplexExample where

import FRP.Yampa

import TORCS.Types
import TORCS.Connect

import Debug.Trace

import Control.Concurrent

soloDrive :: IO ()
soloDrive = do 
  let ds = map myDriver [230,235..300]
  mapM (\d-> threadDelay 1000000 >> startDriver d) ds
  return ()

myDriver :: Double -> Driver
myDriver maxSpeed = proc CarState{..} -> do
    rec
       gear  <- arr shifting -< (rpm,gear')
       steer <- arr (steering 0) -< (track,angle,trackPos)
       (a,b) <- arr (gas maxSpeed) -< (track,speedX,steer,trackPos)
       m     <- arr (restarting maxSpeed) -< (lastLapTime,curLapTime)
    returnA -< defaultDriveState {accel = a, gear = gear, steer = steer, brakes = b, meta = m}

restarting :: Double -> (Double,Double) -> Int
restarting s (lapT,ct) = 
  --if trace (show lapT++" "++show ct) $ lapT > 0 || ct > 200 then trace (show lapT++" - "++show s) 0 else 0
  0

shifting :: (Double,Int) -> Int
shifting (rpm,g) = if 
  | rpm > 7000 -> min 6 (g+1)
  | rpm < 3000 -> max 1 (g-1)
  | otherwise  -> g
 
steering :: Double -> ([Double],Double,Double) -> Double
steering offset (track,angle,trackPos) = let
  turns = angle*14 / pi
  centering = turns - ((trackPos+offset)*0.1)
  clip x = max (-1) $ min x 1
  offtrack = any (==(-1)) track
 in
  if 
    | offtrack -> clip $ if trackPos > 5 then (trackPos/9) * (-1) else (trackPos/18) * (-1)
    | frontDist track > 190 -> 0 
    | otherwise -> clip centering
  
frontDist :: [Double] -> Double
frontDist track = if length track == 19 then track !! 9 else 0
front3 track = map floor $ if length track == 19 then slice 8 10 track else [0]
--inclusive bounds
slice begin end = take (end - (begin-1)) . drop (begin-1)

gas :: Double -> ([Double],Double,Double,Double) -> (Double,Double)
gas targetSpeed (track,speed,steer,trackPos) = let
    fd = frontDist track
    approachingTurn = fd < 100
    braking = approachingTurn && speed > (max 30 (targetSpeed-(2200/fd)))
    turning = frontDist track < 70 && any (>100) track
    offtrack = any (==(-1)) track && (abs trackPos) > 2 
    out = 
     if 
       | offtrack -> "offtrack"
       | braking  -> "braking"++(show $ front3 track)
       | turning  -> "turning"++(show steer)
       | approachingTurn -> "approaching turn"++(show $ front3 track)
       | otherwise -> "straightaway"
  in 
    --trace out $ if 
    if 
      | offtrack        -> (0.5,0)
      | braking         -> (0,1)
      | turning         -> (1,0)
      | approachingTurn -> if speed > 120 then (0,(150-fd)/150) else (0.4,0)
      | otherwise       -> (1,0)
      
