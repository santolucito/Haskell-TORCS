{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiWayIf #-}
module TORCS.Synth.Terms where

import Data.Maybe (fromJust)
import Data.List (findIndex)

initVal = (1)
initVals2 = (1,1)

-- | SteeringSimple
leftofcenter :: Double -> Bool
leftofcenter trackPos =
  trackPos > (0.01)

rightofcenter :: Double -> Bool
rightofcenter trackPos =
  trackPos < -(0.01)

turningleft :: Double -> Bool
turningleft steer =
  steer>0.001

turningright :: Double -> Bool
turningright angle =
  angle< -(0.001)

steerleft:: Double ->Double
steerleft angle =  14*angle/pi
steerright :: Double ->Double
steerright angle = 14*angle/pi

-- | SteeringAdvanced


--pred speed

acceling accel brakes =
  accel ==1 

clearahead track = 
  (track !! 9) > 150
  
slow speed = 
  speed < 30

--fxns speed
speedup = 1
deaccel = 0

hitbrakes = 1
offbrakes = 0

shiftbyrpm g rpm = if
  | rpm > 7000 -> min 6 (g+1)
  | otherwise  -> g
 
shiftbyspeed gear speed = if
  | speed > 200 -> 6
  | speed > 150 -> 5
  | speed > 100 -> 4
  | speed > 50  -> 3
  | speed > 25  -> 2
  | otherwise   -> 1

--pred 

atside trackpos =
  abs trackpos > 0.5

clearroad track =
  (track !! 9) > 150

closetobend track =
  (track !! 9) < 150

straight angle =
  abs angle < 0.03

--functions

steerside track = let
  l = (track !! 8)
  r = (track !! 10)
  m = maximum $ track
  i = fromJust $ findIndex ((==) m) $ track   
 in
  if m <= 8
  then (0.05)--turn left
  else -(0.05)

straighten angle =
  if angle>0
  then (0.001)
  else -(0.001)

turning = 0 
 
turnmod track = let
  l = maximum $ take 9 track
  r = maximum $ drop 9 track
 in
  if l > r then -(0.2) else 0.2 
