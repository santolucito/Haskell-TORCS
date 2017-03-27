{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiWayIf #-}
module TORCS.Terms where

import Data.Maybe (fromJust)
import Data.List (findIndex)

initVals2 = (1,1,0)
initVals = (1,1)

steering :: Double -> Double -> Double
steering angle trackPos = let
  turns = angle*14 / pi
  centering = turns - ((trackPos)*0.2)
  clip x = max (-1) $ min x 1
 in
  clip centering

--lt :: Ord a => a -> a -> Bool
lt = (<)
--gt :: Ord a => a -> a -> Bool
gt = (>)
--minus :: Num a => a -> a -> a
minus = (-)
--plus :: Num a => a -> a -> a
plus = (+)
--multp :: Num a => a -> a -> a
multp = (*)


--pred speed

acceling accel brakes =
  accel ==1 

clearahead track = 
  (track !! 9) > 50
  
slow speed = 
  speed < 30

--fxns speed

deaccel = 0

hitbrakes = 0.1

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

speedup = 1
--pred 

atside trackpos =
  abs trackpos > 0.5

clearroad track =
  (track !! 9) > 90

closetobend track =
  (track !! 9) < 30

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
  
